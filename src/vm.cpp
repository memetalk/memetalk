#include "vm.hpp"
#include "process.hpp"
#include "core_image.hpp"
#include "mec_image.hpp"
#include "defs.hpp"
#include "mmobj.hpp"
#include "prims.hpp"
#include "utils.hpp"
#include "mec_fun.hpp"
#include <cstdlib>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

#include <gc_cpp.h>
#include "gc/gc_allocator.h"

#define DBG(...) if(_log._enabled) { _log << _log.magenta + _log.bold + "[VM|" << __FUNCTION__ << "] " << _log.normal << __VA_ARGS__; }

#define WARNING() MMLog::warning() << "[VM|" << __FUNCTION__ << "] " << _log.normal
#define ERROR() MMLog::error() << "[VM|" << __FUNCTION__ << "] " << _log.normal


VM::VM(int argc, char** argv, bool online, bool profile)
: _log(LOG_VM), _argc(argc), _argv(argv), _online(online), _profile(profile), _debugger_module(MM_NULL) {

  load_config();
  maybe_create_cache_dir(_mec_cache_directory);
  _core_image = new (GC) CoreImage(this, _core_img_filepath);
  _mmobj = new (GC) MMObj(_core_image);
}

void VM::print_retval(Process* proc, oop retval) {
  int exc;
  DBG(" ======== the end ======= " << endl)
  oop retval_str = proc->send_0(retval, new_symbol("toString"), &exc);
  DBG("RETVAL: " << retval << " => " << _mmobj->mm_string_cstr(proc, retval_str) << endl)
}

void VM::print_error(Process* proc, oop retval) {
  int exc;
  oop retval_str = proc->send_0(retval, new_symbol("toString"), &exc);
  ERROR() << "uncaught exception: " << retval << " => " << _mmobj->mm_string_cstr(proc, retval_str) << endl;
}

Process* VM::init() {
  Process* proc = new (GC) Process(this, 0, _online);
  _core_image->load();
  init_primitives(this); //module initialization could execute primitives but
                         //now we are using Symbol objects as entries in the
                         //primitives dict, and without core we don't have the
                         //class Symbol loaded to link objects to.  so lets
                         //just trust core never really `send` -- and probably
                         //shouldn't.
  _mmobj->init();
  return proc;
}



int VM::start() {
  if (_argc < 2) {
    bail("usage: meme <filepath.me> | -m <module>");
  }

  Process* proc = init();

  char* module_path;
  oop imod;
  if (strcmp(_argv[1], "-m") == 0) {
    //meme -m <repo:path@version>
    module_path = _argv[2];
    _first_argv = 2; //first memetalk-land argv
    try {
      imod = instantiate_meme_module(proc, module_path, _mmobj->mm_list_new());
    } catch(mm_exception_rewind e) {
      print_error(proc, e.mm_exception);
      return 1;//* (int*) (void*) &(e.mm_exception);
    }
  } else if (strcmp(_argv[1], "-c") == 0) {
    //meme -c <filepath.me>
    compile(proc, _argv[2]);
    return 0;
  } else {
    //meme <filepath.me> We might need to compile it
    maybe_compile_local_source(proc, _argv[1]);
    imod = instantiate_local_module(proc, _argv[1], _mmobj->mm_list_new());
    _first_argv = 1; //first memetalk-land argv
  }

  int exc;
  oop retval = proc->run(imod, new_symbol("main"), &exc);
  if (_profile) {
    proc->report_profile();
  }
  if (!(exc == 0)) {
    proc->fail(retval);
    return 1;
  } else {
    print_retval(proc, retval);
    return 0;
  }
  //return * (int*) (void*) &retval;
}

std::pair<Process*, oop> VM::start_debugger(Process* target) {
  DBG("debugger for target: " << target << endl)

  static int debugger_id = 0;

  oop imod;
  Process* dbg_proc = new (GC) Process(this, ++debugger_id);
  if (_debugger_module != MM_NULL) {
    imod = _debugger_module;
  } else {
    try {
      imod = instantiate_meme_module(dbg_proc, "central:stdlib/remote_repl", _mmobj->mm_list_new());
    } catch(mm_exception_rewind e) {
      ERROR() << "uncaught exception while instantiating debugger module :(" << endl;
      dbg_proc->fail(e.mm_exception);
      // print_retval(e.mm_exception);
      // return * (int*) (void*) &(e.mm_exception);
    }
  }

  oop oop_target_proc = _mmobj->mm_process_new(target, target);
  int exc;
  oop handler = dbg_proc->send_1(imod, new_symbol("debug"), oop_target_proc, &exc);
  if (exc != 0) {
    dbg_proc->fail(handler);
  }
  return std::pair<Process*, oop>(dbg_proc, handler);
}

oop VM::new_symbol(const char* cstr) {
  std::string s = cstr;
  symbol_map_t::iterator it = _symbols.find(s);
  if (it == _symbols.end()) {
    oop sym = _mmobj->mm_symbol_new(cstr);
    _symbols[s] = sym;
    return sym;
  } else {
    return _symbols[s];
  }
}

oop VM::new_symbol(Process* p, oop str) {
  if (!(_mmobj->mm_object_vt(str) == _core_image->get_prime("String"))) {
    WARNING() << " expected String object, got:" << str << endl;
    p->raise("TypeError", "Expecting String");
  }
  return new_symbol(_mmobj->mm_string_cstr(p, str));
}

void VM::register_primitive(std::string name, prim_function_t fun) {
  _primitives[new_symbol(name.c_str())] = fun;
}

// prim_function_t VM::get_primitive(Process* proc, oop name) {
//   // DBG("VM::get_primitive " << name << endl)
//   // boost::unordered_map<std::string, prim_function_t>::iterator it = _primitives.find(name);
//   // if (it == _primitives.end()) {
//   //   ERROR() << "did not find primitive with name:" << name << endl;
//   //   proc->raise("InternalError", (std::string("primitive not found: ") + name).c_str());
//   // }
//   // return it->second;;
//   return _primitives.at(name);
// }

oop VM::instantiate_meme_module(Process* proc, const char* mod_path, oop module_args_list) {

  DBG( "instantiating repository module " << mod_path << endl);
  MECImage* mec;
  modules_map_t::iterator it = _modules.find(mod_path);
  if (it == _modules.end()) {
    DBG("loading new module " << mod_path << endl);
    int file_size;
    char* data = fetch_module(proc, mod_path, &file_size);
    mec = new (GC) MECImage(proc, _core_image, mod_path, file_size, data);
    _modules[mod_path] = mec;
    mec->load();
  } else {
    DBG("module already loaded " << mod_path << endl);
    mec = it->second;
  }
  return mec->instantiate_module(module_args_list);
}

oop VM::instantiate_local_module(Process* proc, std::string me_file_path, oop module_args_list) {
  std::string mec_file_path = me_file_path + "c";
  std::string mec_file_url = boost::filesystem::absolute(mec_file_path).string();

  DBG( "instantiating local file module " << mec_file_url << endl);
  MECImage* mec;

  modules_map_t::iterator it = _modules.find(mec_file_url);

  if (it == _modules.end()) {
    DBG("loading new module at filepath " << mec_file_path << endl);
    int file_size;
    char* data = read_mec_file(mec_file_path, &file_size);
    mec = new (GC) MECImage(proc, _core_image, mec_file_url, file_size, data);
    _modules[mec_file_url] = mec;
    mec->load();
  } else {
    DBG("module already loaded " << mec_file_url << endl);
    mec = it->second;
  }
  return mec->instantiate_module(module_args_list);
}


oop VM::recompile_fun(Process* proc, oop cfun, int line, const char* text, int* exc) {
  assert(0); //TODO
}

oop VM::compile_fun(Process* proc, const char* text, std::list<std::string> vars, oop cmod, int* exc) {
  assert(0); //TODO
}

void VM::bail(const std::string& msg) {
  std::cerr << msg << std::endl;
  exit(1);
}

void VM::bail() {
  exit(1);
}

oop VM::get_compiled_module(Process* proc, std::string name) {
  modules_map_t::iterator it = _modules.begin();
  name = std::string("/") + name;
  for ( ; it != _modules.end(); it++) {
    if (it->first.find(name) != std::string::npos) { //FIXME
      return it->second->compiled_module();
    }
  }
  if (_modules.find(name) == _modules.end()) {
    proc->raise("KeyError", (std::string("module not found: ") + name).c_str());
  }
  return _modules[name]->compiled_module();
}

char* VM::get_argv(int i) {
  i += _first_argv; //skips vm and vm flags
  if (i < _argc) {
    return _argv[i];
  }
  return NULL;
}


#include <sstream>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
using boost::property_tree::ptree;

void VM::load_config() {
  //0: $MEME_CONFIG
  //1: $PWD/meme.config
  //2: /etc/meme.config
  ptree pt;
  char* meme_config_env_path = getenv("MEME_CONFIG");
  char pwdc[PATH_MAX];
  getcwd(pwdc, PATH_MAX);

  if (meme_config_env_path) {
    DBG("Reading meme.config set in MEME_CONFIG env var: " << meme_config_env_path << endl);
    boost::property_tree::read_json(meme_config_env_path, pt);
  } else {
    try {
      DBG("Trying meme.config in current directory" << endl);
      boost::property_tree::read_json(std::string(pwdc) + "/meme.config", pt);
    } catch(...) {
      DBG("Unable to read meme.config in current directory" << endl);
      try {
        DBG("Trying meme.config in /etc" << endl);
        boost::property_tree::read_json("/etc/meme.config", pt);
      } catch(...) {
        std::cerr << "fatal error:\n\tUnable to read configuration file\n";
        std::cerr << "\tTried:\n";
        std::cerr << "\t* " << "$MEME_CONFIG\n";
        std::cerr << "\t* " << pwdc << "/meme.config\n";
        //std::cerr << "\t* " << home << "/.meme.config\n";
        std::cerr << "\t* " << "/etc/meme.config\n";
        bail();
      }
    }
  }

  try {
    ptree node = pt.get_child("repositories");
    boost::property_tree::ptree::iterator iter;
    for(iter = node.begin(); iter != node.end(); iter++) {
      DBG("config.repositories " << iter->first << " -> " << iter->second.data() << endl);
      _repo_locations[iter->first] = iter->second.data();
    }

    node = pt.get_child("override_to_local");
    for(iter = node.begin(); iter != node.end(); iter++) {
      DBG("override_to_local: " << iter->first << " -> " << iter->second.data() << std::endl);
      _repo_override[iter->first] = iter->second.data();
    }

    _system_path = pt.get_child("system_path").data();
    DBG("config.system_path: " << _system_path << endl);
    _mec_cache_directory = _system_path + "/cache";
    DBG("cache_path: " << _mec_cache_directory << endl);
    _core_img_filepath = pt.get_child("core_path").data();
    DBG("config.core_path: " << _core_img_filepath << endl);
  } catch(...) {
    bail("Error reading config data");
  }
}

char* VM::fetch_module(Process* proc, const std::string& mod_path, int* file_size) {
  DBG("fetch module '" << mod_path << "'" << std::endl);

  std::vector<std::string> strs;
  boost::split(strs, mod_path, boost::is_any_of(":"));
  std::string repo_name = strs[0]; //e.g. central
  std::string repo_path = strs[1]; //e.g. foo/0.0.1/bar

  boost::split(strs, repo_path, boost::is_any_of("/"));
  // boost::filesystem::path::iterator mrepo_path_it = boost::filesystem::path(repo_path).begin();
  std::string package_name = strs[0];  //e.g. foo
  std::string package_id = strs[0] + "-" + strs[1]; //e.g. foo-0.0.1

  DBG("repo name: '" << repo_name
      << "' repo_path: '" << repo_path
      << "' package: '" << package_name
      << "' package_id: '" << package_id << "'" << endl);

  //1-check for overriding rules
  boost::unordered_map<std::string, std::string>::iterator it;
  for (it = _repo_override.begin(); it != _repo_override.end(); it++) {
    DBG("looking for overriding rules: '" << it->first << "'" << endl);
    if (repo_name == it->first) {
      std::string local_path = it->second + mod_path.substr(it->first.length());
      DBG("translating " << mod_path << " to local path " << local_path << std::endl);
      return read_mec_file(local_path + ".mec", file_size);
    }
  }

  for (it = _repo_locations.begin(); it != _repo_locations.end(); it++) {
    if (repo_name == it->first) { //found a repository key

      //1: compute cache directory for repository
      //2: check if module exists in cache
      //3:  if not, fetch package / unpack to cache
      //4: read and return .mec contents

      boost::filesystem::path cached_path = _mec_cache_directory;                 //e.g. ~/.memetalk/cache
      boost::filesystem::path cached_me_path = cached_path / (repo_path + ".me"); //e.g. ~/.memetalk/cache/foo/0.0.1/bar.me
      boost::filesystem::path cached_mec_path = cached_me_path.string() + "c";    //e.g. ~/.memetalk/cache/foo/0.0.1/bar.mec

      DBG("cached .me filepath: " << cached_me_path << endl);
      if (!boost::filesystem::exists(cached_me_path)) {
        DBG("cached .me does not exist. Fetching..." << endl);
        boost::filesystem::path repo_url = it->second;        //e.g. http://modules.memetalk.org/central
        repo_url /= package_name;
        repo_url /= package_id + ".tar.gz";                 //e.g. http://modules.memetalk.org/central/foo/foo-0.0.1.tar.gz
        DBG("repo url: " << repo_url << endl);
        std::stringstream cmd;

        //download package
        std::stringstream tmp;
        tmp << "/tmp/wget_meme_file_" << getpid() << ".tar.gz";
        cmd << "wget -nv -O " << tmp.str() << " " << repo_url.string();
        DBG("executing " << cmd.str() << endl);
        int res = system(cmd.str().c_str());
        if (res != 0) {
          bail(std::string("Could not fetch package ") + repo_url.string());
        }

        DBG("creating memetalk cache directory if doens't exist: " << cached_path << endl);
        boost::filesystem::create_directories(cached_path);

        cmd.str("");
        cmd << "tar xvfz " << tmp.str() << " -C " << _system_path;
        DBG("unpacking: " << cmd.str() << endl);
        res = system(cmd.str().c_str());
        if (res != 0) {
          bail(std::string("Could not unpack ") + tmp.str());
        }
      } else {
        DBG("cached .me exists" << endl);
      }
      maybe_compile_local_source(proc, cached_me_path.string());
      return read_mec_file(cached_mec_path.string(), file_size);
    }
  }
  bail(std::string("fatal error:\n\tmodule not found: ") + mod_path);
  return 0;
}

bool VM::is_mec_file_older_then_source(std::string src_file_path) {
  std::string mec_file_path = src_file_path + "c";

  boost::filesystem::path src_p(src_file_path);
  boost::filesystem::path mec_p(mec_file_path);
  if (boost::filesystem::exists(mec_file_path)) {
    std::time_t t_mec = boost::filesystem::last_write_time(mec_p) ;
    std::time_t t_src = boost::filesystem::last_write_time(src_p) ;
    return (t_mec < t_src);
  } else {
    return true;
  }
}

void VM::maybe_compile_local_source(Process* proc, std::string filepath) {
  DBG("maybe compile local: " << filepath << endl);
  if (!boost::filesystem::exists(filepath)) {
      bail(std::string("file not found: ") + filepath);
  }
  DBG("checking file age: " << filepath << endl);
  if (is_mec_file_older_then_source(filepath)) {
    DBG("perhaps recompile .mec file for " << filepath << endl);
    //avoid recursion
    if (_compile_map[filepath]) {
      //bail(std::string("trying to compile a module the compiler depends on: ") + filepath);
      return;
    }
    _compile_map[filepath] = true;
    compile(proc, filepath);
    _compile_map[filepath] = false;
  }
  DBG("END of maybe_compile_local_source for " << filepath << endl);
}


void VM::compile(Process* proc, std::string filepath) {
  std::string line;
  std::fstream file;
  DBG("opening to recompile  '" << filepath << "'" << endl);
  file.open(filepath.c_str(), std::fstream::in | std::fstream::binary);
  if (!file.is_open()) {
    bail(std::string("file not found: ") + filepath);
  }

  std::getline(file, line);
  if (line.find("meme") != 0) {
    bail(std::string("file is not a memetalk source: ") + filepath);
  }
  std::string compiler_module_path = line.substr(5);
  oop imod;
  try {
    DBG("instantiating compiler module : " << compiler_module_path << endl);
    imod = instantiate_meme_module(proc, compiler_module_path.c_str(), _mmobj->mm_list_new());
  } catch(mm_exception_rewind e) {
    DBG("uops\n");
    print_error(proc, e.mm_exception);
    bail();
  }
  DBG("compiling source with compiler: " << compiler_module_path << endl);
  int exc;
  oop res = proc->send_1(imod, new_symbol("compile"), _mmobj->mm_string_new(filepath), &exc);
  DBG("finished source with compiler: " << compiler_module_path << endl);
  if (exc) {
    DBG("compiler failed: " << compiler_module_path << endl);
    proc->fail(res);
    bail();
  }
}
