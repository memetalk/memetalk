#include "process.hpp"
#include "vm.hpp"
#include "log.hpp"
#include "mmobj.hpp"
// #include "utils.hpp"
// #include "core_image.hpp"
#include "log.hpp"

#include <iostream>
#include <string>
#include <re2/re2.h>
#include <re2/set.h>
#include <vector>

#include <gc_cpp.h>
#include "gc/gc_allocator.h"


#define DBG() _log << _log.yellow + _log.bold + "[prim|" << __FUNCTION__ << "] " << _log.normal
#define WARNING() MMLog::warning() << "[prim|" << __FUNCTION__ << "] " << _log.normal
#define ERROR() MMLog::error() << "[prim|" << __FUNCTION__ << "] " << _log.normal

static MMLog _log(LOG_PRIMS);


class Scanner {
public:
  Scanner(std::vector<std::string>& tokens, int total_keywords, int id, re2::StringPiece& input, re2::RE2::Set* set) :
    _tokens(tokens), _total_keywords(total_keywords), _tk_id(id), _input(input), _set(set) {};
  std::vector<std::string> _tokens;
  int _total_keywords;
  int _tk_id;
  re2::StringPiece _input;
  re2::RE2::Set* _set;
};

static int prim_re2_scanner_new(Process* proc) {
  oop self =  proc->dp();
  oop oop_token_list = proc->get_arg(0);
  oop oop_total_keywords = proc->get_arg(1);
  oop oop_id_tokens = proc->get_arg(2);
  oop oop_input = proc->get_arg(3);

  number token_list_size = proc->mmobj()->mm_list_size(proc, oop_token_list);

  re2::RE2::Set* set = new (GC) re2::RE2::Set(RE2::DefaultOptions, RE2::ANCHOR_START);

  std::vector<std::string> tokens;
  for (number i = 0; i < token_list_size; i++) {
    oop entry = proc->mmobj()->mm_list_entry(proc, oop_token_list, i);
    std::string tk = std::string("(") + proc->mmobj()->mm_string_stl_str(proc, entry) + ")";
    if (set->Add(tk, NULL) == -1) {
      proc->raise("TypeError", "malformed regular expression");
    }
    tokens.push_back(tk);
  }

  if (!set->Compile()) {
    proc->raise("TypeError", "error compiling regular expressions");
  }

  number input_size = proc->mmobj()->mm_string_size(proc, oop_input);
  char *dest = (char*) GC_MALLOC(sizeof(char) * input_size);

  memcpy(dest, proc->mmobj()->mm_string_cstr(proc, oop_input), input_size + 1);
  // std::cerr << "[[" << dest << "]]" << endl;
  re2::StringPiece input(dest);

  ((oop*)self)[2] = (oop) new (GC) Scanner(tokens, untag_small_int(oop_total_keywords), untag_small_int(oop_id_tokens), input, set);

  proc->stack_push(proc->rp());
  return 0;
}

static int prim_re2_scanner_match(Process* proc) {
  oop self =  proc->dp();

  Scanner* scanner = (Scanner*) (oop*)self[2];
  re2::StringPiece& input = scanner->_input;

  std::vector<int> v;
  if (!scanner->_set->Match(input, &v)) {
    proc->stack_push(MM_NULL); //match failed
    return 0;
  }

  oop ret = proc->mmobj()->mm_list_new();
  bool has_id = false;
  bool has_kw = false;
  for (std::vector<int>::iterator it = v.begin(); it != v.end(); it++) {
    // std::cerr << "prim:match found " << *it << endl;
    if (*it == scanner->_tk_id) {
      has_id = true;
    }
    if (*it < scanner->_total_keywords) {
      has_kw = true;
    }
  }
  // std::cerr << "prim:match kw/id? " << has_kw << has_id << endl;
  for (std::vector<int>::iterator it = v.begin(); it != v.end(); it++) {
    if (has_id && has_kw && *it == scanner->_tk_id) {
      //ignore id token when there are keywords in result set
    } else {
      proc->mmobj()->mm_list_append(proc, ret, tag_small_int(*it));
    }
  }
  proc->stack_push(ret);
  return 0;
  // //we should probably receive a `num` argument to chose from
  // //the vector which token we want to consume!
  // std::string txt;
  // if (!re2::RE2::Consume(&input, scanner->_tokens[v[0]], &txt)) {
  //   proc->raise("InternalError", "unable to consume previously matched string");
  // }

  // oop ret = proc->mmobj()->mm_list_new();
  // proc->mmobj()->mm_list_append(proc, ret, tag_small_int(v[0]));
  // proc->mmobj()->mm_list_append(proc, ret, proc->mmobj()->mm_string_new(txt));
  // proc->stack_push(ret);
  return 0;
}

static int prim_re2_scanner_consume(Process* proc) {
  oop self =  proc->dp();
  oop oop_token = proc->get_arg(0);

  Scanner* scanner = (Scanner*) (oop*)self[2];
  re2::StringPiece& input = scanner->_input;

  // std::vector<int> v;
  // if (!scanner->_set->Match(input, &v)) {
  //   proc->stack_push(MM_NULL); //match failed
  //   return 0;
  // }
  int token = untag_small_int(oop_token);
  // if (std::find(v.begin(), v.end(), token) == v.end()) {
  //   proc->stack_push(MM_NULL); // match failed
  //   return 0;
  // }

  //we should probably receive a `num` argument to chose from
  //the vector which token we want to consume!
  std::string txt;
  if (!re2::RE2::Consume(&input, scanner->_tokens[token], &txt)) {
    proc->raise("InternalError", "unable to consume previously matched string");
  }

  // std::cerr << "prim:consumed '" << txt << "' from: " << scanner->_tokens[token] << endl;
  proc->stack_push(proc->mmobj()->mm_string_new(txt));
  return 0;
}

static int prim_re2_scanner_match_regexp(Process* proc) {
  oop self =  proc->dp();
  oop oop_rx = proc->get_arg(0);

  Scanner* scanner = (Scanner*) (oop*)self[2];
  re2::StringPiece& input = scanner->_input;

  std::string txt;
  std::string expr = std::string("(") + proc->mmobj()->mm_string_stl_str(proc, oop_rx) + ")";

  RE2 re(expr);
  if (!re.ok()) {
    proc->raise("TypeError", "malformed regular expression");
  }

  // std::cerr << "match_regexp: " << expr << endl;
  if (!re2::RE2::Consume(&input, re, &txt)) {
    proc->stack_push(MM_NULL);
    return 0;
  }
  // std::cerr << "match_regexp: result: " << txt << endl;
  proc->stack_push(proc->mmobj()->mm_string_new(txt));
  return 0;
}

void re2_init_primitives(VM* vm) {
  vm->register_primitive("re2_scanner_new", prim_re2_scanner_new);
  vm->register_primitive("re2_scanner_match", prim_re2_scanner_match);
  vm->register_primitive("re2_scanner_consume", prim_re2_scanner_consume);
  vm->register_primitive("re2_scanner_match_regexp", prim_re2_scanner_match_regexp);
}
