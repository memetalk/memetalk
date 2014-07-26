#include "defs.hpp"
#include "utils.hpp"
#include "report.hpp"
#include "mmc_image.hpp"
#include "core_image.hpp"
#include "mmobj.hpp"
#include "vm.hpp"

using namespace std;

word MMCImage::HEADER_SIZE = 5 * WSIZE;
word MMCImage::MAGIC_NUMBER = 0x420;

MMCImage::MMCImage(VM* vm, CoreImage* core_image, const std::string& name_or_path)
  : _vm(vm), _mmobj(vm->mmobj()), _core_image(core_image), _name_or_path(name_or_path) {
}

void MMCImage::load_header() {
  word magic_number = unpack_word(_data, 0 * WSIZE);
  _ot_size = unpack_word(_data,  1 * WSIZE);
  _er_size = unpack_word(_data, 2 * WSIZE);
  _es_size = unpack_word(_data, 3 * WSIZE);
  _names_size = unpack_word(_data,  4 * WSIZE);

  debug() << "Header:magic: " << magic_number << " =?= " << MMCImage::MAGIC_NUMBER << endl;
  debug() << "Header:ot_size: " << _ot_size << endl;
  debug() << "Header:er_size: " << _er_size << endl;
  debug() << "Header:es_size: " << _es_size << endl;
  debug() << "Header:names_size: " << _names_size << endl;
}

void MMCImage::link_external_references() {
  const char* base = _data;
  int start_external_refs = HEADER_SIZE + _names_size + _ot_size;

  for (int i = 0; i < _er_size; i += (2 * WSIZE)) {
    word name_offset = unpack_word(_data, start_external_refs + i);
    char* name = (char*) (base + name_offset);
    word obj_offset = unpack_word(_data, start_external_refs + i + WSIZE);
    word* obj = (word*) (base + obj_offset);
    // debug() << obj_offset << " - " << *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
    * (word*) (base + obj_offset) = (word) _core_image->get_prime(name);
    debug() << "External refs " << obj_offset << " - " << (oop) *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
  }
}

oop MMCImage::instantiate_class(oop class_name, oop cclass, oop cclass_dict, std::map<std::string, oop>& mod_classes, oop imodule) {
  char* cname = _mmobj->mm_string_cstr(class_name);
  debug() << "Instantiating class " << cname << endl;
  oop super_name = _mmobj->mm_compiled_class_super_name(cclass);
  char* super_name_str = _mmobj->mm_string_cstr(super_name);
  debug() << "Super " << super_name_str << endl;

  oop super_class = NULL;
  if (mod_classes.find(super_name_str) != mod_classes.end()) {
    debug() << "Super class already instantiated" << endl;
    super_class = mod_classes.at(super_name_str);
  } else if (_mmobj->mm_dictionary_has_key(cclass_dict, super_name)) {
    debug() << "Super class not instantiated. recursively instantiate it" << endl;
    super_class = instantiate_class(super_name, _mmobj->mm_dictionary_get(cclass_dict, super_name), cclass_dict, mod_classes, imodule);
  }
  //else if (super_name in module arguments) {
  else if (_core_image->has_class(super_name_str)) {
    debug() << "Super class got from super module (core)" << endl;
    super_class = _core_image->get_prime(super_name_str);
  } else {
    bail("Super class not found");
  }

  oop class_funs_dict = _mmobj->mm_cfuns_to_funs_dict(_mmobj->mm_compiled_class_own_methods(cclass), imodule);
  oop class_behavior = _mmobj->mm_class_behavior_new(super_class, class_funs_dict);

  oop funs_dict = _mmobj->mm_cfuns_to_funs_dict(_mmobj->mm_compiled_class_methods(cclass), imodule);
  number num_fields = _mmobj->mm_compiled_class_num_fields(cclass);
  oop klass = _mmobj->mm_class_new(class_behavior, super_class, funs_dict, cclass, num_fields);
  mod_classes[cname] = klass;
  debug() << "User class " << cname << " = "
          << klass << " behavior: " << class_behavior
          << " dict: " << funs_dict
          << " own_dict: " << class_funs_dict
          << endl;
  return klass;
}


void MMCImage::assign_module_arguments(oop imodule, oop module_arguments_list) {
  //if cmodule->params size != module_arguments_list size: arity error
  //TODO
  bail("TODO");
}

void MMCImage::load_default_dependencies_and_assign_module_arguments(oop imodule) {
  //for each p in _cmod->default_params:
  //   imod = load it
  //   idx = index(p.name, cmod->params)
  //   imod[idx] = imod
  oop params_list = _mmobj->mm_compiled_module_params(_compiled_module);
  oop default_params_dict = _mmobj->mm_compiled_module_default_params(_compiled_module);
  number dict_size = _mmobj->mm_dictionary_size(default_params_dict);
  for (int i = 0; i < dict_size; i++) {
    oop lhs_name = _mmobj->mm_dictionary_entry_key(default_params_dict, i);
    oop mod_name = _mmobj->mm_dictionary_entry_value(default_params_dict, i);
    oop imd = _vm->instantiate_module(_mmobj->mm_string_cstr(mod_name), MM_NULL);
    number midx = _mmobj->mm_list_index_of(params_list, lhs_name);
    if (midx == -1) {
      bail("Could not bind unknown module parameter");
    }
    _mmobj->mm_module_set_module_argument(imodule, imd, midx);
  }
}

void  MMCImage::create_param_getters(oop imodule, oop imod_dict, oop params_list) {
  number num_params = _mmobj->mm_list_size(params_list);

  for (int i = 0; i < num_params; i++) {
    oop name = _mmobj->mm_list_entry(params_list, i);
    char* str = _mmobj->mm_string_cstr(name);
    debug() << "Creating getter for param " << str << " " << i << endl;
    oop getter = _mmobj->mm_new_slot_getter(imodule, _compiled_module, name, i + 3); //imod: vt, delegate, dict
    _mmobj->mm_dictionary_set(imod_dict, i, _vm->new_symbol(str), getter);
  }
}


oop MMCImage::instantiate_module(oop module_arguments_list) {

  // word* cmod = (word*) _compiled_module;
  // debug() << "CompiledModule: " << cmod << endl;
  // debug() << "CompiledModule vt: " << (word*) *cmod << endl;

  oop cclass_dict = _mmobj->mm_compiled_module_classes(_compiled_module);

  // debug() << "CompiledModule class_dict: " << cclass_dict << endl;
  // debug() << "CompiledModule class_dict vt: " << (word*) *((word*)cclass_dict) << endl;

  number num_classes = _mmobj->mm_dictionary_size(cclass_dict);
  // debug() << "CompiledModule num_classes: " << num_classes << endl;

  oop params = _mmobj->mm_compiled_module_params(_compiled_module);
  number num_params = _mmobj->mm_list_size(params);

  oop imodule = _mmobj->mm_module_new(num_params, num_classes,
                                      _compiled_module,
                                      _core_image->get_module_instance());

  debug() << "imodule " << imodule << " params:" << num_params
          << " classes:" << num_classes << " (size: " << (num_params + num_classes + 3) << ")" << endl;

  oop fun_dict = _mmobj->mm_compiled_module_functions(_compiled_module);

  number num_funs = _mmobj->mm_dictionary_size(fun_dict);

  // debug() << "CompiledModule num_functions: " << num_funs << endl;

  oop imod_dict = _mmobj->mm_dictionary_new(num_params + num_funs + num_classes); // num_classes -> getters
  _mmobj->mm_module_set_dictionary(imodule, imod_dict);

  if (module_arguments_list) {
    assign_module_arguments(imodule, module_arguments_list);
  } else {
    load_default_dependencies_and_assign_module_arguments(imodule);
  }

  create_param_getters(imodule, imod_dict, params);

  int imod_dict_idx = num_params;

  // //for each CompiledFunction:
  // // mod[dict] += Function
  // // mod[i] = Function

  for (int i = 0; i < num_funs; i++) {
    oop str_name = _mmobj->mm_dictionary_entry_key(fun_dict, i);
    char* str = _mmobj->mm_string_cstr(str_name);
    oop cfun = _mmobj->mm_dictionary_entry_value(fun_dict, i);
    oop fun = _mmobj->mm_function_from_cfunction(cfun, imodule);
    _mmobj->mm_dictionary_set(imod_dict, imod_dict_idx++, _vm->new_symbol(str), fun);
  }

  std::map<std::string, oop> mod_classes; //store each Class created here, so we do parent look up more easily

  int imod_idx = num_params + 4; //imod: vt, delegate, dict, cmod

  for (int i = 0; i < num_classes; i++) {
    oop str_name = _mmobj->mm_dictionary_entry_key(cclass_dict, i);
    char* cname = _mmobj->mm_string_cstr(str_name);

    oop cclass = _mmobj->mm_dictionary_entry_value(cclass_dict, i);
    oop klass;

    if (mod_classes.find(cname) != mod_classes.end()) {
      debug() << "Class " << cname << " already instantiated" << endl;
      klass = mod_classes.at(cname);
    } else {
      //recursively instantiate it
      klass = instantiate_class(str_name, cclass, cclass_dict, mod_classes, imodule);
    }

    * (oop*) & imodule[imod_idx] = klass;
    oop klass_getter = _mmobj->mm_new_slot_getter(imodule, cclass, str_name, imod_idx);

    imod_idx++;

    _mmobj->mm_dictionary_set(imod_dict, imod_dict_idx++, _vm->new_symbol(cname), klass_getter);
  }
  return imodule;
}


oop MMCImage::load() {
  _data = read_mmc_file(_name_or_path, &_data_size);
  load_header();
  relocate_addresses(_data, _data_size, HEADER_SIZE + _names_size + _ot_size + _er_size + _es_size);
  link_external_references();
  link_symbols(_data, _es_size, HEADER_SIZE + _names_size + _ot_size + _er_size, _vm, _core_image);
  _compiled_module = (oop) * (word*)(& _data[HEADER_SIZE + _names_size]);
  return _compiled_module;
}
