#include "defs.hpp"
#include "utils.hpp"
#include "report.hpp"
#include "mmc_loader.hpp"
#include "core_loader.hpp"

using namespace std;

word MMCImage::HEADER_SIZE = 4 * WSIZE;
word MMCImage::MAGIC_NUMBER = 0x420;

MMCImage::MMCImage(CoreImage* core_image, const char* filepath)
  : _core_image(core_image), _filepath(filepath) {
}

void MMCImage::load_header() {
  word magic_number = unpack_word(_data, 0 * WSIZE);
  _ot_size = unpack_word(_data,  1 * WSIZE);
  _es_size = unpack_word(_data, 2 * WSIZE);
  _names_size = unpack_word(_data,  3 * WSIZE);

  debug() << "Header:magic: " << magic_number << " =?= " << MMCImage::MAGIC_NUMBER << endl;
  debug() << "Header:ot_size: " << _ot_size << endl;
  debug() << "Header:es_size: " << _es_size << endl;
  debug() << "Header:names_size: " << _names_size << endl;
}

void MMCImage::fix_external_references() {
  const char* base = _data;
  int start_external_symbols = HEADER_SIZE + _names_size + _ot_size;

  for (int i = 0; i < _es_size; i += (2 * WSIZE)) {
    word name_offset = unpack_word(_data, start_external_symbols + i);
    char* name = (char*) (base + name_offset);
    word obj_offset = unpack_word(_data, start_external_symbols + i + WSIZE);
    word* obj = (word*) (base + obj_offset);
    // debug() << obj_offset << " - " << *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
    * (word*) (base + obj_offset) = (word) _core_image->get_prime(name);
    debug() << obj_offset << " - " << *obj << " [" << name << "] -> " << _core_image->get_prime(name) << endl;
  }
}

oop MMCImage::instantiate_module(/* module arguments */) {

  // word* cmod = (word*) _compiled_module;
  // debug() << "CompiledModule: " << cmod << endl;
  // debug() << "CompiledModule vt: " << (word*) *cmod << endl;

  oop class_dict = (oop) ((word*)_compiled_module)[6];

  // debug() << "CompiledModule class_dict: " << class_dict << endl;
  // debug() << "CompiledModule class_dict vt: " << (word*) *((word*)class_dict) << endl;

  long num_classes = ((word*)class_dict)[2];
  // debug() << "CompiledModule num_classes: " << num_classes << endl;

  int num_entries = 3 +  num_classes; /* 3: vt + delegate + dict */

  oop imodule = (oop) malloc(sizeof(word) * num_entries);

  ((word**) imodule)[0] = imodule; //imodule[vt] = imodule
  ((word**) imodule)[1] = _core_image->get_module_instance(); // imodule[delegate]


  // oop fun_dict = (oop) ((word*)_compiled_module)[6];
  // long num_funs = ((word*)fun_dict)[2];

  // oop dict = mm_dict_new(num_funs);
  // ((word**) imodule)[2] = dict;

  // //for each CompiledFunction:
  // // mod[dict] += Function
  // // mod[i] = Function

  // for (int i = 0; i < num_funs; i++) {
  //   char* name = mm_dict_name(fun_dict, i);
  //   mm_dict_set_name(dict, i, name);

  //   oop cfun = mm_dict_value(fun_dict, i);
  //   oop fun = mm_function_from_cfunction(cfun);
  //   mm_dict_set_value(dict, i, fun);
  // }

  //for each CompiledClass:
  // lookup parent (params, module, parent module recursively)
  // create ClassBehavior
  // mod[i] Class
  // mod[dict] += accessor for Class
}

void MMCImage::load() {
  _data = read_file(_filepath, &_data_size);
  load_header();
  relocate_addresses(_data, _data_size, HEADER_SIZE + _names_size + _ot_size + _es_size);
  fix_external_references();
  _compiled_module = (oop) * (word*)(& _data[HEADER_SIZE + _names_size]);
  instantiate_module();
}
