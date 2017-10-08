#include <vm.hpp>

static oop lookup_bottom_instance(MMObj* mmobj, oop obj) {
  if (obj == MM_NULL) {
    return NULL;
  } else if (mmobj->mm_object_vt(mmobj->mm_object_delegate(obj)) == mmobj->core()->get_prime("Object")) {
    return obj;
  } else {
    return lookup_bottom_instance(mmobj, mmobj->mm_object_delegate(obj));
  }
}

static inline void* get_field(oop instance) {
  return ((oop*)instance)[2]; //@self
}

static inline void set_field(oop bottom_instance, void* obj) {
  ((oop*)bottom_instance)[2] = (oop) obj;
}

static inline void* get_instance(MMObj* mmobj, oop memeobj) {
  oop bottom = lookup_bottom_instance(mmobj, memeobj);
  if (bottom) {
    return get_field(bottom);
  }
  return NULL;
}

static void set_instance(MMObj* mmobj, oop memeobj, void* obj) {
  oop bottom = lookup_bottom_instance(mmobj, memeobj);
  set_field(bottom, obj);
}

static oop get_meme_class(Process* proc, const char *name) {
  int exc = 0;
  return proc->send_0(proc->mp(), proc->vm()->new_symbol(name), &exc);
}

static oop meme_instance(Process* proc, const char *name, void* obj) {
  oop klass = get_meme_class(proc, name);
  oop instance = proc->mmobj()->alloc_instance(proc, klass);
  set_instance(proc->mmobj(), instance, obj);
  return instance;
}
