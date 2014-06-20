#ifndef VM_HPP
#define VM_HPP

class CoreImage;

class VM {
public:
  VM(const char* core_img_filepath);
  int start(char* filepath);
private:
  CoreImage* _core_image;
};

#endif
