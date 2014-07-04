#ifndef MMC_LOADER
#define MMC_LOADER

class MMCImage {
  static word MAGIC_NUMBER;
  static word HEADER_SIZE;
public:
  MMCImage(const char*);
  void load();
private:
  void load_header();

  const char* _filepath;
  int _data_size;
  char* _data;

  //header
  word _ot_size;
  word _es_size;
  word _names_size;
};


#endif
