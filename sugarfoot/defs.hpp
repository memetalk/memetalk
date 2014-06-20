#define  WSIZE 8

#if WSIZE == 4
  typedef int word;
#elif WSIZE == 8
  typedef long word;
#endif

typedef void* oop;
