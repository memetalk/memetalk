#define  WSIZE 8

#if WSIZE == 4
  typedef int word;
  typedef int number;
#elif WSIZE == 8
  typedef long word;
  typedef long number;
#endif

typedef char byte;
typedef word* oop;

#define DEFAULT_STACK_SIZE sizeof(word) * 1024 * 10
