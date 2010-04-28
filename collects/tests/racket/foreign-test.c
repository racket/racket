#include <stdlib.h>

typedef unsigned char byte;

#ifdef _WIN32
#define X __declspec(dllexport)
#else
#define X
#endif

X int  add1_int_int   (int  x) { return x + 1; }
X int  add1_byte_int  (byte x) { return x + 1; }
X byte add1_int_byte  (int  x) { return x + 1; }
X byte add1_byte_byte (byte x) { return x + 1; }
X int  decimal_int_int_int    (int  x, int  y) { return 10*x + y; }
X int  decimal_byte_int_int   (byte x, int  y) { return 10*x + y; }
X int  decimal_int_byte_int   (int  x, byte y) { return 10*x + y; }
X int  decimal_byte_byte_int  (byte x, byte y) { return 10*x + y; }
X byte decimal_int_int_byte   (int  x, int  y) { return 10*x + y; }
X byte decimal_byte_int_byte  (byte x, int  y) { return 10*x + y; }
X byte decimal_int_byte_byte  (int  x, byte y) { return 10*x + y; }
X byte decimal_byte_byte_byte (byte x, byte y) { return 10*x + y; }

X int  callback3_int_int_int    (int(*f)(int))   { if (f) return f(3); else return 79; }
X int  callback3_byte_int_int   (int(*f)(byte))  { return f(3); }
X int  callback3_int_byte_int   (byte(*f)(int))  { return f(3); }
X int  callback3_byte_byte_int  (byte(*f)(byte)) { return f(3); }
X byte callback3_int_int_byte   (int(*f)(int))   { return f(3); }
X byte callback3_byte_int_byte  (int(*f)(byte))  { return f(3); }
X byte callback3_int_byte_byte  (byte(*f)(int))  { return f(3); }
X byte callback3_byte_byte_byte (byte(*f)(byte)) { return f(3); }

X int g1;
X int  curry_ret_int_int   (int  x) { return g1 + x; }
X int  curry_ret_byte_int  (byte x) { return g1 + x; }
X byte curry_ret_int_byte  (int  x) { return g1 + x; }
X byte curry_ret_byte_byte (byte x) { return g1 + x; }
X void* curry_int_int_int   (int  x)  { g1 = x; return &curry_ret_int_int;   }
X void* curry_byte_int_int  (byte x)  { g1 = x; return &curry_ret_int_int;   }
X void* curry_int_byte_int  (int  x)  { g1 = x; return &curry_ret_byte_int;  }
X void* curry_byte_byte_int (byte x)  { g1 = x; return &curry_ret_byte_int;  }
X void* curry_int_int_byte  (int  x)  { g1 = x; return &curry_ret_int_byte;  }
X void* curry_byte_int_byte (byte x)  { g1 = x; return &curry_ret_int_byte;  }
X void* curry_int_byte_byte (int  x)  { g1 = x; return &curry_ret_byte_byte; }
X void* curry_byte_byte_byte(byte x)  { g1 = x; return &curry_ret_byte_byte; }

X int g2;
X int ho_return(int x) { return g2 + x; }
X void* ho(int(*f)(int), int x) { g2 = f(x); return ho_return; }

X void *g3 = NULL;
X int use_g3(int x) { return ((int(*)(int))g3)(x); }

/* typedef int(*int2int)(int); */
/* typedef int2int(*int_to_int2int)(int); */
/* int hoho(int x, int_to_int2int f) { */
X int hoho(int x, int(*(*f)(int))(int)) { return (f(x+1))(x-1); }

X int grab7th(void *p) { return ((char *)p)[7]; }

X int vec4(int x[]) { return x[0]+x[1]+x[2]+x[3]; }

typedef struct _char_int { unsigned char a; int b; } char_int;
X int charint_to_int(char_int x) { return ((int)x.a) + x.b; }
X char_int int_to_charint(int x) {
  char_int result;
  result.a = (unsigned char)x;
  result.b = x;
  return result;
}
X char_int charint_swap(char_int x) {
  char_int result;
  result.a = (unsigned char)x.b;
  result.b = (int)x.a;
  return result;
}

int(*grabbed_callback)(int) = NULL;
X void grab_callback(int(*f)(int)) { grabbed_callback = f; }
X int use_grabbed_callback(int n) { return grabbed_callback(n); }
