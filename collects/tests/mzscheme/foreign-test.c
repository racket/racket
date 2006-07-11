#include <stdlib.h>

typedef unsigned char byte;

int  add1_int_int   (int  x) { return x + 1; }
int  add1_byte_int  (byte x) { return x + 1; }
byte add1_int_byte  (int  x) { return x + 1; }
byte add1_byte_byte (byte x) { return x + 1; }
int  decimal_int_int_int    (int  x, int  y) { return 10*x + y; }
int  decimal_byte_int_int   (byte x, int  y) { return 10*x + y; }
int  decimal_int_byte_int   (int  x, byte y) { return 10*x + y; }
int  decimal_byte_byte_int  (byte x, byte y) { return 10*x + y; }
byte decimal_int_int_byte   (int  x, int  y) { return 10*x + y; }
byte decimal_byte_int_byte  (byte x, int  y) { return 10*x + y; }
byte decimal_int_byte_byte  (int  x, byte y) { return 10*x + y; }
byte decimal_byte_byte_byte (byte x, byte y) { return 10*x + y; }

int  callback3_int_int_int    (int(*f)(int))   { return f(3); }
int  callback3_byte_int_int   (int(*f)(byte))  { return f(3); }
int  callback3_int_byte_int   (byte(*f)(int))  { return f(3); }
int  callback3_byte_byte_int  (byte(*f)(byte)) { return f(3); }
byte callback3_int_int_byte   (int(*f)(int))   { return f(3); }
byte callback3_byte_int_byte  (int(*f)(byte))  { return f(3); }
byte callback3_int_byte_byte  (byte(*f)(int))  { return f(3); }
byte callback3_byte_byte_byte (byte(*f)(byte)) { return f(3); }

int g1;
int  curry_return_int_int   (int  x) { return g1 + x; }
int  curry_return_byte_int  (byte x) { return g1 + x; }
byte curry_return_int_byte  (int  x) { return g1 + x; }
byte curry_return_byte_byte (byte x) { return g1 + x; }
void* curry_int_int_int   (int  x)  { g1 = x; return &curry_return_int_int;   }
void* curry_byte_int_int  (byte x)  { g1 = x; return &curry_return_int_int;   }
void* curry_int_byte_int  (int  x)  { g1 = x; return &curry_return_byte_int;  }
void* curry_byte_byte_int (byte x)  { g1 = x; return &curry_return_byte_int;  }
void* curry_int_int_byte  (int  x)  { g1 = x; return &curry_return_int_byte;  }
void* curry_byte_int_byte (byte x)  { g1 = x; return &curry_return_int_byte;  }
void* curry_int_byte_byte (int  x)  { g1 = x; return &curry_return_byte_byte; }
void* curry_byte_byte_byte(byte x)  { g1 = x; return &curry_return_byte_byte; }

int g2;
int ho_return(int x) { return g2 + x; }
void* ho(int(*f)(int), int x) { g2 = f(x); return ho_return; }


void *g3 = NULL;
int use_g3(int x) { return ((int(*)(int))g3)(x); }

/* typedef int(*int2int)(int); */
/* typedef int2int(*int_to_int2int)(int); */
/* int hoho(int x, int_to_int2int f) { */
int hoho(int x, int(*(*f)(int))(int)) { return (f(x+1))(x-1); }
