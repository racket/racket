
#ifdef _WIN32
__declspec(dllexport) void gc_fprintf(int ignored, const char *c, ...);
# define GCPRINT gc_fprintf
# define GCOUTF 0
# define GCFLUSHOUT() /* empty */
# define GCPRINT_TO_WINDOWS_CONSOLE
#endif

#ifndef GCPRINT
# define GCPRINT fprintf
# define GCOUTF stderr
# define GCFLUSHOUT() fflush(NULL)
#endif

/**************** Windows stderr ****************/

#ifdef GCPRINT_TO_WINDOWS_CONSOLE

static BOOL WINAPI IgnoreEverything(DWORD evt)
{
  return TRUE;
}

static void GC_prim_stringout(char *s, int len)
{
  static HANDLE console;
  DWORD wrote;

  if (!console) {
    COORD size;
    CONSOLE_SCREEN_BUFFER_INFO info;

    console = GetStdHandle(STD_ERROR_HANDLE);
    if (!GetConsoleScreenBufferInfo(console, &info)) {
      /* Since getting the screen buffer info failed, 
	 we must be in GUI mode. Create a console
	 window --- and set the handler so that closing
	 the window doesn't abort GRacket! */
      AllocConsole();
      console = GetStdHandle(STD_ERROR_HANDLE);
      GetConsoleScreenBufferInfo(console, &info);
      size = info.dwSize;
      if (size.Y < 500) {
	size.Y = 500;
	SetConsoleScreenBufferSize(console, size);
      }
      SetConsoleCtrlHandler(IgnoreEverything, TRUE);
    }
  }

  while (len) {
    if (WriteFile(console, s, len, &wrote, NULL)) {
      len -= wrote;
      s += wrote;
      if (len)
	Sleep(10);
    } else {
      /* Give up */
      break;
    }
  }
}

#include <stdarg.h>
#include <ctype.h>

#define NP_BUFSIZE 512

/* Non-allocating printf. */
void gc_fprintf(int ignored, const char *c, ...)
{
  char buffer[NP_BUFSIZE];
  int pos;
  va_list args;

  va_start(args, c);

  pos = 0;
  while (*c) {
    if (*c == '%') {
      int len = -1, slen;
      int islong = 0;
      char *s;

      if (pos) {
	GC_prim_stringout(buffer, pos);
	pos = 0;
      }

      c++;
      if (isdigit(*c)) {
	len = 0;
	while (isdigit(*c)) {
	  len = (len * 10) + (*c - '0');
	  c++;
	}
      }

      if (*c == 'l') {
	islong = 1;
	c++;
      }
      while (isdigit(*c)) {
	c++;
      }
      if (*c == '.') {
	c++;
      }
      while (isdigit(*c)) {
	c++;
      }
      
      switch (*c) {
      case 'd':
      case 'x':
      case 'i':
      case 'p':
	{
	  long v;
	  int d, i;

	  if (islong) {
	    v = va_arg(args, long);
	  } else {
	    v = va_arg(args, int);
	  }
	  
	  if (!v) {
	    s = "0";
	    slen = 1;
	  } else {
	    int neg = 0;

	    i = NP_BUFSIZE - 2;
	    
	    if (v < 0) {
	      neg = 1;
	      v = -v;
	    }

	    d = (((*c) == 'd') ? 10 : 16);
	    while (v) {
	      int digit = (v % d);
	      if (digit < 10)
		digit += '0';
	      else
		digit += 'a' - 10;
	      buffer[i--] = digit;
	      v = v / d;
	    }
	    if (neg)
	      buffer[i--] = '-';

	    s = buffer + i + 1;
	    slen = (NP_BUFSIZE - 2) - i;
	  }
	}
	break;
      case 's':
	s = va_arg(args, char*);
	slen = strlen(s);
	break;
      case 'c':
	{
	  int v;
	  v = va_arg(args, int);
	  s = buffer;
	  buffer[0] = (char)v;
	  slen = 1;
	}
	break;
      default:
	s = "???";
	slen = 3;
	break;
      }

      c++;

      if (len != -1) {
	if (slen > len)
	  slen = len;
	else {
	  int i;
	  for (i = slen; i < len; i++)
	    GC_prim_stringout(" ", 1);
	}
      }
      
      if (slen)
	GC_prim_stringout(s, slen);
    } else {
      if (pos == (NP_BUFSIZE - 1)) {
	GC_prim_stringout(buffer, pos);
	pos = 0;
      }
      buffer[pos++] = *(c++);
    }
  }

  if (pos)
    GC_prim_stringout(buffer, pos);

  /* Suggest a flush: */
  GC_prim_stringout(NULL, 0);

  va_end(args);
}

#endif
