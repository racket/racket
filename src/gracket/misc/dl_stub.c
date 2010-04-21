/*
  This is used for static-linking in UNIX when otherwise compiled for
   dynamic linking.
*/

#include "scheme.h"
#include <dlfcn.h>

void *dlopen(const char *filename, int flags)
{
  scheme_signal_error("load-extension: can't use with a statically-linked"
		      " MrEd");

  return NULL;
}

char *dlerror()
{
  return NULL;
}

void *dlsym(void *dl, const char *name)
{
  return NULL;
}

int dlclose(void *dl)
{
  return -1;
}

int _DYNAMIC;
