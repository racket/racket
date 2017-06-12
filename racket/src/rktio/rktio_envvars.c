#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>

void *rktio_envvars_to_block(rktio_t *rktio, rktio_envvars_t *envvars)
{
  void *p;
  p = malloc(sizeof(char *));
  *(char **)p = NULL;
  return p;
}
