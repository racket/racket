#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>

rktio_t *rktio_init(void)
{
  rktio_t *rktio;

  rktio = malloc(sizeof(rktio_t));
  memset(rktio, 0, sizeof(rktio_t));

  rktio_alloc_global_poll_set(rktio);
  if (!rktio_initialize_signal(rktio)) {
    rktio_destroy(rktio);
    return NULL;
  }
    
  return rktio;
}

void rktio_destroy(rktio_t *rktio)
{
  free(rktio);
}

/* Useful on Windows to make sure everyone is using the same malloc()
   and fre(): */
void rktio_free(void *p)
{
  free(p);
}

