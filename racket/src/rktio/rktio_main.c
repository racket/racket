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
  
  if (!rktio_process_init(rktio)) {
    rktio_destroy(rktio);
    return NULL;
  }

#ifdef RKTIO_SYSTEM_WINDOWS
  if (!rktio_winsock_init(rktio)) {
    rktio_destroy(rktio);
    return NULL;
  }
#endif
  
  rktio_init_time(rktio);
  rktio_init_wide(rktio);

  rktio_syslog_init(rktio);

  return rktio;
}

void rktio_destroy(rktio_t *rktio)
{
  rktio_syslog_clean(rktio);
  rktio_error_clean(rktio);
  rktio_process_deinit(rktio);
  rktio_free_ghbn(rktio);
  rktio_free_global_poll_set(rktio);
  rktio_stop_fs_change(rktio);
#ifdef RKTIO_SYSTEM_WINDOWS
  rktio_winsock_done(rktio);
#endif
  free(rktio);
}

/* Useful on Windows to make sure everyone is using the same malloc()
   and free(): */
void rktio_free(void *p)
{
  free(p);
}
