#include "rktio.h"
#include "rktio_private.h"
#include <stdlib.h>
#include <string.h>
#ifdef OS_X
# include <sys/types.h>
# include <sys/sysctl.h>
#endif

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
  rktio_init_cpu(rktio);

  rktio_syslog_init(rktio);

#ifdef OS_X
  {
    int a[2], i, k = 0;
    size_t len;
    char *vers;

    a[0] = CTL_KERN;
    a[1] = KERN_OSRELEASE;
    sysctl(a, 2, NULL, &len, NULL, 0);
    vers = malloc(len * sizeof(char));
    sysctl(a, 2, vers, &len, NULL, 0);

    for (i = 0; (i < len) && (vers[i] != '.'); i++)
      k = (k * 10) + vers[i] - '0';

    rktio->macos_kernel_version = k;

    free(vers);
  }
#endif

  return rktio;
}

void rktio_destroy(rktio_t *rktio)
{
  rktio_stop_background(rktio);
  rktio_syslog_clean(rktio);
  rktio_dll_clean(rktio);
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
