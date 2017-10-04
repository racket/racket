#include "rktio.h"
#include "rktio_private.h"
#ifdef SUPPORT_BACKGROUND_SLEEP_THREAD
# include <string.h>
# include <stdlib.h>
# include <unistd.h>
# include <errno.h>
#endif

/*========================================================================*/
/* Sleeping in a separate OS thread                                       */
/*========================================================================*/

#ifdef SUPPORT_BACKGROUND_SLEEP_THREAD

/* Sleep-in-thread support needed for GUIs Mac OS.
   To merge waiting on a CoreFoundation event with a select(), an embedding
   application can attach a single socket to an event callback, and then
   create a Mac thread to call the usual sleep and write to the socket when
   data is available. */

typedef struct {
  pthread_mutex_t lock;
  pthread_cond_t cond;
  int count;
} pt_sema_t;

static void pt_sema_init(pt_sema_t *sem)
{
  pthread_mutex_init(&sem->lock, NULL);
  pthread_cond_init(&sem->cond, NULL);
  sem->count = 0;
}

static void pt_sema_wait(pt_sema_t *sem)
{
  pthread_mutex_lock(&sem->lock);
  while (sem->count <= 0)
    pthread_cond_wait(&sem->cond, &sem->lock);
  sem->count--;
  pthread_mutex_unlock(&sem->lock);
}

static void pt_sema_post(pt_sema_t *sem)
{
  pthread_mutex_lock(&sem->lock);
  sem->count++;
  if (sem->count > 0)
    pthread_cond_signal(&sem->cond);
  pthread_mutex_unlock(&sem->lock);
}

typedef struct background_sleep_t {
  pthread_t th;
  pt_sema_t sleeping_sema, done_sema;
  int done; /* => background thread should stop */
  float nsecs;
  int woke_fd;
  rktio_poll_set_t *fds;
  rktio_ltps_t *lt;
} background_sleep_t;

static void *do_background_sleep(void *_rktio)
{
  rktio_t *rktio = _rktio;
  intptr_t len;

  while (1) {
    pt_sema_wait(&rktio->background->sleeping_sema);

    if (rktio->background->done)
      break;

    rktio_sleep(rktio, rktio->background->nsecs, rktio->background->fds, rktio->background->lt);
    do {
      len = write(rktio->background->woke_fd, "y", 1);
    } while ((len == -1) && (errno == EINTR));

    pt_sema_post(&rktio->background->done_sema);
  }
  
  return NULL;
}

rktio_ok_t rktio_start_sleep(rktio_t *rktio, float nsecs, rktio_poll_set_t *fds, rktio_ltps_t *lt, int woke_fd)
{
  if (!rktio->background) {
    rktio->background = malloc(sizeof(background_sleep_t));
    memset(rktio->background, 0, sizeof(background_sleep_t));
    pt_sema_init(&rktio->background->sleeping_sema);
    pt_sema_init(&rktio->background->done_sema);
    
    if (pthread_create(&rktio->background->th, NULL, do_background_sleep, rktio)) {
      get_posix_error();
      return 0;
    }
  }

  rktio->background->nsecs = nsecs;
  rktio->background->fds = fds;
  rktio->background->lt = lt;
  rktio->background->woke_fd = woke_fd;
  pt_sema_post(&rktio->background->sleeping_sema);

  return 1;
}

void rktio_end_sleep(rktio_t *rktio)
{
  rktio_signal_received(rktio);
  pt_sema_wait(&rktio->background->done_sema);

  /* Clear external event flag */
  rktio_flush_signals_received(rktio);
}

void rktio_stop_background(rktio_t *rktio)
{
  if (rktio->background) {
    rktio->background->done = 1;
    pt_sema_post(&rktio->background->sleeping_sema);
    pthread_join(rktio->background->th, NULL);
    free(rktio->background);
  }
}

#else

rktio_ok_t rktio_start_sleep(rktio_t *rktio, float nsecs, rktio_poll_set_t *fds, rktio_ltps_t *lt, int woke_fd)
{
  set_racket_error(RKTIO_ERROR_UNSUPPORTED);
  return 0;
}

void rktio_end_sleep(rktio_t *rktio)
{
}

void rktio_stop_background(rktio_t *rktio)
{
}

#endif
