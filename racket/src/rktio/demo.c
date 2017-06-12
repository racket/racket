#include "rktio.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void do_check_valid(rktio_t *rktio, int ok, int where)
{
  /* Beware that a reported error is nonsense if the failure
     was an unexpected result insteda of an error result. */
  if (!ok) {
    printf("error at %d: %d@%d = %s\n",
           where,
           rktio_get_last_error(rktio),
           rktio_get_last_error_kind(rktio),
           rktio_get_error_string(rktio,
                                  rktio_get_last_error_kind(rktio),
                                  rktio_get_last_error(rktio)));
  }
}

static void do_check_expected_error(rktio_t *rktio, int err, int where)
{
  if (!err) {
    printf("error expected at %d\n",
           where);
  }
}

static void do_check_expected_racket_error(rktio_t *rktio, int err, int what, int where)
{
  if (!err) {
    printf("error expected at %d\n",
           where);
  } else if ((what != rktio_get_last_error(rktio))
             || (RKTIO_ERROR_KIND_RACKET != rktio_get_last_error_kind(rktio))) {
    printf("wrong error at %d: %d@%d = %s\n",
           where,
           rktio_get_last_error(rktio),
           rktio_get_last_error_kind(rktio),
           rktio_get_error_string(rktio,
                                  rktio_get_last_error_kind(rktio),
                                  rktio_get_last_error(rktio)));
  }
}

#define check_valid(e) do_check_valid(rktio, ((e)?1:0), __LINE__)
#define check_expected_error(e) do_check_expected_error(rktio, e, __LINE__)
#define check_expected_racket_error(e, what) do_check_expected_racket_error(rktio, e, what, __LINE__)

static rktio_ltps_t *try_check_ltps(rktio_t *rktio,
                                    rktio_fd_t *fd, /* read mode */
                                    rktio_fd_t *fd2, /* write mode */
                                    rktio_ltps_handle_t **_h1, 
                                    rktio_ltps_handle_t **_h2)
{
  rktio_ltps_t *lt;
  rktio_ltps_handle_t *h1, *h2, *hx, *hy;

  lt = rktio_open_ltps(rktio);

  /* Add read handle for fd1 */
  h1 = rktio_ltps_add(rktio, lt, fd, RKTIO_LTPS_CHECK_READ);
  if (!h1
      && (rktio_get_last_error_kind(rktio) == RKTIO_ERROR_KIND_RACKET)
      && (rktio_get_last_error(rktio) == RKTIO_ERROR_UNSUPPORTED)) {
    check_valid(rktio_ltps_close(rktio, lt));
    return NULL;
  }
  check_expected_racket_error(!h1, RKTIO_ERROR_LTPS_NOT_FOUND);
  h1 = rktio_ltps_add(rktio, lt, fd, RKTIO_LTPS_CREATE_READ);
  check_valid(h1);
  hx = rktio_ltps_add(rktio, lt, fd, RKTIO_LTPS_CREATE_READ);
  check_valid(hx == h1);
  hx = rktio_ltps_add(rktio, lt, fd, RKTIO_LTPS_CHECK_WRITE);
  check_expected_racket_error(!hx, RKTIO_ERROR_LTPS_NOT_FOUND);

  /* Add write handle for fd2 */
  h2 = rktio_ltps_add(rktio, lt, fd2, RKTIO_LTPS_CHECK_READ);
  check_expected_racket_error(!h2, RKTIO_ERROR_LTPS_NOT_FOUND);
  h2 = rktio_ltps_add(rktio, lt, fd2, RKTIO_LTPS_CHECK_WRITE);
  check_expected_racket_error(!h2, RKTIO_ERROR_LTPS_NOT_FOUND);
  h2 = rktio_ltps_add(rktio, lt, fd2, RKTIO_LTPS_CREATE_WRITE);
  check_valid(h2);
  hx = rktio_ltps_add(rktio, lt, fd2, RKTIO_LTPS_CREATE_READ);
  check_valid(hx);

  /* Removing `fd2` should signal the handles `h2` and `hx` */
  hy = rktio_ltps_add(rktio, lt, fd2, RKTIO_LTPS_REMOVE);
  check_expected_racket_error(!hy, RKTIO_ERROR_LTPS_REMOVED);
  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_valid((hy == h2) || (hy == hx));
  free(hy);
  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_valid((hy == h2) || (hy == hx));
  free(hy);
  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_expected_racket_error(!hy, RKTIO_ERROR_LTPS_NOT_FOUND);
  /* Add write handle for fd2 again: */
  h2 = rktio_ltps_add(rktio, lt, fd2, RKTIO_LTPS_CREATE_WRITE);
  check_valid(h2);

  *_h1 = h1;
  *_h2 = h2;

  return lt;
}

void check_ltps_write_ready(rktio_t *rktio, rktio_ltps_t *lt, rktio_ltps_handle_t *h2)
{
  rktio_ltps_handle_t *hy;

  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_expected_racket_error(!hy, RKTIO_ERROR_LTPS_NOT_FOUND);

  check_valid(rktio_ltps_poll(rktio, lt));
  
  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_valid(hy == h2);
  rktio_free(hy);
  
  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_expected_racket_error(!hy, RKTIO_ERROR_LTPS_NOT_FOUND);
}

void check_ltps_read_ready(rktio_t *rktio, rktio_ltps_t *lt, rktio_ltps_handle_t *h1)
{
  rktio_ltps_handle_t *hy;

  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_expected_racket_error(!hy, RKTIO_ERROR_LTPS_NOT_FOUND);
  
  check_valid(rktio_ltps_poll(rktio, lt));
  
  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_valid(hy == h1);
  rktio_free(hy);
  
  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_expected_racket_error(!hy, RKTIO_ERROR_LTPS_NOT_FOUND);
}

void check_ltps_read_and_write_ready(rktio_t *rktio, rktio_ltps_t *lt, rktio_ltps_handle_t *h1, rktio_ltps_handle_t *h2)
{
  rktio_ltps_handle_t *hy;

  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_expected_racket_error(!hy, RKTIO_ERROR_LTPS_NOT_FOUND);

  check_valid(rktio_ltps_poll(rktio, lt));
  
  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  if (hy == h1) {
    rktio_free(hy);
    hy = rktio_ltps_get_signaled_handle(rktio, lt);
    check_valid(hy == h2);
  } else {
    check_valid(hy == h2);
    rktio_free(hy);
    hy = rktio_ltps_get_signaled_handle(rktio, lt);
    check_valid(hy == h1);
  }
  rktio_free(hy);

  hy = rktio_ltps_get_signaled_handle(rktio, lt);
  check_expected_racket_error(!hy, RKTIO_ERROR_LTPS_NOT_FOUND);
}

static void check_hello_content(rktio_t *rktio, char *fn)
{
  rktio_fd_t *fd;
  intptr_t amt;
  char buffer[256], *s;
  
  fd = rktio_open(rktio, fn, RKTIO_OPEN_READ);
  check_valid(fd);
  check_valid(rktio_poll_read_ready(rktio, fd) == RKTIO_POLL_READY);
  amt = rktio_read(rktio, fd, buffer, sizeof(buffer));
  check_valid(amt == 5);
  check_valid(!strncmp(buffer, "hello", 5));
  amt = rktio_read(rktio, fd, buffer, sizeof(buffer));
  check_valid(amt == RKTIO_READ_EOF);
  check_valid(rktio_close(rktio, fd));
}

static void wait_read(rktio_t *rktio, rktio_fd_t *fd)
{
  rktio_poll_set_t *ps;
  ps = rktio_make_poll_set(rktio);
  check_valid(ps);
  rktio_poll_add(rktio, fd, ps, RKTIO_POLL_READ);
  rktio_sleep(rktio, 0, ps, NULL);
  rktio_poll_set_close(rktio, ps);
}

static void check_read_write_pair(rktio_t *rktio, rktio_fd_t *fd, rktio_fd_t *fd2, int immediate_available)
{
  rktio_ltps_t *lt;
  rktio_ltps_handle_t *h1, *h2;
  intptr_t amt, i;
  char buffer[256];

  lt = try_check_ltps(rktio, fd, fd2, &h1, &h2);
  /* We expect `lt` to work everywhere exception Windows and with kqueue on non-sockets: */
#if !defined(RKTIO_SYSTEM_WINDOWS)
# if !defined(HAVE_KQUEUE_SYSCALL)
  check_valid(lt);
# else
  if (rktio_fd_is_socket(rktio, fd) && rktio_fd_is_socket(rktio, fd2))
    check_valid(lt);
# endif
#endif

  /* fd2 can write, fd cannot yet read */
  check_valid(!rktio_poll_read_ready(rktio, fd));
  if (lt)
    check_ltps_write_ready(rktio, lt, h2);

  /* Round-trip data through pipe: */
  if (rktio_fd_is_udp(rktio, fd2)) {
    amt = rktio_udp_sendto(rktio, fd2, NULL, "hello", 5);
  } else
    amt = rktio_write(rktio, fd2, "hello", 5);
  check_valid(amt == 5);

  if (!immediate_available) {
    /* Wait for read to be ready; should not block for long */
    wait_read(rktio, fd);
  }
  
  check_valid(rktio_poll_read_ready(rktio, fd) == RKTIO_POLL_READY);
  if (lt) {
    check_ltps_read_ready(rktio, lt, h1);
    check_valid(rktio_ltps_close(rktio, lt));
  }

  if (rktio_fd_is_udp(rktio, fd)) {
    rktio_length_and_addrinfo_t *r;
    do {
      r = rktio_udp_recvfrom(rktio, fd, buffer, sizeof(buffer));
    } while (!r
             && (rktio_get_last_error_kind(rktio) == RKTIO_ERROR_KIND_RACKET)
             && (rktio_get_last_error(rktio) == RKTIO_ERROR_INFO_TRY_AGAIN));
    check_valid(r);
    amt = r->len;
    free(r->addr);
    free(r);
  } else
    amt = rktio_read(rktio, fd, buffer, sizeof(buffer));
  check_valid(amt == 5);
  check_valid(!strncmp(buffer, "hello", 5));
  check_valid(!rktio_poll_read_ready(rktio, fd));

  /* Close pipe ends: */
  check_valid(rktio_close(rktio, fd2));

  if (!rktio_fd_is_udp(rktio, fd)) {
    if (!immediate_available) {
      /* Wait for EOF to be ready; should not block for long */
      wait_read(rktio, fd);
    }
    
    amt = rktio_read(rktio, fd, buffer, sizeof(buffer));
    check_valid(amt == RKTIO_READ_EOF);
  }
  
  check_valid(rktio_close(rktio, fd));
}

#define AMOUNT_TO_WRITE_AND_BLOCK 1000000
#define AMOUNT_FOR_UDP 1000

static void check_fill_write(rktio_t *rktio, rktio_fd_t *fd2, rktio_addrinfo_t *dest_addr, intptr_t limit)
{
  intptr_t i, amt;

  if (!limit)
    limit = AMOUNT_TO_WRITE_AND_BLOCK;
  
  /* should eventually block, unless UDP: */
  for (i = 0; i < limit; i++) {
    if (dest_addr) {
      amt = rktio_udp_sendto(rktio, fd2, dest_addr, "hello", 5);
    } else
      amt = rktio_write(rktio, fd2, "hello", 5);
    check_valid(amt != RKTIO_WRITE_ERROR);
    if (!amt)
      break;
  }
  check_valid(i > 0);
  if (!rktio_fd_is_udp(rktio, fd2))
    check_valid(i < limit);
}

static void check_drain_read(rktio_t *rktio, rktio_fd_t *fd2, intptr_t limit)
{
  intptr_t i, amt;
  char buffer[256];

  if (!limit)
    limit = AMOUNT_TO_WRITE_AND_BLOCK;

  /* should eventually block: */
  for (i = 0; i < limit; i++) {
    amt = rktio_read(rktio, fd2, buffer, sizeof(buffer));
    check_valid(amt != RKTIO_READ_ERROR);
    check_valid(amt != RKTIO_READ_EOF);
    if (!amt)
      break;
  }
  check_valid(i > 0);
  check_valid(i < limit);
}

void check_many_lookup(rktio_t *rktio)
{
# define LOOKUPS_N 10
  int i, j;
  rktio_addrinfo_lookup_t *lookup[LOOKUPS_N];
  rktio_addrinfo_t *addr;
  rktio_poll_set_t *ps;

  for (i = 0; i < LOOKUPS_N; i++) {
    if (i & 1)
      lookup[i] = rktio_start_addrinfo_lookup(rktio, "localhost", 50+i, -1, 0, 1);
    else
      lookup[i] = rktio_start_addrinfo_lookup(rktio, "racket-lang.org", 50+i, -1, 0, 1);
    check_valid(lookup[i]);
  }

  for (j = 0; j < LOOKUPS_N; j++) {
    ps = rktio_make_poll_set(rktio);
    check_valid(ps);

    for (i = 0; i < LOOKUPS_N; i++) {
      if (lookup[i])
        rktio_poll_add_addrinfo_lookup(rktio, lookup[i], ps);
    }

    rktio_sleep(rktio, 0, ps, NULL);
    rktio_poll_set_close(rktio, ps);

    for (i = 0; i < LOOKUPS_N; i++) {
      if (lookup[i] && (rktio_poll_addrinfo_lookup_ready(rktio, lookup[i]) == RKTIO_POLL_READY)) {
        if ((i % 3) == 2)
          rktio_addrinfo_lookup_stop(rktio, lookup[i]);
        else {
          addr = rktio_addrinfo_lookup_get(rktio, lookup[i]);
          check_valid(addr);
          rktio_free_addrinfo(rktio, addr);
        }
        lookup[i] = NULL;
        break;
      }
    }
  }
}

rktio_addrinfo_t *lookup_loop(rktio_t *rktio,
                              const char *hostname, int portno,
                              int family, int passive, int tcp)
{
  rktio_addrinfo_lookup_t *lookup;
  rktio_addrinfo_t *addr;
  rktio_poll_set_t *ps;
  
  ps = rktio_make_poll_set(rktio);
  check_valid(ps);

  lookup = rktio_start_addrinfo_lookup(rktio, hostname, portno, family, passive, tcp);
  check_valid(lookup);

  rktio_poll_add_addrinfo_lookup(rktio, lookup, ps);
  rktio_sleep(rktio, 0, ps, NULL);
  rktio_poll_set_close(rktio, ps);
  check_valid(rktio_poll_addrinfo_lookup_ready(rktio, lookup) == RKTIO_POLL_READY);

  addr = rktio_addrinfo_lookup_get(rktio, lookup);
  check_valid(addr);

  return addr;
}

static void pause_for_process(rktio_t *rktio, rktio_process_t *process, int dont_rely_on_sigchild)
{
  int done;
  
  do {
    rktio_poll_set_t *ps;
    ps = rktio_make_poll_set(rktio);
    check_valid(ps);
    rktio_poll_add_process(rktio, process, ps);
    if (dont_rely_on_sigchild) {
      do {
        rktio_sleep(rktio, 0.1, ps, NULL);
      } while (!rktio_poll_process_done(rktio, process));
    } else {
      rktio_sleep(rktio, 0, ps, NULL);
    }
    rktio_poll_set_close(rktio, ps);
    done = rktio_poll_process_done(rktio, process);
    check_valid(done != RKTIO_PROCESS_ERROR);
  } while (!done);
}

static rktio_fd_t *connect_loop(rktio_t *rktio, rktio_addrinfo_t *addr, rktio_addrinfo_t *local_addr)
{
  rktio_connect_t *conn;
  rktio_poll_set_t *ps;
  rktio_fd_t *fd;

  conn = rktio_start_connect(rktio, addr, local_addr);
  check_valid(conn);

  while (1) {
    ps = rktio_make_poll_set(rktio);
    check_valid(ps);
    
    rktio_poll_add_connect(rktio, conn, ps);
    rktio_sleep(rktio, 0, ps, NULL);
    rktio_poll_set_close(rktio, ps);
    check_valid(rktio_poll_connect_ready(rktio, conn) == RKTIO_POLL_READY);

    fd = rktio_connect_finish(rktio, conn);
    if (!fd) {
      if ((rktio_get_last_error_kind(rktio) == RKTIO_ERROR_KIND_RACKET)
          && (rktio_get_last_error(rktio) == RKTIO_ERROR_CONNECT_TRYING_NEXT)) {
        /* loop to try again */
      } else {
        check_valid(fd);
      }
    } else
      break;
  }

  return fd;
}

int main(int argc, char **argv)
{
  rktio_t *rktio;
  rktio_size_t *sz;
  rktio_fd_t *fd, *fd2;
  intptr_t amt, i, saw_file;
  int perms;
  char *s, *pwd;
  rktio_directory_list_t *ls;
  rktio_file_copy_t *cp;
  rktio_timestamp_t *ts1, *ts1a;
  rktio_ltps_t *lt;
  rktio_ltps_handle_t *h1, *h2;
  int verbose = 0, dont_rely_on_sigchild = 0;

  for (i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "-v"))
      verbose = 1;
    else if (!strcmp(argv[i], "--sleep-blocks-sigchld")) {
      /* Seems useful for Valgrind on MacOS */
      dont_rely_on_sigchild = 1;
    } else {
      printf(" unrecognized flag %s\n", argv[i]);
      return 1;
    }
  }
  
  rktio = rktio_init();

  /* Basic file I/O */

  fd = rktio_open(rktio, "test1", RKTIO_OPEN_WRITE | RKTIO_OPEN_CAN_EXIST);
  check_valid(fd);
  check_valid(rktio_poll_write_ready(rktio, fd) != RKTIO_POLL_ERROR);
  amt = rktio_write(rktio, fd, "hello", 5);
  check_valid(amt == 5);
  check_valid(rktio_close(rktio, fd));

  check_valid(rktio_file_exists(rktio, "test1"));
  check_valid(!rktio_directory_exists(rktio, "test1"));
  check_valid(rktio_is_regular_file(rktio, "test1"));

  s = rktio_get_current_directory(rktio);
  check_valid(s);
  check_valid(rktio_directory_exists(rktio, s));
  check_valid(!rktio_file_exists(rktio, s));
  check_valid(!rktio_is_regular_file(rktio, s));
  check_expected_racket_error(!rktio_open(rktio, s, RKTIO_OPEN_WRITE | RKTIO_OPEN_CAN_EXIST),
                              RKTIO_ERROR_IS_A_DIRECTORY);
  pwd = s;

  sz = rktio_file_size(rktio, "test1");
  check_valid(sz);
  check_valid(sz->lo == 5);
  check_valid(sz->hi == 0);
  free(sz);

  fd = rktio_open(rktio, "test2", RKTIO_OPEN_WRITE | RKTIO_OPEN_MUST_EXIST);
  check_expected_error(!fd);

  fd = rktio_open(rktio, "test1", RKTIO_OPEN_WRITE);
  check_expected_racket_error(!fd, RKTIO_ERROR_EXISTS);

  check_hello_content(rktio, "test1");

  /* Copying, renaming, and deleting files */

  if (rktio_file_exists(rktio, "test1a"))
    check_valid(rktio_delete_file(rktio, "test1a", 1));
  if (rktio_file_exists(rktio, "test1b"))
    check_valid(rktio_delete_file(rktio, "test1b", 1));

  ts1 = rktio_get_file_modify_seconds(rktio, "test1a");
  check_expected_error(!ts1);
  perms = rktio_get_file_or_directory_permissions(rktio, "test1a", 1);
  check_expected_error(perms == -1);
  check_expected_error(!rktio_set_file_or_directory_permissions(rktio, "test1a", 511));

  ts1 = rktio_get_file_modify_seconds(rktio, "test1");
  perms = rktio_get_file_or_directory_permissions(rktio, "test1", 0);
  check_valid(perms != -1);
  check_valid(perms & (RKTIO_PERMISSION_READ << 6));
  check_valid(perms & (RKTIO_PERMISSION_WRITE << 6));
  perms = rktio_get_file_or_directory_permissions(rktio, "test1", 1);
  check_valid(perms != -1);
  check_valid(perms & (RKTIO_PERMISSION_READ << 6));
  check_valid(perms & (RKTIO_PERMISSION_WRITE << 6));
  check_valid(rktio_set_file_or_directory_permissions(rktio, "test1", perms & (0x7 << 6)));
  check_valid((perms & (0x7 << 6)) == rktio_get_file_or_directory_permissions(rktio, "test1", 1));
  rktio_set_file_or_directory_permissions(rktio, "test1", perms);

  cp = rktio_copy_file_start(rktio, "test1a", "test1", 0);
  check_valid(cp);
  while (!rktio_copy_file_is_done(rktio, cp)) {
    check_valid(rktio_copy_file_step(rktio, cp));
  }
  rktio_copy_file_stop(rktio, cp);
  check_hello_content(rktio, "test1a");

  ts1a = rktio_get_file_modify_seconds(rktio, "test1a");
  check_valid(*ts1a >= *ts1);

  rktio_set_file_modify_seconds(rktio, "test1a", *ts1 - 10);
  free(ts1a);
  ts1a = rktio_get_file_modify_seconds(rktio, "test1a");
  check_valid(*ts1a == (*ts1 - 10));
  
  free(ts1);
  free(ts1a);

  check_valid(rktio_file_exists(rktio, "test1a"));
  cp = rktio_copy_file_start(rktio, "test1a", "test1", 0);
  check_expected_racket_error(!cp, RKTIO_ERROR_EXISTS);

  cp = rktio_copy_file_start(rktio, "test1a", "test1", 1);
  check_valid(cp);
  rktio_copy_file_stop(rktio, cp);

  check_valid(rktio_rename_file(rktio, "test1b", "test1a", 0));
  check_valid(rktio_file_exists(rktio, "test1b"));
  check_expected_racket_error(!rktio_rename_file(rktio, "test1b", "test1", 0),
                              RKTIO_ERROR_EXISTS);
  check_valid(rktio_file_exists(rktio, "test1"));
  check_valid(rktio_file_exists(rktio, "test1b"));
  check_valid(!rktio_file_exists(rktio, "test1a"));
  
  check_valid(rktio_delete_file(rktio, "test1b", 0));
  check_valid(!rktio_file_exists(rktio, "test1b"));

  /* Listing directory content */

  ls = rktio_directory_list_start(rktio, pwd, 0);
  check_valid(ls);
  saw_file = 0;
  while (1) {
    s = rktio_directory_list_step(rktio, ls);
    check_valid(s);
    if (!*s) break;
    if (!strcmp(s, "test1"))
      saw_file = 1;
    check_valid(strcmp(s, "test1b"));
    free(s);
  }
  check_valid(saw_file);

  /* We expect `lt` to work on regular files except on Windows and epoll: */
#if !defined(RKTIO_SYSTEM_WINDOWS) && !defined(HAVE_EPOLL_SYSCALL)
  fd = rktio_open(rktio, "test1", RKTIO_OPEN_READ);
  check_valid(fd);
  fd2 = rktio_open(rktio, "test1", RKTIO_OPEN_WRITE | RKTIO_OPEN_CAN_EXIST);
  check_valid(fd2);
  lt = try_check_ltps(rktio, fd, fd2, &h1, &h2);
  check_valid(lt);
  check_ltps_read_and_write_ready(rktio, lt, h1, h2);
  check_valid(rktio_ltps_close(rktio, lt));
  check_valid(rktio_close(rktio, fd));
  check_valid(rktio_close(rktio, fd2));
#endif

  /* Pipes, non-blocking operations, and more long-term poll sets */

  fd = rktio_open(rktio, "demo_fifo", RKTIO_OPEN_READ);
  check_valid(fd);
  check_valid(!rktio_poll_read_ready(rktio, fd));
  fd2 = rktio_open(rktio, "demo_fifo", RKTIO_OPEN_WRITE | RKTIO_OPEN_CAN_EXIST);
  check_valid(fd2);
  check_valid(!rktio_poll_read_ready(rktio, fd));

  check_read_write_pair(rktio, fd, fd2, 1);

  /* Open pipe ends again: */
  fd2 = rktio_open(rktio, "demo_fifo", RKTIO_OPEN_WRITE | RKTIO_OPEN_CAN_EXIST);
  check_valid(fd2);
  check_fill_write(rktio, fd2, NULL, 0);

  fd = rktio_open(rktio, "demo_fifo", RKTIO_OPEN_READ);
  check_valid(fd);
  check_drain_read(rktio, fd2, 0);
    
  check_valid(rktio_close(rktio, fd));
  check_valid(rktio_close(rktio, fd2));

  /* Networking */

  if (verbose)
    printf("tcp\n");

  {
    rktio_addrinfo_t *addr;
    rktio_listener_t *lnr;

    check_many_lookup(rktio);
    
    addr = lookup_loop(rktio, NULL, 4536, -1, 1, 1);

    lnr = rktio_listen(rktio, addr, 5, 1);
    check_valid(lnr);
    rktio_free_addrinfo(rktio, addr);

    check_valid(!rktio_poll_accept_ready(rktio, lnr));

    addr = lookup_loop(rktio, "localhost", 4536, -1, 0, 1);
    fd = connect_loop(rktio, addr, NULL);

    check_valid(rktio_poll_accept_ready(rktio, lnr) == RKTIO_POLL_READY);

    fd2 = rktio_accept(rktio, lnr);
    check_valid(fd2);
    check_valid(!rktio_poll_accept_ready(rktio, lnr));

    {
      /* Dup'ing and closing old should work as well as using directly */
      rktio_fd_t *fdx;
      
      fdx = rktio_dup(rktio, fd);
      check_valid(fdx);
      check_valid(rktio_close(rktio, fd));
      fd = fdx;
      
      fdx = rktio_dup(rktio, fd2);
      check_valid(fdx);
      check_valid(rktio_close(rktio, fd2));
      fd2 = fdx;
    }

    {
      char **strs;
      
      strs = rktio_socket_address(rktio, fd2);
      check_valid(strs);
      printf("client: %s %s\n", strs[0], strs[1]);
      free(strs[0]);
      free(strs[1]);
      free(strs);
      
      strs = rktio_socket_peer_address(rktio, fd2);
      check_valid(strs);
      printf("server: %s %s\n", strs[0], strs[1]);
      free(strs[0]);
      free(strs[1]);
      free(strs);
    }

    check_read_write_pair(rktio, fd, fd2, 0);

    fd = connect_loop(rktio, addr, NULL);
    rktio_free_addrinfo(rktio, addr);
    
    fd2 = rktio_accept(rktio, lnr);

    check_fill_write(rktio, fd2, NULL, 0);
    check_drain_read(rktio, fd, 0);

    check_valid(rktio_close(rktio, fd));
    check_valid(rktio_close(rktio, fd2));
    
    rktio_listen_stop(rktio, lnr);
  }

  /* UDP */

  if (verbose)
    printf("udp\n");
  
  {
    rktio_addrinfo_t *intf_addr, *addr;

    intf_addr = lookup_loop(rktio, "localhost", 0, -1, 1, 0);
    check_valid(intf_addr);
    
    fd = rktio_udp_open(rktio, intf_addr);
    check_valid(fd);
    
    addr = lookup_loop(rktio, NULL, 4536, -1, 1, 0);
    check_valid(addr);
    check_valid(rktio_udp_bind(rktio, fd, addr));
    rktio_free_addrinfo(rktio, addr);

    fd2 = rktio_udp_open(rktio, intf_addr);
    check_valid(fd2);

    addr = lookup_loop(rktio, "localhost", 4536, -1, 0, 0);
    check_valid(addr);
    check_valid(rktio_udp_connect(rktio, fd2, addr));
    rktio_free_addrinfo(rktio, addr);

    check_read_write_pair(rktio, fd, fd2, 0);

    /* Again, this time to fill & drain: */

    fd = rktio_udp_open(rktio, intf_addr);

    fd2 = rktio_udp_open(rktio, intf_addr);
    check_valid(fd2);
    addr = lookup_loop(rktio, NULL, 4536, -1, 1, 0);
    check_valid(addr);
    check_valid(rktio_udp_bind(rktio, fd, addr));
    rktio_free_addrinfo(rktio, addr);

    addr = lookup_loop(rktio, "localhost", 4536, -1, 0, 0);
    check_valid(addr);

    check_fill_write(rktio, fd2, addr, AMOUNT_FOR_UDP);
    check_drain_read(rktio, fd, AMOUNT_FOR_UDP+1);

    rktio_free_addrinfo(rktio, addr);
    rktio_free_addrinfo(rktio, intf_addr);

    check_valid(rktio_close(rktio, fd));
    check_valid(rktio_close(rktio, fd2));
  }

  /* Processes */

  if (verbose)
    printf("processes\n");

  {
    rktio_status_t *status;
    rktio_process_result_t *result;
    char *argv[1] = { "/bin/cat" };
    rktio_envvars_t *envvars = rktio_envvars(rktio);
    rktio_fd_t *err_fd = rktio_system_fd(rktio, 2, RKTIO_OPEN_WRITE);
    int i;
    
    result = rktio_process(rktio, argv[0], 1, argv,
                           NULL, NULL, err_fd,
                           pwd, envvars,
                           0,
                           NULL);
    check_valid(result);
    check_valid(!result->stderr_fd);

    status = rktio_process_status(rktio, result->process);
    check_valid(status);
    check_valid(status->running);
    free(status);

    check_valid(!rktio_poll_process_done(rktio, result->process));

    check_read_write_pair(rktio, result->stdout_fd, result->stdin_fd, 0);

    check_valid(rktio_poll_process_done(rktio, result->process) != RKTIO_PROCESS_ERROR);

    pause_for_process(rktio, result->process, dont_rely_on_sigchild);

    status = rktio_process_status(rktio, result->process);
    check_valid(status);
    check_valid(!status->running);
    check_valid(!status->result);
    free(status);

    rktio_process_forget(rktio, result->process);
    free(result);

    /* Run and then break or kill `cat` */
    for (i = 0; i < 2; i++) {
      result = rktio_process(rktio, argv[0], 1, argv,
                             NULL, NULL, err_fd,
                             pwd, envvars,
                             0,
                             NULL);
      check_valid(result);
      
      check_valid(!rktio_poll_process_done(rktio, result->process));
      rktio_sleep(rktio, 0.05, NULL, NULL);
      check_valid(!rktio_poll_process_done(rktio, result->process));

      switch (i) {
      case 0:
        if (verbose)
          printf(" interrupt\n");
        check_valid(rktio_process_interrupt(rktio, result->process));
        break;
      case 1:
        if (verbose)
          printf(" kill\n");
        check_valid(rktio_process_kill(rktio, result->process));
        break;
      }

      pause_for_process(rktio, result->process, dont_rely_on_sigchild);

      status = rktio_process_status(rktio, result->process);
      check_valid(status);
      check_valid(!status->running);
      check_valid(status->result);
      free(status);

      {
        char buffer[1];
        intptr_t amt;
        amt = rktio_read(rktio, result->stdout_fd, buffer, sizeof(buffer));
        check_valid(amt == RKTIO_READ_EOF);
      }
  
      check_valid(rktio_close(rktio, result->stdin_fd));
      check_valid(rktio_close(rktio, result->stdout_fd));
    
      rktio_process_forget(rktio, result->process);
      free(result);
    }

    {
      char *argv[2] = { "/usr/bin/printenv", "RKTIO_EXAMPLE" };

      if (verbose)
        printf(" envvars\n");

      check_valid(!rktio_envvars_get(rktio, envvars, "RKTIO_EXAMPLE"));
      rktio_envvars_set(rktio, envvars, "RKTIO_EXAMPLE", "howdy");
      s = rktio_envvars_get(rktio, envvars, "RKTIO_EXAMPLE");
      check_valid(s);
      check_valid(!strcmp(s, "howdy"));
      free(s);
      
      result = rktio_process(rktio, argv[0], 2, argv,
                             NULL, NULL, err_fd,
                             pwd, envvars,
                             0,
                             NULL);
      check_valid(result);

      /* Assume that a pipe can buffer the minimal output from `printenv`: */
      pause_for_process(rktio, result->process, dont_rely_on_sigchild);
      wait_read(rktio, result->stdout_fd);

      {
        char buffer[32];
        intptr_t amt;
        amt = rktio_read(rktio, result->stdout_fd, buffer, sizeof(buffer));
        check_valid(amt == 6);
        check_valid(!strncmp(buffer, "howdy\n", 6));
      }      

      check_valid(rktio_close(rktio, result->stdin_fd));
      check_valid(rktio_close(rktio, result->stdout_fd));

      rktio_process_forget(rktio, result->process);
      free(result);
    }
    
    rktio_ennvars_free(rktio, envvars);

    rktio_forget(rktio, err_fd);
  }

  if (verbose)
    printf("done\n");

  free(pwd);
  rktio_destroy(rktio);
  
  return 0;
}
