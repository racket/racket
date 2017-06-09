#include "rktio.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void do_check_valid(int ok, int where)
{
  if (!ok) {
    printf("error at %d: %d@%d = %s\n",
           where,
           rktio_get_last_error(),
           rktio_get_last_error_kind(),
           rktio_get_error_string(rktio_get_last_error(),
                                  rktio_get_last_error_kind()));
  }
}

static void do_check_expected_error(int err, int where)
{
  if (!err) {
    printf("error expected at %d\n",
           where);
  }
}

static void do_check_expected_racket_error(int err, int what, int where)
{
  if (!err) {
    printf("error expected at %d\n",
           where);
  } else if ((what != rktio_get_last_error())
             || (RKTIO_ERROR_KIND_RACKET != rktio_get_last_error_kind())) {
    printf("wrong error at %d: %d@%d = %s\n",
           where,
           rktio_get_last_error(),
           rktio_get_last_error_kind(),
           rktio_get_error_string(rktio_get_last_error(),
                                  rktio_get_last_error_kind()));
  }
}

#define check_valid(e) do_check_valid(e, __LINE__)
#define check_expected_error(e) do_check_expected_error(e, __LINE__)
#define check_expected_racket_error(e, what) do_check_expected_racket_error(e, what, __LINE__)

static void check_hello_content(char *fn)
{
  rktio_fd_t *fd;
  intptr_t amt;
  char buffer[256], *s;
  
  fd = rktio_open(fn, RKTIO_OPEN_READ);
  check_valid(!!fd);
  check_valid(rktio_poll_read_ready(fd) != RKTIO_POLL_ERROR);
  amt = rktio_read(fd, buffer, sizeof(buffer));
  check_valid(amt == 5);
  check_valid(!strncmp(buffer, "hello", 5));
  check_valid(rktio_close(fd));
}

int main()
{
  rktio_size_t *sz;
  rktio_fd_t *fd;
  intptr_t amt, i,saw_file;
  char *s, *pwd;
  rktio_directory_list_t *ls;
  rktio_file_copy_t *cp;

  fd = rktio_open("test1", RKTIO_OPEN_WRITE | RKTIO_OPEN_CAN_EXIST);
  check_valid(!!fd);
  check_valid(rktio_poll_write_ready(fd) != RKTIO_POLL_ERROR);
  amt = rktio_write(fd, "hello", 5);
  check_valid(amt == 5);
  check_valid(rktio_close(fd));

  check_valid(rktio_file_exists("test1"));
  check_valid(!rktio_directory_exists("test1"));
  check_valid(rktio_is_regular_file("test1"));

  s = rktio_get_current_directory();
  check_valid(!!s);
  check_valid(rktio_directory_exists(s));
  check_valid(!rktio_file_exists(s));
  check_valid(!rktio_is_regular_file(s));
  check_expected_racket_error(!rktio_open(s, RKTIO_OPEN_WRITE | RKTIO_OPEN_CAN_EXIST),
                              RKTIO_ERROR_IS_A_DIRECTORY);
  pwd = s;

  sz = rktio_file_size("test1");
  check_valid(!!sz);
  check_valid(sz->lo == 5);
  check_valid(sz->hi == 0);
  free(sz);

  fd = rktio_open("test2", RKTIO_OPEN_WRITE | RKTIO_OPEN_MUST_EXIST);
  check_expected_error(!fd);

  fd = rktio_open("test1", RKTIO_OPEN_WRITE);
  check_expected_racket_error(!fd, RKTIO_ERROR_EXISTS);

  check_hello_content("test1");

  if (rktio_file_exists("test1a"))
    check_valid(rktio_delete_file("test1a", 1));
  if (rktio_file_exists("test1b"))
    check_valid(rktio_delete_file("test1b", 1));

  cp = rktio_copy_file_start("test1a", "test1", 0);
  check_valid(!!cp);
  while (!rktio_copy_file_is_done(cp)) {
    check_valid(rktio_copy_file_step(cp));
  }
  rktio_copy_file_stop(cp);
  check_hello_content("test1a");

  check_valid(rktio_file_exists("test1a"));
  cp = rktio_copy_file_start("test1a", "test1", 0);
  check_expected_racket_error(!cp, RKTIO_ERROR_EXISTS);

  cp = rktio_copy_file_start("test1a", "test1", 1);
  check_valid(!!cp);
  rktio_copy_file_stop(cp);

  check_valid(rktio_rename_file("test1b", "test1a", 0));
  check_valid(rktio_file_exists("test1b"));
  check_expected_racket_error(!rktio_rename_file("test1b", "test1", 0),
                              RKTIO_ERROR_EXISTS);
  check_valid(rktio_file_exists("test1"));
  check_valid(rktio_file_exists("test1b"));
  check_valid(!rktio_file_exists("test1a"));
  
  check_valid(rktio_delete_file("test1b", 0));
  check_valid(!rktio_file_exists("test1b"));

  ls = rktio_directory_list_start(pwd, 0);
  check_valid(!!ls);
  saw_file = 0;
  while (1) {
    s = rktio_directory_list_step(ls);
    check_valid(!!s);
    if (!*s) break;
    if (!strcmp(s, "test1"))
      saw_file = 1;
    check_valid(strcmp(s, "test1b"));
  }
  check_valid(saw_file);
 
  return 0;
}
