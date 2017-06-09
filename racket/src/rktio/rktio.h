#ifndef __RKTIO_H__
#define __RKTIO_H__

#include "rktio_config.h"

/*************************************************/
/* Reading and writing files                     */

typedef struct rktio_fd_t rktio_fd_t;

#define RKTIO_OPEN_READ        (1<<0)
#define RKTIO_OPEN_WRITE       (1<<1)
#define RKTIO_OPEN_TRUNCATE    (1<<2)
#define RKTIO_OPEN_APPEND      (1<<3)
#define RKTIO_OPEN_REPLACE     (1<<4)
#define RKTIO_OPEN_MUST_EXIST  (1<<5)
#define RKTIO_OPEN_CAN_EXIST   (1<<6)

rktio_fd_t *rktio_fd(intptr_t system_fd, int modes);

rktio_fd_t *rktio_open(char *src, int modes);
int rktio_close(rktio_fd_t *fd);

#define RKTIO_READ_EOF   (-1)
#define RKTIO_READ_ERROR (-2)
#define RKTIO_WRITE_ERROR (-2)
#define RKTIO_POLL_ERROR (-2)

intptr_t rktio_read(rktio_fd_t *fd, char *buffer, intptr_t len);
intptr_t rktio_write(rktio_fd_t *fd, char *buffer, intptr_t len);

int rktio_poll_read_ready(rktio_fd_t *rfd);
int rktio_poll_write_ready(rktio_fd_t *rfd);
int rktio_poll_write_flushed(rktio_fd_t *rfd);

/*************************************************/
/* File-descriptor sets                          */

typedef struct rktio_poll_set_t rktio_poll_set_t;

#define RKTIO_POLL_READ   RKTIO_OPEN_READ
#define RKTIO_POLL_WRITE  RKTIO_OPEN_WRITE

void rktio_poll_add(rktio_fd_t *rfd, rktio_poll_set_t *fds, int modes);

/*************************************************/
/* Files, directories, and links                 */

int rktio_file_exists(char *filename);
int rktio_directory_exists(char *dirname);
int rktio_link_exists(char *filename);
int rktio_is_regular_file(char *filename);

int rktio_delete_file(char *fn, int enable_write_on_fail);
int rktio_rename_file(char *dest, char *src, int exists_ok);

char *rktio_get_current_directory();
int rktio_set_current_directory(char *expanded);
int rktio_make_directory(char *filename);
int rktio_delete_directory(char *filename, char *current_directory, int enable_write_on_fail);

char *rktio_readlink(char *fullfilename);
int rktio_make_link(char *src, char *dest, int dest_is_directory);

/*************************************************/
/* File attributes                               */

typedef struct {
  unsigned lo, hi;
} rktio_size_t;

typedef intptr_t rktio_timestamp_t;

typedef struct {
  uintptr_t a, b, c;
} rktio_identity_t;

rktio_size_t *rktio_file_size(char *filename);

rktio_timestamp_t *rktio_get_file_modify_seconds(char *file);
int rktio_set_file_modify_seconds(char *file, rktio_timestamp_t secs);

rktio_identity_t *rktio_get_fd_identity(intptr_t fd, char *path);

/*************************************************/
/* Permissions                                   */

/* Must match OS bits: */
#define RKTIO_PERMISSION_READ  0x1
#define RKTIO_PERMISSION_WRITE 0x2
#define RKTIO_PERMISSION_EXEC  0x4

int rktio_get_file_or_directory_permissions(char *filename, int all_bits);
int rktio_set_file_or_directory_permissions(char *filename, int new_bits);

/*************************************************/
/* Directory listing                             */

typedef struct rktio_directory_list_t rktio_directory_list_t;

rktio_directory_list_t *rktio_directory_list_start(char *filename, int is_drive);
char *rktio_directory_list_step(rktio_directory_list_t *dl);

char **rktio_filesystem_root_list();

/*************************************************/
/* File copying                                  */

typedef struct rktio_file_copy_t rktio_file_copy_t;

rktio_file_copy_t *rktio_copy_file_start(char *dest, char *src, int exists_ok);
int rktio_copy_file_is_done(rktio_file_copy_t *fc);
int rktio_copy_file_step(rktio_file_copy_t *fc);
void rktio_copy_file_stop(rktio_file_copy_t *fc);

/*************************************************/
/* System paths                                  */

enum {
  RKTIO_PATH_SYS_DIR,
  RKTIO_PATH_TEMP_DIR,
  RKTIO_PATH_PREF_DIR,
  RKTIO_PATH_PREF_FILE,
  RKTIO_PATH_ADDON_DIR,
  RKTIO_PATH_HOME_DIR,
  RKTIO_PATH_DESK_DIR,
  RKTIO_PATH_DOC_DIR,
  RKTIO_PATH_INIT_DIR,
  RKTIO_PATH_INIT_FILE
};

char *rktio_system_path(int which);
char *rktio_expand_user_tilde(char *filename);

/*************************************************/
/* Errors                                        */

/* Kinds of error values: */
enum {
  RKTIO_ERROR_KIND_POSIX,
  RKTIO_ERROR_KIND_WINDOWS,
  RKTIO_ERROR_KIND_GAI,
  RKTIO_ERROR_KIND_RACKET
};

/* Error IDs of kind RKTIO_ERROR_KIND_RACKET */
enum {
  RKTIO_ERROR_UNSUPPORTED,
  RKTIO_ERROR_EXISTS,
  RKTIO_ERROR_LINK_FAILED,
  RKTIO_ERROR_NOT_A_LINK,
  RKTIO_ERROR_BAD_PERMISSION,
  RKTIO_ERROR_IS_A_DIRECTORY,
  RKTIO_ERROR_NOT_A_DIRECTORY,
  RKTIO_ERROR_NO_TILDE,
  RKTIO_ERROR_ILL_FORMED_USER,
  RKTIO_ERROR_UNKNOWN_USER
};

int rktio_get_last_error(void);
int rktio_get_last_error_kind(void);

char *rktio_get_error_string(int kind, int errid);

/*************************************************/

#endif
