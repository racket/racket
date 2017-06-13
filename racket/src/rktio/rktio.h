#ifndef __RKTIO_H__
#define __RKTIO_H__

#include "rktio_config.h"

#ifndef RKTIO_EXTERN
# define RKTIO_EXTERN extern
#endif

/* A rktio_t value represents an instance of the Racket I/O system.
   Almost every rktio_...() function takes it as the first argument. */
typedef struct rktio_t rktio_t;

RKTIO_EXTERN rktio_t *rktio_init(void);
RKTIO_EXTERN void rktio_destroy(rktio_t *);

/* Normally equivalent to free(), but ensures the same malloc()/free()
   that rktio function use: */
RKTIO_EXTERN void rktio_free(void *p);

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
#define RKTIO_OPEN_SOCKET      (1<<7)
#define RKTIO_OPEN_UDP         (1<<8)
#define RKTIO_OPEN_REGFILE     (1<<10)
#define RKTIO_OPEN_NOT_REGFILE (1<<11)

/* If neither RKTIO_OPEN_REGILE nor RKTIO_OPEN_NOT_REGILE
   are specified, then the value is inferred */

/* A socket registered this way should be non-blocking: */
RKTIO_EXTERN rktio_fd_t *rktio_system_fd(rktio_t *rktio, intptr_t system_fd, int modes);
RKTIO_EXTERN intptr_t rktio_fd_system_fd(rktio_t *rktio, rktio_fd_t *rfd);

RKTIO_EXTERN int rktio_fd_is_regular_file(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_fd_is_socket(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_fd_is_udp(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_fd_is_terminal(rktio_t *rktio, rktio_fd_t *rfd);

RKTIO_EXTERN int rktio_fd_modes(rktio_t *rktio, rktio_fd_t *rfd);

RKTIO_EXTERN rktio_fd_t *rktio_open(rktio_t *rktio, char *src, int modes);
RKTIO_EXTERN int rktio_close(rktio_t *rktio, rktio_fd_t *fd);

RKTIO_EXTERN rktio_fd_t *rktio_dup(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN void rktio_forget(rktio_t *rktio, rktio_fd_t *fd);

#define RKTIO_READ_EOF   (-1)
#define RKTIO_READ_ERROR (-2)
#define RKTIO_WRITE_ERROR (-2)
#define RKTIO_POLL_ERROR (-2)
#define RKTIO_POLL_READY 1

RKTIO_EXTERN intptr_t rktio_read(rktio_t *rktio, rktio_fd_t *fd, char *buffer, intptr_t len);
RKTIO_EXTERN intptr_t rktio_write(rktio_t *rktio, rktio_fd_t *fd, char *buffer, intptr_t len);

RKTIO_EXTERN int rktio_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_poll_write_flushed(rktio_t *rktio, rktio_fd_t *rfd);

#define RKTIO_LOCK_ERROR (-2)
#define RKTIO_LOCK_ACQUIRED 1

RKTIO_EXTERN int rktio_file_lock_try(rktio_t *rktio, rktio_fd_t *rfd, int excl);
RKTIO_EXTERN int rktio_file_unlock(rktio_t *rktio, rktio_fd_t *rfd);

/*************************************************/
/* Network                                       */

typedef struct rktio_addrinfo_lookup_t rktio_addrinfo_lookup_t;
typedef struct rktio_addrinfo_t rktio_addrinfo_t;

RKTIO_EXTERN int rktio_get_ipv4_family(rktio_t *rktio);

RKTIO_EXTERN rktio_addrinfo_lookup_t *rktio_start_addrinfo_lookup(rktio_t *rktio,
                                                                  const char *hostname, int portno,
                                                                  int family, int passive, int tcp);
RKTIO_EXTERN int rktio_poll_addrinfo_lookup_ready(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup);
RKTIO_EXTERN rktio_addrinfo_t *rktio_addrinfo_lookup_get(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup);
RKTIO_EXTERN void rktio_addrinfo_lookup_stop(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup);

RKTIO_EXTERN void rktio_free_addrinfo(rktio_t *rktio, struct rktio_addrinfo_t *a);

typedef struct rktio_listener_t rktio_listener_t;
typedef struct rktio_connect_t rktio_connect_t;

#define RKTIO_SHUTDOWN_READ   RKTIO_OPEN_READ
#define RKTIO_SHUTDOWN_WRITE  RKTIO_OPEN_WRITE

RKTIO_EXTERN rktio_listener_t *rktio_listen(rktio_t *rktio, rktio_addrinfo_t *local, int backlog, int reuse);
RKTIO_EXTERN void rktio_listen_stop(rktio_t *rktio, rktio_listener_t *l);
RKTIO_EXTERN int rktio_poll_accept_ready(rktio_t *rktio, rktio_listener_t *listener);
RKTIO_EXTERN rktio_fd_t *rktio_accept(rktio_t *rktio, rktio_listener_t *listener);

/* Addreses must not be freed until the connection is complete or stopped: */
RKTIO_EXTERN rktio_connect_t *rktio_start_connect(rktio_t *rktio, rktio_addrinfo_t *remote, rktio_addrinfo_t *local);
/* A `RKTIO_ERROR_CONNECT_TRYING_NEXT` error effectively means "try again",
   and the connection object is still valid: */
RKTIO_EXTERN rktio_fd_t *rktio_connect_finish(rktio_t *rktio, rktio_connect_t *conn);
RKTIO_EXTERN void rktio_connect_stop(rktio_t *rktio, rktio_connect_t *conn);
RKTIO_EXTERN int rktio_poll_connect_ready(rktio_t *rktio, rktio_connect_t *conn);

RKTIO_EXTERN int rktio_socket_shutdown(rktio_t *rktio, rktio_fd_t *rfd, int mode);

RKTIO_EXTERN rktio_fd_t *rktio_udp_open(rktio_t *rktio, rktio_addrinfo_t *addr);
RKTIO_EXTERN int rktio_udp_disconnect(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_udp_bind(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr);
RKTIO_EXTERN int rktio_udp_connect(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr);

RKTIO_EXTERN intptr_t rktio_udp_sendto(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr,
                                       char *buffer, intptr_t len);

typedef struct rktio_length_and_addrinfo_t {
  intptr_t len;
  rktio_addrinfo_t *addr;
} rktio_length_and_addrinfo_t;

RKTIO_EXTERN rktio_length_and_addrinfo_t *rktio_udp_recvfrom(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len);

#define RKTIO_PROP_ERROR (-2)

/* The following accessors return RKTIO_PROP_ERROR on failure */
RKTIO_EXTERN int rktio_udp_get_multicast_loopback(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_udp_set_multicast_loopback(rktio_t *rktio, rktio_fd_t *rfd, int on);
RKTIO_EXTERN int rktio_udp_get_multicast_ttl(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_udp_set_multicast_ttl(rktio_t *rktio, rktio_fd_t *rfd, int ttl_val);

RKTIO_EXTERN char **rktio_socket_address(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN char **rktio_socket_peer_address(rktio_t *rktio, rktio_fd_t *rfd);

RKTIO_EXTERN char *rktio_udp_multicast_interface(rktio_t *rktio, rktio_fd_t *rfd);
RKTIO_EXTERN int rktio_udp_set_multicast_interface(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr);

enum {
  RKTIO_ADD_MEMBERSHIP,
  RKTIO_DROP_MEMBERSHIP
};

RKTIO_EXTERN int rktio_udp_change_multicast_group(rktio_t *rktio, rktio_fd_t *rfd,
                                                  rktio_addrinfo_t *group_addr,
                                                  rktio_addrinfo_t *intf_addr,
                                                  int action);

/*************************************************/
/* Environment variables                         */

typedef struct rktio_envvars_t rktio_envvars_t;

RKTIO_EXTERN char *rktio_getenv(rktio_t *rktio, char *name);
RKTIO_EXTERN int rktio_setenv(rktio_t *rktio, const char *name, const char *val);

RKTIO_EXTERN rktio_envvars_t *rktio_envvars(rktio_t *rktio);
RKTIO_EXTERN rktio_envvars_t *rktio_empty_envvars(rktio_t *rktio);
RKTIO_EXTERN rktio_envvars_t *rktio_envvars_copy(rktio_t *rktio, rktio_envvars_t *envvars);
RKTIO_EXTERN void rktio_envvars_free(rktio_t *rktio, rktio_envvars_t *envvars);

RKTIO_EXTERN char *rktio_envvars_get(rktio_t *rktio, rktio_envvars_t *envvars, char *name);
RKTIO_EXTERN void rktio_envvars_set(rktio_t *rktio, rktio_envvars_t *envvars, char *name, char *value);

RKTIO_EXTERN intptr_t rktio_envvars_count(rktio_t *rktio, rktio_envvars_t *envvars);
RKTIO_EXTERN char *rktio_envvars_name_ref(rktio_t *rktio, rktio_envvars_t *envvars, intptr_t i);
RKTIO_EXTERN char *rktio_envvars_value_ref(rktio_t *rktio, rktio_envvars_t *envvars, intptr_t i);

/*************************************************/
/* Processes                                     */

typedef struct rktio_process_t rktio_process_t;

#define RKTIO_PROCESS_NEW_GROUP                 (1<<0)
#define RKTIO_PROCESS_STDOUT_AS_STDERR          (1<<1)
#define RKTIO_PROCESS_WINDOWS_EXACT_CMDLINE     (1<<2)
#define RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION (1<<3)

typedef struct rktio_process_result_t {
  rktio_process_t *process;
  rktio_fd_t *stdin_fd, *stdout_fd, *stderr_fd;
} rktio_process_result_t;

RKTIO_EXTERN rktio_process_result_t *rktio_process(rktio_t *rktio,
                                                   const char *command, int argc, char **argv,
                                                   rktio_fd_t *stdout_fd, rktio_fd_t *stdin_fd, rktio_fd_t *stderr_fd,
                                                   const char *current_directory, rktio_envvars_t *envvars,
                                                   int flags,
                                                   void (*unix_child_process_callback)());

RKTIO_EXTERN int rktio_process_kill(rktio_t *rktio, rktio_process_t *sp);
RKTIO_EXTERN int rktio_process_interrupt(rktio_t *rktio, rktio_process_t *sp);
RKTIO_EXTERN void rktio_process_forget(rktio_t *rktio, rktio_process_t *sp);

#define RKTIO_PROCESS_ERROR (-2)
#define RKTIO_PROCESS_DONE  1

RKTIO_EXTERN int rktio_poll_process_done(rktio_t *rktio, rktio_process_t *sp);

typedef struct rktio_status_t {
  int running;
  int result;
} rktio_status_t;

RKTIO_EXTERN rktio_status_t *rktio_process_status(rktio_t *rktio, rktio_process_t *sp);

RKTIO_EXTERN void rktio_block_child_signals(rktio_t*rktio, int block);

/*************************************************/
/* Filesystem-change events                      */

#define RKTIO_FS_CHANGE_SUPPORTED   (1 << 0)
#define RKTIO_FS_CHANGE_SCALABLE    (1 << 1)
#define RKTIO_FS_CHANGE_LOW_LATENCY (1 << 2)
#define RKTIO_FS_CHANGE_FILE_LEVEL  (1 << 3)

RKTIO_EXTERN int rktio_fs_change_properties(rktio_t *rktio);

typedef struct rktio_fs_change_t rktio_fs_change_t;

RKTIO_EXTERN rktio_fs_change_t *rktio_fs_change(rktio_t *rktio, char *path);
RKTIO_EXTERN void rktio_fs_change_forget(rktio_t *rktio, rktio_fs_change_t *fc);
RKTIO_EXTERN int rktio_poll_fs_change_ready(rktio_t *rktio, rktio_fs_change_t *fc);

/*************************************************/
/* File-descriptor sets for polling              */

/* A poll set works for a single use via rktio_sleep(), as opposed to
   "long-term" poll sets that can be used multiple times. */

typedef struct rktio_poll_set_t rktio_poll_set_t;

#define RKTIO_POLL_READ   RKTIO_OPEN_READ
#define RKTIO_POLL_WRITE  RKTIO_OPEN_WRITE

RKTIO_EXTERN rktio_poll_set_t *rktio_make_poll_set(rktio_t *rktio);
RKTIO_EXTERN void rktio_poll_set_close(rktio_t *rktio, rktio_poll_set_t *fds);

RKTIO_EXTERN void rktio_poll_add(rktio_t *rktio, rktio_fd_t *rfd, rktio_poll_set_t *fds, int modes);
RKTIO_EXTERN void rktio_poll_add_receive(rktio_t *rktio, rktio_listener_t *listener, rktio_poll_set_t *fds);
RKTIO_EXTERN void rktio_poll_add_connect(rktio_t *rktio, rktio_connect_t *conn, rktio_poll_set_t *fds);
RKTIO_EXTERN void rktio_poll_add_addrinfo_lookup(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup, rktio_poll_set_t *fds);
RKTIO_EXTERN void rktio_poll_add_process(rktio_t *rktio, rktio_process_t *sp, rktio_poll_set_t *fds);
RKTIO_EXTERN void rktio_poll_add_fs_change(rktio_t *rktio, rktio_fs_change_t *fc, rktio_poll_set_t *fds);

RKTIO_EXTERN void rktio_poll_set_add_nosleep(rktio_t *rktio, rktio_poll_set_t *fds);

#ifdef RKTIO_SYSTEM_WINDOWS
RKTIO_EXTERN void rktio_poll_set_add_handle(rktio_t *rktio, intptr_t h, rktio_poll_set_t *fds, int repost);
RKTIO_EXTERN void rktio_poll_set_add_eventmask(rktio_t *rktio, rktio_poll_set_t *fds, int mask);
#endif

/*************************************************/
/* Long-term poll sets                           */

/* "Long-term" means that the poll set will be used frequently with
   incremental updates, which means that it's worthwhile to use an OS
   facililty (epoll, kqueue, etc.) to speed up polling. */

typedef struct rktio_ltps_t rktio_ltps_t;
typedef struct rktio_ltps_handle_t rktio_ltps_handle_t;

enum {
  RKTIO_LTPS_CREATE_READ = 1,
  RKTIO_LTPS_CREATE_WRITE,
  RKTIO_LTPS_CHECK_READ,
  RKTIO_LTPS_CHECK_WRITE,
  RKTIO_LTPS_REMOVE,
  /* Internal, for filesystem-change events with kqueue: */
  RKTIO_LTPS_CREATE_VNODE,
  RKTIO_LTPS_CHECK_VNODE,
  RKTIO_LTPS_REMOVE_VNODE
};

RKTIO_EXTERN rktio_ltps_t *rktio_open_ltps(rktio_t *rktio);
RKTIO_EXTERN int rktio_ltps_close(rktio_t *rktio, rktio_ltps_t *lt);

RKTIO_EXTERN rktio_ltps_handle_t *rktio_ltps_add(rktio_t *rktio, rktio_ltps_t *lt, rktio_fd_t *rfd, int mode);
RKTIO_EXTERN void rktio_ltps_handle_set_data(rktio_ltps_t *lt, rktio_ltps_handle_t *s, void *data);
RKTIO_EXTERN void *rktio_ltps_handle_get_data(rktio_ltps_t *lt, rktio_ltps_handle_t *s);

RKTIO_EXTERN int rktio_ltps_poll(rktio_t *rktio, rktio_ltps_t *lt);
RKTIO_EXTERN rktio_ltps_handle_t *rktio_ltps_get_signaled_handle(rktio_t *rktio, rktio_ltps_t *lt);

RKTIO_EXTERN void rktio_sleep(rktio_t *rktio, float nsecs, rktio_poll_set_t *fds, rktio_ltps_t *lt);

/*************************************************/
/* Files, directories, and links                 */

RKTIO_EXTERN int rktio_file_exists(rktio_t *rktio, char *filename);
RKTIO_EXTERN int rktio_directory_exists(rktio_t *rktio, char *dirname);
RKTIO_EXTERN int rktio_link_exists(rktio_t *rktio, char *filename);
RKTIO_EXTERN int rktio_is_regular_file(rktio_t *rktio, char *filename);

RKTIO_EXTERN int rktio_delete_file(rktio_t *rktio, char *fn, int enable_write_on_fail);
RKTIO_EXTERN int rktio_rename_file(rktio_t *rktio, char *dest, char *src, int exists_ok);

RKTIO_EXTERN char *rktio_get_current_directory(rktio_t *rktio);
RKTIO_EXTERN int rktio_set_current_directory(rktio_t *rktio, const char *path);
RKTIO_EXTERN int rktio_make_directory(rktio_t *rktio, char *filename);
RKTIO_EXTERN int rktio_delete_directory(rktio_t *rktio, char *filename, char *current_directory, int enable_write_on_fail);

RKTIO_EXTERN char *rktio_readlink(rktio_t *rktio, char *fullfilename);
RKTIO_EXTERN int rktio_make_link(rktio_t *rktio, char *src, char *dest, int dest_is_directory);

/*************************************************/
/* File attributes                               */

typedef struct {
  unsigned lo, hi;
} rktio_size_t;

typedef intptr_t rktio_timestamp_t;

typedef struct {
  uintptr_t a, b, c;
} rktio_identity_t;

RKTIO_EXTERN rktio_size_t *rktio_file_size(rktio_t *rktio, char *filename);

RKTIO_EXTERN rktio_timestamp_t *rktio_get_file_modify_seconds(rktio_t *rktio, char *file);
RKTIO_EXTERN int rktio_set_file_modify_seconds(rktio_t *rktio, char *file, rktio_timestamp_t secs);

RKTIO_EXTERN rktio_identity_t *rktio_fd_identity(rktio_t *rktio, rktio_fd_t *fd);
RKTIO_EXTERN rktio_identity_t *rktio_path_identity(rktio_t *rktio, char *path, int follow_links);

/*************************************************/
/* Permissions                                   */

/* Must match OS bits: */
#define RKTIO_PERMISSION_READ  0x4
#define RKTIO_PERMISSION_WRITE 0x2
#define RKTIO_PERMISSION_EXEC  0x1

RKTIO_EXTERN int rktio_get_file_or_directory_permissions(rktio_t *rktio, char *filename, int all_bits);
RKTIO_EXTERN int rktio_set_file_or_directory_permissions(rktio_t *rktio, char *filename, int new_bits);

/*************************************************/
/* Directory listing                             */

typedef struct rktio_directory_list_t rktio_directory_list_t;

RKTIO_EXTERN rktio_directory_list_t *rktio_directory_list_start(rktio_t *rktio, char *filename, int is_drive);
RKTIO_EXTERN char *rktio_directory_list_step(rktio_t *rktio, rktio_directory_list_t *dl);

RKTIO_EXTERN char **rktio_filesystem_root_list(rktio_t *rktio);

/*************************************************/
/* File copying                                  */

typedef struct rktio_file_copy_t rktio_file_copy_t;

RKTIO_EXTERN rktio_file_copy_t *rktio_copy_file_start(rktio_t *rktio, char *dest, char *src, int exists_ok);
RKTIO_EXTERN int rktio_copy_file_is_done(rktio_t *rktio, rktio_file_copy_t *fc);
RKTIO_EXTERN int rktio_copy_file_step(rktio_t *rktio, rktio_file_copy_t *fc);
RKTIO_EXTERN void rktio_copy_file_stop(rktio_t *rktio, rktio_file_copy_t *fc);

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

RKTIO_EXTERN char *rktio_system_path(rktio_t *rktio, int which);
RKTIO_EXTERN char *rktio_expand_user_tilde(rktio_t *rktio, char *filename);

/*************************************************/
/* Sleep and signals                             */

typedef struct rktio_signal_handle_t rktio_signal_handle_t;

RKTIO_EXTERN rktio_signal_handle_t *rktio_get_signal_handle(rktio_t *rktio);
RKTIO_EXTERN void rktio_signal_received_at(rktio_signal_handle_t *h);
RKTIO_EXTERN void rktio_signal_received(rktio_t *rktio);

/*************************************************/
/* Time and date                                 */

typedef struct rktio_date_t {
  int nanosecond, second, minute, hour, day, month;
  intptr_t year;
  int day_of_week;
  int day_of_year;
  int is_dst;
  int zone_offset;
  char *zone_name; /* can be NULL */
} rktio_date_t;

RKTIO_EXTERN intptr_t rktio_get_milliseconds(void);
RKTIO_EXTERN double rktio_get_inexact_milliseconds(void);

RKTIO_EXTERN intptr_t rktio_get_process_milliseconds(rktio_t *rktio);
RKTIO_EXTERN intptr_t rktio_get_process_children_milliseconds(rktio_t *rktio);

RKTIO_EXTERN intptr_t rktio_get_seconds(rktio_t *rktio);
RKTIO_EXTERN rktio_date_t *rktio_seconds_to_date(rktio_t *rktio, intptr_t seconds, intptr_t nanoseconds, int get_gmt);

/*************************************************/
/* Errors                                        */

/* Kinds of error values: */
enum {
  RKTIO_ERROR_KIND_POSIX,
  RKTIO_ERROR_KIND_WINDOWS,
  RKTIO_ERROR_KIND_GAI, /* => error sub-code available */
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
  RKTIO_ERROR_UNKNOWN_USER,
  RKTIO_ERROR_INIT_FAILED,
  RKTIO_ERROR_LTPS_NOT_FOUND,
  RKTIO_ERROR_LTPS_REMOVED, /* indicates success, instead of failure */
  RKTIO_ERROR_CONNECT_TRYING_NEXT, /* indicates that failure is not (yet) premanent */
  RKTIO_ERROR_ACCEPT_NOT_READY,
  RKTIO_ERROR_HOST_AND_PORT_BOTH_UNSPECIFIED,
  RKTIO_ERROR_INFO_TRY_AGAIN, /* for UDP */
  RKTIO_ERROR_TRY_AGAIN, /* for UDP */
  RKTIO_ERROR_TRY_AGAIN_WITH_IPV4, /* for TCP listen */
  RKTIO_ERROR_TIME_OUT_OF_RANGE,
};

/* GAI error sub-codes */
enum {
  RKTIO_ERROR_REMOTE_HOST_NOT_FOUND,
  RKTIO_ERROR_LOCAL_HOST_NOT_FOUND,
};

RKTIO_EXTERN int rktio_get_last_error(rktio_t *rktio);
RKTIO_EXTERN int rktio_get_last_error_kind(rktio_t *rktio);

/* The returned strings for `rktio_...error_string()` should not be
   deallocated. */
RKTIO_EXTERN const char *rktio_get_last_error_string(rktio_t *rktio);
RKTIO_EXTERN const char *rktio_get_error_string(rktio_t *rktio, int kind, int errid);

/*************************************************/

#endif
