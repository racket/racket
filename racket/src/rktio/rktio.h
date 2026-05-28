#ifndef __RKTIO_H__
#define __RKTIO_H__ 1

/****
 * Introduction and Conventions */

/*
This library provides I/O functions to Racket for all major OS
platforms. It was created before modern libraries like libuv and
similar libraries were available, and it is specificallt targeted to
the needs of the Racket language.

Racket users never need to call this library directly. It is
documented for people working on Racket internals.

Allocation conventions:

 - Unless otherwise specified, returned data must be deallocated ---
   using a type-specific deallocation function if provided or
   `rktio_free` otherwise. The `rktio_free` function is the same as
   `free`.

 - There's no reference counting. Unless otherwise specified, if
   object A is created given object B, then a client must keep object
   B alive as long as object A exists.

 - String arguments are copied by `rktio_...` functions if the strings
   must be retained. Unless otherwise specified, creating an object A
   with string S doesn't require that S stay live as long as A exists.
   String results are generally allocated and must be freed by the
   client.

Return type conventions:

 - A return type `rktio_ok_t` (alias for `int`) means that 1 is
   returned for success and 0 for error. Use
   `rktio_get_last_error_kind` and `rktio_get_last_error` for more
   information about a 0 result.

 - A return type `rktio_tri_t` (alias for `int`) means that 0 is
   returned for an expected failure, some `RKTIO_...` (alias for 1)
   is returned for success, and `RKTIO_...ERROR` (alias for -2) is
   returned for some error. The function will be annotated with
   `RKTIO_EXTERN_ERR(...)` to indicate the error value. Use
   `rktio_get_last_error_kind` and `rktio_get_last_error` for more
   information about a `RKTIO_...ERROR` result.

 - A return type `rktio_bool_t` means that the result is a simple 0 or
   1, and no error is possible.

 - For a pointer return type, unless otherwise specified, a NULL
   result means an error. Use `rktio_get_last_error_kind` and
   `rktio_get_last_error` for more information about the error.

 - If a function returns `void`, you can rely on it to not change the
   error reported by `rktio_get_last_error_kind` and
   `rktio_get_last_error`.

 - A function annotated with `RKTIO_EXTERN_NOERR` always succeeds, so
   it never sets the error reported by `rktio_get_last_error_kind` and
   `rktio_get_last_error`. If a function returns `void` or
   `rktio_bool_t`, then `RKTIO_EXTERN` is implicitly
   `RKTIO_EXTERN_NOERR`.

 - A function annotated with `RKTIO_EXTERN_STEP` sets the value
   to be reported by `rktio_get_last_error_step` when it returns a
   value that indicates an error.

 - A return type `rktio_result_t *` means that a result encodes either
   success or an error. Such functions can be called concurrently with
   other functions, though typically with a constraint on concurrent
   calls with a common argument (other than a rktio_t`). The result
   sometimes needs to be deallocated with `rktio_free`, in which case
   the function is annotated with `RKTIO_EXTERN_ALLOC_RESULT`. The
   annotation `RKTIO_EXTERN_RESULT` means that no deallocation is
   needed, but the result will be valid only as noted. Either of those
   annotations mentions the accessor that applies to get a success
   result.

Thread and signal conventions:

 - A given `rktio_t` can be used from only one thread at a time,
   except as specified for some functions. Otherwise, as long as the
   initial call to `rktio_init` returns before a second call,
   different `rktio_t` values can be used freely from different
   threads.

 - Unless otherwise specified, anything created with a particular
   `rktio_t` must be used with that same `rktio_t` thereafter (and in
   only one thread at a time).

 - If a function doesn't take a `rktio_t` argument, then it can be
   called concurrently with anything else. Notably,
   `rktio_signal_received_at` does not take a `rktio_t`.

 - If a function returns a `rktio_result_t *`, then it can be called
   concurrently with other functions, but typically there is an
   argument that must be different for concurrent calls.

 - A function declared as `RKTIO_EXTERN_ATOMIC` or
   `RKTIO_EXTERN_ATOMIC_NOERR` can be called concurrently with
   anything else, even though it has a `rktio_t` argument.

 - A function declared as `RKTIO_EXTERN_POLL` or
   `RKTIO_EXTERN_POLL_NOERR` has special concurrency rules with
   respect to `rktio_sleep`. See `rktio_sleep` for more information.

 - SIGCHLD may be enabled, blocked, and/or handled by the `rktio`
   library.

 - On systems where signal handling is thread-specific, as on Linux,
   then `rktio_init` should be called on some thread before any
   additional threads, so that a suitable inheritable signal
   disposition can be configured.

 - On Windows, `rktio_init` should be called in some thread before any
   additional threads to support initialization that spans all
   `rktio_t`s.
*/

#include "rktio_config.h"

/****
 * Function conventions for external (exported) functions.
 * See "Return type conventions" above.
 */

#ifndef RKTIO_EXTERN
#define RKTIO_EXTERN extern
#endif

#define RKTIO_EXTERN_ERR(n) RKTIO_EXTERN
#define RKTIO_EXTERN_NOERR  RKTIO_EXTERN
#define RKTIO_EXTERN_STEP   RKTIO_EXTERN

#define RKTIO_EXTERN_ATOMIC        RKTIO_EXTERN
#define RKTIO_EXTERN_ATOMIC_NOERR  RKTIO_EXTERN

#define RKTIO_EXTERN_POLL       RKTIO_EXTERN
#define RKTIO_EXTERN_POLL_NOERR RKTIO_EXTERN_NOERR

#define RKTIO_EXTERN_RESULT(acc) RKTIO_EXTERN
#define RKTIO_EXTERN_ALLOC_RESULT(acc) RKTIO_EXTERN

/**
 * A set of annotations used by certain functions.
 */

/**
 * Indicates that a result pointer type can be NULL.
 */
#define RKTIO_NULLABLE      /* empty */
/**
 * Indicates that a function blocks indefinitely.
 */
#define RKTIO_BLOCKING      /* empty  */
/**
 * Indicates that a function can dispatch events on Windows.
 */
#define RKTIO_MSG_QUEUE     /* empty */

/****
 * Initialization and General Datatypes */

/**
 * A `rktio_t` value represents an instance of the Racket I/O system,
 * and `rktio_t*` acts as an opaque handle representing that instance
 * context. Almost every `rktio_...` function takes a `rktio_t*` as
 * the first argument.
 */
typedef struct rktio_t rktio_t;

/**
 * Creates a new `rktio_t` instance; call `rktio_init` before any
 * other operation in the library. The first call to `rktio_init` must
 * return before any additional calls (in other threads), but there's
 * no concurrency-ordering requirement after that.
 *
 * If the result is NULL, then there's no way to get an error code, so
 * assume `RKTIO_ERROR_INIT_FAILED`.
 */
RKTIO_EXTERN rktio_t *rktio_init(void);

/**
 * Destroys a `rktio_t` instance and cleans up all internal resources
 * for the instance. Everything allocated explicity, such as a file
 * descriptor returned by `rktio_system_fd`, must be explicitly
 * deallocated/closed before calling `rktio_destroy`.
 */
RKTIO_EXTERN void rktio_destroy(rktio_t *rktio);

/**
 * Frees memory allocated by `rktio_...` functions. This function is
 * normally equivalent to `free`, but calling it ensures a consistent
 * `malloc`/`free` implementation with rktio functions (in case of
 * multiple instances of the C library are present, for example).
 */
RKTIO_EXTERN void rktio_free(void *p);

/**
 * A result of this type is 0 for failure, in which case an error is
 * available via `rktio_get_last_error` and `rktio_get_last_error_kind`,
 * or 1 for success.
 */
typedef int rktio_ok_t;

/**
 * A result of this type means that 0 or 1 represents a boolean
 * result, and any other result will be an `...ERROR` value (specific
 * to the function). If the result represents an error, then more
 * information is available via `rktio_get_last_error` and
 * `rktio_get_last_error_kind`.
 *
 * As a example, `RKTIO_EXTERN_ERR(RKTIO_READ_ERROR) rktio_tri_t [function]`
 * means that `RKTIO_READ_ERROR` is the potential failure rsult.
 */
typedef int rktio_tri_t;

/**
 * A boolean result (with no error possible), specifically 0 or 1.
 */
typedef int rktio_bool_t;

/**
 * A UTF-16 code unit. A `rktio_char16_t *` is meant to be the same as
 * `wchar_t *` on Windows.
 */
typedef unsigned short rktio_char16_t;

/**
 * An argument that is a NUL-terminated string (as opposed to a buffer
 * where a length is provided separately and doesn't need to be
 * NUL-terminated).
 */
typedef const char *rktio_const_string_t;

/**
 * A `rktio_result_t *` result is an alternative to `rktio_ok_t` that
 * allows a function to be similar to the `RKTIO_EXTERN_ATOMIC`
 * convention, which means that the function can be called
 * concurrently. The result needs to be freed with `rktio_free` in
 * cases that are annotated with `RKTIO_EXTERN_ALLOC_RESULT`;
 * otherwise, the function describes how long the result lasts.
 *
 * Note that functions using this result typically have a constraint
 *  on concurrent calls with a common argument (other than a rktio_t`).
 */
typedef struct rktio_result_t rktio_result_t;

/****
 * Windows Dynamic Library Path Management */

/**
 * Functions for managing the dynamic library path on Windows.
 * These should be used with caution, as messing with the DLL
 * paths can lead to a lot of issues.
 */

/**
 * Sets a path to search for loading DLLs, such as `iconv` on Windows.
 * This function departs from all the usual conventions: the given
 * path is in wide-character format, it's not copied, and it's not
 * specific to a `rktio_t` instance.
 */
RKTIO_EXTERN void rktio_set_dll_path(rktio_char16_t *p);

/**
 * Combines a path previously registered with `rktio_set_dll_path`
 * with the given filename. The result is allocated (and should be
 * deallocated) as usual.
 */
RKTIO_EXTERN rktio_char16_t *rktio_get_dll_path(rktio_char16_t *p);

/****
 * File Based I/O */

/*
 * This section contains the types and functions used creating, opening,
 * reading and writing to files.
 *
 */

/**
 * An `rktio_fd_t` is an opaque representation of a file descriptor.
 * It contains an OS-level file descriptor or file handle internally.
 */
typedef struct rktio_fd_t rktio_fd_t;

/**
 * Flags to modify how a file is opened.
 *
 * Depending on the resource that is actually being
 * open, this may be advisory and not enforced.
 *
 */

/**
 * Open for read, but merely advisory for `rktio_system_fd`.
 */
#define RKTIO_OPEN_READ        (1<<0)
/**
 * Open for write, but merely advisory for `rktio_system_fd`.
 */
#define RKTIO_OPEN_WRITE       (1<<1)
/**
 * Open as a text file (which affects only on Windows).
 */
#define RKTIO_OPEN_TEXT        (1<<2)

/**
 * Modifiers for opening a file.
 */

/**
 * When combined with `RKTIO_OPEN_WRITE`, truncate an existing file.
 */
#define RKTIO_OPEN_TRUNCATE    (1<<3)
/**
 * When combined with `RKTIO_OPEN_WRITE`, append to an existing file.
 * This flag implies `RKTIO_OPEN_CAN_EXIST`. */
#define RKTIO_OPEN_APPEND      (1<<4)
/**
 * When combined with `RKTIO_OPEN_WRITE`, fail if the file does not
 *  exist already. */
#define RKTIO_OPEN_MUST_EXIST  (1<<5)
/**
 * When combined with `RKTIO_OPEN_WRITE`, succeed and create a new
 *  file if it does not exist already. */
#define RKTIO_OPEN_CAN_EXIST   (1<<6)

/**
 * System File Descriptor Type
 */

/**
 * For `rktio_system_fd`, treat as a TCP socket. */
#define RKTIO_OPEN_SOCKET      (1<<7)
/**
 * For `rktio_system_fd`, treat as a UDP socket. */
#define RKTIO_OPEN_UDP         (1<<8)

/**
 * If neither `RKTIO_OPEN_REGFILE` nor `RKTIO_OPEN_NOT_REGFILE`
 * are specified, then the value is inferred by `rtkio_system_fd`.
 */

/**
 * For `rktio_system_fd`, treat as a regular file. If this flag and
 *  `RKTIO_OPEN_NOT_REGFILE` are both not present, can be inferred.
 */
#define RKTIO_OPEN_REGFILE     (1<<9)
/**
 * For `rktio_system_fd`, treat as *not* a regular file. If this flag
 *  and `RKTIO_OPEN_REGFILE` are both not present, can be inferred.
 */
#define RKTIO_OPEN_NOT_REGFILE (1<<10)

/**
 * Open if a directory, only. If this flag, `RKTIO_OPEN_NOT_DIR`,
 *  `RKTIO_OPEN_REGFILE`, and `RKTIO_OPEN_NOT_REGFILE` are not
 *  present, can be inferred.
 */
#define RKTIO_OPEN_DIR         (1<<11)
/** Open if *not* a directory, only. If this flag,
 *  `RKTIO_OPEN_NOT_DIR`, `RKTIO_OPEN_REGFILE`, and
 *  `RKTIO_OPEN_NOT_REGFILE` are not present, can be inferred.
 */
#define RKTIO_OPEN_NOT_DIR     (1<<12)

/**
 * For `rktio_system_fd`, set the file descriptor as non-blocking.
 *  This flag is implied when `RKTIO_OPEN_NOT_REGFILE` is inferred.
 */
#define RKTIO_OPEN_INIT        (1<<13)
/**
 * Make `rktio_system_fd` record a socket for reliable clean up on
 * pre-NT Windows --- which means that it is obsolete!
 */
#define RKTIO_OPEN_OWN         (1<<14)

/**
 * Used for `rktio_open` with `RKTIO_OPEN_WRITE`.
 */
#define RKTIO_OPEN_REPLACE_PERMS (1<<15)
/**
 * Enables tracking of terminal output as repoted by
 * `rktio_current_terminal_position`. Implied when
 * `RKTIO_OPEN_NOT_REGFILE` is inferred and the file descriptor is a
 * terminal.
 */
#define RKTIO_OPEN_TRACK_TERMINAL_OUTPUT (1<<16)

/**
 * Wraps an OS-level file descriptor or socket as a `rktio_fd_t`,
 * where `modes` is a combination of `RKTIO_OPEN_...` flags. A socket
 * (as opposed to other file descriptors) registered this way should
 * include `RKTIO_OPEN_SOCKET` and be non-blocking or use
 * `RKTIO_OPEN_INIT`.
 */
RKTIO_EXTERN rktio_fd_t *rktio_system_fd(rktio_t *rktio, intptr_t system_fd, int modes);

/**
 * Extracts a native file descriptor or socket from `rfd`. A file
 * descriptor must not be in pending-open mode as reported by
 * `rktio_fd_is_pending_open`.
 */
RKTIO_EXTERN_NOERR intptr_t rktio_fd_system_fd(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Helper functions to see what an `rktio_fd_t` represents as a file
 * descriptor or file handle. These mostly report the value of a
 * recorded mode flag.
 */

/**
 * Is `rfd` a regular file?
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_fd_is_regular_file(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Is `rfd` a directory?
*/
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_fd_is_directory(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Is `rfd` a socket?
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_fd_is_socket(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Is `rfd` a UDP socket?
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_fd_is_udp(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Is `rfd` a terminal?
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_fd_is_terminal(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * A set of helper function to see what flags are set on a `rktio_fd_t`.
 */

/**
 * Is this file text converted ("/n" to "/r/n" and "/r/n" to "/n")?
 * The `RKTIO_OPEN_TEXT` flag has this effect only on Windows.
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_fd_is_text_converted(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Returns all of the recorded mode flags, including those provided to
 * `rktio_system_fd` and those that are inferred. The
 * `RKTIO_OPEN_INIT` flag is not recorded, however.
 */
RKTIO_EXTERN_NOERR int rktio_fd_modes(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Reports whether `rfd` will block on writing because it corresponds
 * to the write end of a fifo that has no open reader. In that case,
 * `rktio_fd_system_fd` cannot report a file descriptor and
 * `rktio_ltps_add` will error with `RKTIO_ERROR_UNSUPPORTED`.
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_fd_is_pending_open(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Open the resource expression in the string with the given modes.
 *
 * Can report `RKTIO_ERROR_DOES_NOT_EXIST` in place of a system error
 * in read mode, and can report `RKTIO_ERROR_IS_A_DIRECTORY`,
 * `RKTIO_ERROR_EXISTS`, or `RKTIO_ERROR_ACCESS_DENIED` in place of a
 * system error in write mode.
 * On Windows, can report `RKTIO_ERROR_UNSUPPORTED_TEXT_MODE`.
 * If `modes` has `RKTIO_OPEN_WRITE` without `RKTIO_OPEN_READ`,
 * then the result may be a file descriptor in pending-open mode until the read end is opened.
 */
RKTIO_EXTERN rktio_fd_t *rktio_open(rktio_t *rktio, rktio_const_string_t src, int modes);

#define RKTIO_DEFAULT_PERM_BITS 0666

/**
 * Like `rktio_open`, but accepts permission bits that are used if the
 * file is created (which is only relevant if `modes` includes
 * `RKTIO_OPEN_WRITE`). On Unix, `perm_bits` are adjusted by a umask.
 * Otherwise, permission bits are treated in the same way as
 * by `rktio_set_file_or_directory_permissions`.
 */
RKTIO_EXTERN rktio_fd_t *rktio_open_with_create_permissions(
   rktio_t *rktio, rktio_const_string_t src, int modes, int perm_bits);

/**
 * Closes the file descriptor `rfd` and deallocates it.
 *
 * Can report `RKTIO_ERROR_EXISTS` in place of a system error,
 * and can report `RKTIO_ERROR_UNSUPPORTED_TEXT_MODE` on Windows.
 * See also `rktio_write` and `rktio_poll_write_flushed`.
 */
RKTIO_EXTERN rktio_ok_t rktio_close(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * The same as `rktio_close`, but without reporting errors. There's
 * often nothing good to do if a close fails, especially if the close
 * is in the service of handling another failure where you don't want
 * the error code replaced.
 */
RKTIO_EXTERN void rktio_close_noerr(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Copies the file descriptor `rfd`, where the copy and the original
 * must be closed or forgotten independently. The underlying system
 * file descriptor or file handle is duplicated, so take care.
 */
RKTIO_EXTERN rktio_fd_t *rktio_dup(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Deallocates a `rktio_fd_t` without closing the file descriptor,
 * but the descriptor is no longer recorded if it was opened with
 * `RKTIO_OPEN_OWN`.
 */
RKTIO_EXTERN void rktio_forget(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Macros for referring to STDIN, STDOUT and STDERR consistently
 */
#define RKTIO_STDIN  0
#define RKTIO_STDOUT 1
#define RKTIO_STDERR 2

/**
 * Gets stdin, stdout, or stderr, where `which` is `RKTIO_STDIN`,
 * `RKTIO_STDOUT`, or `RKTIO_STDERR`.
 */
RKTIO_EXTERN rktio_fd_t *rktio_std_fd(rktio_t *rktio, int which);

/**
 * On Windows, ensures that a console is available for output, because
 * every Win32 program is created with a console or access to stdin.
 * If a console is created for an application started in GUI mode, the
 * console cannot be closed by the user until the process exits, and
 * then an atexit callback pauses the exit until the user closes the
 * console.
 */
RKTIO_EXTERN void rktio_create_console(void);

/**
 * Values for EOF and common file I/O errors.
 */
#define RKTIO_READ_EOF   (-1)
#define RKTIO_READ_ERROR (-2)
#define RKTIO_WRITE_ERROR (-2)

/**
 * Reads up to `len` bytes from `rfd` into `buffer`.
 *
 * Returns the number of bytes read, possibly 0, in non-blocking mode.
 * Alternatively, the result can be `RKTIO_READ_EOF` for end-of-file
 * or `RKTIO_READ_ERROR` for an error.
 *
 * Although `rktio_read` is intended to have no buffering, text-mode
 * conversion (on Windows) and certain uncooperative OS corners can
 * buffer 1 byte.
 */
RKTIO_EXTERN_ERR(RKTIO_READ_ERROR) intptr_t rktio_read(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len);

/**
 * Writes up to `len` bytes from `buffer` to `rfd`.
 *
 * Returns the number of bytes written, possibly 0, in non-blocking
 * mode. Alternatively, the result can be `RKTIO_WRITE_ERROR` for an
 * error.
 *
 * Although `rktio_write` is intended to write only bytes that can be
 * fully delivered to the OS, there may be OS limitations that require
 * buffering (e.g., on ancient versions of Windows). Use
 * `rktio_poll_write_flushed` to make sure the data is received by the
 * destination before closing `rfd`.
 */
RKTIO_EXTERN_ERR(RKTIO_WRITE_ERROR) intptr_t rktio_write(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t len);

/**
 * Like `rktio_read`, but also reports whether each character was
 * originally two characters that were converted to a single newline
 * for text mode: `is_converted` is filled with a boolean (as a byte)
 * for each character placed in `buffer`.
 */
RKTIO_EXTERN_ERR(RKTIO_READ_ERROR) intptr_t rktio_read_converted(
   rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len, char *is_converted);

/**
 * Like `rktio_read`, but accepting start and end positions within
 * `buffer`.
 */
RKTIO_EXTERN_ERR(RKTIO_READ_ERROR) intptr_t rktio_read_in(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t end);

/**
 * Like `rktio_write`, but accepting start and end positions within
 * `buffer`.
 */
RKTIO_EXTERN_ERR(RKTIO_WRITE_ERROR) intptr_t rktio_write_in(rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t start, intptr_t end);

/**
 * Like `rktio_read_converted`, but accepting start and end positions
 * within `buffer` (the `len` argument is an end position, despite its
 * name) and a start position within `is_converted`.
 */
RKTIO_EXTERN_ERR(RKTIO_READ_ERROR) intptr_t rktio_read_converted_in(
   rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t len, char *is_converted, intptr_t converted_start);

/**
 * Like `rktio_read_in`, but using the `rktio_result_t` protocol.
 * Concurrent calls with any other function must not supply the same
 * `rfd`, and the result is valid until `rfd` is used again.
 */
RKTIO_EXTERN_RESULT(rktio_result_integer) rktio_result_t *rktio_read_in_r(rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t end);

/**
 * Like `rktio_write_in`, but using the `rktio_result_t` protocol.
 * Concurrent calls with any other function must not supply the same
 * `rfd`, and the result is valid until `rfd` is used again.
 */
RKTIO_EXTERN_RESULT(rktio_result_integer) rktio_result_t *rktio_write_in_r(
   rktio_t *rktio, rktio_fd_t *rfd, const char *buffer, intptr_t start, intptr_t end);

/**
 * Like `rktio_read_converted_in`, but using the `rktio_result_t`
 * protocol. Concurrent calls with any other function must not supply
 * the same `rfd`, and the result is valid until `rfd` is used again.
 */
RKTIO_EXTERN_RESULT(rktio_result_integer) rktio_result_t *rktio_read_converted_in_r(
   rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t len, char *is_converted, intptr_t converted_start);

/**
 * Reports the number of bytes that are buffered from the file descriptor.
 * The result is normally zero, but text-mode conversion and the rare
 * uncooperative corner of an OS can make the result 1 byte.
 */
RKTIO_EXTERN_NOERR intptr_t rktio_buffered_byte_count(rktio_t *rktio, rktio_fd_t *rfd);

#define RKTIO_POLL_NOT_READY 0
#define RKTIO_POLL_READY 1
#define RKTIO_POLL_ERROR (-2)

/**
 * Polls whether `rfd` is ready for reading. Returns
 * `RKTIO_POLL_READY`, `RKTIO_POLL_NOT_READY`, or `RKTIO_POLL_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_POLL_ERROR) rktio_tri_t rktio_poll_read_ready(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Polls whether `rfd` is ready for writing. Returns
 * `RKTIO_POLL_READY`, `RKTIO_POLL_NOT_READY`, or `RKTIO_POLL_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_POLL_ERROR) rktio_tri_t rktio_poll_write_ready(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Polls whether data written to `rfd` has been delivered to its
 * destination; see `rktio_write`. Currently, the result is
 * `RKTIO_POLL_NOT_READY` only on Windows, and only for a pipe or
 * similar non-regular file. A pipe counts as "flushed" when the other
 * end has received the data (because the sent data doesn't persist
 * beyond closing the pipe).
 */
RKTIO_EXTERN_ERR(RKTIO_POLL_ERROR) rktio_tri_t rktio_poll_write_flushed(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Like `rktio_poll_read_ready`, but using the `rktio_result_t`
 * protocol. Can be used concurrently with other rktio calls that are
 * not for the same `rfd`. The result is valid until `rfd` is used
 * again.
 */
RKTIO_EXTERN_RESULT(rktio_result_integer) rktio_result_t *rktio_poll_read_ready_r(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Like `rktio_poll_write_ready`, but using the `rktio_result_t`
 * protocol. Can be used concurrently with other rktio calls that are
 * not for the same `rfd`. The result is valid until `rfd` is used
 * again.
 */
RKTIO_EXTERN_RESULT(rktio_result_integer) rktio_result_t *rktio_poll_write_ready_r(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Like `rktio_poll_write_flushed`, but using the `rktio_result_t`
 * protocol. Can be used concurrently with other rktio calls that are
 * not for the same `rfd`. The result is valid until `rfd` is used
 * again.
 */
RKTIO_EXTERN_RESULT(rktio_result_integer) rktio_result_t *rktio_poll_write_flushed_r(rktio_t *rktio, rktio_fd_t *rfd);

#define RKTIO_LOCK_ERROR        (-2)
#define RKTIO_LOCK_ACQUIRED     1
#define RKTIO_LOCK_NOT_ACQUIRED 0

/**
 * Attempts to take an advisory lock on `rfd`, where `excl` attempts
 * to claim an exclusive lock. Returns `RKTIO_LOCK_ACQUIRED`,
 * `RKTIO_LOCK_NOT_ACQUIRED`, or `RKTIO_LOCK_ERROR`.
 *
 * Whether advisory file locks work in various situations depends on
 * many OS details, where the differences involve promoting from
 * non-exclusive to exclusive, taking a lock that is already held,
 * getting an exclusive lock for a file descriptor in read mode,
 * getting a non-exclusive lock in write mode, and whether a lock
 * prevents opening or using another file descriptor.
 */
RKTIO_EXTERN_ERR(RKTIO_LOCK_ERROR) rktio_tri_t rktio_file_lock_try(rktio_t *rktio, rktio_fd_t *rfd, rktio_bool_t excl);

/**
 * Releases an advisory lock on `rfd`. See `rktio_file_lock_try` for
 * caveats about advisory locks.
 */
RKTIO_EXTERN rktio_ok_t rktio_file_unlock(rktio_t *rktio, rktio_fd_t *rfd);

typedef rktio_int64_t rktio_filesize_t;

enum {
  RKTIO_POSITION_FROM_START,
  RKTIO_POSITION_FROM_END
};


/**
 * Sets the file position of `rfd` to `pos` bytes relative to the
 * start or end of the file, where `whence` is
 * `RKTIO_POSITION_FROM_START` or `RKTIO_POSITION_FROM_END`. Can
 * report `RKTIO_ERROR_CANNOT_FILE_POSITION` on Windows.
 */
RKTIO_EXTERN rktio_ok_t rktio_set_file_position(rktio_t *rktio, rktio_fd_t *rfd, rktio_filesize_t pos, int whence);

/**
 * Returns the file position, not taking into account rare input
 * buffering (see `rktio_read`). On Windows, can report
 * `RKTIO_ERROR_CANNOT_FILE_POSITION`, which doesn't have a
 * corresponding Windows error code.
 */
RKTIO_EXTERN rktio_filesize_t *rktio_get_file_position(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Sets the size of the file accessed by `rfd` to `size` bytes. Can
 * report `RKTIO_ERROR_CANNOT_FILE_POSITION` on Windows.
 */
RKTIO_EXTERN rktio_ok_t rktio_set_file_size(rktio_t *rktio, rktio_fd_t *rfd, rktio_filesize_t size);

/**
 * Represents a `rktio_fd_t` that is detached from a specific
 * `rktio_t`.
 */
typedef struct rktio_fd_transfer_t rktio_fd_transfer_t;

/**
 * Returns a variant of `rfd` that does not depend on `rktio`. The
 * `rfd` must not currently have any file locks, and detaching
 * transfers ownership of `rfd` to the result. To use the result, it
 * must be reattached to some `rktio_t` using `rktio_fd_attach`, or it
 * can be freed with `rktio_fd_close_transfer`.
 */
RKTIO_EXTERN_NOERR rktio_fd_transfer_t *rktio_fd_detach(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Attaches a file descriptor that was formerly detached with
 * `rktio_fd_detach` so it can be used again, consuming the `rfdt`.
 */
RKTIO_EXTERN_NOERR rktio_fd_t *rktio_fd_attach(rktio_t *rktio, rktio_fd_transfer_t *rfdt);

/**
 * Closes and frees a detached file descriptor without having to
 * attach it to a `rktio_t`.
 */
RKTIO_EXTERN void rktio_fd_close_transfer(rktio_fd_transfer_t *rfdt);

/**
 * Returns the total number of bytes that have been written to file
 * descriptors that have `RKTIO_OPEN_TRACK_TERMINAL_OUTPUT`.
 */
RKTIO_EXTERN_NOERR uintptr_t rktio_current_terminal_position(void);

/****
 * Pipes */

#define RKTIO_NO_INHERIT_INPUT  (1<<0)
#define RKTIO_NO_INHERIT_OUTPUT (1<<1)

/**
 * Makes a pair of file descriptors for a pipe. Returns an array of
 * two `rktio_fd_t` values, one for each side of the pipe. The first
 * one is the read end, and the second is the write end. The `flags`
 * can declare the intended sharing of the file descriptors with a
 * child process on Windows and platforms where `O_CLOEXEC` is used.
 */
RKTIO_EXTERN rktio_fd_t **rktio_make_pipe(rktio_t *rktio, int flags);

/****
 * Network */

/**
 * These structures and functions handle getting addrinfo for network sources.
 */
typedef struct rktio_addrinfo_lookup_t rktio_addrinfo_lookup_t;
typedef struct rktio_addrinfo_t rktio_addrinfo_t;

#define RKTIO_FAMILY_ANY (-1)

/**
 * Starts an asynchronous lookup of the address for `hostname` and
 * `portno`. The `family` argument should be `RKTIO_FAMILY_ANY` or the
 * result of `rktio_get_ipv4_family`.
 */
RKTIO_EXTERN rktio_addrinfo_lookup_t *rktio_start_addrinfo_lookup(
   rktio_t *rktio, rktio_const_string_t hostname, int portno, int family,
   rktio_bool_t passive, rktio_bool_t tcp);

/**
 * Returns the value to be used as a `family` argument to request IPv4
 * addresses specifically.
 */
RKTIO_EXTERN_ATOMIC_NOERR int rktio_get_ipv4_family(rktio_t *rktio);

/**
 * Checks whether an address is available for a lookup request.
 * Returns `RKTIO_POLL_READY`, `RKTIO_POLL_NOT_READY`, or
 * `RKTIO_POLL_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_POLL_ERROR) rktio_tri_t rktio_poll_addrinfo_lookup_ready(
   rktio_t *rktio, rktio_addrinfo_lookup_t *lookup);

/**
 * Gets the result of a completed lookup, deallocating `lookup`.
 */
RKTIO_EXTERN rktio_addrinfo_t *rktio_addrinfo_lookup_get(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup);

/**
 * Abandons a lookup whose result (or error) is not yet received.
 */
RKTIO_EXTERN void rktio_addrinfo_lookup_stop(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup);

/**
 * Frees the result of a lookup.
 */
RKTIO_EXTERN void rktio_addrinfo_free(rktio_t *rktio, rktio_addrinfo_t *a);

/**
 * This section supports a UNIX socket like API for network sockets.
 */

typedef struct rktio_listener_t rktio_listener_t;
typedef struct rktio_connect_t rktio_connect_t;

#define RKTIO_LISTEN_REUSE           (1<<0)
#define RKTIO_LISTEN_RETRY_ADDRINUSE (1<<1)

/**
 * Creates a listener for the address `local`, where `flags` is a
 * combination of `RKTIO_LISTEN_...` flags. Can fail with
 * `RKTIO_ERROR_TRY_AGAIN_WITH_IPV4`, which suggests trying an address
 * using the family reported by `rktio_get_ipv4_family` instead of
 * `RKTIO_FAMILY_ANY`.
 */
RKTIO_EXTERN rktio_listener_t *rktio_listen_opt(rktio_t *rktio, rktio_addrinfo_t *local, int backlog, int flags);

/**
 * Like `rktio_listen_opt`, but with just one flag,
 * `RKTIO_LISTEN_REUSE` as `reuse`.
 */
RKTIO_EXTERN rktio_listener_t *rktio_listen(rktio_t *rktio, rktio_addrinfo_t *local, int backlog, rktio_bool_t reuse);

/**
 * Stops and deallocates a listener.
 */
RKTIO_EXTERN void rktio_listen_stop(rktio_t *rktio, rktio_listener_t *listener);

/**
 * Polls whether a connection is ready to accept on `listener`.
 * Returns `RKTIO_POLL_READY`, `RKTIO_POLL_NOT_READY`, or
 * `RKTIO_POLL_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_POLL_ERROR) rktio_tri_t rktio_poll_accept_ready(rktio_t *rktio, rktio_listener_t *listener);


/**
 * Accept a connection on the listener.
 */
RKTIO_EXTERN rktio_fd_t *rktio_accept(rktio_t *rktio, rktio_listener_t *listener);


/**
 * Starts a connection request to `remote`, optionally from `local`.
 * Addresses must not be freed until the connection is complete,
 * errored, or stopped.
 */
RKTIO_EXTERN rktio_connect_t *rktio_start_connect(
   rktio_t *rktio, rktio_addrinfo_t *remote, RKTIO_NULLABLE rktio_addrinfo_t *local);

/**
 * Completes a connection attempt started with `rktio_start_connect`.
 * A `RKTIO_ERROR_CONNECT_TRYING_NEXT` error effectively means "try
 * again", and the connection object is still valid. On any other
 * error, or if the connection completes successfully, `conn` is
 * deallocated.
 */
RKTIO_EXTERN rktio_fd_t *rktio_connect_finish(rktio_t *rktio, rktio_connect_t *conn);

/**
 * Stops a connection whose result or error has not been received.
 */
RKTIO_EXTERN void rktio_connect_stop(rktio_t *rktio, rktio_connect_t *conn);
/* Stops a connection whose result or error has not been received. */

/**
 * Polls whether a connection attempt is ready to finish. Returns
 * `RKTIO_POLL_READY`, `RKTIO_POLL_NOT_READY`, or `RKTIO_POLL_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_POLL_ERROR) rktio_tri_t rktio_poll_connect_ready(rktio_t *rktio, rktio_connect_t *conn);

/**
 * Returns a file descriptor that `conn` is currently trying, or
 * returns NULL without setting any error. The result file descriptor
 * should not be closed, and may be closed by a `rktio_connect_finish`
 * or `rktio_connect_stop` call (so if you register it in a long-term
 * poll set, unregister it before trying to finish or stop the
 * connection).
 */
RKTIO_EXTERN rktio_fd_t *rktio_connect_trying(rktio_t *rktio, rktio_connect_t *conn);

#define RKTIO_SHUTDOWN_READ   0
#define RKTIO_SHUTDOWN_WRITE  1

/**
 * Shuts down the read end or write end of a socket, where `mode` is
 * `RKTIO_SHUTDOWN_READ` or `RKTIO_SHUTDOWN_WRITE`. Useful for TCP to
 * report an EOF to the other end. Does not close the socket, but may
 * make it ineligible for further use.
 */
RKTIO_EXTERN rktio_ok_t rktio_socket_shutdown(rktio_t *rktio, rktio_fd_t *rfd, int mode);

/**
 * Changes a connection to enable or disable "TCP_NODELAY" mode,
 * which disables Nagle's algorithm for avoiding small packets.
 */
RKTIO_EXTERN rktio_ok_t rktio_tcp_nodelay(rktio_t *rktio, rktio_fd_t *rfd, rktio_bool_t enable);

/**
 * Changes a connection to enable or disable "SO_KEEPALIVE" where
 * supported, which causes a connection waiting for data to send a
 * periodic ping and trigger a timeout if the other end does not
 * respond. The frequency of keepalive pings is determined by the OS,
 * but it tends to be on the order of once every 2 hours, so it's not
 * a fast timeout mechanism.
 */
RKTIO_EXTERN rktio_ok_t rktio_tcp_keepalive(rktio_t *rktio, rktio_fd_t *rfd, rktio_bool_t enable);

/**
 * Creates a UDP socket. The `addr` argument can be NULL to create a
 * socket without specifying an interface, and `family` is used only
 * if `addr` is not specified.
 */
RKTIO_EXTERN rktio_fd_t *rktio_udp_open(rktio_t *rktio, RKTIO_NULLABLE rktio_addrinfo_t *addr, int family);

/**
 * Connects the UDP socket `rfd` to `addr` as its default
 * send/receive address.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_connect(rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr);

/**
 * Binds the UDP socket `rfd` to the local address `addr`, where
 * `reuse` enables reuse of the address.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_bind(
   rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *addr, rktio_bool_t reuse);

/**
 * Disconnects the UDP socket `rfd`, removing an association created
 * by `rktio_udp_connect`.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_disconnect(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Extends `rktio_write` to accept a destination `addr`, and binds `rfd` if it
 * is not bound already. The `addr` can be NULL if the socket is connected.
 */
RKTIO_EXTERN_ERR(RKTIO_WRITE_ERROR) intptr_t rktio_udp_sendto(
   rktio_t *rktio, rktio_fd_t *rfd, RKTIO_NULLABLE rktio_addrinfo_t *addr, const char *buffer, intptr_t len);

/**
 * Like `rktio_udp_sendto`, but with starting and ending offsets within `buffer`.
 */
RKTIO_EXTERN_ERR(RKTIO_WRITE_ERROR) intptr_t rktio_udp_sendto_in(
   rktio_t *rktio, rktio_fd_t *rfd, RKTIO_NULLABLE rktio_addrinfo_t *addr,
   const char *buffer, intptr_t start, intptr_t end);

/**
 * Like `rktio_udp_sendto_in`, but with an address as raw bytes.
 */
RKTIO_EXTERN_ERR(RKTIO_WRITE_ERROR) intptr_t rktio_udp_sendto_addr_bytes(
   rktio_t *rktio, rktio_fd_t *rfd, const char *addr, intptr_t addr_len,
   const char *buffer, intptr_t start, intptr_t end);

/**
 * Helper structs for udp recvfrom functions.
 */
typedef struct rktio_length_and_addrinfo_t {
  intptr_t len;
  char **address; /* like the result of `rktio_socket_address` */
} rktio_length_and_addrinfo_t;

typedef struct rktio_length_and_addr_bytes_t {
  intptr_t len;
  intptr_t addr_len;
  char *addr_bytes;
} rktio_length_and_addr_bytes_t;

/**
 * Extends `rktio_read` to report the sender. The reported error can
 * be `RKTIO_ERROR_TRY_AGAIN` or `RKTIO_ERROR_INFO_TRY_AGAIN`, where
 * the latter can happen if the socket claims to be ready to read, and
 * the receive can be retried either way.
 */
RKTIO_EXTERN rktio_length_and_addrinfo_t *rktio_udp_recvfrom(
   rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t len);

/**
 * Like `rktio_udp_recvfrom`, but with starting and ending offsets.
 */
RKTIO_EXTERN rktio_length_and_addrinfo_t *rktio_udp_recvfrom_in(
   rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t end);

/**
 * Like `rktio_udp_recvfrom_in`, but returning an address as raw bytes.
 */
RKTIO_EXTERN rktio_length_and_addr_bytes_t *rktio_udp_recvfrom_addr_bytes(
   rktio_t *rktio, rktio_fd_t *rfd, char *buffer, intptr_t start, intptr_t end);

/**
 * Sets the receive buffer size of the UDP socket `rfd` to `size`
 * bytes.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_set_receive_buffer_size(rktio_t *rktio, rktio_fd_t *rfd, int size);

/**
 * Sets the time-to-live of packets sent through the UDP socket `rfd`.
 */
RKTIO_EXTERN_ERR(RKTIO_PROP_ERROR) rktio_tri_t rktio_udp_get_ttl(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Gets the time-to-live of packets sent through the UDP socket `rfd`.
 * The result is the TTL value or `RKTIO_PROP_ERROR`.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_set_ttl(rktio_t *rktio, rktio_fd_t *rfd, int ttl_val);

/**
 * Reports whether multicast packets sent through `rfd` are looped
 * back to the sending host. The result is a boolean or
 * `RKTIO_PROP_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_PROP_ERROR) rktio_tri_t rktio_udp_get_multicast_loopback(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Sets whether multicast packets sent through `rfd` are looped
 * back to the sending host.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_set_multicast_loopback(rktio_t *rktio, rktio_fd_t *rfd, rktio_bool_t on);

/**
 * Gets the time-to-live of multicast packets sent through `rfd`. The
 * result is the TTL value or `RKTIO_PROP_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_PROP_ERROR) rktio_tri_t rktio_udp_get_multicast_ttl(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Sets the time-to-live of multicast packets sent through `rfd`.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_set_multicast_ttl(rktio_t *rktio, rktio_fd_t *rfd, int ttl_val);

#define RKTIO_PROP_ERROR (-2)

/**
 * Reports the interface for multicast packets sent through `rfd`.
 */
RKTIO_EXTERN char *rktio_udp_multicast_interface(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Sets the interface for multicast packets sent through `rfd`. The
 * `addr` argument can be NULL to auto-select the interface.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_set_multicast_interface(
   rktio_t *rktio, rktio_fd_t *rfd, RKTIO_NULLABLE rktio_addrinfo_t *addr);

enum {
  RKTIO_ADD_MEMBERSHIP,
  RKTIO_DROP_MEMBERSHIP
};

/**
 * Adds or drops `rfd` as a member of the multicast group at
 * `group_addr`, where `action` is `RKTIO_ADD_MEMBERSHIP` or
 * `RKTIO_DROP_MEMBERSHIP`. The `intf_addr` argument selects an
 * interface, and it can be NULL.
 */
RKTIO_EXTERN rktio_ok_t rktio_udp_change_multicast_group(
   rktio_t *rktio, rktio_fd_t *rfd, rktio_addrinfo_t *group_addr,
   RKTIO_NULLABLE rktio_addrinfo_t *intf_addr, int action);

/**
 * Returns the address of the socket `rfd` as two strings in an array
 * (where the array itself should be deallocated): address and
 * service.
 */
RKTIO_EXTERN char **rktio_socket_address(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Returns the address of the peer of the socket `rfd` as two strings
 * in an array (where the array itself should be deallocated): address
 * and service.
 */
RKTIO_EXTERN char **rktio_socket_peer_address(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Returns the address of the listener `lnr` as two strings in an
 * array (where the array itself should be deallocated): address and
 * service.
 */
RKTIO_EXTERN char **rktio_listener_address(rktio_t *rktio, rktio_listener_t *lnr);

/**
 * Converts an address in raw-bytes form to two strings in an array
 * (where the array itself should be deallocated): address and
 * service.
 */
RKTIO_EXTERN char **rktio_addr_bytes_address(rktio_t *rktio, const char *addr, intptr_t len);

/****
 * Environment variables */

/**
 * Checks whether a string is valid as a new environment-variable name
 * (e.g., it contains no "=").
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_is_ok_envvar_name(rktio_t *rktio, rktio_const_string_t name);

/**
 * Checks whether environment variables are case-folded by the OS.
 * That doesn't mean that clients need to case-fold names, but clients
 * may want to imitate the OS.
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_are_envvar_names_case_insensitive(rktio_t *rktio);

/**
 * Gets an environment variable value, or reports
 * `RKTIO_ERROR_NO_SUCH_ENVVAR` when returning NULL; the result must
 * be freed.
 */
RKTIO_EXTERN char *rktio_getenv(rktio_t *rktio, rktio_const_string_t name);

/**
 * Sets an environment variable's value, where a NULL value for `val`
 * unsets it.
 */
RKTIO_EXTERN rktio_ok_t rktio_setenv(rktio_t *rktio, rktio_const_string_t name, rktio_const_string_t val);

typedef struct rktio_envvars_t rktio_envvars_t;

/**
 * Extracts all environment variables into a record.
 */
RKTIO_EXTERN rktio_envvars_t *rktio_envvars(rktio_t *rktio);

/**
 * Creates an empty environment-variables record.
 */
RKTIO_EXTERN rktio_envvars_t *rktio_empty_envvars(rktio_t *rktio);

/**
 * Clones an environment-variables record.
 */
RKTIO_EXTERN rktio_envvars_t *rktio_envvars_copy(rktio_t *rktio, rktio_envvars_t *envvars);

/**
 * Deallocates an environment-variables record.
 */
RKTIO_EXTERN void rktio_envvars_free(rktio_t *rktio, rktio_envvars_t *envvars);

/**
 * Accesses an environment-variables record by name.
 */
RKTIO_EXTERN char *rktio_envvars_get(rktio_t *rktio, rktio_envvars_t *envvars, rktio_const_string_t name);

/**
 * Updates an environment-variables record by name.
 */
RKTIO_EXTERN void rktio_envvars_set(rktio_t *rktio, rktio_envvars_t *envvars, rktio_const_string_t name, rktio_const_string_t value);

/**
 * Reports the number of environment variables in a record, which can
 * be accessed by index using `rktio_envvars_name_ref` and
 * `rktio_envvars_value_ref`.
 */
RKTIO_EXTERN_NOERR intptr_t rktio_envvars_count(rktio_t *rktio, rktio_envvars_t *envvars);

/**
 * Accesses an environment-variable name in a record by index.
 */
RKTIO_EXTERN char *rktio_envvars_name_ref(rktio_t *rktio, rktio_envvars_t *envvars, intptr_t i);

/**
 * Accesses an environment-variable value in a record by index.
 */
RKTIO_EXTERN char *rktio_envvars_value_ref(rktio_t *rktio, rktio_envvars_t *envvars, intptr_t i);

/****
 * Processes */

typedef struct rktio_process_t rktio_process_t;

/**
 * The result of `rktio_process`: the new process plus file
 * descriptors for its stdin, stdout, and stderr. Each file descriptor
 * is non-NULL only if the corresponding argument to `rktio_process`
 * was NULL, so that a pipe was created for the new process.
 */
typedef struct rktio_process_result_t {
  rktio_process_t *process;
  rktio_fd_t *stdin_rfd;
  rktio_fd_t *stdout_rfd;
  rktio_fd_t *stderr_rfd;
} rktio_process_result_t;

#define RKTIO_PROCESS_NEW_GROUP                 (1<<0)
#define RKTIO_PROCESS_STDOUT_AS_STDERR          (1<<1)
#define RKTIO_PROCESS_WINDOWS_EXACT_CMDLINE     (1<<2)
#define RKTIO_PROCESS_WINDOWS_CHAIN_TERMINATION (1<<3)
#define RKTIO_PROCESS_NO_CLOSE_FDS              (1<<4)
#define RKTIO_PROCESS_NO_INHERIT_FDS            (1<<5)

/**
 * Creates a new process running `command` with arguments `argv`,
 * where `flags` is a combination of `RKTIO_PROCESS_...` flags. Each
 * of `stdout_fd`, `stdin_fd`, and `stderr_fd` can be NULL, in which
 * case a pipe is created for the new process's corresponding stream
 * and reported in the result. The given `stdin_fd` must not be a
 * pending-open descriptor.
 */
RKTIO_EXTERN rktio_process_result_t *rktio_process(rktio_t *rktio,
   rktio_const_string_t command,
   int argc,
   rktio_const_string_t *argv,
   RKTIO_NULLABLE rktio_fd_t *stdout_fd,
   RKTIO_NULLABLE rktio_fd_t *stdin_fd,
   RKTIO_NULLABLE rktio_fd_t *stderr_fd,
   RKTIO_NULLABLE rktio_process_t *group_proc,
   rktio_const_string_t current_directory,
   rktio_envvars_t *envvars,
   int flags);

/**
 * Reports the flags that are accepted by `rktio_process` on the
 * current OS.
 */
RKTIO_EXTERN_ATOMIC_NOERR int rktio_process_allowed_flags(rktio_t *rktio);

/**
 * Returns the OS-level process ID of `sp`. Always succeeds, whether
 * or not the process is still running, so the result is generally not
 * meaningful if the process is not running.
 */
RKTIO_EXTERN_NOERR int rktio_process_pid(rktio_t *rktio, rktio_process_t *sp);

/**
 * Kills the process `sp`; does not deallocate the process record.
 */
RKTIO_EXTERN rktio_ok_t rktio_process_kill(rktio_t *rktio, rktio_process_t *sp);

/**
 * Interrupts the process `sp`; does not deallocate the process
 * record.
 */
RKTIO_EXTERN rktio_ok_t rktio_process_interrupt(rktio_t *rktio, rktio_process_t *sp);

/**
 * Deallocates a process record, whether or not the process has
 * stopped.
 */
RKTIO_EXTERN void rktio_process_forget(rktio_t *rktio, rktio_process_t *sp);

#define RKTIO_PROCESS_ERROR    (-2)
#define RKTIO_PROCESS_DONE     1
#define RKTIO_PROCESS_RUNNING  0

/**
 * Checks whether the process `sp` has completed. Returns
 * `RKTIO_PROCESS_DONE`, `RKTIO_PROCESS_RUNNING`, or
 * `RKTIO_PROCESS_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_PROCESS_ERROR) rktio_tri_t rktio_poll_process_done(rktio_t *rktio, rktio_process_t *sp);

typedef struct rktio_status_t {
  rktio_bool_t running;
  int result;
} rktio_status_t;

/**
 * Returns the status of the process `sp`, where the `result` value in
 * the status is valid only if `running` is 0.
 */
RKTIO_EXTERN rktio_status_t *rktio_process_status(rktio_t *rktio, rktio_process_t *sp);

/**
 * If you start processes, calling this periodically may ensure that
 * resources are released sooner rather than later.
 */
RKTIO_EXTERN void rktio_reap_processes(rktio_t *rktio);

/****
 * Filesystem-Change Events */

#define RKTIO_FS_CHANGE_SUPPORTED   (1 << 0)
#define RKTIO_FS_CHANGE_SCALABLE    (1 << 1)
#define RKTIO_FS_CHANGE_LOW_LATENCY (1 << 2)
#define RKTIO_FS_CHANGE_FILE_LEVEL  (1 << 3)
#define RKTIO_FS_CHANGE_NEED_LTPS   (1 << 4)

/**
 * Reports properties of the filesystem-change event implementation as
 * a combination of `RKTIO_FS_CHANGE_...` flags.
 */
RKTIO_EXTERN_ATOMIC_NOERR int rktio_fs_change_properties(rktio_t *rktio);

typedef struct rktio_fs_change_t rktio_fs_change_t;
struct rktio_ltps_t; /* forward reference */

/**
 * Creates a filesystem-change tracker that reports changes in `path`
 * after creation of the tracker. The properties reported by
 * `rktio_fs_change_properties` report various aspects of how the
 * tracker behaves. In particular, the `ltps` argument can be NULL
 * unless the `RKTIO_FS_CHANGE_NEED_LTPS` property is reported; if
 * `ltps` is provided, then the tracker must be canceled or discovered
 * ready before `ltps` is closed.
 */
RKTIO_EXTERN rktio_fs_change_t *rktio_fs_change(
   rktio_t *rktio, rktio_const_string_t path, struct rktio_ltps_t *ltps);

/**
 * Cancels and deallocates the filesystem-change tracker `fc`.
 */
RKTIO_EXTERN void rktio_fs_change_forget(rktio_t *rktio, rktio_fs_change_t *fc);

/**
 * Polls whether a change has been detected by the tracker `fc`.
 * Returns `RKTIO_POLL_READY`, `RKTIO_POLL_NOT_READY`, or
 * `RKTIO_POLL_ERROR`.
 */
RKTIO_EXTERN_ERR(RKTIO_POLL_ERROR) rktio_tri_t rktio_poll_fs_change_ready(rktio_t *rktio, rktio_fs_change_t *fc);
/* Returns one of `RKTIO_POLL_READY`, etc. */

/****
 * File Descriptor Polling Sets */

/**
 * A poll set works for a single use via `rktio_sleep`, as opposed to
 * "long-term" poll sets that can be used multiple times. The
 * `rktio_sleep` function accepts one of each and combines them.
 */

typedef struct rktio_poll_set_t rktio_poll_set_t;

#define RKTIO_POLL_READ   RKTIO_OPEN_READ
#define RKTIO_POLL_WRITE  RKTIO_OPEN_WRITE
#define RKTIO_POLL_FLUSH  (RKTIO_OPEN_WRITE << 2)

/**
 * Creates a poll set for a single use via `rktio_sleep`.
 */
RKTIO_EXTERN_POLL rktio_poll_set_t *rktio_make_poll_set(rktio_t *rktio);

/**
 * Deallocates a poll set. Don't reuse a poll set after calling
 * `rktio_sleep`, but do explicitly forget it afterward.
 */
RKTIO_EXTERN_POLL void rktio_poll_set_forget(rktio_t *rktio, rktio_poll_set_t *rfds);

/**
 * Registers a wait on a file descriptor in read and/or write mode or
 * flush mode, where `modes` is a combination of `RKTIO_POLL_...`
 * flags. The flush mode corresponds to `rktio_poll_write_flushed`.
 */
RKTIO_EXTERN_POLL void rktio_poll_add(rktio_t *rktio, rktio_fd_t *rfd, rktio_poll_set_t *rfds, int modes);

/**
 * Registers a wait on a connection becoming available to accept via
 * `listener`.
 */
RKTIO_EXTERN_POLL void rktio_poll_add_accept(rktio_t *rktio, rktio_listener_t *listener, rktio_poll_set_t *rfds);

/**
 * Registers a wait on completion of the connection attempt `conn`.
 */
RKTIO_EXTERN_POLL void rktio_poll_add_connect(rktio_t *rktio, rktio_connect_t *conn, rktio_poll_set_t *rfds);

/**
 * Registers a wait on completion of the address lookup `lookup`.
 */
RKTIO_EXTERN_POLL void rktio_poll_add_addrinfo_lookup(rktio_t *rktio, rktio_addrinfo_lookup_t *lookup, rktio_poll_set_t *rfds);

/**
 * Registers a wait on completion of the process `sp`.
 */
RKTIO_EXTERN_POLL void rktio_poll_add_process(rktio_t *rktio, rktio_process_t *sp, rktio_poll_set_t *rfds);

/**
 * Registers a wait on the filesystem-change tracker `fc`.
 */

RKTIO_EXTERN_POLL void rktio_poll_add_fs_change(rktio_t *rktio, rktio_fs_change_t *fc, rktio_poll_set_t *rfds);

/**
 * Causes a sleep given `rfds` to return immediately.
 */
RKTIO_EXTERN_POLL void rktio_poll_set_add_nosleep(rktio_t *rktio, rktio_poll_set_t *rfds);

/****
 * Windows-Specific Polling Functions */

/**
 * When sleeping on Windows, extra handles or eventmasks can be added to trigger a wake up.
 * These function do nothing on other platforms.
 */

/**
 * When sleeping on Windows, an extra handle can be added to trigger a
 * wake up. This function does nothing on other platforms.
 */
RKTIO_EXTERN_POLL void rktio_poll_set_add_handle(rktio_t *rktio, intptr_t h, rktio_poll_set_t *rfds, int repost);

/**
 * When sleeping on Windows, an extra eventmask can be added to
 * trigger a wake up. This function does nothing on other platforms.
 */
RKTIO_EXTERN_POLL void rktio_poll_set_add_eventmask(rktio_t *rktio, rktio_poll_set_t *rfds, int mask);

/**
 * Call this function when using `rktio_poll_set_add_eventmask` and
 * when matching events are not always consumed from the queue between
 * sleeps. To accommodate messages that are not consumed, the poll set
 * will actually only sleep a short while at first, and then back off
 * exponentially. Call this function when your program does useful
 * work (instead of spinning on sleep) to reset the backoff
 * counter.
 */
RKTIO_EXTERN_POLL void rkio_reset_sleep_backoff(rktio_t *rktio);

/****
 * Long-term Poll Sets */

/**
 * "Long-term" means that the poll set will be used frequently with
 * incremental updates, which means that it's worthwhile to use an OS
 * facility (epoll, kqueue, etc.) to speed up polling.
 */

typedef struct rktio_ltps_t rktio_ltps_t;
typedef struct rktio_ltps_handle_t rktio_ltps_handle_t;

/**
 * Creates a long-term poll set.
 */
RKTIO_EXTERN rktio_ltps_t *rktio_ltps_open(rktio_t *rktio);

/**
 * Closes a long-term poll set. Closing will signal all remaining
 * handles and free all signaled handles, but use
 * `rktio_ltps_remove_all` and `rktio_ltps_get_signaled_handle` if you
 * need to clean up any per-handle data.
 */
RKTIO_EXTERN void rktio_ltps_close(rktio_t *rktio, rktio_ltps_t *lt);

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

/**
 * Adds a file descriptor to a long-term poll set, where `mode` is one
 * of the `RKTIO_LTPS_...` values.
 *
 * Don't free the returned handle; use it with
 * `rktio_ltps_handle_set_data` and `rktio_ltps_handle_get_data`, and
 * free it only when the same handle is returned by
 * `rktio_ltps_get_signaled_handle`. Using the `RKTIO_LTPS_REMOVE`
 * mode causes a previously created handle to be signaled. A
 * successful remove reports `RKTIO_ERROR_LTPS_REMOVED` while
 * returning NULL. A `...CHECK...` or `...REMOVE...` mode that doesn't
 * find the handle reports `RKTIO_ERROR_LTPS_NOT_FOUND`.
 */
RKTIO_EXTERN_POLL rktio_ltps_handle_t *rktio_ltps_add(rktio_t *rktio, rktio_ltps_t *lt, rktio_fd_t *rfd, int mode);

/**
 * Retrieves client data associated with the handle `h`; see
 * `rktio_ltps_add`.
 */
RKTIO_EXTERN_POLL_NOERR void *rktio_ltps_handle_get_data(rktio_t *rktio, rktio_ltps_handle_t *h);

/**
 * Associates client data with the handle `h`; see `rktio_ltps_add`.
 */
RKTIO_EXTERN_POLL void rktio_ltps_handle_set_data(rktio_t *rktio, rktio_ltps_handle_t *h, void *data);

/**
 * Removes all additions to the long-term poll set, signaling all
 * handles.
 */
RKTIO_EXTERN void rktio_ltps_remove_all(rktio_t *rktio, rktio_ltps_t *lt);

/**
 * Enqueues signaled handles for retrieval via `rktio_ltps_get_signaled_handle`.
 */
RKTIO_EXTERN rktio_ok_t rktio_ltps_poll(rktio_t *rktio, rktio_ltps_t *lt);

/**
 * Dequeues a handle that was signaled and enqueued by
 * `rktio_ltps_poll`. Free the returned handle when you're done with
 * it.
 */
RKTIO_EXTERN rktio_ltps_handle_t *rktio_ltps_get_signaled_handle(rktio_t *rktio, rktio_ltps_t *lt);

enum {
  RKTIO_LTPS_HANDLE_NONE,
  RKTIO_LTPS_HANDLE_ZERO,
  RKTIO_LTPS_HANDLE_FREE
};

/**
 * An alternative to receiving the handle via
 * `rktio_ltps_get_signaled_handle`; have signaling automatically
 * either zero the handle content (so the client can detect signaling)
 * or free the handle (because the client is no longer watching it).
 * If `auto_mode` is `RKTIO_LTPS_HANDLE_NONE`, automatic handling is
 * disabled for the handle.
 */
RKTIO_EXTERN void rktio_ltps_handle_set_auto(rktio_t *rktio, rktio_ltps_handle_t *lth, int auto_mode);

/**
 * Waits up to `nsecs` seconds (or forever if `nsecs` is 0), until
 * something registered with `rfds` or `lt` is ready, or until there's
 * some other activity that sometimes causes an early wakeup.
 *
 * This function and functions marked as `RKTIO_EXTERN_POLL` or
 * `RKTIO_EXTERN_POLL_NOERR` can be called concurrently with other
 * functions for the same `rktio`, except for functions that close
 * file descriptors that have been added to `rfds` or `lt`. This
 * function and `RKTIO_EXTERN_POLL`/`RKTIO_EXTERN_POLL_NOERR`
 * functions cannot be called concurrently with each other for the
 * same `rfds` or `lt`.
 */
RKTIO_EXTERN RKTIO_BLOCKING void rktio_sleep(rktio_t *rktio, float nsecs, rktio_poll_set_t *rfds, rktio_ltps_t *lt);

/**
 * Like `rktio_sleep`, but starts a sleep in a background thread. When the
 * background thread is done sleeping, it writes a byte to `woke_fd`, but the
 * background thread can be woken up with `rktio_end_sleep`.
 */
RKTIO_EXTERN rktio_ok_t rktio_start_sleep(rktio_t *rktio, float nsecs, rktio_poll_set_t *rfds, rktio_ltps_t *lt,
                                          int woke_fd);
/**
 * Ends a background sleep started with `rktio_start_sleep`. Call this
 * function exactly once for each successful `rktio_start_sleep`,
 * whether or not the background thread wrote to `woke_fd` already.
 */
RKTIO_EXTERN void rktio_end_sleep(rktio_t *rktio);

/****
 * File and Directory Functions */

/**
 * Reports whether `filename` refers to an existing file. On Windows,
 * check for special filenames (like "aux") before calling this
 * function.
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_file_exists(rktio_t *rktio, rktio_const_string_t filename);

/**
 * Reports whether `dirname` refers to an existing directory.
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_directory_exists(rktio_t *rktio, rktio_const_string_t dirname);

/**
 * Reports whether `filename` refers to an existing link.
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_link_exists(rktio_t *rktio, rktio_const_string_t filename);

/**
 * Reports whether `filename` refers to an existing regular file. On
 * Windows, check for special filenames (like "aux") before calling
 * this function.
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_is_regular_file(rktio_t *rktio, rktio_const_string_t filename);

#define RKTIO_FILE_TYPE_FILE           1
#define RKTIO_FILE_TYPE_DIRECTORY      2
#define RKTIO_FILE_TYPE_LINK           3
#define RKTIO_FILE_TYPE_DIRECTORY_LINK 4

#define RKTIO_FILE_TYPE_ERROR  (-1)

/**
 * Reports the type of `filename`. The result is
 * `RKTIO_FILE_TYPE_ERROR` for error, otherwise one of the
 * `RKTIO_FILE_TYPE_...` values. On Windows, check for special
 * filenames (like "aux") before calling this function.
 */
RKTIO_EXTERN_ERR(RKTIO_FILE_TYPE_ERROR) int rktio_file_type(rktio_t *rktio, rktio_const_string_t filename);

/**
 * Deletes the file `fn`. On Windows, if `enable_write_on_fail` is
 * true and deletion fails with an access error, deletion is retried
 * after attempting to enable write permission on the file.
 */
RKTIO_EXTERN rktio_ok_t rktio_delete_file(rktio_t *rktio, rktio_const_string_t fn, rktio_bool_t enable_write_on_fail);

/**
 * Renames the file `src` to `dest`, where `exists_ok` indicates
 * whether `dest` is allowed to exist already. Can report
 * `RKTIO_ERROR_EXISTS`.
 */
RKTIO_EXTERN rktio_ok_t rktio_rename_file(rktio_t *rktio, rktio_const_string_t dest, rktio_const_string_t src, rktio_bool_t exists_ok);

/**
 * Returns the current directory.
 */
RKTIO_EXTERN char *rktio_get_current_directory(rktio_t *rktio);

/**
 * Sets the current directory to `path`.
 */
RKTIO_EXTERN rktio_ok_t rktio_set_current_directory(rktio_t *rktio, rktio_const_string_t path);

/**
 * Creates the directory `filename`. Can report `RKTIO_ERROR_EXISTS`.
 */
RKTIO_EXTERN rktio_ok_t rktio_make_directory(rktio_t *rktio, rktio_const_string_t filename);

#define RKTIO_DEFAULT_DIRECTORY_PERM_BITS 0777

/**
 * Like `rktio_make_directory`, but with permissions `perm_bits` for
 * the new directory. Can report `RKTIO_ERROR_EXISTS`.
 */
RKTIO_EXTERN rktio_ok_t rktio_make_directory_with_permissions(rktio_t *rktio, rktio_const_string_t filename, int perm_bits);

/**
 * Deletes the directory `filename`. The `current_directory` argument
 * is used on Windows to avoid having `filename` (or a directory
 * within it) as the current directory while trying to delete it. The
 * `enable_write_on_fail` argument also applies to Windows; see
 * `rktio_delete_file`.
 */
RKTIO_EXTERN rktio_ok_t rktio_delete_directory(rktio_t *rktio, rktio_const_string_t filename, rktio_const_string_t current_directory,
                                               rktio_bool_t enable_write_on_fail);

/**
 * Returns the target of the link `fullfilename`. The argument should
 * not have a trailing separator. Can report `RKTIO_ERROR_NOT_A_LINK`.
 */
RKTIO_EXTERN char *rktio_readlink(rktio_t *rktio, rktio_const_string_t fullfilename);

/**
 * Creates a link at `src` that refers to `dest`. The
 * `dest_is_directory` argument is used only on Windows. Can report
 * `RKTIO_ERROR_EXISTS`.
 */
RKTIO_EXTERN rktio_ok_t rktio_make_link(
   rktio_t *rktio, rktio_const_string_t src, rktio_const_string_t dest, rktio_bool_t dest_is_directory);

/****
 * File Attributes */

typedef intptr_t rktio_timestamp_t;

/**
 * Returns the size of the file `filename` in bytes.
 */
RKTIO_EXTERN rktio_filesize_t *rktio_file_size(rktio_t *rktio, rktio_const_string_t filename);

/**
 * Returns the modification timestamp of `file` in seconds.
 */
RKTIO_EXTERN rktio_timestamp_t *rktio_get_file_modify_seconds(rktio_t *rktio, rktio_const_string_t file);

/**
 * Sets the modification timestamp of `file` to `secs`.
 */
RKTIO_EXTERN rktio_ok_t rktio_set_file_modify_seconds(rktio_t *rktio, rktio_const_string_t file, rktio_timestamp_t secs);

typedef struct rktio_stat_t {
  /* Eventually, this should use `int64_t`, available in C99 and up */
  uintptr_t device_id;
  uintptr_t inode;
  uintptr_t mode;
  uintptr_t hardlink_count;
  uintptr_t user_id;
  uintptr_t group_id;
  uintptr_t device_id_for_special_file;
  uintptr_t size;
  uintptr_t block_size;
  uintptr_t block_count;
  uintptr_t access_time_seconds;
  uintptr_t access_time_nanoseconds;
  uintptr_t modify_time_seconds;
  uintptr_t modify_time_nanoseconds;
  uintptr_t ctime_seconds;
  uintptr_t ctime_nanoseconds;
  /* The `st_ctime` field is status change time for Posix and creation time
     for Windows. */
  rktio_bool_t ctime_is_change_time;
} rktio_stat_t;

/**
 * Returns stat information for the file or directory `path`, where
 * `follow_links` selects whether the result describes the target of a
 * link.
 */
RKTIO_EXTERN rktio_stat_t *rktio_file_or_directory_stat(rktio_t *rktio, rktio_const_string_t path, rktio_bool_t follow_links);

/**
 * Returns stat information for the file accessed by `rfd`.
 */
RKTIO_EXTERN rktio_stat_t *rktio_fd_stat(rktio_t *rktio, rktio_fd_t *rfd);

typedef struct rktio_identity_t {
  uintptr_t a;
  uintptr_t b;
  uintptr_t c;
  int a_bits;
  int b_bits;
  int c_bits;
} rktio_identity_t;

/**
 * Returns an identity (such as device and inode numbers) for the file
 * accessed by `rfd`.
 */
RKTIO_EXTERN rktio_identity_t *rktio_fd_identity(rktio_t *rktio, rktio_fd_t *rfd);

/**
 * Returns an identity (such as device and inode numbers) for the file
 * or directory `path`, where `follow_links` selects whether the
 * result describes the target of a link.
 */
RKTIO_EXTERN rktio_identity_t *rktio_path_identity(rktio_t *rktio, rktio_const_string_t path, rktio_bool_t follow_links);

/****
 * Permissions */

/**
 * Should match OS bits:
 */
#define RKTIO_PERMISSION_READ  0x4
#define RKTIO_PERMISSION_WRITE 0x2
#define RKTIO_PERMISSION_EXEC  0x1

#define RKTIO_PERMISSION_ERROR (-1)

/**
 * Reports the permissions of `filename`. The result is
 * `RKTIO_PERMISSION_ERROR` for error, otherwise a combination of
 * bits. If not `all_bits`, then the result uses the
 * `RKTIO_PERMISSION_...` constants.
 */
RKTIO_EXTERN_ERR(RKTIO_PERMISSION_ERROR) int rktio_get_file_or_directory_permissions(
   rktio_t *rktio, rktio_const_string_t filename, rktio_bool_t all_bits);

/**
 * Sets the permissions of `filename`. The `new_bits` format
 * corresponds to `all_bits` for getting permissions. Can report
 * `RKTIO_ERROR_BAD_PERMISSION` for bits that make no sense.
 */
RKTIO_EXTERN rktio_ok_t rktio_set_file_or_directory_permissions(rktio_t *rktio, rktio_const_string_t filename, int new_bits);

typedef struct rktio_directory_list_t rktio_directory_list_t;

/****
 * Directory Listing */

/**
 * Starts a directory listing for `dirname`. On Windows, the given
 * `dirname` must be normalized and not have `.` or `..`.
 */
RKTIO_EXTERN rktio_directory_list_t *rktio_directory_list_start(rktio_t *rktio, rktio_const_string_t dirname);

/**
 * Returns the next element of a directory listing. Returns an
 * unallocated "" and deallocates `dl` when the iteration is complete.
 * A NULL result would mean an error without deallocating `dl`, but
 * that doesn't currently happen.
 */
RKTIO_EXTERN char *rktio_directory_list_step(rktio_t *rktio, rktio_directory_list_t *dl);

/**
 * An atomic-friendly version of `rktio_directory_list_start`.
 */
RKTIO_EXTERN_ALLOC_RESULT(rktio_result_directory_list) rktio_result_t *rktio_directory_list_start_r(
   rktio_t *rktio, rktio_const_string_t dirname);

/**
 * An atomic-friendly version of `rktio_directory_list_step`. The same
 * `dl` must not be used in any concurrent call, and the result is
 * valid until `dl` is used again.
 */
RKTIO_EXTERN_RESULT(rktio_result_string) rktio_result_t *rktio_directory_list_step_r(
   rktio_t *rktio, rktio_directory_list_t *dl);

/**
 * Interrupts a directory listing in progress; not needed after
 * `rktio_directory_list_step` returns "".
 */
RKTIO_EXTERN_ATOMIC void rktio_directory_list_stop(rktio_t *rktio, rktio_directory_list_t *dl);

/**
 * Returns the filesystem roots as a NULL-terminated array. Free each
 * string. Currently never errors.
 */
RKTIO_EXTERN char **rktio_filesystem_roots(rktio_t *rktio);

/****
 * File Copying */

typedef struct rktio_file_copy_t rktio_file_copy_t;

enum {
  RKTIO_COPY_STEP_UNKNOWN,
  RKTIO_COPY_STEP_OPEN_SRC,
  RKTIO_COPY_STEP_OPEN_DEST,
  RKTIO_COPY_STEP_READ_SRC_DATA,
  RKTIO_COPY_STEP_WRITE_DEST_DATA,
  RKTIO_COPY_STEP_READ_SRC_METADATA,
  RKTIO_COPY_STEP_WRITE_DEST_METADATA
};

/**
 * Starts a copy of the file `src` to `dest`. Depending on the OS,
 * this step may perform the whole copy, or it may just get started.
 * Can report `RKTIO_ERROR_EXISTS`, and sets an error step as listed
 * further below.
 */
RKTIO_EXTERN_STEP rktio_file_copy_t *rktio_copy_file_start(
   rktio_t *rktio, rktio_const_string_t dest, rktio_const_string_t src,
   rktio_bool_t exists_ok);

/**
 * Like `rktio_copy_file_start`, but accepts optional permissions to
 * apply to the copy. The `perm_bits` are used only if `use_perm_bits`
 * is set; otherwise, the source file's permissions are used. On Unix,
 * `override_create_perms` determines whether the destination file's
 * permissions are forced to the intended permissions (whether
 * supplied or taken from the `src` file), instead of permissions as
 * possibly modified by `umask` on file creation or as left unchanged
 * because the file already exists.
 */
RKTIO_EXTERN_STEP rktio_file_copy_t *rktio_copy_file_start_permissions(
   rktio_t *rktio, rktio_const_string_t dest, rktio_const_string_t src,
   rktio_bool_t exists_ok, rktio_bool_t use_perm_bits, int perm_bits,
   rktio_bool_t override_create_perms);

/**
 * Reports whether the file copy `fc` is complete.
 */
RKTIO_EXTERN_ATOMIC rktio_bool_t rktio_copy_file_is_done(rktio_t *rktio, rktio_file_copy_t *fc);

/**
 * As long as the copy isn't done, call `rktio_copy_file_step` to make
 * a little progress. Use `rktio_copy_file_finish_permissions`
 * (optionally) and then `rktio_copy_file_stop` when done. An error
 * sets an error step as listed further below.
 */
RKTIO_EXTERN_STEP rktio_ok_t rktio_copy_file_step(rktio_t *rktio, rktio_file_copy_t *fc);

/**
 * Depending on the OS, copies permissions from the source to the
 * destination. This step can be performed at any time between the
 * start and stop. Reports success if this step isn't needed (e.g.,
 * where a copy fully completes when it is started). On error, the
 * step is set to `RKTIO_COPY_STEP_WRITE_DEST_METADATA`.
 */
RKTIO_EXTERN_STEP rktio_ok_t rktio_copy_file_finish_permissions(rktio_t *rktio, rktio_file_copy_t *fc);

/**
 * Deallocates the copy process, interrupting it if the copy is not complete.
 */
RKTIO_EXTERN void rktio_copy_file_stop(rktio_t *rktio, rktio_file_copy_t *fc);

/****
 * System Paths */

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
  RKTIO_PATH_INIT_FILE,
  RKTIO_PATH_CACHE_DIR
};

/**
 * Returns a path to a system directory or file as indicated by
 * `which`, which is one of the `RKTIO_PATH_...` values.
 */
RKTIO_EXTERN char *rktio_system_path(rktio_t *rktio, int which);

/**
 * Expands a path that starts with a tilde to refer to a user's home
 * directory. The path must start with a tilde, otherwise
 * `RKTIO_ERROR_NO_TILDE` is reported. Other possible errors are
 * `RKTIO_ERROR_ILL_FORMED_USER` and `RKTIO_ERROR_UNKNOWN_USER`.
 */
RKTIO_EXTERN char *rktio_expand_user_tilde(rktio_t *rktio, rktio_const_string_t filename);

/**
 * Returns a string describing the current machine and installation,
 * similar to the return of `uname -a` on Unix. If machine information
 * cannot be obtained for some reason, the result is a copy of
 * "<unknown machine>".
 */
RKTIO_EXTERN_NOERR char *rktio_uname(rktio_t *rktio);

/****
 * Sleep and Signals */

/**
 * A `rktio_signal_handle_t` is a value specific to a `rktio_t` that
 * causes any `rktio_sleep` for that `rktio_t` to return (or causes
 * the next `rktio_sleep` to return if one is not in progress).
 */
typedef struct rktio_signal_handle_t rktio_signal_handle_t;

/**
 * Gets the signal handle for the given `rktio_t`.
 */
RKTIO_EXTERN_NOERR rktio_signal_handle_t *rktio_get_signal_handle(rktio_t *rktio);

/**
 * Signals the given handle. This function can be called from any
 * thread or from signal handlers.
 */
RKTIO_EXTERN void rktio_signal_received_at(rktio_signal_handle_t *signal_handle);

/**
 * A shorthand for `rktio_signal_received_at` composed with
 * `rktio_get_signal_handle`.
 */
RKTIO_EXTERN void rktio_signal_received(rktio_t *rktio);

/**
 * The same as `rktio_sleep` with no timeout, no poll set, and no
 * long-term poll set.
 */
RKTIO_EXTERN void rktio_wait_until_signal_received(rktio_t *rktio);

/**
 * Clears any pending signal so that it doesn't interrupt the next
 * `rktio_sleep`.
 */
RKTIO_EXTERN void rktio_flush_signals_received(rktio_t *rktio);

/**
 * Installs OS-level handlers for SIGINT, SIGTERM, and SIGHUP (or
 * Ctl-C on Windows) to signal the handle of `rktio` and also records
 * the signal for reporting via `rktio_poll_os_signal`. Only one
 * `rktio` can be registered this way at a time. This function must
 * not be called in two threads at the same time; more generally, it
 * can only be called when `rktio_will_modify_os_signal_handler`
 * can be called for SIGINT, etc.
 */
RKTIO_EXTERN void rktio_install_os_signal_handler(rktio_t *rktio);

enum {
  RKTIO_OS_SIGNAL_INT,
  RKTIO_OS_SIGNAL_TERM,
  RKTIO_OS_SIGNAL_HUP,
  RKTIO_NUM_OS_SIGNALS
};
#define RKTIO_OS_SIGNAL_NONE (-1)

/**
 * Returns a received signal as a `RKTIO_OS_SIGNAL_...` value (not
 * counting `RKTIO_NUM_OS_SIGNALS`), or `RKTIO_OS_SIGNAL_NONE` if no
 * signal has been received.
 */
RKTIO_EXTERN_NOERR int rktio_poll_os_signal(rktio_t *rktio);

/**
 * Registers with rktio that an operating-system signal handler is
 * about to be modified within the process but outside of rktio, where
 * `sig_id` is a signal identifier --- such as SIGINT or SIGTERM. This
 * notification allows rktio to record the current signal disposition
 * so that it can be restored after forking a new Unix process. Signal
 * registrations should happen only before multiple threads use rktio,
 * and registration of the signal can happen before any `rktio_init`
 * call. After a signal is registered, trying to re-register it after
 * threads start is harmless.
 */
RKTIO_EXTERN void rktio_will_modify_os_signal_handler(int sig_id);

/****
 * Time and Date */

 typedef struct rktio_date_t {
  int nanosecond, second, minute, hour, day, month;
  intptr_t year;
  int day_of_week;
  int day_of_year;
  int is_dst;
  int zone_offset;
  char *zone_name; /* can be NULL; otherwise, free it */
} rktio_date_t;

/**
 * Returns wall-clock time in milliseconds. Overflow may cause the
 * result to wrap around to 0, at least on a 32-bit platform.
 */
RKTIO_EXTERN_NOERR uintptr_t rktio_get_milliseconds(void);

/**
 * Returns wall-clock time in milliseconds. No overflow, but won't
 * strictly increase if the system clock is reset.
 */
RKTIO_EXTERN_NOERR double rktio_get_inexact_milliseconds(void);

/**
 * Returns real time like wall-clock time in milliseconds, but the
 * result will strictly increase, assuming that the host system
 * provides a monotonic clock.
 */
RKTIO_EXTERN_NOERR double rktio_get_inexact_monotonic_milliseconds(rktio_t *rktio);

/**
 * Returns CPU time in milliseconds across all threads within the
 * process. Overflow may cause the result to wrap around to 0, at
 * least on a 32-bit platform.
 */
RKTIO_EXTERN_NOERR uintptr_t rktio_get_process_milliseconds(rktio_t *rktio);

/**
 * Returns CPU time in milliseconds consumed by the process's
 * children. Overflow may cause the result to wrap around to 0, at
 * least on a 32-bit platform.
 */
RKTIO_EXTERN_NOERR uintptr_t rktio_get_process_children_milliseconds(rktio_t *rktio);

/**
 * Returns wall-clock time in seconds.
 */
RKTIO_EXTERN_NOERR rktio_timestamp_t rktio_get_seconds(rktio_t *rktio);

/**
 * Converts a timestamp in `seconds` (and `nanoseconds`) to a date,
 * either for UTC (if `get_gmt`) or the local time zone. A timestamp
 * can be negative to represent a date before 1970.
 */
RKTIO_EXTERN rktio_date_t *rktio_seconds_to_date(rktio_t *rktio, rktio_timestamp_t seconds, int nanoseconds, int get_gmt);

/****
 * Windows ShellExecute */

enum {
  RKTIO_SW_HIDE,
  RKTIO_SW_MAXIMIZE,
  RKTIO_SW_MINIMIZE,
  RKTIO_SW_RESTORE,
  RKTIO_SW_SHOW,
  RKTIO_SW_SHOWDEFAULT,
  RKTIO_SW_SHOWMAXIMIZED,
  RKTIO_SW_SHOWMINIMIZED,
  RKTIO_SW_SHOWMINNOACTIVE,
  RKTIO_SW_SHOWNA,
  RKTIO_SW_SHOWNOACTIVATE,
  RKTIO_SW_SHOWNORMAL
};

/**
 * Supported only on Windows to run `ShellExecute`. The `dir` argument
 * needs to have normalized path separators. The `show_mode` argument
 * is one of the `RKTIO_SW_...` values.
 */
RKTIO_EXTERN RKTIO_MSG_QUEUE rktio_ok_t rktio_shell_execute(
   rktio_t *rktio,
   rktio_const_string_t verb,
   rktio_const_string_t target,
   rktio_const_string_t arg,
   rktio_const_string_t dir,
   int show_mode);

/****
 * Windows Path Conversion */

/**
 * Converts a path to the OS's native wide-character representation.
 * This function is useful only on Windows. Can fail and report
 * `RKTIO_ERROR_INVALID_PATH`.
 */
RKTIO_EXTERN rktio_char16_t *rktio_path_to_wide_path(rktio_t *rktio, rktio_const_string_t path);

/**
 * Converts a path from the OS's native wide-character representation.
 * This function is useful only on Windows.
 */
RKTIO_EXTERN_NOERR char *rktio_wide_path_to_path(rktio_t *rktio, const rktio_char16_t *wpath);

/****
 * System Intrinsics */

/**
 * Returns the number of processing units, either as CPUs, cores, or
 * hyperthreads.
 */
RKTIO_EXTERN_NOERR int rktio_processor_count(rktio_t *rktio);

/****
 * System Logging */

enum {
  RKTIO_LOG_FATAL = 1,
  RKTIO_LOG_ERROR,
  RKTIO_LOG_WARNING,
  RKTIO_LOG_INFO,
  RKTIO_LOG_DEBUG
};

/**
 * Adds a message to the system log, where `level` is a
 * `RKTIO_LOG_...` value. The `name` argument can be NULL, and it is
 * added to the front of the message with a separating ": " if
 * non-NULL. The `exec_name` is the current executable name; it's
 * currently used only on Windows, and the value may matter only the
 * first time that `rktio_syslog` is called.
 */
RKTIO_EXTERN rktio_ok_t rktio_syslog(
   rktio_t *rktio, int level, rktio_const_string_t name, rktio_const_string_t msg, rktio_const_string_t exec_name);

/**
 * Like `rktio_syslog`, but fails silently, so it can be treated as
 * atomic.
 */
RKTIO_EXTERN_ATOMIC_NOERR void rktio_syslog_best_effort(
   rktio_t *rktio, int level, rktio_const_string_t name, rktio_const_string_t msg, rktio_const_string_t exec_name);

/**
 * Writes to the standard destination `RKTIO_STDOUT` or
 * `RKTIO_STDERR`, blocking until the write completes or an error is
 * encountered. This function is meant to be used in a way similar to
 * `rktio_syslog_best_effort`, but for writing to stdout or stderr.
 */
RKTIO_EXTERN_ATOMIC_NOERR void rktio_std_write_in_best_effort(rktio_t *rktio, int which,
                                                              char *buffer, intptr_t start, intptr_t end);

/****
 * Char Encoding and Conversion */

#define RKTIO_CONVERTER_SUPPORTED   (1 << 0)
#define RKTIO_CONVERT_STRCOLL_UTF16 (1 << 1)
#define RKTIO_CONVERT_RECASE_UTF16  (1 << 2)

/**
 * Reports properties of the encoding converter implementation as a
 * combination of `RKTIO_CONVERT...` flags.
 */
RKTIO_EXTERN_ATOMIC_NOERR int rktio_convert_properties(rktio_t *rktio);

typedef struct rktio_converter_t rktio_converter_t;

/**
 * Creates an encoding converter from `from_enc` to `to_enc`.
 */
RKTIO_EXTERN rktio_converter_t *rktio_converter_open(rktio_t *rktio, rktio_const_string_t to_enc, rktio_const_string_t from_enc);

/**
 * Destroys an encoding converter.
 */
RKTIO_EXTERN void rktio_converter_close(rktio_t *rktio, rktio_converter_t *cvt);

#define RKTIO_CONVERT_ERROR (-1)

/**
 * Converts some bytes, following the iconv protocol: each consumed
 * byte increments `*in` and decrements `*in_left`, and each produced
 * byte increments `*out` and decrements `*out_left`. In case of an
 * error, the result is `RKTIO_CONVERT_ERROR` and the last error is
 * set to one of `RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE`,
 * `RKTIO_ERROR_CONVERT_BAD_SEQUENCE`,
 * `RKTIO_ERROR_CONVERT_PREMATURE_END`, or
 * `RKTIO_ERROR_CONVERT_OTHER` --- but an error indicates something
 * within `in` or `out`, and some bytes may have been successfully
 * converted even if an error is reported.
 */
RKTIO_EXTERN_ERR(RKTIO_CONVERT_ERROR) intptr_t rktio_convert(rktio_t *rktio,
   rktio_converter_t *cvt,
   char **in, intptr_t *in_left,
   char **out, intptr_t *out_left);

typedef struct rktio_convert_result_t {
  intptr_t in_consumed;  /* input bytes converted */
  intptr_t out_produced; /* output bytes produced */
  intptr_t converted;    /* characters converted, can be `RKTIO_CONVERT_ERROR` */
} rktio_convert_result_t;

/**
 * The same as `rktio_convert`, but accepting start and end positions
 * and returning results as an allocated struct. A conversion error
 * doesn't return a NULL result; instead, `converted` in the result
 * reports the error.
 */
RKTIO_EXTERN rktio_convert_result_t *rktio_convert_in(rktio_t *rktio,
   rktio_converter_t *cvt,
   char *in, intptr_t in_start, intptr_t in_end,
   char *out, intptr_t out_start, intptr_t out_end);

/**
 * Resets a converter to its initial state.
 */
RKTIO_EXTERN void rktio_convert_reset(rktio_t *rktio, rktio_converter_t *cvt);

/**
 * Upcases (if `to_up`) or downcases (if `!to_up`) the content of `in`
 * using the current locale's encoding and case conversion.
 */
RKTIO_EXTERN_NOERR char *rktio_locale_recase(
   rktio_t *rktio, rktio_bool_t to_up, rktio_const_string_t in);

/**
 * Converts the case of a string encoded in UTF-16 for the system's
 * default locale if the OS provides direct support for it. The
 * `RKTIO_CONVERT_RECASE_UTF16` property from
 * `rktio_convert_properties` reports whether this function will work.
 * Takes and optionally returns a length (`olen` can be NULL), but the
 * UTF-16 sequence is expected to have no nuls.
 */
RKTIO_EXTERN_NOERR rktio_char16_t *rktio_recase_utf16(
   rktio_t *rktio, rktio_bool_t to_up, rktio_char16_t *s1,
   intptr_t len, intptr_t *olen);

/**
 * Returns -1 if `s1` is less than `s2` by the current locale's
 * comparison, positive if `s1` is greater, and 0 if the strings
 * are equal.
 */
RKTIO_EXTERN_NOERR int rktio_locale_strcoll(rktio_t *rktio, rktio_const_string_t s1, rktio_const_string_t s2);

/**
 * Compares two strings encoded in UTF-16 for the system's default
 * locale if the OS provides direct support for it. The
 * `RKTIO_CONVERT_STRCOLL_UTF16` property from
 * `rktio_convert_properties` reports whether this function will work.
 * Takes lengths, but the UTF-16 sequences are expected to include no
 * nuls.
 */
RKTIO_EXTERN_NOERR int rktio_strcoll_utf16(rktio_t *rktio,
   rktio_char16_t *s1, intptr_t l1,
   rktio_char16_t *s2, intptr_t l2,
   rktio_bool_t cvt_case);

/**
 * Returns the name of the current locale's encoding.
 */
RKTIO_EXTERN char *rktio_locale_encoding(rktio_t *rktio);

/**
 * Sets the current locale, which affects rktio string comparisons and
 * conversions. It can also affect the C library's character-property
 * predicates and number printing/parsing by setting a thread-local or
 * process-wide locale, but that effect is not guaranteed. The empty
 * string corresponds to the OS's native locale, and a NULL string
 * pointer corresponds to the C locale.
 */
RKTIO_EXTERN void rktio_set_locale(rktio_t *rktio, rktio_const_string_t name);

/**
 * Similar to `rktio_set_locale`, but sets the locale process-wide.
 */
RKTIO_EXTERN void rktio_set_default_locale(rktio_const_string_t name);

/**
 * Use this function and `rktio_pop_c_numeric_locale` as a pair to
 * temporarily switch the locale to the C locale for number parsing
 * and printing. Unlike `rktio_set_locale`, these functions set and
 * restore the thread-local or even process-wide locale. The result of
 * this function is deallocated when passed to
 * `rktio_pop_c_numeric_locale`.
 */
RKTIO_EXTERN_NOERR void *rktio_push_c_numeric_locale(rktio_t *rktio);

/**
 * Restores the locale saved by a `rktio_push_c_numeric_locale` call
 * that returned `prev`, deallocating `prev`.
 */
RKTIO_EXTERN void rktio_pop_c_numeric_locale(rktio_t *rktio, void *prev);

/**
 * Returns the current system's language and country in a 5-character
 * format such as "en_US".
 */
RKTIO_EXTERN char *rktio_system_language_country(rktio_t *rktio);

/****
 * SHA-1, SHA-224, and SHA-256 */

/**
 * From Steve Reid's implementation at https://www.ghostscript.com/
 */

typedef struct rktio_sha1_ctx_t {
  unsigned int state[5];
  unsigned int count[2];
  unsigned char buffer[64];
} rktio_sha1_ctx_t;

#define RKTIO_SHA1_DIGEST_SIZE 20

/**
 * Initializes a SHA-1 context, which is memory of length
 * `sizeof(rktio_sha1_ctx_t)` containing no pointers.
 */
RKTIO_EXTERN void rktio_sha1_init(rktio_sha1_ctx_t *context);

/**
 * Adds some bytes to the SHA-1 hash.
 */
RKTIO_EXTERN void rktio_sha1_update(
   rktio_sha1_ctx_t *context, const unsigned char *data, intptr_t start, intptr_t end);

/**
 * Gets the final SHA-1 hash value after all bytes have been added,
 * writing `RKTIO_SHA1_DIGEST_SIZE` bytes to `digest`.
 */
RKTIO_EXTERN void rktio_sha1_final(rktio_sha1_ctx_t *context, unsigned char *digest /* RKTIO_SHA1_DIGEST_SIZE */);

typedef struct rktio_sha2_ctx_t {
    unsigned total[2];
    unsigned state[8];
    unsigned char buffer[64];
    int is224;
} rktio_sha2_ctx_t;

#define RKTIO_SHA224_DIGEST_SIZE 28
#define RKTIO_SHA256_DIGEST_SIZE 32

/**
 * Initializes a context for SHA-224 (if `is224`) or SHA-256
 * (otherwise) hashing.
 */
RKTIO_EXTERN void rktio_sha2_init(rktio_sha2_ctx_t *ctx, rktio_bool_t is224);

/**
 * Adds some bytes to the SHA-224 or SHA-256 hash.
 */
RKTIO_EXTERN void rktio_sha2_update(rktio_sha2_ctx_t *ctx,
                                    const unsigned char *data, intptr_t start, intptr_t end);

/**
 * Gets the final hash value after all bytes have been added, writing
 * `RKTIO_SHA224_DIGEST_SIZE` or `RKTIO_SHA256_DIGEST_SIZE` bytes to
 * `digest`.
 */
RKTIO_EXTERN void rktio_sha2_final(rktio_sha2_ctx_t *ctx, unsigned char *digest /* RKTIO_SHA2{24,56}_DIGEST_SIZE */);

/****
 * Dynamic Library Loading */

typedef struct rktio_dll_t rktio_dll_t;

/**
 * Loads a DLL using system-provided functions and search rules, such
 * as dlopen() and its rules. If `as_global` is true, then the library
 * is loaded in "global" mode, which has implications for other
 * libraries trying to find bindings and for searching within the
 * specific library for a binding. The `name` argument can be NULL
 * to mean "the current executable".
 *
 * Some system error-reporting protocols do not fit nicely into the
 * normal rktio error model. If the `RKTIO_ERROR_DLL` error is
 * reported, then `rktio_dll_get_error` must be used before any other
 * `rktio_dll_...` call to get an error string.
 *
 * If a DLL has been loaded with `name` already, the previous result
 * is returned again, but with an internal reference count incremented.
 * The `as_global` argument matters only for the first load of a DLL
 * through a given `name`.
 *
 * Unless the DLL is explicitly unloaded with `rktio_dll_close`, even
 * when the given `rktio` is closed with `rktio_destroy`, loaded
 * libraries remain in the process.
 */
RKTIO_EXTERN rktio_dll_t *rktio_dll_open(rktio_t *rktio, rktio_const_string_t name, rktio_bool_t as_global);

/**
 * Finds an address within `dll` for the `name` export.
 *
 * An error result can be `RKTIO_ERROR_DLL` as for `rktio_dll_open`.
 */
RKTIO_EXTERN void *rktio_dll_find_object(rktio_t *rktio, rktio_dll_t *dll, rktio_const_string_t name);

/**
 * Decrements the reference count on `dll`, and if it goes to zero,
 * unloads the DLL using system-provided functions and destroys the
 * `dll` argument.
 *
 * An error result can be `RKTIO_ERROR_DLL` as for `rktio_dll_open`.
 */
RKTIO_EXTERN rktio_ok_t rktio_dll_close(rktio_t *rktio, rktio_dll_t *dll);

/**
 * Returns an error for a previous `rktio_dll_...` call, or NULL
 * if no error string is available or has already been returned.
 * See `rktio_dll_open` for more information.
 */
RKTIO_EXTERN char *rktio_dll_get_error(rktio_t *rktio);

/**
 * Hook types for dll open, find and close procedures.
 */
typedef void *(*dll_open_proc)(rktio_const_string_t name, rktio_bool_t as_global);
typedef void *(*dll_find_object_proc)(void *h, rktio_const_string_t name);
typedef void (*dll_close_proc)(void *h);

/**
 * Installs procedures that are tried before native mechanisms,
 * currently only supported for Windows.
 */
RKTIO_EXTERN void rktio_set_dll_procs(dll_open_proc dll_open,
   dll_find_object_proc dll_find_object, dll_close_proc dll_close);

/****
 * Errors */

enum {
  RKTIO_ERROR_KIND_POSIX,
  RKTIO_ERROR_KIND_WINDOWS,
  RKTIO_ERROR_KIND_GAI,
  RKTIO_ERROR_KIND_RACKET
};

/**
 * Returns the kind of the most recent error for `rktio` as a
 * `RKTIO_ERROR_KIND_...` value.
 */
RKTIO_EXTERN_NOERR int rktio_get_last_error_kind(rktio_t *rktio);

/**
 * Returns the kind of the error recorded in `res` as a
 * `RKTIO_ERROR_KIND_...` value.
 */
RKTIO_EXTERN_ATOMIC_NOERR int rktio_get_error_kind(rktio_result_t *res);

/**
 * Error IDs of kind RKTIO_ERROR_KIND_RACKET
 */
enum {
  RKTIO_ERROR_UNSUPPORTED = 1,
  RKTIO_ERROR_INVALID_PATH, /* Windows path-decoding failure */
  RKTIO_ERROR_DOES_NOT_EXIST,
  RKTIO_ERROR_EXISTS,
  RKTIO_ERROR_ACCESS_DENIED,
  RKTIO_ERROR_LINK_FAILED,
  RKTIO_ERROR_NOT_A_LINK,
  RKTIO_ERROR_BAD_PERMISSION,
  RKTIO_ERROR_IS_A_DIRECTORY,
  RKTIO_ERROR_NOT_A_DIRECTORY,
  RKTIO_ERROR_UNSUPPORTED_TEXT_MODE,
  RKTIO_ERROR_CANNOT_FILE_POSITION,
  RKTIO_ERROR_NO_TILDE,
  RKTIO_ERROR_ILL_FORMED_USER,
  RKTIO_ERROR_UNKNOWN_USER,
  RKTIO_ERROR_INIT_FAILED,
  RKTIO_ERROR_LTPS_NOT_FOUND,
  RKTIO_ERROR_LTPS_REMOVED, /* indicates success, instead of failure */
  RKTIO_ERROR_CONNECT_TRYING_NEXT, /* indicates that failure is not (yet) permanent */
  RKTIO_ERROR_ACCEPT_NOT_READY,
  RKTIO_ERROR_HOST_AND_PORT_BOTH_UNSPECIFIED,
  RKTIO_ERROR_INFO_TRY_AGAIN, /* for UDP */
  RKTIO_ERROR_TRY_AGAIN, /* for UDP */
  RKTIO_ERROR_TRY_AGAIN_WITH_IPV4, /* for TCP listen */
  RKTIO_ERROR_TIME_OUT_OF_RANGE,
  RKTIO_ERROR_NO_SUCH_ENVVAR,
  RKTIO_ERROR_SHELL_EXECUTE_FAILED,
  RKTIO_ERROR_CONVERT_NOT_ENOUGH_SPACE,
  RKTIO_ERROR_CONVERT_BAD_SEQUENCE,
  RKTIO_ERROR_CONVERT_PREMATURE_END,
  RKTIO_ERROR_CONVERT_OTHER,
  RKTIO_ERROR_DLL, /* use `rktio_dll_get_error` atomically to get error */
};

/**
 * Returns the most recent error for `rktio`. The value is a
 * `RKTIO_ERROR_...` value if the error kind is
 * `RKTIO_ERROR_KIND_RACKET`, otherwise it is an OS-supplied value.
 */
RKTIO_EXTERN_NOERR int rktio_get_last_error(rktio_t *rktio);

/**
 * Returns the error recorded in `res`. The value is a
 * `RKTIO_ERROR_...` value if the error kind is
 * `RKTIO_ERROR_KIND_RACKET`, otherwise it is an OS-supplied value.
 */
RKTIO_EXTERN_ATOMIC_NOERR int rktio_get_error(rktio_result_t *res);

/**
 * Returns the step of the most recent error for `rktio`. Some
 * operations report further information about the step that failed.
 * The meaning of a step number is operation-specific.
 */
RKTIO_EXTERN_NOERR int rktio_get_last_error_step(rktio_t *rktio);

/**
 * Returns the step of the error recorded in `res`. Some operations
 * report further information about the step that failed. The meaning
 * of a step number is operation-specific.
 */
RKTIO_EXTERN_ATOMIC_NOERR int rktio_get_error_step(rktio_result_t *res);

/**
 * Sets the recorded error; use this function in case you need to save
 * and restore error information.
 */
RKTIO_EXTERN void rktio_set_last_error(rktio_t *rktio, int kind, int errid);

/**
 * Sets the recorded error step; use this function in case you need to
 * save and restore error information.
 */
RKTIO_EXTERN void rktio_set_last_error_step(rktio_t *rktio, int step);

/**
 * In a few cases, rktio substitutes a `RKTIO_ERROR_KIND_RACKET` error
 * for an OS-supplied error. This function can sometimes undo the
 * substitution, modifying the current error and kind.
 */
RKTIO_EXTERN void rktio_remap_last_error(rktio_t *rktio);

/**
 * Like `rktio_remap_last_error`, but for the error recorded in `res`.
 */
RKTIO_EXTERN void rktio_remap_error(rktio_result_t *res);

/**
 * Returns a string for the most recent error for `rktio`. The
 * returned string should not be deallocated, but it only lasts
 * reliably until the next call to either of the
 * `rktio_...error_string` functions.
 */
RKTIO_EXTERN_NOERR const char *rktio_get_last_error_string(rktio_t *rktio);

/**
 * Returns a string for the given error kind and ID. The returned
 * string should not be deallocated, but it only lasts reliably until
 * the next call to either of the `rktio_...error_string` functions.
 */
RKTIO_EXTERN_NOERR const char *rktio_get_error_string(rktio_t *rktio, int kind, int errid);

/**
 * Returns a best-effort classification of `errkind` plus `errid` into
 * a portable string name using Posix conventions. For example,
 * `RKTIO_ERROR_KIND_POSIX` plus `ENOENT` returns "ENOENT", as does
 * `RKTIO_ERROR_KIND_WINDOWS` plus `ERROR_FILE_NOT_FOUND`.
 */
RKTIO_EXTERN_ATOMIC_NOERR rktio_const_string_t rktio_classify_error(int errkind, int errid);

/**
 * Detects a `rktio_result_t` pointer representing success.
 */
RKTIO_EXTERN_NOERR rktio_bool_t rktio_result_is_success(rktio_result_t *res);

/**
 * Extracts an integer success result. The available value depends on
 * the function returning the `rktio_result_t` pointer.
 */
RKTIO_EXTERN_NOERR intptr_t rktio_result_integer(rktio_result_t *res);

/**
 * Extracts a string success result. The available value depends on
 * the function returning the `rktio_result_t` pointer.
 */
RKTIO_EXTERN_NOERR char *rktio_result_string(rktio_result_t *res);

/**
 * Extracts a directory-list success result. The available value
 * depends on the function returning the `rktio_result_t` pointer.
 */
RKTIO_EXTERN_NOERR rktio_directory_list_t *rktio_result_directory_list(rktio_result_t *res);

#endif
