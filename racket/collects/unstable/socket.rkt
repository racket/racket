#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/file
         unstable/error)
(provide unix-socket-connect
         unix-socket-available?)

;; Unix domain sockets (connect only)

(define platform
  (let ([os (system-type 'os)]
        [machine (system-type 'machine)])
    (cond [(eq? os 'macosx) 'macosx]
          [(regexp-match #rx"Linux.*86" machine) 'linux86] ;; includes x86_64
          [else #f])))

#|
References:
linux (64):
  Linux Standard Base Core Specification 4.1
macosx (64):
  /usr/include/i386/_types.h: __darwin_socklen_t
  /usr/include/sys/socket.h: AF_UNIX
  /usr/include/sys/un.h: struct sockaddr_un
|#

(define AF_UNIX 1)
(define SOCK_STREAM 1)

(define _socklen_t
  (case platform
    ((linux86) _uint)    ;; in practice, _uint32
    ((macosx) _uint32)))

(define-cstruct _linux_sockaddr_un
  ([sun_family _ushort]
   [sun_path   (make-array-type _byte 108)]))

(define-cstruct _macosx_sockaddr_un
  ([sun_len    _ubyte]
   [sun_family _ubyte]
   [sun_path   (make-array-type _byte 104)]))

(define-ffi-definer define-libc (ffi-lib #f)
  #:default-make-fail make-not-available)

(define-libc socket
  (_fun #:save-errno 'posix
        _int _int _int -> _int))
(define-libc connect
  (_fun #:save-errno 'posix
        _int
        (case platform
          ((linux86) _linux_sockaddr_un-pointer)
          ((macosx)  _macosx_sockaddr_un-pointer)
          (else _pointer)) ;; dummy type to avoid error
        _int
        -> _int))
(define-libc close
  (_fun #:save-errno 'posix
        _int -> _int))
(define-libc scheme_make_fd_output_port
  (_fun _int _racket _bool _bool _bool -> _scheme))

;; make-sockaddr : bytes -> (U _linux_sockaddr_un _macosx_sockaddr_un)
(define (make-sockaddr path)
  (case platform
    ((linux86)
     (make-linux_sockaddr_un AF_UNIX path))
    ((macosx)
     (make-macosx_sockaddr_un (bytes-length path) AF_UNIX path))
    (else (error 'make-sockaddr "not available"))))

(define strerror_r
  (get-ffi-obj (case platform
                 ((linux86) "__xpg_strerror_r")
                 (else "strerror_r"))
               #f
               (_fun (errno) ::
                     (errno : _int)
                     (buf : _bytes = (make-bytes 1000))
                     (buf-len : _uintptr #| size_t |# = (bytes-length buf))
                     -> _void
                     -> (cast buf _bytes _string))
               (lambda () (lambda (errno) #f))))

;; ============================================================

(define unix-socket-available? (and platform #t))

;; unix-socket-connect : path-string -> input-port output-port
;; Connects to the unix domain socket associated with the given path.
(define (unix-socket-connect path0)
  (unless (path-string? path0)
    (raise-argument-error 'unix-socket-connect "path-string?" path0))
  (unless platform
    (error 'unix-socket-connect "unix domain sockets are not supported on this platform"))
  (security-guard-check-file 'unix-socket-connect path0 '(read write))
  (define clean-path (cleanse-path (path->complete-path path0)))
  (define path-b (path->bytes clean-path))
  (unless (< (bytes-length path-b) 100)
    (error* 'unix-socket-connect
            "complete path must be less than 100 bytes"
            '("path" value) path0
            '("complete path" value) clean-path))
  (define s (socket AF_UNIX SOCK_STREAM 0))
  (unless (positive? s)
    (let ([errno (saved-errno)])
      (error* 'unix-socket-connect
              "failed to create socket"
              "errno" errno
              '("error" maybe) (strerror_r errno))))
  (define addr (make-sockaddr path-b))
  (define addrlen (+ (ctype-sizeof _ushort) (bytes-length path-b)))
  (define ce (connect s addr addrlen))
  (unless (zero? ce)
    (close s)
    (let ([errno (saved-errno)])
      (error* 'unix-socket-connect
              "failed to connect socket"
              '("path" value) path0
              "errno" errno
              '("error" maybe) (strerror_r errno))))
  (with-handlers ([(lambda (e) #t)
                   (lambda (e)
                     (close s)
                     (raise e))])
    (scheme_make_fd_output_port s 'socket #f #f #t)))
