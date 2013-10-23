#lang racket/base

;; Support for connecting to UNIX domain sockets.

#|
References:
linux (64):
  Linux Standard Base Core Specification 4.1
macosx (64):
  /usr/include/i386/_types.h: __darwin_socklen_t
  /usr/include/sys/socket.h: AF_UNIX
  /usr/include/sys/un.h: struct sockaddr_un
|#

(require racket/contract
         (rename-in ffi/unsafe (-> -->))
         ffi/unsafe/define
         ffi/file
         unstable/error)

(provide
 (contract-out
  [unix-socket-available?
   boolean?]
  [unix-socket-connect
   (-> unix-socket-path? (values input-port? output-port?))]
  [unix-socket-path?
   (-> any/c boolean?)]))


;; Data structures and error handling code differs between the platforms.
(define platform
  (cond
    [(eq? (system-type 'os) 'macosx)
     'macosx]
    [(regexp-match? #rx"^Linux" (system-type 'machine))
     'linux]
    [else
     #f]))


(define unix-socket-available?
  (and platform #t))


(define AF-UNIX 1)
(define SOCK-STREAM 1)

(define UNIX-PATH-MAX
  (case platform
    [(linux) 108]
    [else    104]))

(define _socklen_t
  (case platform
    [(linux) _uint]
    [else    _uint32]))

(define-cstruct _linux_sockaddr_un
  ([sun_family _ushort]
   [sun_path   (make-array-type _byte UNIX-PATH-MAX)]))

(define-cstruct _macosx_sockaddr_un
  ([sun_len    _ubyte]
   [sun_family _ubyte]
   [sun_path   (make-array-type _byte UNIX-PATH-MAX)]))

(define _sockaddr_un-pointer
  (case platform
    [(linux)  _linux_sockaddr_un-pointer]
    [(macosx) _macosx_sockaddr_un-pointer]
    [else     _pointer]))


(define-ffi-definer define-libc (ffi-lib #f)
  #:default-make-fail make-not-available)

(define-libc socket
  (_fun #:save-errno 'posix
        _int _int _int --> _int))

(define-libc connect
  (_fun #:save-errno 'posix
        _int _sockaddr_un-pointer _int --> _int))

(define-libc close
  (_fun #:save-errno 'posix
        _int --> _int))

(define-libc scheme_make_fd_output_port
  (_fun _int _racket _bool _bool _bool --> _racket))

(define strerror-name
  (case platform
    [(linux) "__xpg_strerror_r"]
    [else    "strerror_r"]))

(define strerror_r
  (get-ffi-obj strerror-name #f
               (_fun (errno) ::
                     (errno : _int)
                     (buf : _bytes = (make-bytes 1000))
                     (buf-len : _size = (bytes-length buf))
                     --> _void
                     --> (cast buf _bytes _string/locale))
               (lambda ()
                 (lambda (errno) #f))))


(define (unix-socket-path? v)
  (and (unix-socket-path->bytes v) #t))

(define (unix-socket-path->bytes path)
  (if (path-string? path)
      ;; On all platforms, normal path of up to UNIX-PATH-MAX bytes after
      ;; conversion to absolute is considered valid and shall be accepted.
      (let ([bstr (path->bytes (cleanse-path (path->complete-path path)))])
        (and (<= (bytes-length bstr) UNIX-PATH-MAX) bstr))

      ;; On Linux, paths may be in so-called abstract namespace where they
      ;; start with #\nul and do not have a corresponding socket file.
      ;; We accept such paths only as byte strings because we don't know
      ;; the correct encoding.
      (and (eq? platform 'linux)
           (bytes? path)
           (> (bytes-length path) 0)
           (<= (bytes-length path) UNIX-PATH-MAX)
           (= (bytes-ref path 0) 0)
           path)))


(define (make-sockaddr path-bytes)
  (case platform
    [(linux)
     (make-linux_sockaddr_un AF-UNIX path-bytes)]
    [(macosx)
     (make-macosx_sockaddr_un (bytes-length path-bytes) AF-UNIX path-bytes)]))


(define (unix-socket-connect path)
  (unless platform
    (error* 'unix-socket-connect
            "unix domain sockets are not supported on this platform"))

  (when (path-string? path)
    (security-guard-check-file 'unix-socket-connect path '(read write)))

  (let* ([path-bytes (unix-socket-path->bytes path)]
         [sockaddr   (make-sockaddr path-bytes)]
         [addrlen    (+ (ctype-sizeof _ushort) (bytes-length path-bytes))]
         [socket-fd  (socket AF-UNIX SOCK-STREAM 0)])
    (unless (positive? socket-fd)
      (let ([errno (saved-errno)])
        (error* 'unix-socket-connect
                "failed to create socket"
                "errno" errno
                '("error" maybe) (strerror_r errno))))

    (unless (zero? (connect socket-fd sockaddr addrlen))
      (close socket-fd)
      (let ([errno (saved-errno)])
        (error* 'unix-socket-connect
                "failed to connect socket"
                '("path" value) path
                "errno" errno
                '("error" maybe) (strerror_r errno))))

    (with-handlers ([(lambda (e) #t)
                     (lambda (exn)
                       (close socket-fd)
                       (raise exn))])
      (scheme_make_fd_output_port socket-fd 'unix-socket #f #f #t))))
