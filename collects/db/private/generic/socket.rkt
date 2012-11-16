#lang racket/base
(require ffi/unsafe
         ffi/file)
(provide unix-socket-connect)

;; The solaris code is untested (and thus disabled).

;; unix-socket-connect : pathlike -> input-port output-port
;; Connects to the unix domain socket associated with the given path.
(define (unix-socket-connect path0)
  (unless (path-string? path0)
    (raise-argument-error 'unix-socket-connect "path-string?" path0))
  (security-guard-check-file 'unix-socket-connect path0 '(read write))
  (let* ([path* (cleanse-path (path->complete-path path0))]
         [path-b (path->bytes path*)])
    (unless (< (bytes-length path-b) 100)
      (error 'unix-socket-connect
             "expected path of less than 100 bytes, got ~e" path*))
    (define s (make-socket))
    (unless (positive? s)
      (error 'unix-socket-connect "failed to create socket"))
    (define addr (make-unix-sockaddr path-b))
    (define addrlen (+ (ctype-sizeof _short) (bytes-length path-b)))
    (define ce (_connect s addr addrlen))
    (unless (zero? ce)
      (_close s)
      (error 'unix-socket-connect "failed to connect socket to path: ~s" path0))
    (with-handlers ([(lambda (e) #t)
                     (lambda (e)
                       (_close s)
                       (raise e))])
      (_make_fd_output_port s 'socket #f #f #t))))

(define platform
  (let ([os (system-type 'os)]
        [machine (system-type 'machine)])
    (cond [(eq? os 'macosx) 'macosx]
          [(regexp-match #rx"Linux.*86" machine) 'linux86]
          [(regexp-match #rx"SunOS" machine) #f #;'solaris]
          [else #f])))

(define _socklen_t _uint)
(define _size_t _int)

(define AF_UNIX 1)
(define SOCK_STREAM
  (case platform
    ((linux86 macosx) 1)
    ((solaris) 2)
    (else #f)))

(define (make-socket)
  (unless (and AF_UNIX SOCK_STREAM)
    (error 'unix-socket-connect "unix-domain sockets not supported on this platform"))
  (_socket AF_UNIX SOCK_STREAM 0))

(define _sockaddr_un_path_part
  (case platform
    ((linux86 solaris)
     (make-cstruct-type (build-list 108 (lambda (i) _byte))))
    ((macosx)
     (make-cstruct-type (build-list 104 (lambda (i) _byte))))
    (else
     ;; kluge: so that later definitions go through.
     _int)))

(define-cstruct _sockaddr_un
  ([sun_family _short]
   [sun_path   _sockaddr_un_path_part]))

(define-cstruct _macosx_sockaddr_un
  ([sun_len    _ubyte]
   [sun_family _ubyte]
   [sun_path   _sockaddr_un_path_part]))

(define (ffi name type)
  (case platform
    ((linux86 solaris macosx)
     (get-ffi-obj name #f type (lambda () #f)))
    (else
     (lambda _ (error name "not supported")))))

(define _socket
  (ffi "socket" (_fun _int _int _int -> _int)))
(define _connect
  (ffi "connect"
       (case platform
         ((linux86 solaris)
          (_fun _int _sockaddr_un-pointer _int -> _int))
         ((macosx)
          (_fun _int _macosx_sockaddr_un-pointer _int -> _int)))))
(define _setsockopt
  (ffi "setsockopt" (_fun _int _int _int _pointer _socklen_t -> _int)))
(define _close
  (ffi "close" (_fun _int -> _int)))
(define _make_fd_output_port
  (ffi "scheme_make_fd_output_port"
       (_fun _int _scheme _bool _bool _bool -> _scheme)))

(define (make-unix-sockaddr path)
  (case platform
    ((linux86 solaris)
     (make-sockaddr_un AF_UNIX path))
    ((macosx)
     (make-macosx_sockaddr_un (bytes-length path) AF_UNIX path))))
