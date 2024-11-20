#lang racket/base
(require "../host/thread.rkt"
         "../host/rktio.rkt"
         "host.rkt"
         "../format/main.rkt"
         "error.rkt")

(provide path-or-fd-stat)

(define (path-or-fd-stat who
                         #:host-path [host-path #f]
                         #:as-link? [as-link? #f] ; used only if `host-path`
                         #:fd [fd #f]
                         #:port [port #f]) ; for errors, and non-#f if `fd` provided
  (define r0 (if fd
                 (rktio_fd_stat rktio fd)
                 (rktio_file_or_directory_stat rktio host-path (not as-link?))))
  (define r (if (rktio-error? r0)
                r0
                (begin0
                  (rktio_stat_to_vector r0)
                  (rktio_free r0))))
  (end-atomic)
  (cond
    [(rktio-error? r0)
     (raise-filesystem-error who
                             r
                             (string-append
                              "cannot get stat result\n"
                              (if host-path
                                  (format "  path: ~a"
                                          (host-> host-path))
                                  "")))]
    [else
     ; The nanosecond struct fields are only the fractional seconds part, i. e.
     ; they're below 1_000_000_000. Thus combine them with the seconds parts to
     ; get the nanoseconds including the whole seconds.
     (define (combined-nanoseconds seconds-index)
       (+ (* #e1e9 (vector-ref r seconds-index))
          (vector-ref r (add1 seconds-index))))
     (define main-hash
       (hasheq 'device-id (vector-ref r 0)
               'inode (vector-ref r 1)
               'mode (vector-ref r 2)
               'hardlink-count (vector-ref r 3)
               'user-id (vector-ref r 4)
               'group-id (vector-ref r 5)
               'device-id-for-special-file (vector-ref r 6)
               'size (vector-ref r 7)
               'block-size (vector-ref r 8)
               'block-count (vector-ref r 9)
               'access-time-seconds (vector-ref r 10)
               'access-time-nanoseconds (combined-nanoseconds 10)
               'modify-time-seconds (vector-ref r 12)
               'modify-time-nanoseconds (combined-nanoseconds 12)))
     (define ctime-hash
       (if (vector-ref r 15)
           (hasheq 'change-time-seconds (vector-ref r 14)
                   'change-time-nanoseconds (combined-nanoseconds 14)
                   'creation-time-seconds 0
                   'creation-time-nanoseconds 0)
           (hasheq 'change-time-seconds 0
                   'change-time-nanoseconds 0
                   'creation-time-seconds (vector-ref r 14)
                   'creation-time-nanoseconds (combined-nanoseconds 14))))
     ; We can't use `hash-union` (from `racket/hash`) in the kernel code, so
     ; simulate the function.
     (for/fold ([new-hash main-hash])
               ([(key value) (in-hash ctime-hash)])
        (hash-set new-hash key value))]))
