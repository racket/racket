#lang racket/base
(require racket/file
         racket/contract/base
         "private/strip-prefix.rkt")

(provide
 (contract-out
  [untar (->* ((or/c path-string? input-port?))
              (#:dest 
               (or/c #f path-string?)
               #:strip-count exact-nonnegative-integer?
               #:filter (path? (or/c path? #f)
                               symbol? exact-integer? (or/c path? #f)
                               exact-nonnegative-integer? exact-nonnegative-integer?
                               . -> . any/c))
              void?)]))

(define (untar in 
               #:dest [dest #f]
               #:strip-count [strip-count 0]
               #:filter [filter void])
  ((if (input-port? in)
       (lambda (in f) (f in))
       call-with-input-file*)
   in
   (lambda (in)
     (let loop ([delays null])
       (define bstr (peek-bytes 512 0 in))
       (if (for/and ([b (in-bytes bstr)]) (zero? b))
           (for ([delay (in-list (reverse delays))])
             (delay))
           (loop (untar-one-from-port in delays
                                      dest strip-count filter)))))))

(define (read-bytes* n in)
  (define s (read-bytes n in))
  (unless (and (bytes? s)
               (= n (bytes-length s)))
    (error 'untar "unexpected EOF"))
  s)

(define (untar-one-from-port in delays
                             dest strip-count filter)
  (define name-bytes (read-bytes* 100 in))
  (define mode (tar-bytes->number (read-bytes* 8 in) in))
  (define owner (tar-bytes->number (read-bytes* 8 in) in))
  (define group (tar-bytes->number (read-bytes* 8 in) in))
  (define size (tar-bytes->number (read-bytes* 12 in) in))
  (define mod-time (tar-bytes->number (read-bytes* 12 in) in))
  (define checksum-bytes (read-bytes* 8 in))
  (define type (case (integer->char (read-byte in))
                 [(#\0) 'file]
                 [(#\1) 'hard-link]
                 [(#\2) 'link]
                 [(#\3) 'character-special]
                 [(#\4) 'block-special]
                 [(#\5) 'dir]
                 [(#\6) 'fifo]
                 [(#\7) 'contiguous-file]
                 [(#\g) 'extended-header]
                 [(#\x) 'extended-header-for-next]
                 [else 'unknown]))
  (define link-target-bytes (read-bytes* 100 in))
  (define ustar? (bytes=? #"ustar\00000" (read-bytes* 8 in)))
  (define owner-bytes (read-bytes* 32 in))
  (define group-bytes (read-bytes* 32 in))
  (define device-major-bytes (read-bytes* 8 in))
  (define device-minor-bytes (read-bytes* 8 in))
  (define filename-prefix-bytes (read-bytes* 155 in))
  (define base-filename (bytes->path
                         (if ustar?
                             (bytes-append (nul-terminated filename-prefix-bytes)
                                           (nul-terminated name-bytes))
                             (nul-terminated name-bytes))))
  (when (absolute-path? base-filename)
    (error 'untar "won't extract a file with an absolute path: ~e" base-filename))
  (define stripped-filename (strip-prefix base-filename strip-count))
  (define filename (and stripped-filename
                        (if dest
                            (build-path dest stripped-filename)
                            stripped-filename)))
  (define link-target (and (eq? type 'link)
                           (bytes->path (nul-terminated link-target-bytes))))
  (read-bytes* 12 in) ; padding
  (define create?
    (filter base-filename filename type size link-target mod-time mode))
  (define total-len (* (ceiling (/ size 512)) 512))
  (cond
   [(and filename create?)
    (case type
      [(dir) 
       (make-directory* filename)
       (cons
        ;; delay directory meta-data updates until after any contained
        ;; files are written
        (lambda ()
          (try-file-op
           (lambda ()
             (file-or-directory-permissions* filename mode #t)))
          (try-file-op
           (lambda ()
             (file-or-directory-modify-seconds* filename mod-time #t))))
        delays)]
      [(file)
       (define-values (base name dir?) (split-path filename))
       (make-directory* base)
       (call-with-output-file*
        filename
        #:exists 'truncate
        (lambda (out)
          (copy-bytes size in out)))
       (try-file-op
        (lambda ()
          (file-or-directory-permissions* filename mode #f)))
       (try-file-op
        (lambda ()
          (file-or-directory-modify-seconds* filename mod-time #f)))
       (copy-bytes (- total-len size) in #f)
       delays]
      [(link)
       (define-values (base name dir?) (split-path filename))
       (make-directory* base)
       (when (file-exists? filename) (delete-file filename))
       (make-file-or-directory-link link-target filename)
       delays]
      [else
       (error 'untar "cannot handle block type: ~a" type)])]
   [else
    (copy-bytes total-len in #f)
    delays]))

(define (copy-bytes amt in out)
  (let ([bstr (make-bytes (min amt 4096))])
    (let loop ([amt amt])
      (unless (zero? amt)
        (define size (min amt 4096))
        (unless (= (read-bytes! bstr in 0 size) size)
          (error 'untar "unexpected EOF"))
        (when out
          (write-bytes bstr out 0 size))
        (loop (- amt size))))))

(define (tar-bytes->number bstr in)
  (define len (bytes-length bstr))
  (cond
   [(bitwise-bit-set? (bytes-ref bstr 0) 7)
    ;; base-256:
    (for/fold ([v 0]) ([i (in-range 1 len)])
      (+ (* v 256) v))]
   [else
    ;; traditional:
    (define skip-tail
      (- len
         (for/or ([i (in-range len 0 -1)])
           (case (integer->char (bytes-ref bstr (sub1 i)))
             [(#\space #\nul) #f]
             [else i]))))
    (for/fold ([v 0]) ([i (in-range (- len skip-tail))])
      (define b (bytes-ref bstr i))
      (if (<= (char->integer #\0) b (char->integer #\7))
          (+ (* v 8) (- b (char->integer #\0)))
          (error 'untar "bad number ~e at ~a" bstr (file-position in))))]))

(define (nul-terminated bstr)
  (subbytes bstr
            0
            (or (for/or ([i (in-range (bytes-length bstr))])
                  (and (zero? (bytes-ref bstr i))
                       i))
                (bytes-length bstr))))

(define (try-file-op thunk)
  (with-handlers ([exn:fail:filesystem?
                   (lambda (exn)
                     (log-error "untar: ~a" (exn-message exn))
                     (void))])
    (thunk)))

(define (file-or-directory-modify-seconds* filename mod-time dir?)
  (unless (and dir? (eq? (system-type) 'windows))
    (file-or-directory-modify-seconds filename mod-time)))

(define (file-or-directory-permissions* file perms dir?)
  (file-or-directory-permissions file
                                 (case (system-type)
                                   [(windows)
                                    ;; corce perms to be the same for user, group, and others
                                    (define user-perms (bitwise-and #o700))
                                    (bitwise-ior user-perms
                                                 (arithmetic-shift user-perms -3)
                                                 (arithmetic-shift user-perms -6))]
                                   [else perms])))

