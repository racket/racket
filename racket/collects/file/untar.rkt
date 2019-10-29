#lang racket/base
(require racket/file
         racket/contract/base
         racket/port
         "private/strip-prefix.rkt"
         "private/check-path.rkt")

(provide
 (contract-out
  [untar (->* ((or/c path-string? input-port?))
              (#:dest 
               (or/c #f path-string?)
               #:strip-count exact-nonnegative-integer?
               #:permissive? any/c
               #:filter (path? (or/c path? #f)
                               symbol? exact-integer? (or/c path? #f)
                               exact-nonnegative-integer? exact-nonnegative-integer?
                               . -> . any/c))
              void?)]))

(define-logger untar)

(define (untar in 
               #:dest [dest #f]
               #:strip-count [strip-count 0]
               #:permissive? [permissive? #f]
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
                                      dest strip-count filter
                                      permissive?
                                      #f
                                      #f)))))))

(define (read-bytes* n in)
  (define s (read-bytes n in))
  (unless (and (bytes? s)
               (= n (bytes-length s)))
    (error 'untar "unexpected EOF"))
  s)

(define (untar-one-from-port in delays
                             dest strip-count filter
                             permissive?
                             path-from-extended-attributes
                             link-target-from-extended-attributes)
  (define name-bytes (read-bytes* 100 in))
  (define mode (tar-bytes->number (read-bytes* 8 in) in))
  (define owner (tar-bytes->number (read-bytes* 8 in) in #:allow-zeros? #t))
  (define group (tar-bytes->number (read-bytes* 8 in) in #:allow-zeros? #t))
  (define size (tar-bytes->number (read-bytes* 12 in) in))
  (define mod-time (tar-bytes->number (read-bytes* 12 in) in))
  (define checksum-bytes (read-bytes* 8 in))
  (define type-byte (integer->char (read-byte in)))
  (define type (case type-byte
                 [(#\0 #\nul) 'file]
                 [(#\1) 'hard-link]
                 [(#\2) 'link]
                 [(#\3) 'character-special]
                 [(#\4) 'block-special]
                 [(#\5) 'dir]
                 [(#\6) 'fifo]
                 [(#\7) 'contiguous-file]
                 [(#\g) 'extended-header]
                 [(#\x) 'extended-header-for-next]
                 [(#\L) 'gnu-long-name]
                 [(#\K) 'gnu-long-link]
                 [else 'unknown]))
  (define link-target-bytes (read-bytes* 100 in))
  (define ustar? (bytes=? #"ustar\00000" (read-bytes* 8 in)))
  (define owner-bytes (read-bytes* 32 in))
  (define group-bytes (read-bytes* 32 in))
  (define device-major-bytes (read-bytes* 8 in))
  (define device-minor-bytes (read-bytes* 8 in))
  (define filename-prefix-bytes (read-bytes* 155 in))
  (define base-filename (or path-from-extended-attributes
                            (bytes->path
                             (let ([name (nul-terminated name-bytes)])
                               (if ustar?
                                   (let ([prefix (nul-terminated filename-prefix-bytes)])
                                     (if (zero? (bytes-length prefix))
                                         name
                                         (bytes-append prefix #"/" name)))
                                   name)))))
  (check-unpack-path 'untar base-filename #:allow-up? permissive?)
  (define stripped-filename (strip-prefix base-filename strip-count))
  (define filename (and stripped-filename
                        (if dest
                            (build-path dest stripped-filename)
                            stripped-filename)))
  (define link-target (and (eq? type 'link)
                           (or link-target-from-extended-attributes
                               (bytes->path (nul-terminated link-target-bytes)))))
  (when (and link-target (not permissive?))
    (check-unpack-path 'untar link-target))
  (read-bytes* 12 in) ; padding
  (define create?
    (filter base-filename filename type size link-target mod-time mode))
  (define total-len (* (ceiling (/ size 512)) 512))
  (cond
   [(and filename create?)
    (case type
      [(dir)
       (log-untar-info "directory: ~a" filename)
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
       (log-untar-info "file: ~a" filename)
       (define-values (base name dir?) (split-path filename))
       (when (path? base) (make-directory* base))
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
       (log-untar-info "link: ~a" filename)
       (define-values (base name dir?) (split-path filename))
       (when (path? base) (make-directory* base))
       (when (file-exists? filename) (delete-file filename))
       (make-file-or-directory-link link-target filename)
       delays]
      [(extended-header-for-next)
       ;; pax record to support long namesand other attributes
       (define extended-header (read-pax in total-len))
       ;; Recur to use given paths, if any:
       (untar-one-from-port in delays
                            dest strip-count filter
                            permissive?
                            (or (let ([v (hash-ref extended-header 'path #f)])
                                  (and v (bytes->path v)))
                                path-from-extended-attributes)
                            (or (let ([v (hash-ref extended-header 'linkpath #f)])
                                  (and v (bytes->path v)))
                                link-target-from-extended-attributes))]
      [(gnu-long-name)
       ;; GNU record to support long names
       (define o (open-output-bytes))
       (copy-bytes total-len in o)
       ;; Recur to use given path:
       (untar-one-from-port in delays
                            dest strip-count filter
                            permissive?
                            (bytes->path (nul-terminated (get-output-bytes o)))
                            link-target-from-extended-attributes)]
      [(gnu-long-link)
       ;; GNU record to support long link targets
       (define o (open-output-bytes))
       (copy-bytes total-len in o)
       ;; Recur to use given link target:
       (untar-one-from-port in delays
                            dest strip-count filter
                            permissive?
                            path-from-extended-attributes
                            (bytes->path (nul-terminated (get-output-bytes o))))]
      [else
       (log-untar-info "ignored ~a[~a]: ~a" type type-byte filename)
       (copy-bytes total-len in #f)
       delays])]
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

(define (tar-bytes->number bstr in #:allow-zeros? [allow-zeros? #f])
  (define len (bytes-length bstr))
  (cond
   [(bitwise-bit-set? (bytes-ref bstr 0) 7)
    ;; base-256:
    (for/fold ([v 0]) ([i (in-range 1 len)])
      (+ (* v 256) v))]
   [(and allow-zeros?
         (for/and ([b (in-bytes bstr)])
           (zero? b)))
    #f]
   [else
    ;; traditional:
    (define skip-head
      (or (for/or ([i (in-range len)])
            (case (integer->char (bytes-ref bstr i))
              [(#\space #\nul) #f]
              [else i]))
          (error 'untar "bad number ~e at ~a" bstr (file-position in))))
    (define skip-tail
      (- len
         (or (for/or ([i (in-range len 0 -1)])
               (case (integer->char (bytes-ref bstr (sub1 i)))
                 [(#\space #\nul) #f]
                 [else i]))
             (error 'untar "bad number ~e at ~a" bstr (file-position in)))))
    (for/fold ([v 0]) ([i (in-range skip-head (- len skip-tail))])
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
                     (log-untar-error (exn-message exn))
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

(define (read-pax in len)
  ;; Format of pax entries is sequence of "<num><space><key>=<value>\n"
  ;; where <num> is the length of that whole line, and the key and value
  ;; are UTF-8 encoded
  (define (finish len accum)
    (when (positive? len)
      (copy-bytes (sub1 len) in #f))
    accum)
  (let loop ([len len] [num-base 0] [digits 0] [accum #hash()])
    (define c (if (positive? len)
                  (read-byte in)
                  0))
    (cond
     [(eof-object? c) (finish len accum)]
     [(zero? c) (finish len accum)]
     [(char-numeric? (integer->char c))
      (loop (sub1 len) (+ (- c (char->integer #\0)) (* num-base 10)) (add1 digits) accum)]
     [(= c (char->integer #\space))
      (cond
       [((- num-base digits 1) . > . (sub1 len))
        ;; Can't read that far, so something has gone wrong
        accum]
       [else
        (define s (read-bytes (- num-base digits 1) in))
        (define m (regexp-match #rx#"^([^=]*)=(.*)\n$" s))
        (loop (- len (- num-base digits))
              0
              0
              (cond
               [(not m) accum]
               [else
                (hash-set accum
                          (string->symbol (bytes->string/utf-8 (cadr m) #\?))
                          ;; pax values are supposed to be UTF-8, but we'll
                          ;; convert raw bytes to a path; that arguably doesn't do
                          ;; the right thing if the source and destination
                          ;; systems use different path encodings, but it makes
                          ;; things work on systems where a path doesn't have to
                          ;; have a string encoding.
                          (caddr m))]))])]
     [else (finish len accum)])))
