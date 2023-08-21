#lang racket/base
(require racket/contract/base
         racket/port
         racket/file
         racket/date
         file/gunzip
         "private/strip-prefix.rkt"
         "private/check-path.rkt")

(provide
 (struct-out exn:fail:unzip:no-such-entry)

 (contract-out
  [unzip (((or/c path-string? input-port?))
          ((or/c (procedure-arity-includes/c 3) (procedure-arity-includes/c 4))
           ;; More precisely (but unimplementable):
           #;
           (or/c (bytes? boolean? input-port? (or/c #f exact-integer? hash?) . -> . any)
                 (bytes? boolean? input-port? . -> . any))
           #:preserve-attributes? any/c
           #:preserve-timestamps? any/c
           #:utc-timestamps? any/c
           #:must-unzip? any/c)
          . ->* . any)]
  
  [make-filesystem-entry-reader (() (#:dest 
                                     (or/c #f path-string?)
                                     #:strip-count
                                     exact-nonnegative-integer?
                                     #:permissive?
                                     any/c
                                     #:exists
                                     (or/c 'skip
                                           'error 'replace 'truncate 'truncate/replace 'append 'update
                                           'can-update 'must-truncate))
                                 . ->* .
                                 ((bytes? boolean? input-port?)
                                  ((or/c hash? #f exact-integer?))
                                  . ->* . any))]

  [read-zip-directory ((or/c path-string? input-port?) . -> . zip-directory?)]
  [zip-directory? (any/c . -> . boolean?)]
  [zip-directory-entries (zip-directory? . -> . (listof bytes?))]
  [zip-directory-contains? (zip-directory? (or/c path-string? bytes?) . -> . boolean?)]
  [zip-directory-includes-directory? (zip-directory? (or/c path-string? input-port?) . -> . boolean?)]
  [unzip-entry (((or/c path-string? input-port?) zip-directory? (or/c path-string? bytes?))
                ((or/c (procedure-arity-includes/c 2) (procedure-arity-includes/c 3))
                 #:preserve-attributes? any/c
                 #:preserve-timestamps? any/c
                 #:utc-timestamps? any/c)
                . ->* .
                any)]

  [call-with-unzip (((or/c path-string? input-port?)
                     (-> path-string? any))
                    (#:must-unzip? any/c)
                    . ->* . any)]
  [call-with-unzip-entry (-> (or/c path-string? input-port?)
                             path-string?
                             (-> path-string? any)
                             any)]

  [path->zip-path (path-string? . -> . bytes?)]))

;; ===========================================================================
;; CONSTANTS
;; ===========================================================================

(define *local-file-header*                      #x04034b50)
(define *archive-extra-record*                   #x08064b50)
(define *central-file-header*                    #x02014b50)
(define *digital-signature*                      #x05054b50)
(define *zip64-end-of-central-directory-record*  #x06064b50)
(define *zip64-end-of-central-directory-locator* #x07064b50)
(define *end-of-central-directory-record*        #x06054b50)

;; ===========================================================================
;; DATATYPES AND UTILITIES
;; ===========================================================================

(define-struct (exn:fail:unzip:no-such-entry exn:fail) (entry)
  #:guard (lambda (msg cm entry who)
            (unless (bytes? entry)
              (raise-argument-error who "bytes?" entry))
            (values msg cm entry)))

;; (alistof bytes zip-entry)
(define-struct zip-directory (contents))

;; nat * boolean
(define-struct zip-entry (offset dir? attributes))

(define (raise-unzip-error message)
  (error 'unzip "~a" message))
  
(define (raise-entry-not-found entry)
  (raise
   (make-exn:fail:unzip:no-such-entry
    (string->immutable-string
     (format "unzip: entry not found: \"~a\"" (bytes->string/latin-1 entry)))
    (current-continuation-marks)
    entry)))

;; zip-directory-entries : zip-directory -> (listof bytes)
(define (zip-directory-entries zipdir)
  (map car (zip-directory-contents zipdir)))

;; zip-directory-lookup : bytes zip-directory -> (option zip-entry)
(define (zip-directory-lookup entry zipdir)
  (let loop ([contents (zip-directory-contents zipdir)])
    (cond
     [(null? contents) #f]
     [(or (bytes=? entry (caar contents))
          (bytes=? (bytes-append entry #"/") (caar contents)))
      (cdar contents)]
     [else (loop (cdr contents))])))

;; zip-directory-contains? : zip-directory (union string path bytes) -> boolean
(define (zip-directory-contains? zipdir entry)
  (if (bytes? entry)
      (and (zip-directory-lookup entry zipdir) #t)
      (zip-directory-contains? zipdir (path->zip-path entry))))

;; matches-directory? : bytes bytes -> boolean
(define (bytes-prefix? dirname entry-name)
  (let ([dirname-len (bytes-length dirname)]
        [entry-name-len (bytes-length entry-name)])
    (and (>= entry-name-len dirname-len)
         (bytes=? (subbytes entry-name 0 dirname-len) dirname))))

;; zip-directory-includes-directory? : zip-directory (union string path bytes) -> boolean
(define (zip-directory-includes-directory? zipdir dirname)
  (if (bytes? dirname)
      (ormap (lambda (pair)
               (bytes-prefix? dirname (car pair)))
             (zip-directory-contents zipdir))
      (zip-directory-includes-directory? zipdir (path->zip-path dirname))))

;; path->zip-path : (union path string) -> bytes
(define (path->zip-path p)
  (let ([p (simplify-path p #f)])
    (if (path? p)
        (bytes->zip-bytes (path->bytes p))
        (bytes->zip-bytes (string->bytes/latin-1 p)))))

(define (bytes->zip-bytes b)
  (case (system-path-convention-type)
    [(windows) (regexp-replace* #rx#"\\\\" b #"/")]
    [else b]))

;; ===========================================================================
;; UNZIPPING ENGINE
;; ===========================================================================

(define *slash-byte* (char->integer #\/))

(define (directory-entry? name)
  (= (bytes-ref name (sub1 (bytes-length name))) *slash-byte*))

(define (read-integer count signed? in big-endian?)
  (define bstr (read-bytes count in))
  (unless (and (bytes? bstr) (= count (bytes-length bstr)))
    (error 'unzip "unexpected EOF"))
  (integer-bytes->integer bstr signed? big-endian?))

(define (peek-integer count signed? in big-endian?)
  (define bstr (peek-bytes count 0 in))
  (unless (and (bytes? bstr) (= count (bytes-length bstr)))
    (error 'unzip "unexpected EOF"))
  (integer-bytes->integer bstr signed? big-endian?))

(define (make-filter-input-port inflate orig-in)
  (define-values (in out) (make-pipe 4096))
  (define fail-exn #f)
  (values
   in
   (thread (lambda ()
             (with-handlers ([exn:fail? (lambda (exn) (set! fail-exn exn))])
               (inflate orig-in out))
             (close-output-port out)))
   (lambda () fail-exn)))

(define (skip-bytes amt in)
  (read-bytes amt in)
  (void))

;; unzip-one-entry : input-port (bytes boolean input-port [exact-integer?] -> a) .... -> a
(define (unzip-one-entry in read-entry ze preserve-attributes? preserve-timestamps? utc?)
  (let ([read-int (lambda (count) (read-integer count #f in #f))])
    (let* ([signature            (read-int 4)]
           [version              (read-bytes 2 in)]
           [bits                 (read-int 2)]
           [compression          (read-int 2)]
           [time                 (read-int 2)]
           [date                 (read-int 2)]
           [crc-32               (read-int 4)]
           [compressed           (read-int 4)]
           [uncompressed         (read-int 4)]
           [filename-length      (read-int 2)]
           [extra-length         (read-int 2)]
           [filename             (read-bytes filename-length in)]
           [extra                (read-bytes extra-length in)])
      (let* ([mark (file-position in)]
             [dir? (directory-entry? filename)]
             ;; appnote VI-J : if bit 3 is set, the fields crc-32,
             ;; compressed size, and uncompressed size are set to
             ;; zero in the local header
             [in0 (if (bitwise-bit-set? bits 3)
                      in
                      (make-limited-input-port in compressed #f))])
        (define maybe-post-thunk
          (let ()
            (define-values (in t get-filter-exn)
              (if (zero? compression)
                  (values in0 #f (lambda () #f))
                  (make-filter-input-port inflate in0)))

            (define read-exn-or-post-thunk
              (with-handlers ([exn:fail? (lambda (exn) (list exn))])
                (define maybe-post-thunk
                  (if (or preserve-attributes?
                          preserve-timestamps?)
                      (read-entry filename dir? in
                                  (let ([ts (and (or preserve-attributes?
                                                     (not dir?))
                                                 (msdos-date+time->seconds date time utc?))])
                                    (if preserve-attributes?
                                        (let ([attrs (zip-entry-attributes ze)])
                                          (if ts
                                              (hash-set attrs 'timestamp ts)
                                              attrs))
                                        ts)))
                      (read-entry filename dir? in)))

                (when preserve-attributes?
                  (unless (or (not maybe-post-thunk)
                              (and (procedure? maybe-post-thunk)
                                   (procedure-arity-includes? maybe-post-thunk 0)))
                    (raise-result-error 'entry-reader "(or/c #f (-> any))" maybe-post-thunk)))

                ;; Read until the end of the deflated stream when compressed size unknown
                (when (bitwise-bit-set? bits 3)
                  (let loop () (unless (eof-object? (read-bytes 1024 in)) (loop))))

                (and preserve-attributes?
                     maybe-post-thunk)))

            (when t (kill-thread t))

            (define either-exn (or (get-filter-exn)
                                   (and (pair? read-exn-or-post-thunk)
                                        (car read-exn-or-post-thunk))))
            (when either-exn (raise either-exn))

            (if preserve-attributes?
                read-exn-or-post-thunk
                (void))))

        ;; appnote VI-C : if bit 3 is set, then the file data
        ;; is immediately followed by a data descriptor
        ;; appnote 4.3.9.3 : the value 0x08074b50 may appear
        ;; as a data descriptor signature immediately
        ;; following the file data
        (if (bitwise-bit-set? bits 3)
            ;; Read possibly signed data descriptor
            (let ([maybe-signature (read-int 4)])
              (skip-bytes (if (= maybe-signature #x08074b50) 12 8)
                          in))
            (skip-bytes (- (+ mark compressed) (file-position in)) in))

        maybe-post-thunk))))

;; find-central-directory : input-port nat -> nat nat nat
(define (find-central-directory in size)
  (let loop ([pos (- size 18)])
    (unless (positive? pos)
      (raise-unzip-error "no central directory"))
    (file-position in pos)
    (let* ([read-int (lambda (count) (read-integer count #f in #f))]
           [signature (read-int 4)])
      (if (= signature *end-of-central-directory-record*)
          (let ([disk-number       (read-int 2)]
                [directory-disk    (read-int 2)]
                [disk-entries      (read-int 2)]
                [entry-count       (read-int 2)]
                [directory-length  (read-int 4)]
                [directory-offset  (read-int 4)]
                [comment-length    (read-int 2)])
            (if (= (- size (file-position in)) comment-length)
                (values directory-offset directory-length entry-count)
                (loop (sub1 pos))))
          (loop (sub1 pos))))))

;; read-central-directory : input-port nat -> (alistof bytes zip-entry)
(define (read-central-directory in size)
  (let-values ([(offset length count) (find-central-directory in size)])
    (file-position in offset)
    (build-list count
                (lambda (i)
                  (let* ([read-int (lambda (count)
                                     (read-integer count #f in #f))]
                         [signature (read-int 4)])
                    (unless (= signature *central-file-header*)
                      (raise-unzip-error
                       (format "bad central file header signature: ~a"
                               signature)))
                    (let ([version             (read-int 2)]
                          [required            (read-int 2)]
                          [bits                (read-int 2)]
                          [compression         (read-int 2)]
                          [time                (read-int 2)]
                          [date                (read-int 2)]
                          [crc-32              (read-int 4)]
                          [compressed          (read-int 4)]
                          [uncompressed        (read-int 4)]
                          [filename-length     (read-int 2)]
                          [extra-length        (read-int 2)]
                          [comment-length      (read-int 2)]
                          [disk-number         (read-int 2)]
                          [internal-attributes (read-int 2)]
                          [external-attributes (read-int 4)]
                          [relative-offset     (read-int 4)])
                      (let* ([filename (read-bytes filename-length in)]
                             [dir? (directory-entry? filename)])
                        (skip-bytes (+ extra-length comment-length) in)
                        (let* ([attribs (case (arithmetic-shift version -8)
                                          [(3 19) ; Unix and (for historical reasons) Mac OS X
                                           (hasheq 'permissions
                                                   (arithmetic-shift external-attributes -16))]
                                          [(0) ; Windows
                                           (hasheq 'permissions
                                                   (if (zero? (bitwise-and external-attributes #x01))
                                                       #o777    ; read+write
                                                       #o555))] ; read only
                                          [else #hasheq()])])
                          (cons filename (make-zip-entry relative-offset dir? attribs))))))))))

(define (msdos-date+time->seconds date time utc?)
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (find-seconds (* 2 (bitwise-and #x1F time))
                  (bitwise-and #x3F (arithmetic-shift time -5))
                  (bitwise-and #x1F (arithmetic-shift time -11))
                  (bitwise-and #x1F date)
                  (bitwise-and #xF (arithmetic-shift date -5))
                  (+ (bitwise-and #x7F (arithmetic-shift date -9)) 1980)
                  (not utc?))))

;; ===========================================================================
;; FRONT END
;; ===========================================================================

(define (call-with-input in proc)
  ((if (input-port? in)
       (lambda (in f) (f in))
       call-with-input-file*)
   in
   proc))

;; unzip : [(or/c path-string? input-port) (bytes boolean input-port -> any)] -> any
(define unzip
  (lambda (orig-in [read-entry (make-filesystem-entry-reader)]
                   #:must-unzip? [must-unzip? #t]
                   #:preserve-attributes? [preserve-attributes? #f]
                   #:preserve-timestamps? [preserve-timestamps? #f]
                   #:utc-timestamps? [utc? #f])
    (call-with-input
     orig-in
     (lambda (in)
       (define tag (peek-integer 4 #f in #f))
       (cond
         [(memv tag (list *local-file-header*
                          *archive-extra-record*
                          *central-file-header*
                          *digital-signature*
                          *zip64-end-of-central-directory-record*
                          *zip64-end-of-central-directory-locator*
                          *end-of-central-directory-record*))
          (cond
            [(not preserve-attributes?)
             ;; Streaming way: doesn't really do the right thing, since
             ;; zip unpacking is supposed to work from the central
             ;; directory in case there are dead files in the archive.
             ;; Also, this way doesn't support using permissions from
             ;; the central directory. But it works on any kind of port.
             (let loop ()
               (define tag (peek-integer 4 #f in #f))
               (cond
                 [(eqv? tag *local-file-header*)
                  (unzip-one-entry in read-entry #f #f preserve-timestamps? utc?)
                  (loop)]
                 [(memv tag (list *archive-extra-record*
                                  *central-file-header*
                                  *digital-signature*
                                  *zip64-end-of-central-directory-record*
                                  *zip64-end-of-central-directory-locator*
                                  *end-of-central-directory-record*))
                  (void)]
                 [must-unzip?
                  (error 'unzip "unexpected input in archive\n  input: ~e" orig-in)]
                 [else (void)]))]
            [else
             ;; Need to be able to use `file-position` to jump around
             (define entries (read-central-directory in (input-size in)))
             (define rev-todos
               (for/fold ([rev-todos null]) ([e (in-list entries)])
                 (define ze (cdr e))
                 (file-position in (zip-entry-offset ze))
                 (define tag (peek-integer 4 #f in #f))
                 (unless (= tag *local-file-header*)
                   (error 'unzip "expected a file entry\n  input: ~e" orig-in))
                 (define todo
                   (unzip-one-entry in read-entry ze preserve-attributes? preserve-timestamps? utc?))
                 (if (and preserve-attributes?
                          todo)
                     (cons todo rev-todos)
                     rev-todos)))

             (for ([todo (in-list (reverse rev-todos))])
               (todo))])]
         [must-unzip?
          (error 'unzip "input does not appear to be an archive\n  input: ~e" orig-in)]
         [else (void)])))))

(define (input-size in)
  (file-position in eof)
  (begin0
   (file-position in)
   (file-position in 0)))

;; read-zip-directory : (union string path) -> zip-directory
(define (read-zip-directory in)
  (make-zip-directory
   (call-with-input
    in
    (lambda (in)
      (read-central-directory in
                              (input-size in))))))

;; unzip-entry : (union string path) zip-directory bytes [(bytes boolean input-port -> a)] -> a
(define unzip-entry
  (lambda (in dir entry-name [read-entry (make-filesystem-entry-reader)]
              #:preserve-attributes? [preserve-attributes? #f]
              #:preserve-timestamps? [preserve-timestamps? #f]
              #:utc-timestamps? [utc? #f])
    (cond
     [(zip-directory-lookup (if (bytes? entry-name) entry-name (path->zip-path entry-name)) dir)
      => (lambda (entry)
           (call-with-input
            in
            (lambda (in)
              (file-position in (zip-entry-offset entry))
              (unzip-one-entry in read-entry entry preserve-attributes? preserve-timestamps? utc?))))]
     [else (raise-entry-not-found entry-name)])))

;; ===========================================================================
;; ENTRY PARSERS
;; ===========================================================================

;; make-filesystem-entry-reader : [output-flag] -> (bytes boolean input-port -> any)
(define make-filesystem-entry-reader
  (lambda (#:dest [dest-dir #f] #:strip-count [strip-count 0] #:permissive? [permissive? #f] #:exists [flag 'error])
    (lambda (name dir? in [timestamp-or-attrs #f])
      (define path (bytes->path name))
      (check-unpack-path 'unzip path #:allow-up? permissive?)
      (let* ([base-path (strip-prefix path strip-count)]
             [path (and base-path
                        (if dest-dir
                            (build-path dest-dir base-path)
                            base-path))])
        (define (windows-adjust bits)
          (if (eq? (system-type) 'windows)
              (if (zero? (bitwise-and #o200 bits))
                  #o555  ; read only
                  #o777) ; read+write
              bits))
        (define (no-post-action) (if (hash? timestamp-or-attrs) #f (void)))
        (cond
          [path
           (cond
             [dir?
              (cond
                [(directory-exists? path)
                 (no-post-action)]
                [else
                 (make-directory* path)
                 ;; maybe post thunk for directory permission
                 (cond
                   [(eq? 'windows (system-type))
                    ;; can't set directory modify time on Windows, and read-only
                    ;; doesn't mean the same thing there on directories
                    (no-post-action)]
                   [else
                    (let ([permissions (and (hash? timestamp-or-attrs)
                                            (hash-ref timestamp-or-attrs 'permissions #f))]
                          ;; we only try to set a timestamp in attributes mode,
                          ;; since it needs to be done via a post action
                          [timestamp (and (hash? timestamp-or-attrs)
                                          (hash-ref timestamp-or-attrs 'timestamp #f))])
                      (or (and (or permissions
                                   timestamp)
                               (lambda ()
                                 (when timestamp
                                   (file-or-directory-modify-seconds path timestamp))
                                 (when permissions
                                   (file-or-directory-permissions path (windows-adjust permissions)))))
                          (no-post-action)))])])]
             [else
              (let ([parent (dirname path)])
                (unless (directory-exists? parent)
                  (make-directory* parent))
                (unless (and (eq? flag 'skip)
                             (file-exists? path))
                  (with-output-to-file path
                    #:exists flag
                    (lambda ()
                      (copy-port in (current-output-port))))
                  (let ([timestamp (cond
                                     [(exact-integer? timestamp-or-attrs)
                                      timestamp-or-attrs]
                                     [(hash? timestamp-or-attrs)
                                      (hash-ref timestamp-or-attrs 'timestamp #f)]
                                     [else #f])])
                    (when timestamp
                      (file-or-directory-modify-seconds path timestamp))
                    (let ([permissions (and (hash? timestamp-or-attrs)
                                            (hash-ref timestamp-or-attrs 'permissions #f))])
                      (when permissions
                        (file-or-directory-permissions path (windows-adjust permissions)))))))
              (no-post-action)])]
          [else (no-post-action)])))))

(define (dirname p)
  (define-values (base name dir?) (split-path p))
  (if (path? base)
      base
      (current-directory)))

(define (call-with-unzip-entry zip-file entry-file user-proc)
  (let ([temp-dir #f])
    (dynamic-wind
        (lambda ()
          (set! temp-dir (make-temporary-file "ziptmp~a" 'directory)))
        (lambda ()
          (let ([directory-entries (read-zip-directory zip-file)])
            (unzip-entry zip-file
                         directory-entries 
                         (path->zip-path entry-file) 
                         (make-filesystem-entry-reader #:dest temp-dir #:exists 'replace))
            (user-proc (build-path temp-dir entry-file))))
        (lambda ()
          (delete-directory/files temp-dir)))))

(define (call-with-unzip zip-file user-proc
                         #:must-unzip? [must-unzip? #t])
  (let ([temp-dir #f])
    (dynamic-wind
        (lambda ()
          (set! temp-dir (make-temporary-file "ziptmp~a" 'directory)))
        (lambda ()
          (unzip zip-file (make-filesystem-entry-reader
                           #:dest temp-dir
                           #:exists 'replace)
                 #:must-unzip? must-unzip?)
          (user-proc temp-dir))
        (lambda ()
          (delete-directory/files temp-dir)))))
