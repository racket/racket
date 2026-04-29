;; A modification of Dave Herman's zip module

(module zip racket/base
  (require file/gzip racket/file racket/port)

  ;; ===========================================================================
  ;; DATA DEFINITIONS
  ;; ===========================================================================

  ;; An msdos-time or an msdos-date is an exact-integer in the respective format
  ;; described at:
  ;;
  ;;     https://web.archive.org/web/20050209013515/http://msdn.microsoft.com/library/en-us/com/htm/cmf_a2c_25gl.asp

  ;; metadata : path * bytes * boolean * integer * integer * nat * integer
  (define-struct metadata
    (path name directory? time date compression attributes))

  ;; header : metadata * exact-integer * nat * nat * nat * integer
  (define-struct header (metadata crc compressed uncompressed size bits))
  (struct zip-entry (kind path content size attribs level)
    #:constructor-name make-zip-entry*)
  (provide struct:zip-entry
           zip-entry?
           zip-entry-kind
           zip-entry-path
           zip-entry-content
           zip-entry-size
           zip-entry-attribs
           zip-entry-level
           make-zip-entry)

  (define (make-zip-entry . args)
    (case (length args)
      [(2)
       (make-zip-entry* 'path
                        (car args)
                        #f
                        #f
                        #hash()
                        (cadr args))]
      [(6) (apply make-zip-entry* args)]
      [else
       (raise-arity-error
        'make-zip-entry
        "(or/c (-> path-string? exact-integer? zip-entry?)\n        (-> (or/c 'path 'file 'directory) path-string? any/c (or/c #f exact-nonnegative-integer?) (hash/c symbol? any/c) exact-integer? zip-entry?))"
        args)]))

  (define-struct archive-entry (metadata source level))
  (define-struct filesystem-source (path))
  (define-struct generated-source (content size))

  ;; ===========================================================================
  ;; CONSTANTS etc
  ;; ===========================================================================

  (define *spec-version*          62) ; version 6.2
  (define *required-version*      20) ; version 2.0
  (define *stored-method*          0)
  (define *deflated-method*        8)
  (define *default-deflate-level*  6)
  (define *minimum-msdos-seconds* 315532800) ; 1980-01-01 00:00:00 UTC
  (define *zip-comment* #"packed by Racket - http://racket-lang.org/")

  ;; PKZIP specification:
  ;; http://www.pkware.com/company/standards/appnote/

  (define *local-file-header*                      #x04034b50)
  (define *archive-extra-record*                   #x08064b50)
  (define *central-file-header*                    #x02014b50)
  (define *digital-signature*                      #x05054b50)
  (define *zip64-end-of-central-directory-record*  #x06064b50)
  (define *zip64-end-of-central-directory-locator* #x07064b50)
  (define *end-of-central-directory-record*        #x06054b50)

  (define (get-system-id type)
    (case type
      [(windows)    0]
      [(macos)      7]
      [(macosx)     3] ; although Mac OS X = 19, it's not recognized by unzip on macOS
      [else         3]))
  (define *os-specific-separator-regexp*
    (case (system-type)
      [(unix macosx oskit) #rx"/"]
      [(windows) #rx"\\\\"]
      [(macos) #rx":"]))

  (provide zip-verbose)
  (define zip-verbose (make-parameter #f))

  (define *crc32-polynomial* #xedb88320)
  (define *crc32-table*
    (build-vector
     256
     (lambda (i)
       (let loop ([n i] [k 0])
         (if (= k 8)
             n
             (loop (if (odd? n)
                       (bitwise-xor *crc32-polynomial* (arithmetic-shift n -1))
                       (arithmetic-shift n -1))
                   (add1 k)))))))

  ;; ===========================================================================
  ;; FILE CREATION
  ;; ===========================================================================

  ;; date->msdos-time : date -> msdos-time
  ;; See https://formats.kaitai.io/dos_datetime/ for format specification.
  (define (date->msdos-time date round-down?)
    ;; MS-DOS time format uses 5 bits for seconds (0-29, representing 0-58 in 2-sec increments).
    ;; When rounding up with seconds=59, cap at 29 to avoid overflow to invalid value 60.
    (bitwise-ior (min 29 (arithmetic-shift (+ (if round-down? 0 1) (date-second date)) -1))
                 (arithmetic-shift (date-minute date) 5)
                 (arithmetic-shift (date-hour date) 11)))

  ;; date->msdos-date : date -> msdos-date
  (define (date->msdos-date date)
    (bitwise-ior (date-day date)
                 (arithmetic-shift (date-month date) 5)
                 (arithmetic-shift (- (date-year date) 1980) 9)))

  ;; seekable-port? : port -> boolean
  (define (seekable-port? port)
    (or (string-port? port)
        (and (file-stream-port? port)
             (with-handlers ([void (lambda (exn) #f)])
               (file-position port (file-position port))
               #t))))

  (define (write-int n size)
    (write-bytes (integer->integer-bytes n size #f #f)))

  (define (normalize-path path)
    (cond [(path? path)   path]
          [(string? path) (string->path path)]
          [(bytes? path)  (bytes->path path)]
          [else path]))

  (define (check-level who level)
    (unless (and (exact-integer? level)
                 (<= 0 level 9))
      (raise-argument-error who
                            "(integer-in 0 9)"
                            level)))

  (define (check-zip-entry who entry)
    (unless (memq (zip-entry-kind entry) '(path file directory))
      (raise-argument-error who
                            "(or/c 'path 'file 'directory)"
                            (zip-entry-kind entry)))
    (unless (and (path-string? (zip-entry-path entry))
                 (relative-path? (normalize-path (zip-entry-path entry))))
      (raise-argument-error who
                            "(and/c path-string? relative-path?)"
                            (zip-entry-path entry)))
    (check-level who (zip-entry-level entry))
    (unless (or (not (zip-entry-size entry))
                (exact-nonnegative-integer? (zip-entry-size entry)))
      (raise-argument-error who
                            "(or/c #f exact-nonnegative-integer?)"
                            (zip-entry-size entry)))
    (unless (hash? (zip-entry-attribs entry))
      (raise-argument-error who
                            "hash?"
                            (zip-entry-attribs entry)))
    (case (zip-entry-kind entry)
      [(path) (void)]
      [(directory)
       (when (zip-entry-content entry)
         (raise-arguments-error who
                                "directory entries cannot have content"
                                "content" (zip-entry-content entry)))
       (when (and (zip-entry-size entry)
                  (not (zero? (zip-entry-size entry))))
         (raise-arguments-error who
                                "directory entries cannot have a non-zero size"
                                "size" (zip-entry-size entry)))]
      [(file)
       (unless (or (input-port? (zip-entry-content entry))
                   (procedure? (zip-entry-content entry))
                   (bytes? (zip-entry-content entry)))
         (raise-argument-error who
                               "(or/c input-port? procedure? bytes?)"
                               (zip-entry-content entry)))])
    entry)

  (define (compression-method directory? level)
    (if (or directory? (zero? level))
        *stored-method*
        *deflated-method*))

  (define (crc32-update crc bstr)
    (for/fold ([crc crc]) ([b (in-bytes bstr)])
      (bitwise-xor (vector-ref *crc32-table*
                               (bitwise-and (bitwise-xor crc b) #xff))
                   (arithmetic-shift crc -8))))

  (define (copy-stored-port in out)
    (let loop ([crc #xffffffff] [count 0])
      (define bstr (read-bytes 4096 in))
      (cond
        [(eof-object? bstr)
         (define final-crc (bitwise-and (bitwise-xor crc #xffffffff) #xffffffff))
         (values count count final-crc)]
        [else
         (write-bytes bstr out)
         (loop (crc32-update crc bstr)
               (+ count (bytes-length bstr)))])))

  (define (call-with-source-input-port source proc)
    (cond
      [(filesystem-source? source)
       (call-with-input-file* (filesystem-source-path source) proc)]
      [(generated-source? source)
       (define content (generated-source-content source))
       (cond
         [(input-port? content) (proc content)]
         [(bytes? content) (call-with-input-bytes content proc)]
         [else
          (define in (content))
          (begin0
            (proc in)
            (close-input-port in))])]
      [else (error 'zip "internal error: unknown source ~e" source)]))

  (define (expected-size-mismatch who path expected actual)
    (raise-arguments-error who
                           "entry size does not match content length"
                           "path" path
                           "expected" expected
                           "actual" actual))

  (define (copy-source source compression level path)
    (call-with-source-input-port
     source
     (lambda (in)
       (let-values ([(uncompressed compressed crc)
                     (if (= compression *stored-method*)
                         (copy-stored-port in (current-output-port))
                         (deflate in (current-output-port) #:level level))])
         (when (and (generated-source? source)
                    (generated-source-size source)
                    (not (= uncompressed (generated-source-size source))))
           (expected-size-mismatch 'zip path (generated-source-size source) uncompressed))
         (values uncompressed compressed crc)))))

  ;; zip-one-entry : archive-entry boolean -> header
  (define (zip-one-entry archive-entry seekable?)
    (let* ([metadata    (archive-entry-metadata archive-entry)]
           [source      (archive-entry-source archive-entry)]
           [level       (archive-entry-level archive-entry)]
           [directory?  (metadata-directory? metadata)]
           [path        (metadata-path metadata)]
           [filename    (metadata-name metadata)]
           [filename-length (bytes-length filename)]
           [bits        (if seekable? 0 #b1000)]
           [time        (metadata-time metadata)]
           [date        (metadata-date metadata)]
           [compression (metadata-compression metadata)]
           [mark1       #f]
           [mark2       #f])
      (when (zip-verbose)
        (eprintf "zip: compressing ~a...\n" filename))
      ;; write the contents to the output stream:
      (write-int *local-file-header*    4)  ; signature
      (write-int *required-version*     2)  ; version
      (write-int bits                   2)  ; bits
      (write-int compression            2)  ; compression
      (write-int time                   2)  ; time
      (write-int date                   2)  ; date
      (when seekable? (set! mark1 (file-position (current-output-port))))
      (write-int 0                      4)  ; crc-32
      (write-int 0                      4)  ; compressed
      (write-int 0                      4)  ; uncompressed
      (write-int filename-length        2)  ; filename-length
      (write-int 0                      2)  ; extra-length
      (write-bytes filename)                ; filename
      (if directory?
        (make-header metadata 0 0 0 (+ filename-length 30) bits)
        (let-values ([(uncompressed compressed crc)
                      (copy-source source compression level path)])
          (if seekable?
            (begin (set! mark2 (file-position (current-output-port)))
                   (file-position (current-output-port) mark1))
            (write-int #x08074b50 4)) ; EXT signature
          (write-int crc            4)  ; crc-32
          (write-int compressed     4)  ; compressed
          (write-int uncompressed   4)  ; uncompressed
          (when seekable? (file-position (current-output-port) mark2))

          ;; return the header information
          (make-header metadata crc compressed uncompressed
                       (+ filename-length compressed
                          (if seekable? 30 46))
                       bits)))))

  ;; write-end-of-central-directory : nat nat nat ->
  (define (write-end-of-central-directory count start size)
    (let ([comment-length (bytes-length *zip-comment*)])
      (write-int #x06054b50     4) ; signature
      (write-int 0              2) ; # this disk
      (write-int 0              2) ; # disk with start of central dir.
      (write-int count          2) ; # entries in central dir. on this disk
      (write-int count          2) ; # entries in central dir.
      (write-int size           4) ; size of central dir.
      (write-int start          4) ; offset of start of central dir.
      (write-int comment-length 2)
      (write-bytes *zip-comment*)))

  ;; write-central-directory : (listof header) symbol? ->
  (define (write-central-directory headers sys-type)
    (let ([count (length headers)])
      (let loop ([headers headers] [offset 0] [size 0])
        (if (null? headers)
          ;; no digital signature (why?)
          (write-end-of-central-directory count offset size)
          (let* ([header (car headers)]
                 [bits (header-bits header)]
                 [metadata (header-metadata header)]
                 [filename-length (bytes-length (metadata-name metadata))]
                 [attributes (metadata-attributes metadata)]
                 [compression (metadata-compression metadata)]
                 [version (bitwise-ior *spec-version*
                                       (arithmetic-shift (get-system-id sys-type)
                                                         8))])
            (write-int #x02014b50                   4)
            (write-int version                      2)
            (write-int *required-version*           2)
            (write-int bits                         2)
            (write-int compression                  2)
            (write-int (metadata-time metadata)     2)
            (write-int (metadata-date metadata)     2)
            (write-int (header-crc header)          4)
            (write-int (header-compressed header)   4)
            (write-int (header-uncompressed header) 4)
            (write-int filename-length              2)
            (write-int 0                            2)
            (write-int 0                            2) ; comment length
            (write-int 0                            2)
            (write-int 0                            2) ; internal attributes
            (write-int attributes                   4) ; external attributes
            (write-int offset                       4)
            (write-bytes (metadata-name metadata))
            (loop (cdr headers)
                  (+ offset (header-size header))
                  (+ size filename-length 46)))))))

  ;; The PKZIP specification includes an entry in the central directory for
  ;; an entry's "external file attributes," which for standard ZIP files is
  ;; the MS-DOS (i.e., FAT) directory attribute byte, and the Unix zip adds
  ;; the Unix bits as the higher two bytes.

  ;; This is for reference
  ;; (define *msdos:read-only*  #x01)
  ;; (define *msdos:hidden*     #x02)
  ;; (define *msdos:system*     #x04)
  ;; (define *msdos:volume*     #x08)
  ;; (define *msdos:directory*  #x10)
  ;; (define *msdos:archive*    #x20)
  ;; (define *unix:directory*   #o40000)
  ;; (define *unix:char-dev*    #o20000)
  ;; (define *unix:fifo*        #o10000)
  ;; (define *unix:suid*        #o04000)
  ;; (define *unix:sgid*        #o02000)
  ;; (define *unix:sticky*      #o01000)
  ;; (define *unix:owner-read*  #o00400)
  ;; (define *unix:owner-write* #o00200)
  ;; (define *unix:owner-exe*   #o00100)
  ;; (define *unix:group-read*  #o00040)
  ;; (define *unix:group-write* #o00020)
  ;; (define *unix:group-exe*   #o00010)
  ;; (define *unix:other-read*  #o00004)
  ;; (define *unix:other-write* #o00002)
  ;; (define *unix:other-exe*   #o00001)
  (define (path-attributes path dir? permissions)
    (define syms (and path
                      (not permissions)
                      (file-or-directory-permissions path)))
    (let ([dos (if dir?
                   #x10
                   (let ([read-only?
                          (if permissions
                              (zero? (bitwise-and #o200 permissions))
                              (not (memq 'write syms)))])
                     (if read-only? #x01 0)))]
          [unix (apply bitwise-ior (if dir?
                                       #o40000
                                       ;; pkzip sets this bit:
                                       #x8000)
                       (or (and permissions
                                (list permissions))
                           (map (lambda (p)
                                  (case p
                                    [(read)    #o444]
                                    [(write)   #o200] ; mask out write bits
                                    [(execute) #o111]))
                                syms)))])
      (bitwise-ior dos (arithmetic-shift unix 16))))

  ;; with-trailing-slash : bytes -> bytes
  (define (with-trailing-slash bytes)
    (regexp-replace #rx#"/*$" bytes "/"))

  ;; with-slash-separator : bytes -> bytes
  (define (with-slash-separator bytes)
    (regexp-replace* *os-specific-separator-regexp* bytes #"/"))

  (define (default-modify-seconds entry-path kind attribs get-timestamp)
    (hash-ref attribs
              'modify-seconds
              (lambda ()
                (if (eq? kind 'path)
                    (get-timestamp entry-path)
                    *minimum-msdos-seconds*))))

  (define (default-permissions path kind directory? attribs)
    (hash-ref attribs
              'permissions
              (lambda ()
                (cond
                  [(eq? kind 'path) #f]
                  [directory? #o755]
                  [else #o644]))))

  ;; build-metadata : path (or/c path-string? #f) boolean exact-integer (or/c #f exact-integer?) exact-integer boolean boolean (or/c path-string? #f) -> metadata
  (define (build-metadata path-prefix archive-path directory? modify-seconds permissions
                          level utc? round-down? source-path)
    (check-level 'zip level)
    (let* ([archive-path (normalize-path archive-path)]
           [mod  (seconds->date modify-seconds (not utc?))]
           [attr (path-attributes (and source-path (normalize-path source-path))
                                  directory?
                                  permissions)]
           [name-path (if path-prefix
                          (build-path path-prefix archive-path)
                          archive-path)]
           [name (with-slash-separator (path->bytes name-path))]
           [name (if directory? (with-trailing-slash name) name)]
           [time (date->msdos-time mod round-down?)]
           [date (date->msdos-date mod)]
           [comp (compression-method directory? level)])
      (make-metadata archive-path name directory? time date comp attr)))

  (define (path-or-entry->archive-entry who path-or-entry default-level get-timestamp
                                        utc? round-down? path-prefix)
    (cond
      [(zip-entry? path-or-entry)
       (define entry (check-zip-entry who path-or-entry))
       (define kind (zip-entry-kind entry))
       (define archive-path (zip-entry-path entry))
       (define level (zip-entry-level entry))
       (define attribs (zip-entry-attribs entry))
       (define directory? (eq? kind 'directory))
       (define metadata
         (build-metadata path-prefix
                         archive-path
                         directory?
                         (default-modify-seconds archive-path kind attribs get-timestamp)
                         (default-permissions #f kind directory? attribs)
                         level
                         utc?
                         round-down?
                         (and (eq? kind 'path) archive-path)))
       (define source
         (case kind
           [(directory) #f]
           [(path) (make-filesystem-source (normalize-path archive-path))]
           [(file) (make-generated-source (zip-entry-content entry)
                                          (zip-entry-size entry))]))
       (make-archive-entry metadata source level)]
      [else
       (define path (normalize-path path-or-entry))
       (define level default-level)
       (define directory? (directory-exists? path))
       (make-archive-entry
        (build-metadata path-prefix
                        path
                        directory?
                        (get-timestamp path)
                        #f
                        level
                        utc?
                        round-down?
                        path)
        (and (not directory?) (make-filesystem-source path))
        level)]))

  (define (zip-entries->headers who path-or-entries seekable? default-level get-timestamp
                                utc? round-down? path-prefix)
    (append
     ;; synthesize directories for `path-prefix` as needed:
     (reverse
      (let loop ([path-prefix path-prefix])
        (cond
         [(not path-prefix) null]
         [else
         (define-values (base name dir?) (split-path path-prefix))
          (define r (loop (and (path? base) base)))
          (cons
           (zip-one-entry
            (make-archive-entry
             (build-metadata #f path-prefix #t (current-seconds) #o755 default-level utc? round-down? #f)
             #f
             default-level)
            seekable?)
           r)])))
     (map (lambda (path-or-entry)
            (zip-one-entry
             (path-or-entry->archive-entry who path-or-entry default-level
                                           get-timestamp utc? round-down? path-prefix)
             seekable?))
          path-or-entries)))

  (define (close-path-or-entries path-or-entries)
    (let loop ([path-or-entries path-or-entries]
               [seen null]
               [closed null])
      (cond
        [(null? path-or-entries) (reverse closed)]
        [else
         (define path-or-entry (car path-or-entries))
         (cond
           [(zip-entry? path-or-entry)
            (loop (cdr path-or-entries)
                  seen
                  (cons path-or-entry closed))]
           [else
            (let loop2 ([paths (pathlist-closure (list path-or-entry))]
                        [seen seen]
                        [closed closed])
              (cond
                [(null? paths)
                 (loop (cdr path-or-entries) seen closed)]
                [(member (car paths) seen)
                 (loop2 (cdr paths) seen closed)]
                [else
                 (loop2 (cdr paths)
                        (cons (car paths) seen)
                        (cons (car paths) closed))]))])])))

  ;; ===========================================================================
  ;; FRONT END
  ;; ===========================================================================

  ;; zip-write : (listof (or/c relative-path zip-entry?)) ->
  ;; writes a zip file to current-output-port
  (provide zip->output)
  (define (zip->output path-or-entries [out (current-output-port)]
                       #:level [level *default-deflate-level*]
                       #:timestamp [timestamp #f]
                       #:get-timestamp [get-timestamp (if timestamp
                                                          (lambda (p) timestamp)
                                                          file-or-directory-modify-seconds)]
                       #:utc-timestamps? [utc? #f]
                       #:round-timestamps-down? [round-down? #f]
                       #:path-prefix [path-prefix #f]
                       #:system-type [sys-type (system-type)])
    (check-level 'zip->output level)
    (parameterize ([current-output-port out])
      (let* ([seekable? (seekable-port? (current-output-port))]
             [headers (zip-entries->headers 'zip->output path-or-entries seekable? level get-timestamp
                                            utc? round-down? path-prefix)])
        (when (zip-verbose)
          (eprintf "zip: writing headers...\n"))
        (write-central-directory headers sys-type))
      (when (zip-verbose)
        (eprintf "zip: done.\n"))))

  ;; zip : output-file paths ->
  (provide zip)
  (define (zip zip-file
               #:level [level *default-deflate-level*]
               #:timestamp [timestamp #f]
               #:get-timestamp [get-timestamp (if timestamp
                                                  (lambda (p) timestamp)
                                                  file-or-directory-modify-seconds)]
               #:utc-timestamps? [utc? #f]
               #:round-timestamps-down? [round-down? #f]
               #:path-prefix [path-prefix #f]
               #:system-type [sys-type (system-type)]
               . path-or-entries)
    (with-output-to-file zip-file
      (lambda () (zip->output (close-path-or-entries path-or-entries)
                              #:level level
                              #:get-timestamp get-timestamp
                              #:utc-timestamps? utc?
                              #:round-timestamps-down? round-down?
                              #:path-prefix path-prefix
                              #:system-type sys-type))))
  
  )
