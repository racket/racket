;; A modification of Dave Herman's zip module

(module zip racket/base
  (require file/gzip racket/file)

  ;; ===========================================================================
  ;; DATA DEFINITIONS
  ;; ===========================================================================

  ;; An msdos-time or an msdos-date is an exact-integer in the respective format
  ;; described at:
  ;;
  ;;     http://msdn.microsoft.com/library/en-us/com/htm/cmf_a2c_25gl.asp

  ;; metadata : path * bytes * boolean * integer * integer * nat * integer
  (define-struct metadata
    (path name directory? time date compression attributes))

  ;; header : metadata * exact-integer * nat * nat * nat
  (define-struct header (metadata crc compressed uncompressed size))

  ;; ===========================================================================
  ;; CONSTANTS etc
  ;; ===========================================================================

  (define *spec-version*      62) ; version 6.2
  (define *required-version*  20) ; version 2.0
  (define *compression-level*  8) ; I don't think this is configurable
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

  (define *system*
    (case (system-type)
      [(unix oskit) 3]
      [(windows)    0]
      [(macos)      7]
      [(macosx)    19]))
  (define *os-specific-separator-regexp*
    (case (system-type)
      [(unix macosx oskit) #rx"/"]
      [(windows) #rx"\\\\"]
      [(macos) #rx":"]))

  (provide zip-verbose)
  (define zip-verbose (make-parameter #f))

  ;; ===========================================================================
  ;; FILE CREATION
  ;; ===========================================================================

  ;; date->msdos-time : date -> msdos-time
  (define (date->msdos-time date)
    (bitwise-ior (ceiling (/ (date-second date) 2))
                 (arithmetic-shift (date-minute date) 5)
                 (arithmetic-shift (date-hour date) 11)))

  ;; date->msdos-date : date -> msdos-date
  (define (date->msdos-date date)
    (bitwise-ior (date-day date)
                 (arithmetic-shift (date-month date) 5)
                 (arithmetic-shift (- (date-year date) 1980) 9)))

  ;; seekable-port? : port -> boolean
  (define (seekable-port? port)
    (and (file-stream-port? port)
         (with-handlers ([void (lambda (exn) #f)])
           (file-position port (file-position port))
           #t)))

  (define (write-int n size)
    (write-bytes (integer->integer-bytes n size #f #f)))

  ;; zip-one-entry : metadata boolean -> header
  (define (zip-one-entry metadata seekable?)
    (let* ([directory?  (metadata-directory? metadata)]
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
        (make-header metadata 0 0 0 (+ filename-length 30))
        (let-values ([(uncompressed compressed crc)
                      (with-input-from-file path
                        (lambda ()
                          (deflate (current-input-port)
                                   (current-output-port))))])
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
                          (if seekable? 30 46)))))))

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

  ;; write-central-directory : (listof header) (or/c #f exact-integer?) ->
  (define (write-central-directory headers timestamp)
    (let ([count (length headers)])
      (let loop ([headers headers] [offset 0] [size 0])
        (if (null? headers)
          ;; no digital signature (why?)
          (write-end-of-central-directory count offset size)
          (let* ([header (car headers)]
                 [metadata (header-metadata header)]
                 [filename-length (bytes-length (metadata-name metadata))]
                 [attributes (metadata-attributes metadata)]
                 [compression (metadata-compression metadata)]
                 [version (bitwise-ior *spec-version*
                                       (arithmetic-shift (if timestamp
                                                             3
                                                             *system*)
                                                         8))])
            (write-int #x02014b50                   4)
            (write-int version                      2)
            (write-int *required-version*           2)
            (write-int 0                            2)
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
  (define (path-attributes path dir?)
    (let ([dos  (if dir? #x10 0)]
          [unix (apply bitwise-ior (if dir? #o40000 0)
                       (map (lambda (p)
                              (case p
                                [(read)    #o444]
                                [(write)   #o200] ; mask out write bits
                                [(execute) #o111]))
                            (file-or-directory-permissions path)))])
      (bitwise-ior dos (arithmetic-shift unix 16))))

  ;; with-trailing-slash : bytes -> bytes
  (define (with-trailing-slash bytes)
    (regexp-replace #rx#"/*$" bytes "/"))

  ;; with-slash-separator : bytes -> bytes
  (define (with-slash-separator bytes)
    (regexp-replace* *os-specific-separator-regexp* bytes #"/"))

  ;; build-metadata : relative-path (or/c #f exact-integer?) -> metadata
  (define (build-metadata path timestamp)
    (let* ([mod  (seconds->date (or timestamp
                                    (file-or-directory-modify-seconds path)))]
           [dir? (directory-exists? path)]
           [path (cond [(path? path)   path]
                       [(string? path) (string->path path)]
                       [(bytes? path)  (bytes->path path)])]
           [name (with-slash-separator (path->bytes path))]
           [name (if dir? (with-trailing-slash name) name)]
           [time (date->msdos-time mod)]
           [date (date->msdos-date mod)]
           [comp (if dir? 0 *compression-level*)]
           [attr (path-attributes path dir?)])
      (make-metadata path name dir? time date comp attr)))

  ;; ===========================================================================
  ;; FRONT END
  ;; ===========================================================================

  ;; zip-write : (listof relative-path) ->
  ;; writes a zip file to current-output-port
  (provide zip->output)
  (define (zip->output files [out (current-output-port)]
                       #:timestamp [timestamp #f])
    (parameterize ([current-output-port out])
      (let* ([seekable? (seekable-port? (current-output-port))]
             [headers ; note: Racket's `map' is always left-to-right
              (map (lambda (file)
                     (zip-one-entry (build-metadata file timestamp) seekable?))
                   files)])
        (when (zip-verbose)
          (eprintf "zip: writing headers...\n"))
        (write-central-directory headers timestamp))
      (when (zip-verbose)
        (eprintf "zip: done.\n"))))

  ;; zip : output-file paths ->
  (provide zip)
  (define (zip zip-file #:timestamp [timestamp #f]
               . paths)
    ;; (when (null? paths) (error 'zip "no paths specified"))
    (with-output-to-file zip-file
      (lambda () (zip->output (pathlist-closure paths) 
                              #:timestamp timestamp))))

  )
