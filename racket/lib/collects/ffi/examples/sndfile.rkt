#lang racket/base

(require ffi/unsafe
         ffi/vector
         racket/match
         racket/runtime-path)

;; the constants in this file are pulled from version 1.0.21 of the
;; libsndfile header file. However, I think it would be a mistake to
;; specify this in the call to ffi-lib; it appears that this version is
;; a conservative extension of the earlier version (1.0.17?), and I
;; think you'll get graceful failures if the version is wrong.

(define-runtime-path mac-ffi-path "./lib/libsndfile")
(define libsndfile
  (match (system-type)
    ['unix (ffi-lib "libsndfile"   '("1.0.21" "1.0.20" ""))]
    ['macosx (ffi-lib mac-ffi-path '("1.0.21" "1.0.20" ""))]
    ['windows (error 'libsndfile "libsndfile not supported on windows.")]))

;; ==================== Types etc ====================

;; This is the scheme representation of the soundfile that is handled by
;; libsndfile.

;; In libsndfile the sndfile object is represented as a pointer. When
;; translating scheme->c the struct will just return the pointer. When
;; translating from c->scheme, ie. creating the object in scheme it will be
;; wrapped by an object finalizer that uses the libsndfile fuction sf_close that
;; returns a 0 upon successful termination or an error.
(define-struct sndfile (ptr [info #:mutable]))
(define _sndfile
  (make-ctype _pointer sndfile-ptr
    (lambda (p)
      ;; can't check here for null-ness of p, the check has to be
      ;; specific to the call.
      (make-sndfile p #f))))

;; sf_count_t is a count type that depends on the operating system however it
;; seems to be a long int for all the supported ones so in this case we just
;; define it as an int64.
(define _sf-count-t _int64)

(define _sf-mode
  (_bitmask '(sfm-read  = #x10
              sfm-write = #x20
              sfm-rdwrt = #x30

              ;; I really have no clue what these are for:
              sf-ambisonic-none = #x40
              sf-ambisonic-b-format = #x41)))

(define str-types '(title copyright software artist comment date album license))
(define _sf-str-type (_enum (cons "dummy" str-types))) ; begins from 1
(define _sf-error (_enum '(no-error unrecognised-format system malformed-file unsupported-encoding)))

(define _sf-format
  (let ([majors ; Major formats
         '((wav       #x010000) ; Microsoft WAV format (little endian default).
           (aiff      #x020000) ; Apple/SGI AIFF format (big endian).
           (au        #x030000) ; Sun/NeXT AU format (big endian).
           (raw       #x040000) ; RAW PCM data.
           (paf       #x050000) ; Ensoniq PARIS file format.
           (svx       #x060000) ; Amiga IFF / SVX8 / SV16 format.
           (nist      #x070000) ; Sphere NIST format.
           (voc       #x080000) ; VOC files.
           (ircam     #x0A0000) ; Berkeley/IRCAM/CARL
           (w64       #x0B0000) ; Sonic Foundry's 64 bit RIFF/WAV
           (mat4      #x0C0000) ; Matlab (tm) V4.2 / GNU Octave 2.0
           (mat5      #x0D0000) ; Matlab (tm) V5.0 / GNU Octave 2.1
           (pvf       #x0E0000) ; Portable Voice Format
           (xi        #x0F0000) ; Fasttracker 2 Extended Instrument
           (htk       #x100000) ; HMM Tool Kit format
           (sds       #x110000) ; Midi Sample Dump Standard
           (avr       #x120000) ; Audio Visual Research
           (wavex     #x130000) ; MS WAVE with WAVEFORMATEX
           (sd2       #x160000) ; Sound Designer 2
           (flac      #x170000) ; FLAC lossless file format
           (caf       #x180000) ; Core Audio File format
           (wve       #x190000) ; Psion WVE format
           (ogg       #x200000) ; Xiph OGG container
           (mpc2k     #x210000) ; Akai MPC 2000 sampler
           (rf64      #x220000) ; RF64 WAV file
           )]
        [subtypes ; Subtypes from here on
         '((pcm-s8    #x0001)     ; Signed 8 bit data
           (pcm-16    #x0002)     ; Signed 16 bit data
           (pcm-24    #x0003)     ; Signed 24 bit data
           (pcm-32    #x0004)     ; Signed 32 bit data
           (pcm-u8    #x0005)     ; Unsigned 8 bit data (WAV and RAW only)
           (float     #x0006)     ; 32 bit float data
           (double    #x0007)     ; 64 bit float data

           (ulaw      #x0010)     ; U-Law encoded
           (alaw      #x0011)     ; A-Law encoded
           (ima-adpcm #x0012)     ; IMA ADPCM
           (ms-adpcm  #x0013)     ; Microsoft ADPCM

           (gsm610    #x0020)     ; GSM 6.10 encoding
           (vox-adpcm #x0021)     ; OKI / Dialogix ADPCM

           (g721-32   #x0030)     ; 32kbs G721 ADPCM encoding
           (g723-24   #x0031)     ; 24kbs G723 ADPCM encoding
           (g723-40   #x0032)     ; 40kbs G723 ADPCM encoding

           (dwvw-12   #x0040)     ; 12 bit Delta Width Variable Word encoding
           (dwvw-16   #x0041)     ; 16 bit Delta Width Variable Word encoding
           (dwvw-24   #x0042)     ; 24 bit Delta Width Variable Word encoding
           (dwvw-n    #x0043)     ; N bit Delta Width Variable Word encoding

           (dpcm-8    #x0050)     ; 8 bit differential PCM (XI only)
           (dpcm-16   #x0051)     ; 16 bit differential PCM (XI only)

           (vorbis    #x0060)     ; Xiph Vorbis encoding.
           )]
        [endians ; Endian-ness options
         '((file      #x00000000) ; Default file endian-ness
           (little    #x10000000) ; Force little endian-ness
           (big       #x20000000) ; Force big endian-ness
           (cpu       #x30000000) ; Force CPU endian-ness
           )]
        [submask  #x0000FFFF]
        [typemask #x0FFF0000]
        [endmask  #x30000000])
    (define (rev-find n l)
      (let loop ([l l])
        (cond [(null? l) #f]
              [(eq? n (cadar l)) (caar l)]
              [else (loop (cdr l))])))
    (make-ctype _int
      (lambda (syms)
        (let ([major #f] [subtype #f] [endian #f])
          (for-each
           (lambda (sym)
             (cond [(assq sym majors) =>
                    (lambda (x)
                      (if major
                        (error 'sf-format "got two major modes: ~s" syms)
                        (set! major (cadr x))))]
                   [(assq sym subtypes) =>
                    (lambda (x)
                      (if subtype
                        (error 'sf-format "got two subtype modes: ~s" syms)
                        (set! subtype (cadr x))))]
                   [(assq sym endians) =>
                    (lambda (x)
                      (if endian
                        (error 'sf-format "got two endian modes: ~s" syms)
                        (set! endian (cadr x))))]
                   [else (error 'sf-format "got a bad symbol: ~s" sym)]))
           (if (list? syms) syms (list syms)))
          (bitwise-ior (or major 0) (or subtype 0) (or endian 0))))
      (lambda (n)
        (let ([subtype (rev-find (bitwise-and n submask)  subtypes)]
              [major   (rev-find (bitwise-and n typemask) majors)]
              [endian  (rev-find (bitwise-and n endmask)  endians)])
          (unless subtype
            (error 'sf-format "got a bad number from C for subtype: ~x"
                   (bitwise-and n submask)))
          (unless major
            (error 'sf-format "got a bad number from C for major: ~x"
                   (bitwise-and n typemask)))
          (unless endian
            (error 'sf-format "got a bad number from C for endian: ~x"
                   (bitwise-and n endmask)))
          (list major subtype endian))))))

(define-cstruct _sf-info
  ((frames     _sf-count-t)
   (samplerate _int)
   (channels   _int)
   (format     _sf-format)
   (sections   _int)
   (seekable   _bool)))

;; ==================== Utilities ====================

;; extract the given function from the library by replacing
;; hyphens with underscores in the name.
(define-syntax defsndfile
  (syntax-rules (:)
    [(_ name : type ...)
     (define name
       (get-ffi-obj (regexp-replaces (symbol->string 'name) '((#rx"-" "_")))
                    libsndfile (_fun type ...)))]))

(define (n-split l n)
  (let loop ([l l][i 0][a2 null][a null])
    (cond
     [(null? l) (let ([a (if (null? a2)
                             a
                             (cons (reverse a2) a))])
                  (reverse a))]
     [(= i n) (loop l 0 null (cons (reverse a2) a))]
     [else (loop (cdr l) (add1 i) (cons (car l) a2) a)])))

;; ==================== sndfile API ====================

(defsndfile sf-strerror : _sndfile -> _string)
(defsndfile sf-error    : _sndfile -> _int)

(defsndfile sf-close : _sndfile -> _int)

;; sf-open : path mode [optional-sf-info-pointer] -> sndfile
(defsndfile sf-open : (path mode . info) ::
  (path : _file)
  (mode : _sf-mode)
  (info : _sf-info-pointer
        = (if (pair? info) (car info) (make-sf-info 0 0 0 '() 0 #f)))
  -> (sf : _sndfile)
  -> (if (sndfile-ptr sf)
         (begin (set-sndfile-info! sf info) sf)
         ;; goofy way to get the error code:
         (error 'sf-open "~a" (sf-strerror (make-sndfile #f #f)))))

(defsndfile sf-format-check  : _sf-info-pointer -> _bool)

(defsndfile sf-readf-short   : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-readf-int     : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-readf-float   : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-readf-double  : _sndfile _pointer _sf-count-t -> _sf-count-t)

(defsndfile sf-writef-short  : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-writef-int    : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-writef-float  : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-writef-double : _sndfile _pointer _sf-count-t -> _sf-count-t)

(defsndfile sf-get-string    : _sndfile _sf-str-type -> _string)
(defsndfile sf-set-string    : _sndfile _sf-str-type _string -> _bool)

;; ==================== Utilities for the Scheme interface ====================

;; get-meta-strings : sndfile -> (listof (list/c sf-str-type string))
;; produce an association list for the meta-information associated with
;; the sndfile

;; Q: can sf-get-string signal an error? apparently so. I think it makes
;; sense to identify these errors with the simple lack of the string,
;; which requires no change to the code. I hope that such an error
;; doesn't pollute later operations on the soundfile.
(define (get-meta-strings sndfile)
  (for/list ([s (in-list str-types)]
             #:when (sf-get-string sndfile s))
    (list s (string-copy (sf-get-string sndfile s)))))

;; set-meta-strings : sndfile (listof (list/c sf-str-type string)) -> (void)
;; given a sndfile and an association list between symbols and strings,
;; attach each string to the soundfile under the given symbol.
;; ** Ignores errors that occur during calls to sf-set-string. **
(define (set-meta-strings sndfile meta)
  (for ([st (in-list str-types)])
    (let ([found (assq st meta)])
      (when found (sf-set-string sndfile st (cadr found))))))


;; read-sound-internal : path-string -> (values/c (or/c cblock (listof (listof sample))) a-list)
;; read the data from a file.
(define (read-sound-internal file)
  (let* ([sndfile  (sf-open file 'sfm-read)]
         [strings  (get-meta-strings sndfile)]
         [info     (sndfile-info sndfile)]
         [frames   (sf-info-frames info)]
         [channels (sf-info-channels info)]
         [stype    (case (sample-type)
                     [(short) _int16] [(int) _int] [(float) _double*])]
         [readf    (sample-type->reader (sample-type))]
         [cblock   ((sample-type->vector-maker (sample-type)) (* frames channels))]
         [num-read (readf sndfile ((sample-type->cpointer-extractor (sample-type)) cblock) frames)])
    (unless (= frames num-read)
      (error 'read-sound-internal "wanted ~e frames, but got ~e: ~e"
             frames num-read (sf-strerror sndfile)))
    (begin0 (values cblock
                    `((frames     ,frames)
                      (samplerate ,(sf-info-samplerate info))
                      (channels   ,channels)
                      (format     ,(sf-info-format     info))
                      (sections   ,(sf-info-sections   info))
                      ,@strings))
      (unless (= 0 (sf-close sndfile))
        (error 'read-sound-internal "error while closing file: ~e"
               (sf-strerror sndfile))))))


;; split-sound-cblock : cblock ffi-type nat nat -> (listof frame)
;; ... where frame is (listof sample-value)
;; NOT SUPPORTING THE LIST INTERFACE RIGHT NOW (2010-09-24)
#;(define (split-sound-cblock cblock stype frames channels)
  (let* ([data (cblock->list cblock stype (* frames channels))])
    (n-split data channels)))


;; frame-list->cblock : (listof (listof sample)) nat nat ctype -> cblock
(define (frame-list->cblock data frames channels type)
  (cond
   [(null? data) #f]
   ;; planning to get rid of this special-case...:
   [(and (= 1 channels) (not (pair? (car data)))) (list->cblock data type)]
   [else
    (let ([test (lambda (x)
                  (and (list? x) (= channels (length x)) (andmap number? x)))])
      (unless (andmap test data)
        (error 'frame-list->cblock "got a bad frame: ~e"
               (ormap (lambda (x) (and (not (test x)) x)) data))))
    (let ([cblock (malloc (* channels frames) type)]
          [i 0])
      (let loop ([d data])
        (cond [(number? d) (ptr-set! cblock type i d) (set! i (add1 i))]
              [(pair? d) (loop (car d)) (loop (cdr d))]))
      cblock)]))


;; there are some ugly hidden invariants here: what if the sample-type doesn't match
;; what's specified in the format? This is a question about libsndfile, and I should check it out...

;; write-sound-internal/s16vector
(define (write-sound-internal/s16vector file cblock format samplerate frames channels sample-type meta)
  (check-filename-format format file)
  (let* ([writef     (sample-type->writer sample-type)]
         [info       (make-sf-info frames samplerate channels format 1 #f)]
         [_          (unless (sf-format-check info)
                       (error 'write-sound-internal "bad format: ~s" format))]
         [sndfile    (sf-open file 'sfm-write info)]
         [_          (set-meta-strings sndfile meta)]
         [num-write  (writef sndfile (s16vector->cpointer cblock) frames)])
    (unless (= frames num-write)
      (error 'write-sound-internal/s16vector
             "wanted to write ~s frames, but wrote only ~s. ~s"
             frames num-write (sf-strerror sndfile)))
    (unless (= 0 (sf-close sndfile))
      (error 'write-sound-internal/s16vector "failed to close file: ~s" (sf-strerror sndfile)))
    (void)))

;; write-sound-internal/lists : path-string (listof (listof sample)) (listof (list/c symbol? string?)) -> (void)
;; a bunch of guessing happens here...
(define (write-sound-internal/lists file data meta)
  (let* ([frames     (length data)]
         [channels   (if (or (null? data) (not (pair? (car data))))
                       1 ; 1-channel if no data, or data is not made of lists
                       (length (car data)))]
         [stype      (case (sample-type)
                       [(short) _int16] [(int) _int] [(float) _float] [(double) _double*])]
         [cblock     (frame-list->cblock data frames channels stype)]
         [format     (cond [(assq 'format meta) => cadr]
                           [else (guess-format file)])]
         [samplerate (cond [(assq 'samplerate meta) => cadr]
                           [else (default-samplerate)])])
    (write-sound-internal/s16vector file cblock format samplerate frames channels (sample-type) meta)))

(define file-format-table
  '((#rx"\\.aiff?" (aiff  pcm-16  file))
    (#rx"\\.wave?" (wav   pcm-16  file))
    (#rx"\\.au"    (au    pcm-16  file))
    (#rx"\\.snd"   (au    pcm-16  file))
    (#rx"\\.svx"   (svx   pcm-16  file))
    (#rx"\\.paf"   (paf   pcm-16  big))
    (#rx"\\.fap"   (paf   pcm-16  little))
    (#rx"\\.nist"  (nist  pcm-16  little))
    (#rx"\\.ircam" (ircam pcm-16  little))
    (#rx"\\.sf"    (ircam pcm-16  little))
    (#rx"\\.voc"   (voc   pcm-16  file))
    (#rx"\\.w64"   (w64   pcm-16  file))
    (#rx"\\.raw"   (raw   pcm-16  cpu))
    (#rx"\\.mat4"  (mat4  pcm-16  little))
    (#rx"\\.mat5"  (mat5  pcm-16  little))
    (#rx"\\.mat"   (mat4  pcm-16  little))
    (#rx"\\.pvf"   (pvf   pcm-16  file))
    (#rx"\\.sds"   (sds   pcm-16  file))
    (#rx"\\.xi"    (xi    dpcm-16 file))))

(define (guess-format filename)
  (let loop ([xs file-format-table])
    (cond [(null? xs) (default-file-format)]
          [(regexp-match (caar xs) filename) (cadar xs)]
          [else (loop (cdr xs))])))

;; check-filename-format : format filename -> (void)
;; check that the format is compatible with the given pathname; it
;; should not be possible to write, e.g., an aiff file to a filename
;; ending in ".voc". If we don't recognize the filename extension, it's
;; okay.
;; EFFECT: signals an error if the format and filename are incompatible
(define (check-filename-format format filename)
  (match format
    [(list major minor file)
     (let loop ([xs file-format-table])
       (cond [(null? xs) (void)]
             [(regexp-match (caar xs) filename)
              (match (cadar xs)
                [(list major/f minor/f file/f)
                 (unless (eq? major major/f)
                   (error 'check-filename-format
                          "can't use format ~s with filename ~s."
                          format
                          filename))]
                [other
                 (error 'check-filename-format
                        "internal error: unrecognized format format in filename table: ~s" other)])]
             [else (loop (cdr xs))]))]
    [other
     (error 'check-filename-format
            "illegal format format: ~s" other)]))

;; return the reader that corresponds to a given sample-type
(define sample-type->reader
  (match-lambda
    ['short  sf-readf-short]
    ['int    sf-readf-int]
    ['double sf-readf-double]))

;; return the writer that corresponds to a given sample-type
(define sample-type->writer
  (match-lambda
    ['short  sf-writef-short]
    ['int    sf-writef-int]
    ['double sf-writef-double]))

;; return the vector-maker that corresponds to a given sample-type
(define sample-type->vector-maker
  (match-lambda
    ['short  make-s16vector]
    ['int    make-s32vector]
    ['double make-f64vector]))

;; return the cpointer-extractor that corresponds to a given sample-type
(define sample-type->cpointer-extractor
  (match-lambda
    ['short  s16vector->cpointer]
    ['int    s32vector->cpointer]
    ['double flvector->cpointer]))


;; ==================== Exposed Scheme interface ====================

;; types of samples we handle: 'short, 'int, 'double
(provide sample-type)
(define sample-type
  (make-parameter
   'float (lambda (x)
            (if (memq x '(short int double))
              x
              (error 'sample-type "bad or unsupported type: ~s" x)))))



;; TODO: add a parameter that will determine if you get a list, vector or
;; srfi/4-like thing.  possibly also determine if a list/vector gets automatic
;; treatment of 1-channel - not converting it into a list of singleton lists.

(provide default-samplerate)
(define default-samplerate
  (make-parameter
   11025 (lambda (x)
           (if (and (integer? x) (positive? x))
             x (error 'default-samplerate "bad samplerate: ~s" x)))))

(provide default-file-format)
(define default-file-format ; no guard, but should be good for _sf-format
  (make-parameter '(wav pcm-16 file)))

;; read-sound : path-string -> (listof frame)
;; ... where frame is (listof sample)
(provide read-sound)
(define (read-sound file)
  (let*-values ([(data meta) (read-sound-internal file)])
    data))

(provide read-sound*)
(define (read-sound* file)
  (read-sound-internal file #:split #t))

(provide write-sound)
(define (write-sound file data)
  (write-sound-internal/lists file data '()))

(provide write-sound*)
(define (write-sound* file data meta)
  (write-sound-internal/lists file data meta))


;; here's a simplified interface that leaves sounds packed as
;; C data.  It's 2-channel 32-bit float only. Also, it discards
;; all meta-information except length and sample-rate.

(define global-channels 2)

;; read-sound/s16vector : path-string -> (list/c _pointer nat nat)
;; read the file into a buffer, return the data, the number of frames,
;; and the sample rate.
(provide read-sound/s16vector)
(define (read-sound/s16vector file)
  (parameterize ([sample-type 'short])
    (let*-values ([(cblock meta) (read-sound-internal file)])
      (list cblock (cadr (assq 'frames meta)) (cadr (assq 'samplerate meta))))))

;; write-sound/s16vector : _pointer nat nat path-string -> (void)
;; write the cblock sound to the given file as a wav.
(provide write-sound/s16vector)
(define (write-sound/s16vector data sample-rate file)
  (write-sound-internal/s16vector file data '(wav pcm-16 file)
                               sample-rate
                               (/ (s16vector-length data) global-channels)
                               2
                               'short
                               ;; meta-data not supported.
                               '()))


;; test cases for check-filename-format: Commented out until there's a critical mass.

;; okay even though minor and file don't match:
;(check-not-exn (lambda () (check-filename-format '(wav float file) "/tmp/zabaglione.wav")))
;; major doesn't match:
;(check-exn exn:fail? (lambda () (check-filename-format '(wav float file) "/tmp/zabaglione.mat")))
;; bad input format:
;(check-exn exn:fail? (lambda () (check-filename-format '(spam spam spam spam) "/tmp/zabaglione.wav")))
