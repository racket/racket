(module sndfile mzscheme

(require (lib "foreign.ss")) (unsafe!)

(define libsndfile (ffi-lib "libsndfile"))

;; ==================== Types etc ====================

;; This is the scheme represtenatation of the soundfile that is handeled by
;; libsndfile.

;; In libsndfile the sndfile object is represenetd as a pointer when
;; translating scheme->c the struct will just return the pointer when
;; translating from c->scheme, ie. creating the object in scheme it will be
;; wraped by an object finilazer that uses the libsndfile fuction sf_close that
;; return a 0 upon sucsessfull termination or an error.
(define-struct sndfile (ptr info))
(define _sndfile
  (make-ctype _pointer sndfile-ptr
    (lambda (p)
      (if p
        (make-sndfile p #f)
        (error '_sndfile "got a NULL pointer (bad info?)")))))

;; sf_count_t is a count type that depends on the operating system however it
;; seems to be a long int for all teh supported ones so in this scase we just
;; define it as two ints.
(define _sf-count-t _int64)

(define _sf-mode
  (_bitmask '(sfm-read  = #x10
              sfm-write = #x20
              sfm-rdwrt = #x30)))

(define str-types '(title copyright software artist comment date))
(define _sf-str-type (_enum (cons "dummy" str-types))) ; begins from 1

(define _sf-format
  (let ([majors ; Major formats
         '((wav       #x010000)   ; Microsoft WAV format (little endian)
           (aiff      #x020000)   ; Apple/SGI AIFF format (big endian)
           (au        #x030000)   ; Sun/NeXT AU format (big endian)
           (raw       #x040000)   ; RAW PCM data
           (paf       #x050000)   ; Ensoniq PARIS file format
           (svx       #x060000)   ; Amiga IFF / SVX8 / SV16 format
           (nist      #x070000)   ; Sphere NIST format
           (voc       #x080000)   ; VOC files
           (ircam     #x0A0000)   ; Berkeley/IRCAM/CARL
           (w64       #x0B0000)   ; Sonic Foundry's 64 bit RIFF/WAV
           (mat4      #x0C0000)   ; Matlab (tm) V4.2 / GNU Octave 2.0
           (mat5      #x0D0000)   ; Matlab (tm) V5.0 / GNU Octave 2.1
           (pvf       #x0E0000)   ; Portable Voice Format
           (xi        #x0F0000)   ; Fasttracker 2 Extended Instrument
           (htk       #x100000)   ; HMM Tool Kit format
           (sds       #x110000)   ; Midi Sample Dump Standard
           (avr       #x120000)   ; Audio Visual Research
           (wavex     #x130000)   ; MS WAVE with WAVEFORMATEX
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

(define-syntax defsndfile
  (syntax-rules (:)
    [(_ name : type ...)
     (define name
       (get-ffi-obj (regexp-replaces 'name '((#rx"-" "_")))
                    libsndfile (_fun type ...)))]))

(define (n-split! l n)
  (let ([r '()])
    (let loop ([i 0] [l (reverse! l)])
      (unless (null? l)
        (when (zero? i) (set! r (cons '() r)))
        (let ([next (cdr l)])
          (set-cdr! l (car r))
          (set-car! r l)
          (loop (modulo (add1 i) n) next))))
    r))

;; ==================== sndfile API ====================

(defsndfile sf-close : _sndfile -> _int)

(defsndfile sf-open : (path mode . info) ::
  (path : _file)
  (mode : _sf-mode)
  (info : _sf-info-pointer
        = (if (pair? info) (car info) (make-sf-info 0 0 0 '() 0 #f)))
  -> (sf : _sndfile)
  -> (begin (set-sndfile-info! sf info) sf))

(defsndfile sf-format-check  : _sf-info-pointer -> _bool)

(defsndfile sf-readf-short   : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-readf-int     : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-readf-double  : _sndfile _pointer _sf-count-t -> _sf-count-t)

(defsndfile sf-writef-short  : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-writef-int    : _sndfile _pointer _sf-count-t -> _sf-count-t)
(defsndfile sf-writef-double : _sndfile _pointer _sf-count-t -> _sf-count-t)

(defsndfile sf-get-string    : _sndfile _sf-str-type -> _string)
(defsndfile sf-set-string    : _sndfile _sf-str-type _string -> _bool)

;; ==================== Utilities for the Scheme interface ====================

(define (get-strings sndfile)
  (let loop ([sts str-types] [r '()])
    (cond [(null? sts) (reverse! r)]
          [(sf-get-string sndfile (car sts)) =>
           (lambda (x)
             (loop (cdr sts) (cons (list (car sts) (string-copy x)) r)))]
          [else (loop (cdr sts) r)])))

(define (set-strings sndfile meta)
  (for-each (lambda (st)
              (cond [(assq st meta) =>
                     (lambda (x) (sf-set-string sndfile st (cadr x)))]))
            str-types))

(define (read-sound-internal file meta?)
  (let* ([sndfile  (sf-open file 'sfm-read)]
         [strings  (and meta? (get-strings sndfile))]
         [info     (sndfile-info sndfile)]
         [frames   (sf-info-frames info)]
         [channels (sf-info-channels info)]
         [stype    (case (sample-type)
                     [(short) _int16] [(int) _int] [(float) _double*])]
         [readf    (case (sample-type)
                     [(short) sf-readf-short]
                     [(int) sf-readf-int]
                     [(float) sf-readf-double])]
         [cblock   (malloc (* frames channels) stype)]
         [num-read (readf sndfile cblock frames)]
         [data     (cblock->list cblock stype (* num-read channels))]
         [data     (if (> channels 1) (n-split! data channels) data)])
    (unless (= frames num-read)
      (error 'read-sound-internal
             "wanted ~s frames, but got ~s" frames num-read))
    (begin0 (if meta?
              (values data `((frames     ,frames)
                             (samplerate ,(sf-info-samplerate info))
                             (channels   ,channels)
                             (format     ,(sf-info-format     info))
                             (sections   ,(sf-info-sections   info))
                             ,@strings))
              data)
      (sf-close sndfile))))

(define (frame-list->cblock data frames channels type)
  (cond
   [(null? data) #f]
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

(define (write-sound-internal file data meta)
  (let* ([frames     (length data)]
         [channels   (if (or (null? data) (not (pair? (car data))))
                       1 ; 1-channel if no data, or data is not made of lists
                       (length (car data)))]
         [stype      (case (sample-type)
                       [(short) _int16] [(int) _int] [(float) _double*])]
         [writef     (case (sample-type)
                       [(short) sf-writef-short]
                       [(int) sf-writef-int]
                       [(float) sf-writef-double])]
         [cblock     (frame-list->cblock data frames channels stype)]
         [format     (cond [(assq 'format meta) => cadr]
                           [else (guess-format file)])]
         [samplerate (cond [(assq 'samplerate meta) => cadr]
                           [else (default-samplerate)])]
         [info       (make-sf-info frames samplerate channels format 1 #f)]
         [_          (unless (sf-format-check info)
                       (error 'write-sound-internal "bad format: ~s" format))]
         [sndfile    (sf-open file 'sfm-write info)]
         [_          (set-strings sndfile meta)]
         [num-write  (writef sndfile cblock frames)])
    (unless (= frames num-write)
      (error 'write-sound-internal
             "wanted to write ~s frames, but wrote only ~s" frames num-write))
    (sf-close sndfile)
    (void)))

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

;; ==================== Exposed Scheme interface ====================

;; types of samples we handle: 'short, 'int, or 'float
(provide sample-type)
(define sample-type
  (make-parameter
   'float (lambda (x)
            (if (memq x '(short int float))
              x (error 'sample-type "bad type: ~s" x)))))

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

(provide read-sound)
(define (read-sound file)
  (read-sound-internal file #f))

(provide read-sound*)
(define (read-sound* file)
  (read-sound-internal file #t))

(provide write-sound)
(define (write-sound file data)
  (write-sound-internal file data '()))

;; meta is used only for samplerate & format
(provide write-sound*)
(define (write-sound* file data meta)
  (write-sound-internal file data meta))

)
