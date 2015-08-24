#lang racket/base
(require racket/contract
         compiler/private/pe-rsrc)

(provide ico?
         (contract-out
          [ico-width (ico? . -> . exact-positive-integer?)]
          [ico-height (ico? . -> . exact-positive-integer?)]
          [ico-depth (ico? . -> . (or/c 1 2 4 8 16 24 32))]
          [ico-format (ico? . -> . (or/c 'bmp 'png))]
          
          [read-icos ((or/c path-string? input-port?) . -> . (listof ico?))]
          [read-icos-from-exe ((or/c path-string? input-port?) . -> . (listof ico?))]

          [write-icos ([(listof ico?) (or/c path-string? output-port?)]
                       [#:exists (or/c 'error 'append 'update 'can-update
                                       'replace 'truncate
                                       'must-truncate 'truncate/replace)]
                       . ->* .
                       void?)]
          [replace-icos ((listof ico?) path-string? . -> . void?)]
          [replace-all-icos ((listof ico?) path-string? . -> . void?)]
          
          [ico->argb (ico? . -> . bytes?)]
          [ico->png-bytes (ico? . -> . bytes?)]
          [argb->ico ([(integer-in 1 256) (integer-in 1 256) bytes? ]
                      [#:depth (one-of/c 1 2 4 8 24 32)]
                      . ->* . 
                      ico?)]
          [png-bytes->ico ([bytes?]
                           []
                           . ->* . 
                           ico?)]))

;; parse-ico build-ico

(define (byte->integer p)
  (read-byte p))
(define (word->integer p)
  (integer-bytes->integer (read-bytes 2 p) #f #f))
(define (dword->integer p)
  (integer-bytes->integer (read-bytes 4 p) #f #f))

;; The 0 added in the alpha position apparently means "ignore the alpha
;;  and use the mask, instead"
(define (3/2word->integer p)
  (integer-bytes->integer (bytes-append (read-bytes 3 p) #"\0") #f #f))

(define (integer->word i p)
  (display (integer->integer-bytes i 2 #f #f) p))
(define (integer->dword i p)
  (display (integer->integer-bytes i 4 #f #f) p))
(define (integer->3/2word i p)
  (display (subbytes (integer->integer-bytes i 4 #f #f) 0 3) p))

(define-struct ico (desc data) #:mutable)
;; desc is (list width height colors 0 planes bitcount)
;; data is (cons pos bytes)

(define (ico-width i) (let ([v (car (ico-desc i))])
                        (if (= v 0)
                            (if (eq? (ico-format i) 'bmp)
                                256
                                (ico-png-width i))
                            v)))
(define (ico-height i) (let ([v (cadr (ico-desc i))])
                         (if (= v 0)
                             (if (eq? (ico-format i) 'bmp)
                                 256
                                (ico-png-height i))
                             v)))
(define (ico-depth i) 
  (let ([cols (caddr (ico-desc i))])
    (if (zero? cols)
        (list-ref (ico-desc i) 5)
        (integer-length (sub1 cols)))))
(define (ico-colors i) (num-colors (ico-desc i)))

(define (ico-format i)
  (define bstr (cdr (ico-data i)))
  (define tag (subbytes bstr 0 2))
  (cond
   [(and ((bytes-length bstr) . > . 4)
         (equal? (subbytes bstr 0 4) #"\211PNG"))
    'png]
   [else
    ;; Asume BMP
    'bmp]))

(define (ico-png-width i)
  (png-width (cdr (ico-data i))))
(define (png-width bstr)
  (integer-bytes->integer (subbytes bstr 16 20) #f #t))
(define (ico-png-height i)
  (png-height (cdr (ico-data i))))
(define (png-height bstr)
  (integer-bytes->integer (subbytes bstr 20 24) #f #t))

(define (num-colors l)
  (let ([n (caddr l)])
    (if (zero? n)
        (arithmetic-shift 1 (list-ref l 5))
        n)))

(define (replace-icos ico-icos exe-file)
  (let ([exe-icos (read-icos-from-exe exe-file)])
    (let ([p (open-output-file exe-file #:exists 'update)])
      (dynamic-wind
       void
       (lambda ()
         (for-each (lambda (exe-ico)
                     (let ([best-ico-ico
                            ;; Find exact match?
                            (ormap (lambda (ico-ico)
                                     (let ([le (ico-desc exe-ico)]
                                           [li (ico-desc ico-ico)])
                                       (and (= (ico-width exe-ico) (ico-width ico-ico))
                                            (= (ico-height exe-ico) (ico-height ico-ico))
                                            (= (num-colors li) (num-colors le))
                                            (= (bytes-length (cdr (ico-data exe-ico)))
                                               (bytes-length (cdr (ico-data ico-ico))))
                                            ico-ico)))
                                   ico-icos)])
                       (let ([ico-ico (or best-ico-ico
                                          ;; Look for a conversion, if we
                                          ;; need a 16x16, 32x32, or 48x48
                                          ;; ico
                                          (and
                                           (= (ico-width exe-ico)
                                              (ico-height exe-ico))
                                           (memq (car (ico-desc exe-ico))
                                                 '(16 32 48))
                                           (let ([biggest-colorest #f])
                                             (for-each 
                                              (lambda (ico-ico)
                                                (let ([w (ico-width ico-ico)]
                                                      [exew (ico-width exe-ico)])
                                                  (when (and
                                                         (= w (ico-width ico-ico))
                                                         (memq w '(16 32 48))
                                                         (or
                                                          (not biggest-colorest)
                                                          (and (= w exew)
                                                               (not (= exew (ico-width biggest-colorest))))
                                                          (and (= w exew)
                                                               (> (num-colors (ico-desc ico-ico))
                                                                  (num-colors (ico-desc biggest-colorest))))
                                                          (and (not (= exew (ico-width biggest-colorest)))
                                                               (or (> w (ico-width biggest-colorest))
                                                                   (> (num-colors (ico-desc ico-ico))
                                                                      (num-colors (ico-desc biggest-colorest)))))))
                                                    (set! biggest-colorest ico-ico))))
                                              ico-icos)
                                             (and
                                              biggest-colorest
                                              ;; Convert...
                                              (let* ([src-size (ico-width biggest-colorest)]
                                                     [dest-size (ico-width exe-ico)]
                                                     [src (parse-ico biggest-colorest)]
                                                     [image (list-ref src 3)]
                                                     [mask (list-ref src 4)]
                                                     [has-alpha? (<= 256 (num-colors (ico-desc biggest-colorest)))])
                                                (if (= src-size dest-size)
                                                    (build-ico exe-ico
                                                               (if has-alpha?
                                                                   image
                                                                   (mask->alpha image mask))
                                                               mask
                                                               #t)
                                                    (let ([cvt
                                                           (cond
                                                            [(and (= src-size 32) (= dest-size 16))
                                                             (lambda (i) (48->16 (32->48 i)))]
                                                            [(and (= src-size 32) (= dest-size 48)) 
                                                             32->48]
                                                            [(and (= src-size 48) (= dest-size 16)) 
                                                             48->16]
                                                            [(and (= src-size 48) (= dest-size 32))
                                                             48->32]
                                                            [(and (= src-size 16) (= dest-size 32)) 
                                                             16->32]
                                                            [(and (= src-size 16) (= dest-size 48)) 
                                                             (lambda (i) (32->48 (16->32 i)))])])
                                                      (let ([mask (cvt mask)])
                                                        (build-ico exe-ico 
                                                                   (if has-alpha?
                                                                       (cvt image)
                                                                       (mask->alpha (cvt image) mask))
                                                                   mask
                                                                   #t)))))))))])
                         (unless ico-ico (log-error "no icon conversion available to ~a" (ico-desc exe-ico)))
                         (when ico-ico
                           (file-position p (let ([d (car (ico-data exe-ico))])
                                              (if (vector? d)
                                                  (vector-ref d 0)
                                                  d)))
                           (display (cdr (ico-data ico-ico)) p)))))
                   exe-icos))
       (lambda () (close-output-port p))))))

(define (replace-all-icos ico-list exe-file)
  (define-values (pe rsrcs)
    (call-with-input-file*
     exe-file
     read-pe+resources))
  (define-values (type name language icos file-pos)
    (resource-ref/path rsrcs 14 #f #f))
  (define old-icos
    (if icos
        (get-icos (open-input-bytes icos) rsrcs)
        null))
  (define (ico-res-type i) (vector-ref (car (ico-data i)) 1))
  (define (ico-res-name i) (vector-ref (car (ico-data i)) 2))
  (define (ico-res-language i) (vector-ref (car (ico-data i)) 3))
  (define cleaned-rsrcs
    (for/fold ([rsrcs rsrcs]) ([old-i (in-list old-icos)])
      (resource-remove rsrcs
                       (ico-res-type old-i)
                       (ico-res-name old-i)
                       (ico-res-language old-i))))
  ;; Replace individual icons where size and depth match
  (define-values (new-rsrcs named-icos)
    (for/fold ([rsrcs cleaned-rsrcs] [named-icos null]) ([i (in-list ico-list)])
      (define old-i (for/or ([old-i (in-list old-icos)])
                      (and (= (ico-width i) (ico-width old-i))
                           (= (ico-height i) (ico-height old-i))
                           (= (ico-depth i) (ico-depth old-i))
                           old-i)))
      (define name
        (cond
         [old-i (ico-res-name old-i)]
         [else
          ;; Generate next unused id:
          (let loop ([id 1])
            (if (resource-ref rsrcs 3 id 1033)
                (loop (add1 id))
                id))]))
      (values (resource-set rsrcs
                            3
                            name
                            1033
                            (cdr (ico-data i)))
              (cons (ico (ico-desc i)
                         (cons (vector #f 4 name 1033)
                               (cdr (ico-data i))))
                    named-icos))))
  ;; Update ico:
  (define ready-rsrcs
    (resource-set new-rsrcs type name language (make-icos-header named-icos)))
  ;; Write new resources:
  (update-resources exe-file pe ready-rsrcs))

(define (make-icos-header icos)
  (define o (open-output-bytes))
  (integer->word 0 o)
  (integer->word 1 o)
  (integer->word (length icos) o)
  (for ([i (in-list icos)])
    (define desc (ico-desc i))
    (write-byte (list-ref desc 0) o)
    (write-byte (list-ref desc 1) o)
    (write-byte (list-ref desc 2) o)
    (write-byte (list-ref desc 3) o)
    (integer->word (list-ref desc 4) o)
    (integer->word (list-ref desc 5) o)
    (integer->dword (bytes-length (cdr (ico-data i))) o)
    (integer->word (vector-ref (car (ico-data i)) 2) o))
  (get-output-bytes o))

;; ------------------------------
;;  Image parsing
;; ------------------------------

(define (get-icos file rsrcs)
  (let ([p (if (input-port? file)
               file
               (open-input-file file))])
    (dynamic-wind
     void
     (lambda ()
       (unless (= 0 (word->integer p))
         (error 'get-icos "~a doesn't start with 0" file))
       (unless (= 1 (word->integer p))
         (error "type isn't 1"))
       (let ([cnt (word->integer p)])
         (let ([icos (let loop ([i 0])
                       (if (= i cnt)
                           null
                           (cons
                            (make-ico
                             (list (byte->integer p)   ; w
                                   (byte->integer p)   ; h
                                   (byte->integer p)   ; colors
                                   (byte->integer p)   ; 0
                                   (word->integer p)   ; planes
                                   (word->integer p))  ; bitcount
                             (list (dword->integer p)  ; bytes
                                   ((if rsrcs          ; where or icon id
                                        word->integer 
                                        dword->integer)
                                    p)))
                            (loop (add1 i)))))])
           ;; (printf "~a\n" icos)
           (for-each (lambda (ico)
                       (set-ico-data!
                        ico
                        (let ([size (car (ico-data ico))]
                              [where (cadr (ico-data ico))])
                          (cond
                           [rsrcs
                            (define-values (type name lang bstr file-pos)
                              (resource-ref/path rsrcs 3 where #f))
                            (cons (vector file-pos type name lang)
                                  bstr)]
                           [else
                            (file-position p where)
                            (cons where
                                  (read-bytes size p))])))
                       ;; If colors, planes, and bitcount are all 0,
                       ;;  get the info from the DIB data
                       (let ([desc (ico-desc ico)])
                         (when (and (zero? (list-ref desc 2))
                                    (zero? (list-ref desc 4))
                                    (zero? (list-ref desc 5)))
                           (let ([bi (bitmapinfo ico)])
                             (set-ico-desc! ico
                                            (list*
                                             (list-ref desc 0)
                                             (list-ref desc 1)
                                             (list-ref desc 2)
                                             (list-ref desc 3)
                                             (list-ref bi 3)
                                             (list-ref bi 4)
                                             (list-tail desc 6)))))))
                     icos)
           icos)))
     (lambda ()
       (when (path-string? file)
         (close-input-port p))))))

(define (bitmapinfo ico)
  (let ([p (open-input-bytes (cdr (ico-data ico)))])
    (list (dword->integer p)    ; size == 40 in practice
          (dword->integer p)    ; width
          (dword->integer p)    ; height
          (word->integer p)     ; planes
          (word->integer p)     ; bitcount
          (dword->integer p)    ; compression == 0
          (dword->integer p)    ; size image
          (dword->integer p)    ; x pixels per meter == 0
          (dword->integer p)    ; y pixels per meter == 0
          (dword->integer p)    ; used == 0
          (dword->integer p)))) ; important == 0

;; Assumes that bits-per-pixel is 1, 2, 4, 8, 16, 24, or 32.
;; Also assumes that (bits-per-pixel * width) is a multiple of 8.
(define (parse-dib ico)
  (let* ([bi (bitmapinfo ico)]
         [header-size (list-ref bi 0)]
         [num-colors (caddr (ico-desc ico))]
         [w (list-ref bi 1)]
         [h (/ (list-ref bi 2) 2)]
         [bits-per-pixel (list-ref bi 4)])
    (let ([p (open-input-bytes (cdr (ico-data ico)))])
      ;; Skip header
      (read-bytes header-size p)
      (let* ([read-n
              (lambda (n read-one combine)
                (let loop ([i n][r null])
                  (if (= i 0)
                      (reverse r)
                      (loop (sub1 i)
                            (combine (read-one p) r)))))]
             [read-lines 
              (lambda (w h read-one combine)
                (if (zero? (modulo w 4))
                    (read-n (* w h) read-one combine)
                    (let loop ([h h])
                      (if (zero? h)
                          null
                          (append (read-n w read-one combine)
                                  (begin
                                    ;; pad line to dword:
                                    (read-n (- 4 (modulo w 4)) byte->integer cons)
                                    ;; read next line:
                                    (loop (sub1 h))))))))]
             [split-bits (lambda (b)
                           (list
                            (bitwise-and b 1)
                            (arithmetic-shift (bitwise-and b 2) -1)
                            (arithmetic-shift (bitwise-and b 4) -2)
                            (arithmetic-shift (bitwise-and b 8) -3)
                            (arithmetic-shift (bitwise-and b 16) -4)
                            (arithmetic-shift (bitwise-and b 32) -5)
                            (arithmetic-shift (bitwise-and b 64) -6)
                            (arithmetic-shift (bitwise-and b 128) -7)))])
        (let ([main-image
               (cond
                [(= bits-per-pixel 32)
                 ;; RGB mode:
                 (read-n (* w h) dword->integer cons)]
                [(= bits-per-pixel 24)
                 ;; RGB mode:
                 (read-n (* w h) 3/2word->integer cons)]
                [else
                 ;; Index mode:
                 (let ([color-table (list->vector
                                     (read-n (if (zero? num-colors)
                                                 (arithmetic-shift 1 bits-per-pixel)
                                                 num-colors)
                                             dword->integer cons))]
                       [image (read-lines (/ w (/ 8 bits-per-pixel))
                                          h
                                          (lambda (p)
                                            (let ([b (byte->integer p)])
                                              (case bits-per-pixel
                                                [(1) (split-bits b)]
                                                [(2)
                                                 (list
                                                  (bitwise-and b 3)
                                                  (arithmetic-shift (bitwise-and b 12) -2)
                                                  (arithmetic-shift (bitwise-and b 48) -4)
                                                  (arithmetic-shift (bitwise-and b 192) -6))]
                                                [(4)
                                                 (list
                                                  (bitwise-and b 15)
                                                  (arithmetic-shift (bitwise-and b 240) -4))]
                                                [(8) (list b)])))
                                          append)])
                   (map (lambda (i) (vector-ref color-table i)) image))])])
          (let ([mask (read-lines (/ w 8)
                                  h
                                  (lambda (p) (split-bits (byte->integer p)))
                                  append)])
            ;; The following check is commented out, because the "start.ico" file
            ;; for the GUI package manager has an extra 128 0s --- and I don't know
            ;; why. Maybe extra padding is allowed.
            #;
            (unless (eof-object? (read-byte p))
              (error 'parse-dib "not extactly at end"))
            (list main-image mask)))))))

;; rgb->indexed
;;  The color-reduction strategy isn't great, and because it
;;  depends on hash-table order, it's non-deterministic in
;;  principle. But the actual hash-table implementation is
;;  deterministic, of course. Also, the re-ordering of the
;;  image via the hash tables tends to produce better
;;  (pseudo-random) representatives of the image for colors.
(define (rgb->indexed image num-colors)
  (let ([image (map (lambda (i) (bitwise-and #xFFFFFF i)) image)] ; drop alphas, if any
        [table (make-vector num-colors 0)]
        [ht (make-hash)]
        [map-ht (make-hash)]
        [color-dist (lambda (a b)
                      (sqrt (+ (expt (- (bitwise-and #xFF a)
                                        (bitwise-and #xFF b))
                                     2)
                               (expt (- (arithmetic-shift (bitwise-and #xFF00 a) -8)
                                        (arithmetic-shift (bitwise-and #xFF00 b) -8))
                                     2)
                               (expt (- (arithmetic-shift (bitwise-and #xFF0000 a) -16)
                                        (arithmetic-shift (bitwise-and #xFF0000 b) -16))
                                     2))))])
    (for-each (lambda (c)
                (hash-set! 
                 ht 
                 c
                 (add1
                  (hash-ref ht c 0))))
              image)
    (let ([kv-sorted (sort (hash-map ht cons)
                           (lambda (a b) (< (cdr a) (cdr b))))])
      (let ([n 0])
        (for-each (lambda (kv)
                    (let ([key (car kv)])
                      (let ([n (if (< n (sub1 num-colors))
                                   n
                                   ;; Find closest match:
                                   (let ([n 0])
                                     (let loop ([i 1])
                                       (unless (= i num-colors)
                                         (when (< (color-dist key (vector-ref table i))
                                                  (color-dist key (vector-ref table n)))
                                           (set! n i))
                                         (loop (add1 i))))
                                     n))])
                        (vector-set! table n key)
                        (hash-set! map-ht key n))
                      (when (< n (sub1 num-colors))
                        (set! n (add1 n)))))
                  kv-sorted)))
    (values (vector->list table)
            (map (lambda (c) (hash-ref map-ht c)) image))))

;; Assumes that bits-per-pixel is 1, 2, 4, 8, or 32.
;; Also assumes that (bits-per-pixel * width) is a multiple of 8.
(define (build-dib ico image mask check?)
  (let* ([bi (bitmapinfo ico)]
         [header-size (list-ref bi 0)]
         [num-colors (caddr (ico-desc ico))]
         [w (list-ref bi 1)]
         [h (/ (list-ref bi 2) 2)]
         [bits-per-pixel (list-ref bi 4)])
    (let ([orig-p (open-input-bytes (cdr (ico-data ico)))]
          [result-p (open-output-bytes)])
      ;; Copy header:
      (display (read-bytes header-size orig-p) result-p)
      (let ([get-lines (lambda (image bits-per-pixel)
                         (map (lambda (line)
                                ;; pad line to dword boundary
                                (let ([line-bytes (/ (* w bits-per-pixel) 8)])
                                  (if (zero? (modulo line-bytes 4))
                                      line
                                      (append line 
                                              (vector->list
                                               (make-vector (* (- 4 (modulo line-bytes 4)) 
                                                               (/ 8 bits-per-pixel))
                                                            0))))))
                              ;; break out lines
                              (let loop ([l image])
                                (if (null? l)
                                    null
                                    (cons (let loop ([l l][i 0])
                                            (if (= i w)
                                                null
                                                (cons (car l) (loop (cdr l) (add1 i)))))
                                          (loop (list-tail l w)))))))]
            [bits->dwords (lambda (l bpp)
                            (let ([chunk-size (/ 32 bpp)]
                                  [1byte (lambda (l)
                                           (bitwise-ior
                                            (arithmetic-shift (list-ref l 0) 7)
                                            (arithmetic-shift (list-ref l 1) 6)
                                            (arithmetic-shift (list-ref l 2) 5)
                                            (arithmetic-shift (list-ref l 3) 4)
                                            (arithmetic-shift (list-ref l 4) 3)
                                            (arithmetic-shift (list-ref l 5) 2)
                                            (arithmetic-shift (list-ref l 6) 1)
                                            (arithmetic-shift (list-ref l 7) 0)))]
                                  [2byte (lambda (l)
                                           (bitwise-ior
                                            (arithmetic-shift (list-ref l 0) 6)
                                            (arithmetic-shift (list-ref l 1) 4)
                                            (arithmetic-shift (list-ref l 2) 2)
                                            (arithmetic-shift (list-ref l 3) 0)))]
                                  [4byte (lambda (l)
                                           (bitwise-ior
                                            (arithmetic-shift (list-ref l 0) 4)
                                            (arithmetic-shift (list-ref l 1) 0)))])
                              (let loop ([l l])
                                (if (null? l)
                                    null
                                    (cons (case bpp
                                            [(1) (bitwise-ior
                                                  (arithmetic-shift (1byte (list-tail l 0)) 0)
                                                  (arithmetic-shift (1byte (list-tail l 8)) 8)
                                                  (arithmetic-shift (1byte (list-tail l 16)) 16)
                                                  (arithmetic-shift (1byte (list-tail l 24)) 24))]
                                            [(2) (bitwise-ior
                                                  (2byte l)
                                                  (arithmetic-shift (2byte (list-tail l 4)) 8)
                                                  (arithmetic-shift (2byte (list-tail l 8)) 16)
                                                  (arithmetic-shift (2byte (list-tail l 12)) 24))]
                                            [(4) (bitwise-ior
                                                  (4byte l)
                                                  (arithmetic-shift (4byte (list-tail l 2)) 8)
                                                  (arithmetic-shift (4byte (list-tail l 4)) 16)
                                                  (arithmetic-shift (4byte (list-tail l 6)) 24))]
                                            [(8) (bitwise-ior
                                                  (car l)
                                                  (arithmetic-shift (list-ref l 1) 8)
                                                  (arithmetic-shift (list-ref l 2) 16)
                                                  (arithmetic-shift (list-ref l 3) 24))])
                                          (loop (list-tail l chunk-size)))))))])
        (cond
         [(= bits-per-pixel 32)
          (for-each (lambda (col) (integer->dword col result-p))
                    image)]
         [(= bits-per-pixel 24)
          (for-each (lambda (col) (integer->3/2word col result-p))
                    image)]
         [else
          (let-values ([(colors indexed-image) (rgb->indexed image (arithmetic-shift 1 bits-per-pixel))])
            ;; color table
            (for-each (lambda (col) (integer->dword col result-p))
                      colors)
            (let* ([lines (get-lines indexed-image bits-per-pixel)]
                   [dwords (apply append (map (lambda (l) (bits->dwords l bits-per-pixel)) 
                                              lines))])
              (for-each (lambda (col) (integer->dword col result-p))
                        dwords)))])
        (let* ([lines (get-lines mask 1)]
               [dwords (apply append (map (lambda (l) (bits->dwords l 1)) lines))])
          (for-each (lambda (col) (integer->dword col result-p))
                    dwords))
        (let ([s (get-output-bytes result-p)])
          (when check?
            (unless (= (bytes-length s) (bytes-length (cdr (ico-data ico))))
              (error 'build-dib "bad result size ~a != ~a"
                     (bytes-length s) (bytes-length (cdr (ico-data ico))))))
          s)))))

(define (parse-ico ico)
  (let ([image (parse-dib ico)])
    (list (ico-width ico)
          (ico-height ico)
          (ico-colors ico)
          (car image)     ; list of image pixels
          (cadr image)))) ; list of mask pixels

(define (ico->argb ico)
  (unless (eq? 'bmp (ico-format ico))
    (error 'ico->argb "icon not in BMP format"))
  (let* ([image (parse-ico ico)]
         [pixels (list-ref image 3)]
         [len (length pixels)]
         [bstr (make-bytes (* 4 len))]
         [w (ico-width ico)]
         [h (ico-height ico)]
         [has-alpha? (= 32 (ico-depth ico))])
    (for ([p (in-list pixels)]
          [m (in-list (list-ref image 4))]
          [i (in-naturals)])
      (let* ([y (- h (quotient i w) 1)]
             [x (modulo i w)]
             [i (+ x (* w y))])
        (bytes-set! bstr (+ 0 (* i 4)) (if has-alpha?
                                           (bitwise-and #xff (arithmetic-shift p -24))
                                           (if (zero? m) 255 0)))
        (bytes-set! bstr (+ 1 (* i 4)) (bitwise-and #xff (arithmetic-shift p -16)))
        (bytes-set! bstr (+ 2 (* i 4)) (bitwise-and #xff (arithmetic-shift p -8)))
        (bytes-set! bstr (+ 3 (* i 4)) (bitwise-and #xff p))))
    bstr))

(define (ico->png-bytes ico)
  (unless (eq? 'png (ico-format ico))
    (error 'ico->argb "icon not in PNG format"))
  (cdr (ico-data ico)))

(define (build-ico base-ico image mask check?)
  (make-ico (ico-desc base-ico)
            (cons (car (ico-data base-ico))
                  (build-dib base-ico image mask check?))))

(define (read-icos ico-file)
  (get-icos ico-file #f))

(define (read-icos-from-exe exe-file)
  (define-values (pe rsrcs)
    (call-with-input-file*
     exe-file
     read-pe+resources))
  (define icos (resource-ref rsrcs 14 #f #f))
  (get-icos (open-input-bytes icos) rsrcs))

(define (write-header w h depth o)
  (integer->dword 40 o) ; size
  (integer->dword w o)  ; width
  (integer->dword (* 2 h) o)  ; height
  (integer->word 1 o)   ; planes
  (integer->word depth o)  ; bitcount
  (integer->dword 0 o)  ; compression
  (integer->dword 0 o)  ; size image
  (integer->dword 0 o)  ; x pixels per meter
  (integer->dword 0 o)  ; y pixels per meter
  (integer->dword 0 o)  ; used
  (integer->dword 0 o))  ; important

(define (png-bytes->ico bstr)
  (define (256+->0 v) (if (v . >= . 256) 0 v))
  (ico (list (256+->0 (png-width bstr)) (256+->0 (png-height bstr)) 0 0 1 32)
       (cons #f bstr)))

(define (argb->ico w h argb #:depth [depth 32])
  (let ([o (open-output-bytes)])
    (write-header w h 32 o)
    ;; Got ARGB, need BGRA
    (let* ([flip-pixels (lambda (s)
                          (let ([s (bytes-copy s)])
                            (let loop ([p 0])
                              (unless (= p (bytes-length s))
                                (let ([a (bytes-ref s p)]
                                      [r (bytes-ref s (+ p 1))]
                                      [g (bytes-ref s (+ p 2))]
                                      [b (bytes-ref s (+ p 3))])
                                  (bytes-set! s p b)
                                  (bytes-set! s (+ p 1) g)
                                  (bytes-set! s (+ p 2) r)
                                  (bytes-set! s (+ p 3) a)
                                  (loop (+ p 4)))))
                            s))]
           [rgba (flip-pixels argb)]
           [row-size (if (zero? (modulo w 32))
                         w
                         (+ w (- 32 (remainder w 32))))]
           [mask (make-bytes (* h row-size 1/8) 0)])
      (let loop ([i (* w h 4)])
        (unless (zero? i)
          (let ([alpha (bytes-ref rgba (- i 1))])
            (when (< alpha 10)
              ;; white mask -> zero alpha; add white pixel to mask
              (bytes-set! rgba (- i 1) 0)
              (let ([pos (+ (* (quotient (sub1 (/ i 4)) w) row-size)
                            (remainder (sub1 (/ i 4)) w))])
                (bytes-set! mask 
                            (quotient pos 8)
                            (bitwise-ior
                             (arithmetic-shift 1 (- 7 (remainder pos 8)))
                             (bytes-ref mask (quotient pos 8)))))))
          (loop (- i 4))))
      ;; Windows icos are upside-down:
      (let ([flip (lambda (str row-width)
                    (apply
                     bytes-append
                     (reverse
                      (let loop ([pos 0])
                        (if (= pos (bytes-length str))
                            null
                            (cons (subbytes str pos (+ pos row-width))
                                  (loop (+ pos row-width))))))))])                      
        (display (flip rgba (* w 4)) o)
        (display (flip mask (/ row-size 8)) o))
      (define (256->0 v) (if (= v 256) 0 v))
      (let ([ico (make-ico (list (256->0 w) (256->0 h) 0 0 1 32)
                           (cons 0 (get-output-bytes o)))])
        (cond
         [(= depth 32) ico]
         [else (let ([o (open-output-bytes)])
                 (write-header w h depth o)
                 (define image (parse-dib ico))
                 (build-ico (make-ico (list (256->0 w) (256->0 h) 
                                            (if (depth . <= . 8)
                                                (256->0 (expt 2 depth))
                                                0)
                                            0 1 depth)
                                      (cons 0 (get-output-bytes o)))
                            (car image)
                            (cadr image)
                            #f))])))))
                            

(define (write-icos icos file #:exists [exists 'error])
  (let ([p #f])
    (dynamic-wind
     (lambda ()
       (set! p (if (output-port? file)
                   file
                   (open-output-file file #:exists exists))))
     (lambda ()
       (integer->word 0 p)
       (integer->word 1 p) ; 1 = icon
       (define count (length icos))
       (integer->word count p)
       (for/fold ([offset (+ 6 (* 16 count))]) ([i (in-list icos)])
         (define size (bytes-length (cdr (ico-data i))))
         (write-byte (car (ico-desc i)) p)
         (write-byte (cadr (ico-desc i)) p)
         (write-byte (list-ref (ico-desc i) 2) p)
         (write-byte 0 p)
         (integer->word (list-ref (ico-desc i) 4) p)
         (integer->word (list-ref (ico-desc i) 5) p)
         (integer->dword size p)
         (integer->dword offset p)
         (+ offset size))
       (for ([i (in-list icos)])
         (write-bytes (cdr (ico-data i)) p)))
     (lambda ()
       (unless (output-port? file)
         (close-output-port p))))))

;; ------------------------------
;;  Image conversion
;; ------------------------------

(define (mask->alpha image mask)
  (map (lambda (i m)
         (if (zero? m)
             (bitwise-ior #xFF000000 i)
             m))
       image mask))

(define (first-n n l)
  (let loop ([l l][i n])
    (if (zero? i)
        null
        (cons (car l) (loop (cdr l) (sub1 i))))))

(define (16->32 l)
  (let loop ([l l])
    (if (null? l) 
        null
        (let ([l2 (let loop ([l (first-n 16 l)])
                    (if (null? l)
                        null
                        (list* (car l) (car l) (loop (cdr l)))))])
          (append l2 l2
                  (loop (list-tail l 16)))))))

(define (32->48 l)
  (let loop ([l l][dup? #t])
    (if (null? l) 
        null
        (let ([l2 (let loop ([l (first-n 32 l)])
                    (if (null? l)
                        null
                        (list* (car l) (car l) (cadr l)
                               (loop (cddr l)))))])
          (append l2
                  (if dup? l2 null)
                  (loop (list-tail l 32) (not dup?)))))))

(define (48->16 l)
  (let loop ([l l])
    (if (null? l) 
        null
        (let ([l2 (let loop ([l (first-n 48 l)])
                    (if (null? l)
                        null
                        (cons (car l) (loop (cdddr l)))))])
          (append l2
                  (loop (list-tail l 144)))))))

(define (48->32 l)
  (let loop ([l l][step 0])
    (if (null? l) 
        null
        (let ([l2 (let loop ([l (first-n 48 l)][step 0])
                    (if (null? l)
                        null
                        (if (= 1 (modulo step 3))
                            (loop (cdr l) 2)
                            (cons (car l) (loop (cdr l) (add1 step))))))])
          (append (if (= 1 (modulo step 3)) null l2)
                  (loop (list-tail l 48) (add1 step)))))))
