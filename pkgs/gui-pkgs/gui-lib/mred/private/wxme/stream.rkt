#lang racket/base
(require racket/class
         "../syntax.rkt"
         "private.rkt"
         racket/snip/private/private
         racket/snip/private/snip
         "editor-data.rkt"
         (only-in "cycle.rkt"
                  set-editor-stream-in%!
                  set-editor-stream-out%!))

(provide editor-stream-in%
         editor-stream-out%
         editor-stream-in-base%
         editor-stream-in-bytes-base%
         editor-stream-in-file-base%
         editor-stream-out-base%
         editor-stream-out-bytes-base%
         editor-stream-out-file-base%)

;; ----------------------------------------

(defclass editor-stream% object%

  (super-new)

  (define scl (get-the-snip-class-list))
  (define bdl (get-the-editor-data-class-list))
  (define/public (get-s-scl) scl)
  (define/public (get-s-bdl) bdl)

  (define sl null)
  (define dl null)

  (define/public (get-sl) sl)
  (define/public (get-dl) dl)
  (define/public (set-sl n) (set! sl n))
  (define/public (set-dl n) (set! dl n))
  (define/public (add-sl v) (set! sl (cons v sl)))
  (define/public (add-dl v) (set! dl (cons v dl)))

  (define sll null)
  (define style-count 0)
  (define/public (get-s-sll) sll)
  (define/public (set-s-sll v) (set! sll v))
  (define/public (get-s-style-count) style-count)
  (define/public (set-s-style-count v) (set! style-count v))

  (define/public (do-reading-version sclass)
    (or (ormap (lambda (scl)
                 (and (eq? (snip-class-link-c scl) sclass)
                      (snip-class-link-reading-version scl)))
               sl)
        ;; Class didn't show up in the header?
        ;; Assume we're reading the current version.
        (send sclass get-version)))

  (define/public (do-map-position sclass-or-dclass)
    (if (sclass-or-dclass . is-a? . snip-class%)
        (or (ormap (lambda (scl)
                     (and (eq? (snip-class-link-c scl) sclass-or-dclass)
                          (snip-class-link-map-position scl)))
                   sl)
            -1)
        (or (ormap (lambda (dcl)
                     (and (eq? (editor-data-class-link-c dcl) sclass-or-dclass)
                          (editor-data-class-link-map-position dcl)))
                   dl)
            -1)))

  (define/public (do-get-header-flag sclass)
    (or (ormap (lambda (scl)
                 (and (eq? (snip-class-link-c scl) sclass)
                      (snip-class-link-header-flag scl)))
               sl)
        0))

  (define/public (do-set-header-flag sclass)
    (ormap (lambda (scl)
             (and (eq? (snip-class-link-c scl) sclass)
                  (begin
                    (set-snip-class-link-header-flag! scl #t)
                    #t)))
           sl)
    (void)))

;; ----------------------------------------

(defclass editor-stream-in-base% object%
  (super-new)
  (def/public (tell) 0)
  (def/public (seek [exact-nonnegative-integer? i]) (void))
  (def/public (skip [exact-nonnegative-integer? i]) (void))
  (def/public (bad?) #t)
  (def/public (read [vector? v])
    (let ([s (make-bytes (vector-length v))])
      (let ([n (read-bytes s)])
        (for ([i (in-range n)])
          (vector-set! v i (integer->char (bytes-ref s i))))
        n)))
  (def/public (read-bytes [bytes? v]
                          [exact-nonnegative-integer? [start 0]]
                          [exact-nonnegative-integer? [end (bytes-length v)]]) 
    0)
  (def/public (read-byte)
    (let ([s (make-bytes 1)])
      (and (= 1 (read-bytes s 0 1))
           (bytes-ref s 0)))))

(defclass editor-stream-out-base% object%
  (super-new)
  (def/public (tell) 0)
  (def/public (seek [exact-nonnegative-integer? i]) (void))
  (def/public (skip [exact-nonnegative-integer? i]) (void))
  (def/public (bad?) #t)
  (def/public (write [(make-list char?) v])
    (write-bytes (string->bytes/latin-1 (list->string v) (char->integer #\?))))
  (def/public (write-bytes [bytes? v]
                           [exact-nonnegative-integer? [start 0]]
                           [exact-nonnegative-integer? [end (bytes-length v)]])
    (void)))

;; ----------------------------------------

(define mz:read-byte read-byte)

(defclass editor-stream-in-port-base% editor-stream-in-base%
  (init-field port)
  (super-new)

  (def/override (tell)
    (file-position port))

  (def/override (seek [exact-nonnegative-integer? i])
    (file-position port i))

  (def/override (skip [exact-nonnegative-integer? i])
    (file-position port (+ i (file-position port))))

  (def/override (bad?) #f)

  (def/override (read-bytes [bytes? v]
                            [exact-nonnegative-integer? [start 0]]
                            [exact-nonnegative-integer? [end (bytes-length v)]]) 
    (let ([r (read-bytes! v port start end)])
      (if (eof-object? r)
          0
          r)))

  (def/override (read-byte)
    (let ([v (mz:read-byte port)])
      (if (eof-object? v) #f v))))

(defclass editor-stream-in-file-base% editor-stream-in-port-base%
  (super-new))

(defclass editor-stream-in-bytes-base% editor-stream-in-port-base%
  (init s)
  (super-new [port (open-input-bytes s)]))

;; ----------------------------------------

(define write-bytes-proc write-bytes)

(defclass editor-stream-out-port-base% editor-stream-out-base%
  (init-field port)
  (super-new)

  (def/override (tell)
    (file-position port))

  (def/override (seek [exact-nonnegative-integer? i])
    (file-position port i))

  (def/override (skip [exact-nonnegative-integer? i])
    (file-position port (+ i (file-position port))))

  (def/override (bad?) #f)

  (def/override (write-bytes [bytes? v]
                             [exact-nonnegative-integer? [start 0]]
                             [exact-nonnegative-integer? [end (bytes-length v)]]) 
    (write-bytes-proc v port start end)
    (void)))

(defclass editor-stream-out-file-base% editor-stream-out-port-base%
  (super-new))

(defclass editor-stream-out-bytes-base% editor-stream-out-port-base%
  (define s (open-output-bytes))
  (super-new [port s])

  (def/public (get-bytes)
    (get-output-bytes s)))

;; ----------------------------------------

(define in-read-byte (generic editor-stream-in-base% read-byte))

(defclass editor-stream-in% editor-stream%
  (init-rest args)

  (define f
    (case-args
     args
     [([editor-stream-in-base% base]) base]
     (init-name 'editor-stream-in%)))

  (define boundaries null)
  (define is-bad? #f)
  (define items 0)
  (define pos-map (make-hash))

  (define read-version 8)
  (define s-read-version #"08")

  (super-new)

  (define/public (set-s-read-version bstr)
    (set! s-read-version bstr)
    (set! read-version (or (string->number (bytes->string/utf-8 bstr)) 0)))
  (define/public (get-wxme-version) read-version)

  (define s-read-format #"WXME")
  (define/public (set-s-read-format bstr)
    (set! s-read-format bstr))
  (define/public (get-s-read-format)
    s-read-format)

  (define/private (do-skip-whitespace)
    (define (bad!) (set! is-bad? #t) 0)
    (if is-bad?
        0
        (let loop ([prev-byte 0])
          (let ([b (send-generic f in-read-byte)])
            (if (not b)
                (bad!)
                (case (integer->char b)
                  [(#\#)
                   (let ([pos (send f tell)]
                         [b (send-generic f in-read-byte)])
                     (if (and b
                              (= b (char->integer #\|)))
                         ;; skip to end of comment
                         (let cloop ([saw-bar? #f]
                                     [saw-hash? #f]
                                     [nesting 0])
                           (let ([b (send-generic f in-read-byte)])
                             (if (not b)
                                 (bad!)
                                 (cond
                                  [(and saw-bar? (= b (char->integer #\#)))
                                   (if (zero? nesting)
                                       (loop (char->integer #\space))
                                       (cloop #f #f (sub1 nesting)))]
                                  [(and saw-hash? (= b (char->integer #\|)))
                                   (cloop #t #f (add1 nesting))]
                                  [else (cloop (= b (char->integer #\|))
                                               (= b (char->integer #\#))
                                               nesting)]))))
                         (begin
                           (send f seek pos)
                           (char->integer #\#))))]
                  [(#\;)
                   ;; skip to end of comment
                   (let cloop ()
                     (let ([b (send-generic f in-read-byte)])
                       (if (not b)
                           (bad!)
                           (if (or (= b (char->integer #\newline))
                                   (= b (char->integer #\return)))
                               (loop (char->integer #\space))
                               (cloop)))))]
                  [else
                   (if (char-whitespace? (integer->char b))
                       (loop b)
                       b)]))))))

  (define/private (skip-whitespace [buf #f])
    (let ([c (do-skip-whitespace)])
      (when buf
        (bytes-set! buf 0 c))
      c))

  (define/private (is-delim? b)
    (cond
     [(char-whitespace? (integer->char b)) #t]
     [(= b (char->integer #\#))
      (let ([pos (send f tell)]
            [b (send-generic f in-read-byte)])
        (let ([d? (= b (char->integer #\|))])
          (send f seek (if d? (sub1 pos) pos))
          d?))]
     [(= b (char->integer #\;))
      (send f seek (sub1 (send f tell)))
      #t]
     [else #f]))

  (define/private (get-number get-exact?)
    (let ([c0 (skip-whitespace)])
      (if (check-boundary)
          (if get-exact? 0 0.0)
          (let* ([l 
                  ;; As fast path, accum integer result
                  (let loop ([counter 50][c c0][v 0])
                    (if (zero? counter)
                        null
                        (if (or (not c) 
                                (is-delim? c))
                            (or v null)
                            (let ([rest (loop (sub1 counter) 
                                              (send-generic f in-read-byte)
                                              (and v
                                                   (c . >= . (char->integer #\0))
                                                   (c . <= . (char->integer #\9))
                                                   (+ (* v 10) (- c (char->integer #\0)))))])
                              (if (exact-integer? rest)
                                  rest
                                  (cons (integer->char c) rest))))))])
            (inc-item-count)
            (let ([n (if (exact-integer? l)
                         l
                         (string->number (list->string l)))])
              (cond
               [(and get-exact? (exact-integer? n)) n]
               [(real? n) (exact->inexact n)]
               [else
                (set! is-bad? #t)
                (if get-exact? 0 0.0)]))))))

  (define/private (get-a-string limit recur?)
    (let* ([orig-len (if recur?
                         (if (limit . < . 16)
                             limit
                             16)
                         (let ([v (get-exact)])
                           (if (check-boundary)
                               0
                               v)))]
           [buf (make-bytes 32)]
           [fail (lambda ()
                   (set! is-bad? #t)
                   #"")])
      (if recur?
          (bytes-set! buf 0 (char->integer #\#))
          (begin
            (skip-whitespace buf)
            (when is-bad?
              (bytes-set! buf 0 0))))
      (cond
       [(= (bytes-ref buf 0) (char->integer #\#))
        (if (and (= (send f read-bytes buf 1 2) 1)
                 (= (bytes-ref buf 1) (char->integer #\")))
            (let-values ([(si s) (make-pipe)]
                         [(tmp) (make-bytes (+ orig-len 2))])
              (display "#\"" s)
              (let loop ([get-amt (add1 orig-len)]) ;; add 1 for closing quote
                (let ([got-amt (send f read-bytes tmp 0 get-amt)])
                  (if (not (= got-amt get-amt))
                      (fail)
                      (begin
                        (write-bytes tmp s 0 got-amt)
                        (let ([done?
                               (let loop ([i 0])
                                 (cond
                                  [(= i got-amt) #f]
                                  [(= (bytes-ref tmp i) (char->integer #\")) #t]
                                  [(= (bytes-ref tmp i) (char->integer #\\))
                                   (if (= (add1 i) got-amt)
                                       ;; need to read escaped character
                                       (if (not (= (send f read-bytes tmp got-amt (add1 got-amt)) 1))
                                           (fail)
                                           (begin
                                             (write-bytes tmp s got-amt (add1 got-amt))
                                             #f))
                                       (loop (+ i 2)))]
                                  [else (loop (+ i 1))]))])
                          (if done?
                              (begin
                                (close-output-port s)
                                (unless recur? (inc-item-count))
                                (let ([s (with-handlers ([exn:fail:read? (lambda (x) #f)]) 
                                           (read si))])
                                  (if (or (not s)
                                          (not (eof-object? (read-byte si))))
                                      (fail)
                                      (if (if recur?
                                              ((bytes-length s) . <= . limit)
                                              (= (bytes-length s) orig-len))
                                          s
                                          (fail)))))
                              (loop 1))))))))
            (fail))]
       [(and (not recur?) (= (bytes-ref buf 0) (char->integer #\()))
        ;; read a sequence of strings
        (let loop ([accum null]
                   [left-to-get orig-len])
          (skip-whitespace buf)
          (if (or is-bad? 
                  (negative? left-to-get))
              (fail)
              (cond
               [(= (bytes-ref buf 0) (char->integer #\)))
                ;; got all byte strings
                (if (zero? left-to-get)
                    (begin
                      (inc-item-count)
                      (apply bytes-append (reverse accum)))
                    (fail))]
               [(= (bytes-ref buf 0) (char->integer #\#))
                (let ([v (get-a-string left-to-get #t)])
                  (if is-bad?
                      (fail)
                      (loop (cons v accum)
                            (- left-to-get (bytes-length v)))))]
               [else (fail)])))]
       [else (fail)])))

  (define/private (inc-item-count)
    (set! items (add1 items))
    (tell))

  (define/private (skip-one recur?)
    (let ([buf (make-bytes 1)]
          [fail (lambda () (set! is-bad? #t) (void))]
          [success (lambda () (unless recur? (inc-item-count)))])
      (if recur?
          (bytes-set! buf 0 (char->integer #\#))
          (skip-whitespace buf))
      (unless is-bad?
        (cond
         [(= (bytes-ref buf 0) (char->integer #\#))
          ;; byte string
          (if (and (= 1 (send f read-bytes buf))
                   (= (bytes-ref buf 0) (char->integer #\")))
              (let loop ()
                (if (= 1 (send f read-bytes buf))
                    (cond
                     [(= (bytes-ref buf 0) (char->integer #\\))
                      (if (= 1 (send f read-bytes buf))
                          (loop)
                          (fail))]
                     [(= (bytes-ref buf 0) (char->integer #\"))
                      (success)]
                     [else (loop)])
                    (fail)))
              (fail))]
         [(= (bytes-ref buf 0) (char->integer #\)))
          ;; list of byte strings
          (let loop ()
            (if is-bad?
                (fail)
                (if (not (= (send f read-bytes buf) 1))
                    (fail)
                    (if (is-delim? (bytes-ref buf 0))
                        (cond
                         [(= (bytes-ref buf 0) (char->integer #\)))
                          (success)]
                         [(= (bytes-ref buf 0) (char->integer #\#))
                          (skip-one #t)
                          (loop)]
                         [else (fail)])
                        (loop)))))]
         [else
          ;; number -- skip anything delimited
          (let loop ()
            (if (not (= (send f read-bytes buf) 1))
                (fail)
                (if (is-delim? (bytes-ref buf 0))
                    (success)
                    (loop))))]))))

  (def/public (get-fixed-exact)
    (if (check-boundary)
        0
        (if (read-version . < . 8)
            (let ([buf (make-bytes 4)])
              (send f read-bytes buf)
              (integer-bytes->integer
               buf
               #t
               (if (= read-version 1)
                   (system-big-endian?)
                   #t)))
            (get-exact))))

  (def/public (get-fixed [box? vb])
    (set-box! vb (get-fixed-exact)))

  #|
   integer format specified by first byte:
     bit 8: 0 - read 7-bit (positive) number
     bit 8: 1 - ...
        bit 7: 0 - read abother byte for 15-bit (positive) number
	bit 7: 1 - negative and long numbers...
	 bit 1: 1 - read another 8-bit (signed) number
	 bit 1: 0 - ...
	   bit 2: 1 - read another 16-bit (signed) number
	   bit 2: 0 - read another 32-bit (signed) number
  |#

  (def/public (get-exact)
    (if (check-boundary)
        0
        (if (read-version . < . 8)
            (let ([buf (make-bytes 4)]
                  [fail (lambda () (set! is-bad? #t) 0)])
              (if (not (= 1 (send f read-bytes buf 0 1)))
                  (fail)
                  (let ([b (bytes-ref buf 0)])
                    (if (positive? (bitwise-and b #x80))
                        (if (positive? (bitwise-and b #x40))
                            (cond
                             [(positive? (bitwise-and b #x01))
                              (if (= 1 (send f read-bytes buf 0 1))
                                  (let ([b (bytes-ref buf 0)])
                                    (if (b . > . 127)
                                        (- b 256)
                                        b))
                                  (fail))]
                             [(positive? (bitwise-and b #x02))
                              (if (= 2 (send f read-bytes buf 0 2))
                                  (integer-bytes->integer buf #t #t 0 2)
                                  (fail))]
                             [else
                              (if (= 4 (send f read-bytes buf 0 4))
                                  (integer-bytes->integer buf #t #t)
                                  (fail))])
                            (if (= 1 (send f read-bytes buf 0 1))
                                (+ (arithmetic-shift (bitwise-and b #x3F) 8)
                                   (bytes-ref buf 0))
                                (fail)))
                        b))))
            (get-number #t))))

  (def/public (get-inexact)
    (if (check-boundary)
        0
        (if (read-version . < . 8)
            (let ([buf (make-bytes 8)])
              (send f read-bytes buf)
              (floating-point-bytes->real
               buf
               (if (= read-version 1)
                   (system-big-endian?)
                   #t)))
            (get-number #f))))

  (define/private (do-get-bytes)
    (if (check-boundary)
        #""
        (if (read-version . < . 8)
            (let* ([len (get-exact)]
                   [s (make-bytes len)])
              (send f read-bytes s)
              s)
            (get-a-string #f #f))))
  
  (def/public (get-bytes [maybe-box? [len #f]])
    (let ([s (do-get-bytes)])
      (when len
        (set-box! len (max 1 (bytes-length s))))
      (subbytes s 0 (max 0 (sub1 (bytes-length s))))))
  
  (def/public (get-unterminated-bytes [maybe-box? [len #f]])
    (let ([s (do-get-bytes)])
      (when len
        (set-box! len (bytes-length s)))
      s))

  (def/public (get-unterminated-bytes! [(make-box exact-nonnegative-integer?) len] 
                                       [(lambda (s) (and (bytes? s) (not (immutable? s)))) s])
    (let ([s2 (do-get-bytes)])
      (if ((bytes-length s2) . <= . (unbox len))
          (begin
            (bytes-copy! s 0 s2)
            (set-box! len (bytes-length s2)))
          (set! is-bad? #t))))

  (def/public (get [(make-box real?) b])
    (unless (check-boundary)
      (if (exact-integer? (unbox b))
          (set-box! b (get-exact))
          (set-box! b (get-inexact)))))

  (def/public (set-boundary [exact-nonnegative-integer? n])
    (set! boundaries (cons (+ (tell) n) boundaries)))

  (def/public (remove-boundary)
    (set! boundaries (cdr boundaries)))

  (define/private (check-boundary)
    (if is-bad?
        #t
        (cond
         [(and (pair? boundaries)
               (items . >= . (car boundaries)))
          (set! is-bad? #t)
          (error 'editor-stream-in%
                 "overread (caused by file corruption?; ~a vs ~a)" items (car boundaries))]
         [(send f bad?)
          (set! is-bad? #t)
          (error 'editor-stream-in% "stream error")]
         [else #f])))

  (def/public (skip [exact-nonnegative-integer? n])
    (if (read-version . < . 8)
        (send f skip n)
        (jump-to (+ n items))))

  (def/public (tell)
    (if (read-version . < . 8)
        (send f tell)
        (let ([pos (send f tell)])
          (when (not (equal? (hash-ref pos-map items pos) pos))
            (error "again"))
          (hash-set! pos-map items pos)
          items)))

  (def/public (jump-to [exact-nonnegative-integer? pos])
    (if (read-version . < . 8)
        (send f seek pos)
        (let ([p (hash-ref pos-map pos #f)])
          (if (not p)
              (begin
                (let loop ()
                  (when (and (items . < . pos) (not is-bad?))
                    (skip-one #f)
                    (loop)))
                (unless (= items pos)
                  (set! is-bad? #t)))
              (begin
                (send f seek p)
                (set! items pos))))))

  (def/public (ok?) (not is-bad?)))

(set-editor-stream-in%! editor-stream-in%)

;; ----------------------------------------

(defclass editor-stream-out% editor-stream%
  (init-rest args)

  (define f
    (case-args
     args
     [([editor-stream-out-base% base]) base]
     (init-name 'editor-stream-out%)))

  (define is-bad? #f)
  (define col 72)
  (define items 0)
  (define pos-map (make-hash))

  (super-new)

  (define/private (check-ok)
    (unless is-bad?
      (when (send f bad?)
        (error 'editor-stream-out% "stream error"))))

  (def/public (put-fixed [exact-integer? v])
    (check-ok)
    (let-values ([(new-col spc)
                  (if ((+ col 12) . > . 72)
                      (values 11 #"\n")
                      (values (+ col 12) #" "))])
      (let ([s (number->string v)])
        (send f
              write-bytes
              (bytes-append spc
                            (make-bytes (- 11 (string-length s)) (char->integer #\space))
                            (string->bytes/latin-1 s))))
      (set! col new-col)
      (set! items (add1 items)))
    this)

  (define/public (put . args)
    (case-args
     args
     [([exact-nonnegative-integer? n][bytes? s])
      (do-put-bytes (subbytes s 0 n))]
     [([bytes? s])
      (do-put-bytes (bytes-append s #"\0"))]
     [([exact-integer? n])
      (do-put-number n)]
     [([real? n])
      (do-put-number (exact->inexact n))]
     (method-name 'editor-stream-out% 'put)))

  (def/public (put-unterminated [bytes? s])
    (do-put-bytes s))

  (define/private (do-put-bytes orig-s)
    (define (single-string)
      (if ((bytes-length orig-s) . < . 72)
          (let ([s (open-output-bytes)])
            (write orig-s s)
            (let* ([v (get-output-bytes s)]
                   [len (bytes-length v)])
              (if (len . >= . 72)
                  (multiple-strings)
                  (begin
                    (if ((+ col len 1) . > . 72)
                        (send f write-bytes #"\n")
                        (send f write-bytes #" "))
                    (send f write-bytes v)
                    (set! col 72))))) ;; forcing a newline after every string makes the file more readable
          (multiple-strings)))
    (define (multiple-strings)
      (send f write-bytes #"\n(")
      (let loop ([offset 0][remain (bytes-length orig-s)])
        (unless (zero? remain)
          (let lloop ([amt (min 50 remain)][retry? #t])
            (let ([s (open-output-bytes)])
              (write (subbytes orig-s offset (+ offset amt)) s)
              (let* ([v (get-output-bytes s #t)]
                     [len (bytes-length v)])
                (if (len . <= . 71)
                    (if (and (len . < . 71) 
                             retry?
                             (amt . < . remain))
                        (lloop (add1 amt) #t)
                        (begin
                          (send f write-bytes #"\n ")
                          (send f write-bytes v)
                          (loop (+ offset amt) (- remain amt))))
                    (lloop (quotient amt 2) #f)))))))
      (send f write-bytes #"\n)")
      (set! col 1))

    (check-ok)
    (do-put-number (bytes-length orig-s))
    (single-string)
    (set! items (add1 items))
    this)

  (define/private (do-put-number v)
    (check-ok)
    (let* ([s (string->bytes/latin-1 (format " ~a" v))]
           [len (bytes-length s)])
      (if ((+ col len) . > . 72)
          (begin
            (set! col (sub1 len))
            (bytes-set! s 0 (char->integer #\newline)))
          (set! col (+ col len)))
      (send f write-bytes s)
      (set! items (add1 items))
      this))

  (def/public (tell)
    (let ([pos (send f tell)])
      (hash-set! pos-map items (cons pos col))
      items))

  (def/public (jump-to [exact-nonnegative-integer? icount])
    (unless is-bad?
      (let ([p (hash-ref pos-map icount #f)])
        (when p
          (send f seek (car p))
          (set! col (cdr p))
          (set! items icount)))))
  
  (def/public (ok?) (not is-bad?))

  (def/public (pretty-finish)
    (unless is-bad?
      (when (positive? col)
        (send f write-bytes #"\n")
        (set! col 0))))

  (def/public (pretty-start)
    (define (show s)
      (send f write-bytes (if (string? s) (string->bytes/latin-1 s) s)))
    (when (positive? col)
      (show #"\n"))
    (show #"#|\n   This file uses the GRacket editor format.\n")
    (show (format "   Open this file in DrRacket version ~a or later to read it.\n" (version)))
    (show #"\n")
    (show #"   Most likely, it was created by saving a program in DrRacket,\n")
    (show #"   and it probably contains a program with non-text elements\n")
    (show #"   (such as images or comment boxes).\n")
    (show #"\n")
    (show #"            http://racket-lang.org/\n|#\n")
    (set! col 0)))

(set-editor-stream-out%! editor-stream-out%)
