;; Originally:
;; "genwrite.scm" generic write used by pp.scm
;; copyright (c) 1991, marc feeley

;; Pretty-printer for Racket
;;  Handles structures, cycles, and graphs

;; TO INSTALL this pretty-printer into Racket's read-eval-print loop,
;; require this module and evaluate:
;;      (current-print pretty-print-handler)

#lang racket/base
(require racket/private/port
         racket/flonum
         racket/fixnum)

(provide pretty-print
         pretty-write
         pretty-display
         pretty-print-columns
         pretty-print-depth
         pretty-print-handler
         pretty-print-size-hook
         pretty-print-print-hook
         pretty-print-pre-print-hook
         pretty-print-post-print-hook
         pretty-print-print-line
         pretty-print-show-inexactness
         pretty-print-exact-as-decimal
         pretty-print-.-symbol-without-bars
         pretty-print-abbreviate-read-macros
         
         pretty-print-style-table?
         pretty-print-current-style-table
         pretty-print-extend-style-table
         pretty-print-remap-stylable
         
         pretty-format
         pretty-printing
         pretty-print-newline
         make-tentative-pretty-print-output-port
         tentative-pretty-print-port-transfer
         tentative-pretty-print-port-cancel)

(define-struct pretty-print-style-table (hash))

(define pretty-print-extend-style-table
  (lambda (table symbols like-symbols)
    (let ([terr (lambda (kind which)
                  (raise-argument-error
                   'pretty-print-extend-style-table
                   kind
                   which
                   table symbols like-symbols))])
      (unless (or (not table) (pretty-print-style-table? table))
        (terr "(or/c pretty-print-style-table? #f)" 0))
      (unless (and (list? symbols)
                   (andmap symbol? symbols))
        (terr "(listof symbol?)" 1))
      (unless (and (list? like-symbols)
                   (andmap symbol? like-symbols))
        (terr "(listof symbol?)" 1))
      (unless (= (length symbols) (length like-symbols))
        (raise-arguments-error
         'pretty-print-extend-style-table
         "length of first list doesn't match the length of the second list"
         "first list length" (length symbols) 
         "second list length" (length like-symbols)
         "first list" symbols
         "second list" like-symbols)))
    (let ([ht (if table (pretty-print-style-table-hash table) (make-hasheq))]
          [new-ht (make-hasheq)])
      (hash-for-each
       ht
       (lambda (key val)
         (hash-set! new-ht key val)))
      (for-each
       (lambda (symbol like-symbol)
         (let ((s (hash-ref ht
                            like-symbol
                            (lambda () #f))))
           (hash-set! new-ht symbol (or s like-symbol))))
       symbols like-symbols)
      (make-pretty-print-style-table new-ht))))

(define pretty-print-abbreviate-read-macros (make-parameter #t))

(define pretty-print-current-style-table 
  (make-parameter 
   (pretty-print-extend-style-table #f null null)
   (lambda (s)
     (unless (pretty-print-style-table? s)
       (raise-argument-error
        'pretty-print-current-style-table 
        "pretty-print-style-table?"
        s))
     s)))

(define pretty-print-.-symbol-without-bars
  (make-parameter #f (lambda (x) (and x #t))))

(define pretty-print-show-inexactness 
  (make-parameter #f
                  (lambda (x) (and x #t))))

(define pretty-print-exact-as-decimal
  (make-parameter #f
                  (lambda (x) (and x #t))))

(define pretty-print-columns 
  (make-parameter 79
                  (lambda (x)
                    (unless (or (eq? x 'infinity)
                                (integer? x))
                      (raise-argument-error 
                       'pretty-print-columns
                       "(or/c integer? 'infinity)"
                       x))
                    x)))

(define pretty-print-depth
  (make-parameter #f
                  (lambda (x)
                    (unless (or (not x) (number? x))
                      (raise-argument-error 
                       'pretty-print-depth
                       "(or/c number? #f)"
                       x))
                    x)))

(define can-accept-n?
  (lambda (n x)
    (procedure-arity-includes? x n)))

(define pretty-print-size-hook
  (make-parameter (lambda (x display? port) #f)
                  (lambda (x)
                    (unless (can-accept-n? 3 x)
                      (raise-argument-error 
                       'pretty-print-size-hook
                       "(any/c any/c any/c . -> . any/c)"
                       x))
                    x)))

(define pretty-print-print-hook
  (make-parameter void
                  (lambda (x)
                    (unless (can-accept-n? 3 x)
                      (raise-argument-error 
                       'pretty-print-print-hook
                       "(any/c any/c any/c . -> . any/c)"
                       x))
                    x)))

(define pretty-print-print-line
  (make-parameter (lambda (line port offset width)
                    (when (and (number? width)
                               (not (eq? 0 line)))
                      (newline port))
                    0)
                  (lambda (x)
                    (unless (can-accept-n? 4 x)
                      (raise-argument-error 
                       'pretty-print-print-line
                       "(procedure-arity-includes/c 4)"
                       x))
                    x)))

(define pretty-print-pre-print-hook
  (make-parameter void
                  (lambda (x)
                    (unless (can-accept-n? 2 x)
                      (raise-argument-error 
                       'pretty-print-pre-print-hook
                       "(any/c any/c . -> . any/c)"
                       x))
                    x)))

(define pretty-print-post-print-hook
  (make-parameter void
                  (lambda (x)
                    (unless (can-accept-n? 2 x)
                      (raise-argument-error 
                       'pretty-print-post-print-hook
                       "(any/c any/c . -> . any/c)"
                       x))
                    x)))

(define pretty-printing
  (make-parameter #f (lambda (x) (and x #t))))

(define pretty-print-remap-stylable
  (make-parameter (λ (x) #f) 
                  (λ (f) 
                    (unless (can-accept-n? 1 f)
                      (raise-argument-error
                       'pretty-print-remap-stylable
                       "(symbol? . -> . (or/c symbol? #f))"
                       f))
                    (λ (x)
                      (let ([res (f x)])
                        (unless (or (not res) (symbol? res))
                          (raise-result-error
                           'pretty-print-remap-stylable
                           "(or/c symbol? #f)"
                           res))
                        res)))))

(define make-pretty-print
  (lambda (name display? as-qq?)
    (letrec ([pretty-print
              (lambda (obj [port (current-output-port)] [qq-depth 0] #:newline? [newline? #t])
                (unless (output-port? port)
                  (raise-argument-error name "output-port?" port))
                (unless (or (equal? qq-depth 0)
                            (equal? qq-depth 1))
                  (raise-argument-error name "(or/c 0 1)" qq-depth))
                (unless (boolean? newline?)
                  (raise-argument-error name "boolean?" newline?))
                (let ([width (pretty-print-columns)]
                      [size-hook (pretty-print-size-hook)]
                      [print-hook (pretty-print-print-hook)]
                      [pre-hook (pretty-print-pre-print-hook)]
                      [post-hook (pretty-print-post-print-hook)])
                  (generic-write obj display? #:newline? newline?
                                 width
                                 (make-printing-port port 
                                                     pre-hook
                                                     post-hook
                                                     print-hook
                                                     (pretty-print-print-line))
                                 (print-graph) (print-struct) (print-hash-table)
                                 (and (not display?) (print-vector-length)) (print-box)
                                 (and (not display?) as-qq? (print-as-expression)) qq-depth
                                 (pretty-print-depth)
                                 (lambda (o display?)
                                   (size-hook o display? port)))
                  (void)))])
      pretty-print)))

(define pretty-print (make-pretty-print 'pretty-print #f #t))
(define pretty-display (let ([pp (make-pretty-print 'pretty-display #t #f)])
                         (lambda (v [o (current-output-port)] #:newline? [n? #t])
                           (pp v o #:newline? n?))))
(define pretty-write   (let ([pp (make-pretty-print 'pretty-write #f #f)])
                         (lambda (v [o (current-output-port)] #:newline? [n? #t])
                           (pp v o #:newline? n?))))

(define-struct mark (str def) #:mutable)
(define-struct hide (val))

(define (make-tentative-output-port pport width esc)
  (let* ([content null]
         [special-ok? (port-writes-special? pport)]
         ;; The null device counts for us:
         [/dev/null
          (let-values ([(line col pos) (port-next-location pport)])
            (relocate-output-port 
             (let ([p (open-output-nowhere special-ok?)])
               (port-count-lines! p)
               p)
             (or line 1) (or col 0) (or pos 1)))]
         [first-line? #t]
         [check-esc (lambda ()
                      (let-values ([(l c p) (port-next-location /dev/null)])
                        (when (or (c . > . width)
                                  (not first-line?))
                          (esc))))]
         [p (make-output-port
             'tentative
             always-evt
             (lambda (s start end block? break?)
               (write-bytes s /dev/null start end)
               (check-esc)
               (set! content (cons (subbytes s start end) content))
               (- end start))
             void
             (and special-ok?
                  (lambda (special block break?)
                    (write-special special /dev/null)
                    (check-esc)
                    (set! content (cons (cons 'special special) content))
                    #t))
             #f #f
             (lambda ()
               (port-next-location /dev/null)))])
    (port-count-lines! /dev/null)
    (port-count-lines! p)
    (register-printing-port p 
                            (make-print-port-info
                             (lambda () (reverse content))
                             (box #t)
                             (lambda (v)
                               (set! content (cons (cons 'pre v) content)))
                             (lambda (v)
                               (set! content (cons (cons 'post v) content)))
                             (lambda (v len display?)
                               (display (make-string len #\.) /dev/null)
                               (set! content (cons (list* 'hooked v len display?)
                                                   content)))
                             (lambda (use-line? offset width)
                               (when (and (number? width)
                                          (not first-line?))
                                 (newline p))
                               (set! first-line? #f)
                               0)
                             esc))))

(define (make-tentative-pretty-print-output-port pport width esc)
  (let ([p (make-tentative-output-port pport width esc)])
    (port-write-handler p (port-write-handler pport))
    (port-display-handler p (port-display-handler pport))
    (port-print-handler p (port-print-handler pport))
    p))

(define (make-printing-port port pre-print post-print output-hooked print-line)
  (let-values ([(line col pos) (port-next-location port)])
    (let* ([orig-counts? (and line col pos)]
           [p (if orig-counts?
                  (relocate-output-port port line col pos #f)
                  (transplant-output-port port #f 1 #f))]
           [line -1])
      (port-count-lines! p)
      (register-printing-port p 
                              (make-print-port-info 
                               (lambda () null)
                               (box #t)
                               (lambda (v)
                                 (pre-print v port))
                               (lambda (v)
                                 (post-print v port))
                               (lambda (v len display?)
                                 (output-hooked v display? p))
                               (lambda (use-line? offset width)
                                 (set! line (add1 line))
                                 (print-line (and use-line? line) p offset width))
                               void)))))

(struct printing-port (port info)
  #:property prop:output-port 0)

(define-struct print-port-info (get-content
                                def-box
                                pre-print
                                post-print
                                output-hooked
                                print-line
                                esc))

(define (register-printing-port p info)
  (printing-port p info))

(define (register-printing-port-like p pport)
  (printing-port p (printing-port-info pport)))

(define (get pport selector)
  (selector (printing-port-info pport)))

(define (printing-port-pre-print pport)
  (get pport print-port-info-pre-print))
(define (printing-port-post-print pport)
  (get pport print-port-info-post-print))
(define (printing-port-def-box pport)
  (get pport print-port-info-def-box))
(define (printing-port-output-hooked pport)
  (get pport print-port-info-output-hooked))
(define (printing-port-print-line pport)
  (get pport print-port-info-print-line))
(define (printing-port-esc pport)
  (get pport print-port-info-esc))

(define orig-display (port-display-handler (open-output-string)))
(define orig-write (port-write-handler (open-output-string)))

(define (pretty-print-newline pport width)
  (cond
   [(printing-port? pport)
    (let-values ([(l col p) (port-next-location pport)])
      ((printing-port-print-line pport) #t (or col 0) width))]
   [(output-port? pport)
    (newline pport)]
   [else
    (raise-argument-error 'pretty-print-newline "output-port?" pport)]))

(define (tentative-pretty-print-port-transfer a-pport pport)
  (let ([content ((get a-pport print-port-info-get-content))])
    (for-each (lambda (elem)
                (if (bytes? elem)
                    (write-bytes elem pport)
                    (case (car elem)
                      [(special) (write-special (cdr elem) pport)]
                      [(pre) ((printing-port-pre-print pport) (cdr elem))]
                      [(post) ((printing-port-post-print pport) (cdr elem))]
                      [(hooked) ((printing-port-output-hooked pport) 
                                 (cadr elem) (caddr elem) (cdddr elem))])))
              content)))

(define (tentative-pretty-print-port-cancel pport)
  (set-box! (get pport print-port-info-def-box) #f))

(define (add-spaces n port)
  (if (> n 0)
      (if (> n 7)
          (begin
            (write-string "        " port)
            (add-spaces (- n 8) port))
          (write-string "        " port 0 n))
      (void)))

(define (prefab?! obj v)
  (let ([d (prefab-struct-key obj)])
    (and d
         (begin
           (vector-set! v 0 d)
           #t))))

(define-struct unquoted (val))
(define struct-ellipses (string->uninterned-symbol "..."))

(define (generic-write obj display? width pport
                       print-graph? print-struct? print-hash-table? print-vec-length? 
                       print-box? print-as-qq? qq-depth
                       depth size-hook #:newline? newline?)

  (define pair-open (if (print-pair-curly-braces) "{" "("))
  (define pair-close (if (print-pair-curly-braces) "}" ")"))
  (define mpair-open (if (print-mpair-curly-braces) "{" "("))
  (define mpair-close (if (print-mpair-curly-braces) "}" ")"))
  
  (define table (make-hasheq)) ; Hash table for looking for loops

  (define show-inexactness? (pretty-print-show-inexactness))
  (define exact-as-decimal? (pretty-print-exact-as-decimal))

  (define long-bools? (print-boolean-long-form))
  
  (define-syntax-rule (mkvector->repeatless-list name v-length v-ref equal-op? ->list)
    (define name
      (if print-vec-length?
          (lambda (v)
            (let ([len (v-length v)])
              (if (zero? len)
                  null
                  (let ([last (v-ref v (sub1 len))])
                    (let loop ([i (- len 2)])
                      (if (i . < . 0)
                          (list last)
                          (let ([e (v-ref v i)])
                            (if (equal-op? e last)
                                (loop (sub1 i))
                                (let loop ([i (sub1 i)][r (list e last)])
                                  (if (i . < . 0)
                                      r
                                      (loop (sub1 i) (cons (v-ref v i) r))))))))))))
          ->list)))

  (mkvector->repeatless-list vector->repeatless-list vector-length vector-ref eq? vector->list)
  (mkvector->repeatless-list flvector->repeatless-list flvector-length flvector-ref equal?
                             (lambda (v) (for/list ([x (in-flvector v)]) x)))
  (mkvector->repeatless-list fxvector->repeatless-list fxvector-length fxvector-ref eq?
                             (lambda (v) (for/list ([x (in-fxvector v)]) x)))

  (define (extract-sub-objects obj pport)
    (let ([p (open-output-nowhere 'null (port-writes-special? pport))]
          [l null])
      (let ([record (lambda (o p) (set! l (cons o l)))])
        (port-write-handler p record)
        (port-display-handler p record)
        (port-print-handler p record))
      (parameterize ([pretty-printing #f])
        ((custom-write-accessor obj) obj p #f))
      l))

  (define found-cycle
    (or print-graph?
        (let loop ([obj obj])
          (and (or (vector? obj)
                   (pair? obj)
                   (mpair? obj)
                   (and (box? obj)
                        print-box?)
                   (and (custom-write? obj)
                        (not (struct-type? obj)))
                   (and (struct? obj) print-struct?)
                   (and (hash? obj) print-hash-table?))
               (or (hash-ref table obj #f)
                   (begin
                     (hash-set! table obj #t)
                     (let ([cycle
                            (cond
                             [(vector? obj)
                              (let ([len (vector-length obj)])
                                (let vloop ([i 0])
                                  (if (= i len)
                                      #f
                                      (or (loop (vector-ref obj i))
                                          (vloop (add1 i))))))]
                             [(pair? obj)
                              (or (loop (car obj))
                                  (loop (cdr obj)))]
                             [(mpair? obj)
                              (or (loop (mcar obj))
                                  (loop (mcdr obj)))]
                             [(and (box? obj) print-box?) (loop (unbox obj))]
                             [(and (custom-write? obj)
                                   (not (struct-type? obj)))
                              (loop (extract-sub-objects obj pport))]
                             [(struct? obj)
                              (ormap loop 
                                     (vector->list (struct->vector obj)))]
                             [(hash? obj)
                              (for/or ([(k v) (in-hash obj)])
                                (or (loop v) (loop k)))])])
                       (hash-remove! table obj)
                       cycle)))))))

  (define __dummy__
    (when found-cycle
      (let loop ([obj obj])
        (if (or (vector? obj)
                (pair? obj)
                (mpair? obj)
                (and (box? obj)
                     print-box?)
                (and (custom-write? obj)
                     (not (struct-type? obj)))
                (and (struct? obj) print-struct?)
                (and (hash? obj) print-hash-table?))
            ;; A little confusing: use #t for not-found
            (let ([p (hash-ref table obj #t)])
              (when (not (mark? p))
                (if p
                    (begin
                      (hash-set! table obj #f)
                      (cond
                       [(vector? obj)
                        (let ([len (vector-length obj)])
                          (let vloop ([i 0])
                            (unless (= i len)
                              (loop (vector-ref obj i))
                              (vloop (add1 i)))))]
                       [(pair? obj)
                        (loop (car obj))
                        (loop (cdr obj))]
                       [(mpair? obj)
                        (loop (mcar obj))
                        (loop (mcdr obj))]
                       [(and (box? obj) print-box?) (loop (unbox obj))]
                       [(and (custom-write? obj)
                             (not (struct-type? obj)))
                        (loop (extract-sub-objects obj pport))]
                       [(struct? obj)
                        (for-each loop 
                                  (vector->list (struct->vector obj)))]
                       [(hash? obj)
                        (hash-for-each
                         obj
                         (lambda (k v)
                           (loop k)
                           (loop v)))]))
                    (begin
                      (hash-set! table obj 
                                 (make-mark #f (box #f)))))))
            (void)))))

  (define escapes-table
    (let* ([table (make-hasheq)]
           [local-compound (and print-as-qq?
                                (make-hasheq))]
           [is-compound! (lambda (obj)
                           (hash-set! local-compound obj #t))]
           [escapes! (lambda (obj)
                       (hash-set! table obj #t)
                       #t)]
           [orf (lambda (a b) (or a b))])
      (when print-as-qq?
        (let loop ([obj obj])
          (cond
           [(hash-ref table obj #f)
            ;; already decided that it escapes
            #t]
           [(and local-compound 
                 (hash-ref local-compound obj #f))
            ;; either still deciding (so assume #f) or
            ;; already decided that no escape is needed
            #f]
           [else
            (cond
             [(vector? obj)
              (is-compound! obj)
              (let ([len (vector-length obj)])
                (let vloop ([esc? #f][i 0])
                  (if (= i len)
                      (and esc? 
                           (escapes! obj))
                      (vloop (or (loop (vector-ref obj i)) esc?) 
                             (add1 i)))))]
             [(flvector? obj)
              (is-compound! obj)
              ;; always unquoted:
              #t]
             [(fxvector? obj)
              (is-compound! obj)
              ;; always unquoted:
              #t]
             [(pair? obj)
              (is-compound! obj)
              (and (orf (loop (car obj))
                        (loop (cdr obj)))
                   (escapes! obj))]
             [(mpair? obj)
              (is-compound! obj)
              (loop (mcar obj))
              (loop (mcdr obj))
              ;; always unquoted:
              #t]
             [(and (box? obj) print-box?) 
              (is-compound! obj)
              (and (loop (unbox obj))
                   (escapes! obj))]
             [(and (custom-write? obj)
                   (not (struct-type? obj)))
              (is-compound! obj)
              (let ([kind (if (custom-print-quotable? obj)
                              (custom-print-quotable-accessor obj)
                              'self)])
                (and (or (and (loop (extract-sub-objects obj pport))
                              (not (memq kind '(self always))))
                         (memq kind '(never)))
                     (escapes! obj)))]
             [(struct? obj)
              (is-compound! obj)
              (and (or (loop (struct->vector obj))
                       (not (prefab-struct-key obj)))
                   (escapes! obj))]
             [(hash? obj)
              (is-compound! obj)
              (and (for/fold ([esc? #f]) ([(k v) (in-hash obj)])
                     (or (orf (loop v) 
                              (loop k))
                         esc?))
                   (escapes! obj))]
             [else #f])])))
      table))

  (define cycle-counter 0)

  (define found (if found-cycle
                    table 
                    #f))

  (define dsub1 (lambda (d)
                  (if d
                      (sub1 d)
                      #f)))

  (define (pre-print pport obj)
    ((printing-port-pre-print pport) obj))
  (define (post-print pport obj)
    ((printing-port-post-print pport)
     obj))
  (define (output-hooked pport obj len display?)
    ((printing-port-output-hooked pport)
     obj len display?))

  (define expr-found
    (lambda (pport ref)
      (let ([n cycle-counter])
        (set! cycle-counter (add1 cycle-counter))
        (set-mark-str! ref 
                       (string-append "#"
                                      (number->string n)
                                      "#"))
        (set-mark-def! ref (printing-port-def-box pport))
        (display (string-append "#"
                                (number->string n)
                                "=")
                 pport))))
  
  (define check-expr-found
    (lambda (obj pport check? c-k d-k n-k)
      (let ([ref (and check? 
                      found
                      (hash-ref found obj #f))])
        (if (and ref (unbox (mark-def ref)))
            (if c-k
                (c-k (mark-str ref))
                (display (mark-str ref) pport))
            (if (and ref d-k)
                (d-k)
                (begin
                  (when ref
                    (expr-found pport ref))
                  (n-k)))))))

  (define (write-custom recur obj pport depth display? width qd multi-line?)
    (let-values ([(l c p) (port-next-location pport)])
      (let ([p (relocate-output-port pport l c p)])
        (port-count-lines! p)
        (let ([writer (lambda (v port)
                        (recur port v (dsub1 depth) #f #f))]
              [displayer (lambda (v port)
                           (recur port v (dsub1 depth) #t #f))]
              [printer (case-lambda 
                        [(v port) (recur port v (dsub1 depth) #f qd)]
                        [(v port qd) (recur port v (dsub1 depth) #f qd)])])
          (port-write-handler p writer)
          (port-display-handler p displayer)
          (port-print-handler p printer))
        (let ([p (register-printing-port-like p pport)])
          (parameterize ([pretty-printing multi-line?]
                         [pretty-print-columns (or width 'infinity)])
            ((custom-write-accessor obj) obj p (or qd (not display?))))))))

  ;; ------------------------------------------------------------
  
  (define (convert-pair obj)
    (cond
     [(list? obj) (cons (make-unquoted 'list) 
                        ;; reconstruct first pair in case it
                        ;; starts a cycle:
                        (cons (car obj) (cdr obj)))]
     [(and (pair? (cdr obj)) 
           (not (and found
                     (hash-ref found (cdr obj) #f))))
      (cons (make-unquoted 'list*)
            (cons
             (car obj)
             (let loop ([obj (cdr obj)])
               (cond
                [(and found (hash-ref found obj #f)) (list obj)]
                [(pair? obj) (cons (car obj) (loop (cdr obj)))]
                [else (list obj)]))))]
     [else (list (make-unquoted 'cons) (car obj) (cdr obj))]))

  (define (convert-hash obj expr?)
    (let ([l (hash-map obj (lambda (k v)
                             (if expr?
                                 (list k v)
                                 (cons k (make-hide v)))))])
      (if expr?
          (cons (make-unquoted
                 (if (hash-eq? obj)
                     'hasheq
                     (if (hash-eqv? obj)
                         'hasheqv
                         'hash)))
                (apply append l))
          l)))
  
  ;; ------------------------------------------------------------
  ;; wr: write on a single line
  (define (wr* pport obj depth display? qd)

    (define (out str)
      (write-string str pport))
    
    (define (wr obj depth qd)
      (wr* pport obj depth display? qd))

    (define (wr-expr expr depth pair? car cdr open close qd)
      (if (and (read-macro? expr pair? car cdr qd)
               (equal? open "("))
          (begin
            (out (read-macro-prefix expr car))
            ;; Special case: "," or "#," followed by a symbol that might start "@"
            (cond
              [(and (or (eq? (car expr) 'unquote)
                        (eq? (car expr) 'unsyntax))
                    (symbol? (cadr expr)))
               (define s (if display?
                             (symbol->string (cadr expr))
                             (format "~s" (cadr expr))))
               (when (and (positive? (string-length s))
                          (eqv? #\@ (string-ref s 0)))
                 ;; Avoid ambiguity by adding a space
                 (out " "))
               (out s)]
              [else
               (wr (read-macro-body expr car cdr) depth qd)]))
          (wr-lst expr #t depth pair? car cdr open close qd)))

    (define (wr-lst l check? depth pair? car cdr open close qd)
      (if (pair? l)
          (if (and depth (zero? depth))
              (begin
                (out open)
                (out "...")
                (out close))
              (begin
                (out open)
                (wr (car l) (dsub1 depth) qd)
                (let loop ([l (cdr l)])
                  (check-expr-found
                   l pport (and check? (pair? l))
                   (lambda (s) (out " . ") (out s) (out close))
                   (lambda ()
                     (out " . ")
                     (check-expr-found ;; will find it!
                      l pport #t
                      #f #f
                      (lambda ()
                        (wr-lst l check? (dsub1 depth) pair? car cdr open close qd)))
                     (out close))
                   (lambda ()
                     (cond 
                      [(pair? l) 
                       (if (and (eq? (do-remap (car l)) 'unquote)
                                (not (equal? qd 1))
                                (pair? (cdr l))
                                (null? (cdr (cdr l))))
                           (begin
                             (out " . ,")
                             (wr (car (cdr l)) (dsub1 depth) qd)
                             (out close))
                           (begin
                             (out " ")
                             (wr (car l) (dsub1 depth) qd)
                             (loop (cdr l))))]
                      [(null? l) (out close)]
                      [else
                       (out " . ")
                       (wr l (dsub1 depth) qd)
                       (out close)]))))))
          (begin
            (out open)
            (out close))))

    (unless (hide? obj)
      (pre-print pport obj))
    (if (and depth 
             (negative? depth)
             (not (hide? obj)))
        (out "...")
        
        (cond 
         [(size-hook obj display?)
          => (lambda (len)
               (output-hooked pport obj len display?))]
         
         [(pair? obj)
          (check-expr-found
           obj pport #t
           #f #f
           (lambda ()
             (let* ([qd (to-quoted out qd obj)]
                    [pair (if (and qd (zero? qd))
                              (convert-pair obj)
                              obj)])
               (wr-expr pair depth pair? car cdr pair-open pair-close qd))))]
         [(mpair? obj) 
          (check-expr-found
           obj pport #t
           #f #f
           (lambda ()
             (if (and qd (zero? qd))
                 (wr-expr (list (make-unquoted 'mcons) (mcar obj) (mcdr obj))
                          depth pair? car cdr pair-open pair-close qd)
                 (wr-expr obj depth mpair? mcar mcdr mpair-open mpair-close qd))))]
         [(null? obj)
          (let ([qd (to-quoted out qd obj)])
            (wr-lst obj #f depth pair? car cdr "(" ")" qd))]
         [(vector? obj)   
          (check-expr-found
           obj pport #t
           #f #f
           (lambda ()
             (let ([qd (to-quoted out qd obj)]
                   [vecl (vector->repeatless-list obj)])
               (if (and qd (zero? qd))
                   (wr-lst (cons (make-unquoted 'vector) vecl)
                           #f depth pair? car cdr "(" ")" qd)
                   (begin
                     (out "#")
                     (when print-vec-length?
                       (out (number->string (vector-length obj))))
                     (wr-lst vecl #f depth pair? car cdr "(" ")" qd))))))]
         [(flvector? obj)   
          (check-expr-found
           obj pport #t
           #f #f
           (lambda ()
             (let ([vecl (flvector->repeatless-list obj)])
               (if (and qd (zero? qd))
                   (wr-lst (cons (make-unquoted 'flvector) vecl)
                           #f depth pair? car cdr "(" ")" qd)
                   (begin
                     (out "#fl")
                     (when print-vec-length?
                       (out (number->string (flvector-length obj))))
                     (wr-lst vecl #f depth pair? car cdr "(" ")" qd))))))]
         [(fxvector? obj)   
          (check-expr-found
           obj pport #t
           #f #f
           (lambda ()
             (let ([vecl (fxvector->repeatless-list obj)])
               (if (and qd (zero? qd))
                   (wr-lst (cons (make-unquoted 'fxvector) vecl)
                           #f depth pair? car cdr "(" ")" qd)
                   (begin
                     (out "#fx")
                     (when print-vec-length?
                       (out (number->string (fxvector-length obj))))
                     (wr-lst vecl #f depth pair? car cdr "(" ")" qd))))))]
         [(and (box? obj)
               print-box?)
          (check-expr-found
           obj pport #t
           #f #f
           (lambda ()
             (let ([qd (to-quoted out qd obj)])
               (if (and qd (zero? qd))
                   (wr-lst (list (make-unquoted 'box) (unbox obj))
                           #f depth pair? car cdr "(" ")" qd)
                   (begin
                     (out "#&") 
                     (wr (unbox obj) (dsub1 depth) qd))))))]
         [(and (custom-write? obj) 
               (not (struct-type? obj)))
          (check-expr-found
           obj pport #t
           #f #f
           (lambda ()
             (parameterize ([pretty-print-columns 'infinity])
               (let ([qd (let ([kind (if (custom-print-quotable? obj)
                                         (custom-print-quotable-accessor obj)
                                         'self)])
                           (if (memq kind '(self never))
                               qd
                               (to-quoted out qd obj)))])
                 (write-custom wr* obj pport depth display? width qd #f)))))]
         [(hide? obj)
          (wr* pport (hide-val obj) depth display? qd)]
         [(unquoted? obj)
          (orig-write (unquoted-val obj) pport)]
         [(struct? obj)
          (if (and print-struct?
                   (not (and depth
                             (zero? depth))))
              (check-expr-found
               obj pport #t
               #f #f
               (lambda ()
                 (let* ([v (struct->vector obj struct-ellipses)]
                        [pf? (prefab?! obj v)])
                   (let ([qd (if pf?
                                 (to-quoted out qd obj)
                                 qd)])
                     (when (or (not qd) (positive? qd))
                       (out "#")
                       (when pf? (out "s")))
                     (wr-lst (let ([l (vector->list v)])
                               (if (and qd (zero? qd))
                                   (cons (make-unquoted (object-name obj))
                                         (cdr l))
                                   l))
                             #f (dsub1 depth) pair? car cdr "(" ")" 
                             qd)))))
              (parameterize ([print-struct #f])
                ((if display? orig-display orig-write) obj pport)))]
         [(hash? obj)  
          (if (and print-hash-table?
                   (not (and depth
                             (zero? depth))))
              (check-expr-found
               obj pport #t
               #f #f
               (lambda ()
                 (let* ([qd (to-quoted out qd obj)]
                        [expr? (and qd (zero? qd))])
                   (unless expr?
                     (out (if (hash-eq? obj)
                              "#hasheq"
                              (if (hash-eqv? obj)
                                  "#hasheqv"
                                  "#hash"))))
                   (wr-lst (convert-hash obj expr?)
                           #f depth
                           pair? car cdr "(" ")" qd))))
              (parameterize ([print-hash-table #f])
                ((if display? orig-display orig-write) obj pport)))]
         [(boolean? obj)
          (out (if long-bools?
                   (if obj "#true" "#false")
                   (if obj "#t" "#f")))]
         [(number? obj)
          (when (and show-inexactness?
                     (inexact? obj))
            (out "#i"))
          (out ((if exact-as-decimal?
                    number->decimal-string
                    number->string)
                obj))]
         [(and (pretty-print-.-symbol-without-bars)
               (eq? obj '|.|))
          (out ".")]
         [(and qd (or (symbol? obj)
                      (keyword? obj)))
          (unless (eq? obj struct-ellipses)
            (to-quoted out qd obj))
          (orig-write obj pport)]
         [else
          ((if display? orig-display orig-write) obj pport)]))
    (unless (hide? obj)
      (post-print pport obj)))

  ;; ------------------------------------------------------------
  ;; pp: write on (potentially) multiple lines
  (define (pp* pport obj depth display? qd)

    (define (pp obj depth)
      (pp* pport obj depth display? qd))

    (define (out str)
      (write-string str pport))
    
    (define (spaces n)
      (add-spaces n pport))

    (define (ccol)
      (let-values ([(l col p) (port-next-location pport)])
        col))

    (define (indent to)
      (let ([col (ccol)])
        (if (< to col)
            (begin
              (let ([col ((printing-port-print-line pport) #t col width)])
                (spaces (- to col))))
            (spaces (max 0 (- to col))))))

    (define (pr obj extra pp-pair depth qd)
      ;; may have to split on multiple lines
      (let* ([obj (if (hide? obj) (hide-val obj) obj)]
             [can-multi (and width
                             (not (size-hook obj display?))
                             (or (pair? obj)
                                 (mpair? obj)
                                 (vector? obj) 
                                 (flvector? obj) 
                                 (fxvector? obj) 
                                 (and (box? obj) print-box?)
                                 (and (custom-write? obj)
                                      (not (struct-type? obj)))
                                 (and (struct? obj) print-struct?)
                                 (and (hash? obj) print-hash-table?)))]
             [graph-ref (if can-multi
                            (and found (hash-ref found obj #f))
                            #f)]
             [old-counter cycle-counter])
        (if (and can-multi
                 (or (not graph-ref) 
                     (not (unbox (mark-def graph-ref)))))
            ;; It might be possible to split obj across lines.
            ;; Try writing the obj, but accumulate the info that goes out
            ;;  into a-pport
            (let ([a-pport
                   (let/ec esc
                     (letrec ([a-pport (make-tentative-output-port
                                        pport 
                                        (- width extra)
                                        (lambda () (esc a-pport)))])
                       ;; Here's the attempt to write on one line:
                       (wr* a-pport obj depth display? qd)
                       a-pport))])
              (let-values ([(l c p) (port-next-location a-pport)])
                (if (<= c (- width extra))
                    ;; All can be printed on one line, so just dump the
                    ;;  accumulated text
                    (tentative-pretty-print-port-transfer a-pport pport)
                    ;; Doesn't fit on one line, so start over
                    (begin
                      (tentative-pretty-print-port-cancel a-pport)
                      (set! cycle-counter old-counter)
                      (when graph-ref
                        (expr-found pport graph-ref))
                      (pre-print pport obj)
                      (cond
                       [(pair? obj) 
                        (let* ([qd (to-quoted out qd obj)]
                               [pair (if (and qd (zero? qd))
                                         (convert-pair obj)
                                         obj)])
                          (pp-pair pair extra depth 
                                   pair? car cdr pair-open pair-close
                                   qd))]
                       [(mpair? obj)
                        (if (and qd (zero? qd))
                            (pp-pair (list (make-unquoted 'mcons) (mcar obj) (mcdr obj))
                                     extra depth 
                                     pair? car cdr pair-open pair-close
                                     qd)
                            (pp-pair obj extra depth 
                                     mpair? mcar mcdr mpair-open mpair-close
                                     qd))]
                       [(vector? obj)
                        (let ([qd (to-quoted out qd obj)]
                              [vecl (vector->repeatless-list obj)])
                          (if (and qd (zero? qd))
                              (pp-pair (cons (make-unquoted 'vector) vecl)
                                       extra depth 
                                       pair? car cdr pair-open pair-close
                                       qd)
                              (begin
                                (out "#")
                                (when print-vec-length?
                                  (out (number->string (vector-length obj))))
                                (pp-list vecl extra pp-expr #f depth
                                         pair? car cdr pair-open pair-close
                                         qd))))]
                       [(flvector? obj)
                        (let ([vecl (flvector->repeatless-list obj)])
                          (if (and qd (zero? qd))
                              (pp-pair (cons (make-unquoted 'flvector) vecl)
                                       extra depth
                                       pair? car cdr pair-open pair-close
                                       qd)
                              (begin
                                (out "#fl")
                                (when print-vec-length?
                                  (out (number->string (flvector-length obj))))
                                (pp-list vecl extra pp-expr #f depth
                                         pair? car cdr pair-open pair-close
                                         qd))))]
                       [(fxvector? obj)
                        (let ([vecl (fxvector->repeatless-list obj)])
                          (if (and qd (zero? qd))
                              (pp-pair (cons (make-unquoted 'fxvector) vecl)
                                       extra depth
                                       pair? car cdr pair-open pair-close
                                       qd)
                              (begin
                                (out "#fx")
                                (when print-vec-length?
                                  (out (number->string (fxvector-length obj))))
                                (pp-list vecl extra pp-expr #f depth
                                         pair? car cdr pair-open pair-close
                                         qd))))]
                       [(and (custom-write? obj)
                             (not (struct-type? obj)))
                        (let ([qd (let ([kind (if (custom-print-quotable? obj)
                                                (custom-print-quotable-accessor obj)
                                                'self)])
                                    (if (memq kind '(self never))
                                        qd
                                        (to-quoted out qd obj)))])
                          (write-custom pp* obj pport depth display? width qd #t))]
                       [(struct? obj) ; print-struct is on if we got here
                        (let* ([v (struct->vector obj struct-ellipses)]
                               [pf? (prefab?! obj v)])
                          (let ([qd (if pf?
                                        (to-quoted out qd obj)
                                        qd)])
                            (when (or (not qd) (positive? qd))
                              (out "#")
                              (when pf? (out "s")))
                            (pp-list (let ([l (vector->list v)])
                                       (if (and qd (zero? qd))
                                           (cons (make-unquoted (object-name obj))
                                                 (cdr l))
                                           l))
                                     extra pp-expr #f depth
                                     pair? car cdr pair-open pair-close
                                     qd)))]
                       [(hash? obj)
                        (let* ([qd (to-quoted out qd obj)]
                               [expr? (and qd (zero? qd))])
                          (unless expr?
                            (out (if (hash-eq? obj)
                                     "#hasheq"
                                     (if (hash-eqv? obj)
                                         "#hasheqv"
                                         "#hash"))))
                          (pp-list (convert-hash obj expr?) extra pp-expr #f depth
                                   pair? car cdr pair-open pair-close
                                   qd))]
                       [(and (box? obj) print-box?)
                        (let ([qd (to-quoted out qd obj)])
                          (if (and qd (zero? qd))
                              (pp-pair (list (make-unquoted 'box) (unbox obj))
                                       extra depth 
                                       pair? car cdr pair-open pair-close
                                       qd)
                              (begin
                                (out "#&") 
                                (pr (unbox obj) extra pp-pair depth qd))))])
                      (post-print pport obj)))))
            ;; Not possible to split obj across lines; so just write directly
            (wr* pport obj depth display? qd))))

    (define (pp-expr expr extra depth
                     apair? acar acdr open close
                     qd)
      (if (and (read-macro? expr apair? acar acdr qd)
               (equal? open "(")
               (not (and found (hash-ref found (acdr expr) #f))))
          (begin
            (out (read-macro-prefix expr acar))
            (pr (read-macro-body expr acar acdr)
                extra
                pp-expr
                depth
                qd))
          (let ((head (acar expr)))
            (if (or (and (symbol? head)
                         (not (size-hook head display?)))
                    ((pretty-print-remap-stylable) head))
                (let ((proc (style head expr apair? acar acdr)))
                  (if proc
                      (let* ([qd (to-quoted out qd expr)]
                             [pair (if (and qd (zero? qd))
                                       (cons (make-unquoted 'list) obj)
                                       obj)])
                        (proc expr extra depth
                              apair? acar acdr open close
                              qd))
                      (if (and #f
                               ;; Why this special case? Currently disabled.
                               (> (string-length 
                                   (symbol->string
                                    (if (symbol? head)
                                        head
                                        ((pretty-print-remap-stylable) head))))
                                  max-call-head-width))
                          (pp-general expr extra #f #f #f pp-expr depth
                                      apair? acar acdr open close
                                      qd)
                          (pp-list expr extra pp-expr #t depth
                                   apair? acar acdr open close
                                   qd))))
                (pp-list expr extra pp-expr #t depth
                         apair? acar acdr open close
                         qd)))))

    (define (wr obj depth qd)
      (wr* pport obj depth display? qd))

    ;; (head item1
    ;;       item2
    ;;       item3)
    (define (pp-call expr extra pp-item depth
                     apair? acar acdr open close
                     qd)
      (out open)
      (wr (acar expr) (dsub1 depth) qd)
      (let ([col (+ (ccol) 1)])
        (pp-down close (acdr expr) col col extra pp-item #t #t depth
                 apair? acar acdr open close
                 qd)))

    ;; (head item1 item2
    ;;   item3
    ;;   item4)
    (define (pp-two-up expr extra pp-item depth
                       apair? acar acdr open close
                       qd)
      (out open)
      (let ([col (ccol)])
        (wr (acar expr) (dsub1 depth) qd)
        (out " ")
        (wr (acar (acdr expr)) (dsub1 depth) qd)
        (pp-down close (acdr (acdr expr)) (+ (ccol) 1) (+ col 1) extra pp-item #t #t depth
                 apair? acar acdr open close 
                 qd)))

    ;; (head item1
    ;;   item2
    ;;   item3)
    (define (pp-one-up expr extra pp-item depth
                       apair? acar acdr open close
                       qd)
      (out open)
      (let ([col (ccol)])
        (wr (acar expr) (dsub1 depth) qd)
        (pp-down close (acdr expr) (+ (ccol) 1) (+ col 1) extra pp-item #t #t depth
                 apair? acar acdr open close 
                 qd)))

    ;; (item1
    ;;  item2
    ;;  item3)
    (define (pp-list l extra pp-item check? depth
                     apair? acar acdr open close
                     qd)
      (out open)
      (let ([col (ccol)])
        (pp-down close l col col extra pp-item #f check? depth
                 apair? acar acdr open close
                 qd)))

    (define (pp-down closer l col1 col2 extra pp-item check-first? check-rest? depth
                     apair? acar acdr open close 
                     qd)
      (let loop ([l l] [icol col1] [check? check-first?])
        (check-expr-found
         l pport (and check? (apair? l))
         (lambda (s) 
           (indent col2)
           (out ".")
           (indent col2)
           (out s)
           (out closer))
         (lambda ()
           (indent col2)
           (out ".")
           (indent col2)
           (pr l extra pp-item depth qd)
           (out closer))
         (lambda ()
           (cond 
            [(apair? l)
             (let ([rest (acdr l)])
               (let ([extra (if (null? rest) (+ extra 1) 0)])
                 (indent icol)
                 (pr (acar l) extra pp-item (dsub1 depth) qd)
                 (loop rest col2 check-rest?)))]
            [(null? l)
             (out closer)]
            [else
             (indent col2)
             (out ".")
             (indent col2)
             (pr l (+ extra 1) pp-item (dsub1 depth) qd)
             (out closer)])))))

    (define (pp-general expr extra named? pp-1 pp-2 pp-3 depth
                        apair? acar acdr open close
                        qd)

      (define (tail1 rest col1 col3)
        (if (and pp-1 (apair? rest))
            (let* ((val1 (acar rest))
                   (rest (acdr rest))
                   (extra (if (null? rest) (+ extra 1) 0)))
              (indent col3)
              (pr val1 extra pp-1 depth qd)
              (tail2 rest col1 col3))
            (tail2 rest col1 col3)))

      (define (tail2 rest col1 col3)
        (if (and pp-2 (apair? rest))
            (let* ((val1 (acar rest))
                   (rest (acdr rest))
                   (extra (if (null? rest) (+ extra 1) 0)))
              (indent col3)
              (pr val1 extra pp-2 depth qd)
              (tail3 rest col1))
            (tail3 rest col1)))

      (define (tail3 rest col1)
        (pp-down close rest col1 col1 extra pp-3 #f #t depth
                 apair? acar acdr open close
                 qd))

      (let* ([head (acar expr)]
             [rest (acdr expr)]
             [col (ccol)])
        (out open)
        (wr head (dsub1 depth) qd)
        (if (and named? (apair? rest))
            (let* ((name (acar rest))
                   (rest (acdr rest)))
              (out " ")
              (wr name (dsub1 depth) qd)
              (tail1 rest (+ col indent-general) (+ (ccol) 1)))
            (tail1 rest (+ col indent-general) (+ (ccol) 1)))))

    (define (pp-expr-list l extra depth
                          apair? acar acdr open close
                          qd)
      (pp-list l extra pp-expr #t depth
               apair? acar acdr open close
               qd))

    (define (pp-lambda expr extra depth
                       apair? acar acdr open close
                       qd)
      (pp-general expr extra #f pp-expr-list #f pp-expr depth
                  apair? acar acdr open close
                  qd))

    (define (pp-if expr extra depth
                   apair? acar acdr open close
                   qd)
      (pp-general expr extra #f pp-expr #f pp-expr depth
                  apair? acar acdr open close
                  qd))

    (define (pp-cond expr extra depth
                     apair? acar acdr open close
                     qd)
      (pp-list expr extra pp-expr-list #t depth
               apair? acar acdr open close
               qd))

    (define (pp-syntax-case expr extra depth
                            apair? acar acdr open close
                            qd)
      (pp-two-up expr extra pp-expr-list depth
                 apair? acar acdr open close
                 qd))

    (define (pp-module expr extra depth
                       apair? acar acdr open close
                       qd)
      (pp-two-up expr extra pp-expr depth
                 apair? acar acdr open close
                 qd))

    (define (pp-make-object expr extra depth
                            apair? acar acdr open close
                            qd)
      (pp-one-up expr extra pp-expr-list depth
                 apair? acar acdr open close
                 qd))

    (define (pp-case expr extra depth
                     apair? acar acdr open close
                     qd)
      (pp-general expr extra #f pp-expr #f pp-expr-list depth
                  apair? acar acdr open close
                  qd))

    (define (pp-and expr extra depth
                    apair? acar acdr open close
                    qd)
      (pp-call expr extra pp-expr depth
               apair? acar acdr open close
               qd))

    (define (pp-let expr extra depth
                    apair? acar acdr open close
                    qd)
      (let* ((rest (acdr expr))
             (named? (and (apair? rest) (symbol? (do-remap (acar rest))))))
        (pp-general expr extra named? pp-expr-list #f pp-expr depth
                    apair? acar acdr open close
                    qd)))

    (define (pp-begin expr extra depth
                      apair? acar acdr open close
                      qd)
      (pp-general expr extra #f #f #f pp-expr depth
                  apair? acar acdr open close
                  qd))

    (define (pp-do expr extra depth
                   apair? acar acdr open close
                   qd)
      (pp-general expr extra #f pp-expr-list pp-expr-list pp-expr depth
                  apair? acar acdr open close
                  qd))

    ;; define formatting style (change these to suit your style)

    (define indent-general 2)

    (define max-call-head-width 5)

    (define (no-sharing? expr count apair? acdr)
      (if (apair? expr)
          (if (and found 
                   (hash-ref found (acdr expr) #f))
              #f
              (or (zero? count)
                  (no-sharing? (acdr expr) (sub1 count) apair? acdr)))
          #f))

    (define (style head expr apair? acar acdr)
      (case (look-in-style-table head)
        ((lambda λ define define-macro define-syntax
                 syntax-rules
                 shared
                 unless when)
         (and (no-sharing? expr 1 apair? acdr)
              pp-lambda))
        ((if set! set!-values)
         (and (no-sharing? expr 1 apair? acdr)
              pp-if))
        ((cond case-lambda)
         (and (no-sharing? expr 0 apair? acdr)
              pp-cond))
        ((case class) 
         (and (no-sharing? expr 1 apair? acdr)
              pp-case))
        ((and or import export 
              require require-for-syntax require-for-template 
              provide link
              public private override rename inherit field init)
         (and (no-sharing? expr 0 apair? acdr)
              pp-and))
        ((let letrec let*
           let-values letrec-values let*-values
           let-syntax letrec-syntax
           let-syntaxes letrec-syntaxes)
         (and (no-sharing? expr
                           (if (and (apair? (acdr expr))
                                    (symbol? (acar (acdr expr))))
                               2
                               1)
                           apair?
                           acdr)
              pp-let))
        ((begin begin0)
         (and (no-sharing? expr 0 apair? acdr)
              pp-begin))
        ((do letrec-syntaxes+values)
         (and (no-sharing? expr 2 apair? acdr)
              pp-do))
        ((module)
         (and (no-sharing? expr 2 apair? acdr)
              pp-module))
        ((send syntax-case instantiate)
         (and (no-sharing? expr 2 apair? acdr)
              pp-syntax-case))
        ((make-object)
         (and (no-sharing? expr 1 apair? acdr)
              pp-make-object))

        (else #f)))

    (pr obj 0 pp-expr depth qd))

  (define (to-quoted out qd obj)
    (and qd
         (if (zero? qd)
             (if (hash-ref escapes-table obj #f)
                 qd
                 (begin
                   (out "'")
                   (add1 qd)))
             qd)))

  ;; ------------------------------------------------------------
  ;; This is where generic-write's body expressions start

  ((printing-port-print-line pport) #t 0 width)
  (let ([qd (if print-as-qq? qq-depth #f)])
    (let-values ([(l col p) (port-next-location pport)])
      (if (and width (not (eq? width 'infinity)))
          (pp* pport obj depth display? qd)
          (wr* pport obj depth display? qd))))
  (when newline?
    (let-values ([(l col p) (port-next-location pport)])
      ((printing-port-print-line pport) #f col width))))

(define (look-in-style-table raw-head)
  (let ([head (do-remap raw-head)])
    (or (hash-ref (pretty-print-style-table-hash
                   (pretty-print-current-style-table))
                  head
          #f)
        head)))

(define (do-remap raw-head)
  (cond
    [((pretty-print-remap-stylable) raw-head)
     => 
     values]
    [else raw-head]))

(define (read-macro? l pair? car cdr qd)
  (define (length1? l) (and (pair? l) (null? (cdr l))))
  (and (pretty-print-abbreviate-read-macros)
       (let ((head (do-remap (car l))) (tail (cdr l)))
         (case head
           ((quote quasiquote syntax
             quasisyntax unsyntax unsyntax-splicing
             unquote unquote-splicing)
            (length1? tail))
           (else #f)))))
         
(define (read-macro-body l car cdr)
  (car (cdr l)))

(define (read-macro-prefix l car)
  (let ((head (do-remap (car l))))
    (case head
      ((quote)             "'")
      ((quasiquote)        "`")
      ((unquote)           ",")
      ((unquote-splicing)  ",@")
      ((syntax)            "#'")
      ((quasisyntax)       "#`")
      ((unsyntax)          "#,")
      ((unsyntax-splicing) "#,@"))))

(define pretty-print-handler
  (lambda (v)
    (unless (void? v)
            (pretty-print v))))

(define (number->decimal-string x)
  (cond
   [(or (inexact? x)
        (integer? x))
    (number->string x)]
   [(not (real? x))
    (let ([r (real-part x)]
          [i (imag-part x)])
      (format "~a~a~ai"
              (number->decimal-string r)
              (if (negative? i)
                  ""
                  "+")
              (number->decimal-string i)))]
   [else
    (let ([n (numerator x)]
          [d (denominator x)])
      ;; Count powers of 2 in denomintor
      (let loop ([v d][2-power 0])
        (if (and (positive? v)
                 (even? v))
            (loop (arithmetic-shift v -1) (add1 2-power))
            ;; Count powers of 5 in denominator
            (let loop ([v v][5-power 0])
              (if (zero? (remainder v 5))
                  (loop (quotient v 5) (add1 5-power))
                  ;; No more 2s or 5s. Anything left?
                  (if (= v 1)
                      ;; Denominator = (* (expt 2 2-power) (expt 5 5-power)).
                      ;; Print number as decimal.
                      (let* ([10-power (max 2-power 5-power)]
                             [scale (* (expt 2 (- 10-power 2-power))
                                       (expt 5 (- 10-power 5-power)))]
                             [s (number->string (* (abs n) scale))]
                             [orig-len (string-length s)]
                             [len (max (add1 10-power) orig-len)]
                             [padded-s (if (< orig-len len)
                                           (string-append
                                            (make-string (- len orig-len) #\0)
                                            s)
                                           s)])
                        (format "~a~a.~a"
                                (if (negative? n) "-" "")
                                (substring padded-s 0 (- len 10-power))
                                (substring padded-s (- len 10-power) len)))
                      ;; d has factor(s) other than 2 and 5.
                      ;; Print as a fraction.
                      (number->string x)))))))]))

(define (pretty-format t [w (pretty-print-columns)] #:mode [mode 'print])
  (parameterize ([pretty-print-columns w])
    (let ([op (open-output-string)])
      ((case mode
         [(print)   pretty-print]
         [(write)   pretty-write]
         [(display) pretty-display]
         [else (raise-argument-error 'pretty-format "(or/c 'print 'write 'display)" mode)])
       t op #:newline? #f)
      (get-output-string op))))




