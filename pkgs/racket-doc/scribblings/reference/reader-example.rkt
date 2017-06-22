
(module reader-example racket/base
  (require scribble/struct
           (only-in scribble/core 
                    make-style
                    make-table-columns)
           scribble/decode
           scribble/manual
           scribble/scheme
           racket/class
           (for-label racket/base))

  (provide reader-examples
           read-quote-table
           dispatch-table
           dispatch
           metavar
           cilitchar
           ci0litchar)

  (define (as-flow i) (make-flow (list (if (block? i)
                                           i
                                           (make-paragraph (if (list? i)
                                                               i
                                                               (list i)))))))
  
  (define spacer (hspace 1))

  (define (reader-examples #:symbols? [symbols? #t] 
                           #:example-note [example-note ""]
                           . strs)
    (make-table
     #f
     (list
      (list (as-flow (list "Examples" example-note ":")))
      (list (make-flow
             (list
              (make-table
               (make-style #f (list (make-table-columns
                                     (list (make-style #f '(baseline))
                                           (make-style #f '(baseline))
                                           (make-style #f '(baseline))))))
               (map (lambda (s)
                      (list (as-flow (list spacer
                                           (litchar s)))
                            (as-flow (list spacer
                                           "reads equal to"
                                           spacer))
                            (as-flow (let ([v (read (open-input-string s))])
                                       (cond
                                        [(eof-object? v)
                                         (make-element 'italic '("nothing"))]
                                        [(string? v)
                                         (make-element value-color
                                                       (list (racketfont
                                                              (regexp-replace* #rx"[\\]\""
                                                                               (regexp-replace*
                                                                                #rx"[\\][\\]"
                                                                                (format "~s" v)
                                                                                "\\\\x5C")
                                                                               "\\\\x22"))))]
                                        [else
                                         (let ([e (let loop ([v v])
                                                    (cond
                                                     [(memq v '(quasiquote unquote +)) `',v]
                                                     [(symbol? v) (if symbols?
                                                                      `(quote ,v)
                                                                      `(string->symbol ,(format "~a" v)))]
                                                     [(number? v)
                                                      (let loop ([v v])
                                                        (if (inexact? v)
                                                            `(exact->inexact ,(loop (inexact->exact v)))
                                                            (cond
                                                             [(integer? v) v]
                                                             [(real? v) `(/ ,(numerator v)
                                                                            ,(denominator v))]
                                                             [(complex? v) `(make-complex ,(loop (real-part v))
                                                                                          ,(loop (imag-part v)))])))]
                                                     [(list? v) `(list ,@(map loop v))]
                                                     [(vector? v) `(vector ,@(map loop (vector->list v)))]
                                                     [(box? v) `(box ,(loop (unbox v)))]
                                                     [(and (pair? v)
                                                           (eq? v (cdr v))
                                                           (eq? 1 (car v)))
                                                      (racketblock0 (let* ([ph (make-placeholder #f)]
                                                                           [v (cons 1 ph)])
                                                                      (placeholder-set! ph v)
                                                                      (make-reader-graph v)))]
                                                     [(pair? v) `(cons ,(loop (car v)) ,(loop (cdr v)))]
                                                     [(bytes? v) `(bytes ,@(map loop (bytes->list v)))]
                                                     [(char? v) `(integer->char ,(char->integer v))]
                                                     [(keyword? v) `(string->keyword ,(keyword->string v))]
                                                     [(or (regexp? v)
                                                          (byte-regexp? v))
                                                      `(,(cond
                                                          [(pregexp? v) 'pregexp]
                                                          [(byte-pregexp? v) 'byte-pregexp]
                                                          [(byte-regexp? v) 'byte-regexp]
                                                          [else 'regexp])
                                                        ,(object-name v))]
                                                     [(hash? v)
                                                      `(,(if (hash-eq? v)
                                                             'make-...eq
                                                             'make-...)
                                                        (quote ,(hash-map v cons)))]
                                                     [else v]))])
                                           (if (block? e)
                                               e
                                               (to-element (syntax-ize e 0))))])))))
                    strs))))))))

  (define (read-quote-table . l)
    (make-table
     #f
     (map (lambda (p)
            (list (as-flow spacer)
                  (as-flow (car p))
                  (as-flow (list spacer "adds" spacer))
                  (as-flow (cadr p))))
          l)))


  (define (dispatch-table . l)
    (make-table
     '((alignment . (left right left left)))
     (map (lambda (p)
            (list (as-flow spacer)
                  (as-flow (car p))
                  (as-flow spacer)
                  (as-flow (cadr p))))
          l)))

  (define (dispatch a . b)
    (list a (make-element #f (decode-content b))))

  (define (metavar . s)
    (make-element 'italic (decode-content s)))

  (define (cilitchar s #:just-first? [just-first? #f])
    (let ([ss (map list->string
                   (let loop ([l (string->list s)])
                     (if (null? l)
                         (list null)
                         (let ([r (loop (cdr l))])
                           (if (char-alphabetic? (car l))
                               (if just-first?
                                   (list
                                    (cons (char-downcase (car l)) (cdr l))
                                    (cons (char-upcase (car l)) (cdr l)))
                                   (append
                                    (map (lambda (i) (cons (char-downcase (car l)) i)) r)
                                    (map (lambda (i) (cons (char-upcase (car l)) i)) r)))
                               (map (lambda (i) (cons (car l) i)) r))))))])
      (case (length ss) 
        [(1) (litchar (car ss))]
        [(2) (make-element #f (list (litchar (car ss)) " or " 
                                    (litchar (cadr ss))))]
        [else
         (make-element #f
                       (let loop ([ss ss])
                         (if (null? (cdr ss))
                             (list " or " (litchar (car ss)))
                             (list* (litchar (car ss))
                                    ", "
                                    (loop (cdr ss))))))])))

  (define (ci0litchar s)
    (cilitchar s #:just-first? #t)))

