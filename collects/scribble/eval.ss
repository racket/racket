
(module eval (lib "new-lambda.ss" "scribblings")
  (require "manual.ss"
           "struct.ss"
           "scheme.ss"
           "decode.ss"
           (lib "class.ss")
           (lib "file.ss")
           (lib "string.ss"))

  (provide interaction
           interaction-eval
           interaction-eval-show
           schemeblock+eval
           schememod+eval
           def+int
           defs+int
           examples
           examples*
           defexamples
           defexamples*
           as-examples

           current-int-namespace
           eval-example-string

           scribble-eval-handler)

  (define current-int-namespace (make-parameter (current-namespace)))
  (define scribble-eval-handler (make-parameter (lambda (c? x) (eval x))))

  (define image-counter 0)

  (define maxlen 60)

  (define (interleave title expr-paras val-list+outputs)
    (make-table
     #f
     (append
      (if title (list (list (make-flow (list title)))) null)
      (let loop ([expr-paras expr-paras]
                 [val-list+outputs val-list+outputs]
                 [first? #t])
        (if (null? expr-paras)
            null
            (append
             (list (list (let ([p (car expr-paras)])
                           (if (flow? p)
                               p
                               (make-flow (list p))))))
             (if (string=? "" (cdar val-list+outputs))
                 null
                 (list
                  (list
                   (make-flow 
                    (list
                     (let ([s (regexp-split #rx"\n"
                                            (regexp-replace #rx"\n$"
                                                            (cdar val-list+outputs)
                                                            ""))])
                       (if (= 1 (length s))
                           (make-paragraph
                            (list
                             (hspace 2)
                             (span-class "schemestdout" (car s))))
                           (make-table
                            #f
                            (map (lambda (s)
                                   (list (make-flow (list (make-paragraph
                                                           (list
                                                            (hspace 2)
                                                            (span-class "schemestdout" s)))))))
                                 s)))))))))
             (if (string? (caar val-list+outputs))
                 ;; Error result case:
                 (map
                  (lambda (s)
                    (list (make-flow (list (make-paragraph
                                            (list
                                             (hspace 2)
                                             (span-class "schemeerror"
                                                         (italic s))))))))
                  (let sloop ([s (caar val-list+outputs)])
                     (if ((string-length s) . > . maxlen)
                         ;; break the error message into multiple lines:
                         (let loop ([pos (sub1 maxlen)])
                           (cond
                            [(zero? pos) (cons (substring s 0 maxlen)
                                               (sloop (substring s maxlen)))]
                            [(char-whitespace? (string-ref s pos))
                             (cons (substring s 0 pos)
                                   (sloop (substring s (add1 pos))))]
                            [else (loop (sub1 pos))]))
                         (list s))))
                 ;; Normal result case:
                 (let ([val-list (caar val-list+outputs)])
                   (if (equal? val-list (list (void)))
                       null
                       (map (lambda (v)
                              (list (make-flow (list (make-paragraph 
                                                      (list
                                                       (hspace 2)
                                                       (span-class "schemeresult"
                                                                   (to-element/no-color v))))))))
                            val-list))))
             (loop (cdr expr-paras)
                   (cdr val-list+outputs)
                    #f)))))))

  (define (do-eval s)
    (syntax-case s (code:comment eval:alts)
     [(code:line v (code:comment . rest))
      (do-eval #'v)]
     [(eval:alts p e)
      (do-eval #'e)]
     [else
      (let ([o (open-output-string)])
        (parameterize ([current-output-port o])
          (with-handlers ([exn? (lambda (e)
                                  (cons (exn-message e)
                                        (get-output-string o)))])
            (cons (let ([v (do-plain-eval s #t)])
                    (copy-value v (make-hash-table)))
                  (get-output-string o)))))]))

  (define (install ht v v2)
    (hash-table-put! ht v v2)
    v2)

  ;; Since we evaluate everything in an interaction before we typeset,
  ;;  copy each value to avoid side-effects.
  (define (copy-value v ht)
    (cond
     [(and v (hash-table-get ht v #f))
      => (lambda (v) v)]
     [(string? v) (install ht v (string-copy v))]
     [(bytes? v) (install ht v (bytes-copy v))]
     [(pair? v) (let ([p (cons #f #f)])
                  (hash-table-put! ht v p)
                  (set-car! p (copy-value (car v) ht))
                  (set-cdr! p (copy-value (cdr v) ht))
                  p)]
     [(vector? v) (let ([v2 (make-vector (vector-length v))])
                    (hash-table-put! ht v v2)
                    (let loop ([i (vector-length v2)])
                      (unless (zero? i)
                        (let ([i (sub1 i)])
                          (vector-set! v2 i (copy-value (vector-ref v i) ht))
                          (loop i))))
                    v2)]
     [(box? v) (let ([v2 (box #f)])
                 (hash-table-put! ht v v2)
                 (set-box! v2 (copy-value (unbox v) ht))
                 v2)]
     [else v]))
            
  (define (strip-comments stx)
    (syntax-case stx (code:comment code:blank)
     [((code:comment . _) . rest)
      (strip-comments #'rest)]
     [(a . b)
      (datum->syntax-object stx
                            (cons (strip-comments #'a)
                                  (strip-comments #'b))
                            stx
                            stx
                            stx)]
     [code:blank #'(void)]
     [else stx]))
      

  (define (do-plain-eval s catching-exns?)
    (parameterize ([current-namespace (current-int-namespace)])
        (call-with-values (lambda () ((scribble-eval-handler) catching-exns? (strip-comments s))) list)))

  (define-syntax interaction-eval
    (syntax-rules ()
      [(_ e) (#%expression
              (begin (parameterize ([current-command-line-arguments #()])
                       (do-plain-eval (quote-syntax e) #f))
                     ""))]))


  (define (show-val v)
    (span-class "schemeresult"
                (to-element/no-color v)))

  (define-syntax interaction-eval-show
    (syntax-rules ()
      [(_ e) (#%expression
              (parameterize ([current-command-line-arguments #()])
                (show-val (car (do-plain-eval (quote-syntax e) #f)))))]))

  (define (eval-example-string s)
    (eval (read (open-input-string s))))

  (parameterize ([current-namespace (current-int-namespace)])
    (eval `(define eval-example-string ,eval-example-string)))

  (define-syntax schemeinput*
    (syntax-rules (eval-example-string eval:alts)
      [(_ (eval-example-string s))
       (make-paragraph
        (list
         (hspace 2)
         (tt "> ")
         (span-class "schemevalue" (schemefont s))))]
      [(_ (eval:alts a b)) (schemeinput* a)]
      [(_ e) (schemeinput e)]))

  (define-code schemeblock+line (to-paragraph/prefix (hspace 2) 
                                                     (hspace 2)
                                                     (list " ")))

  (define-syntax (schemedefinput* stx)
    (syntax-case stx (eval-example-string define define-values define-struct)
      [(_ (eval-example-string s))
       #'(schemeinput* (eval-example-string s))]
      [(_ (define . rest))
       (syntax-case stx ()
         [(_ e) #'(schemeblock+line e)])]
      [(_ (define-values . rest))
       (syntax-case stx ()
         [(_ e) #'(schemeblock+line e)])]
      [(_ (define-struct . rest))
       (syntax-case stx ()
         [(_ e) #'(schemeblock+line e)])]
      [(_ (code:line (define . rest) . rest2))
       (syntax-case stx ()
         [(_ e) #'(schemeblock+line e)])]
      [(_ e) #'(schemeinput e)]))

  (define-syntax titled-interaction
    (syntax-rules ()
      [(_ t schemeinput* e ...)
       (interleave t
                   (list (schemeinput* e) ...)
                   (map do-eval (list (quote-syntax e) ...)))]))

    (define-syntax interaction
      (syntax-rules ()
        [(_ e ...) (titled-interaction #f schemeinput* e ...)]))

  (define-syntax schemeblock+eval
    (syntax-rules ()
      [(_ e ...)
       (#%expression
        (begin (interaction-eval e) ...
               (schemeblock e ...)))]))

  (define-syntax schememod+eval
    (syntax-rules ()
      [(_ name e ...)
       (#%expression
        (begin (interaction-eval e) ...
               (schememod name e ...)))]))

  (define-syntax def+int
    (syntax-rules ()
      [(_ def e ...)
       (make-splice (list (schemeblock+eval def)
                          (interaction e ...)))]))

  (define-syntax defs+int
    (syntax-rules ()
      [(_ [def ...] e ...)
       (make-splice (list (schemeblock+eval def ...)
                          (interaction e ...)))]))

  (define example-title
    (make-paragraph (list "Examples:")))
  (define-syntax examples
    (syntax-rules ()
      [(_ e ...)
       (titled-interaction example-title schemeinput* e ...)]))
  (define-syntax examples*
    (syntax-rules ()
      [(_ example-title e ...)
       (titled-interaction example-title schemeinput* e ...)]))
  (define-syntax defexamples
    (syntax-rules ()
      [(_ e ...)
       (titled-interaction example-title schemedefinput* e ...)]))
  (define-syntax defexamples*
    (syntax-rules ()
      [(_ example-title e ...)
       (titled-interaction example-title schemedefinput* e ...)]))

  (define (do-splice l)
    (cond
     [(null? l) null]
     [(splice? (car l)) (append (splice-run (car l))
                                (do-splice (cdr l)))]
     [else (cons (car l) (do-splice (cdr l)))]))

  (define as-examples
    (case-lambda
     [(t) (as-examples example-title t)]
     [(example-title t)
      (make-table #f
                  (list
                   (list (make-flow (list example-title)))
                   (list (make-flow (do-splice (list t))))))])))
