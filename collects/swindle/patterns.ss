#lang mzscheme

(provide (all-from-except mzscheme
                          define-values
                          define
                          let-values
                          let*-values
                          letrec-values
                          let
                          let*
                          letrec
                          set!
                          set!-values
                          lambda))

(provide (rename define-values~ define-values)
         (rename define~        define)
         (rename let-values~    let-values)
         (rename let*-values~   let*-values)
         (rename letrec-values~ letrec-values)
         (rename let~           let)
         (rename let*~          let*)
         (rename letrec~        letrec)
         (rename set!~          set!)
         (rename set!-values~   set!-values)
         (rename lambda~        lambda))
(define-syntaxes (define-values~
                  define~
                  let-values~
                  let*-values~
                  letrec-values~
                  let~
                  let*~
                  letrec~
                  set!~
                  set!-values~
                  lambda~)
  (let ()
    (define (id->handlers id)
      (and (identifier? id)
           (syntax-local-value
            (datum->syntax-object id
                                  (string->symbol
                                   (string-append "extended-arg-keyword:"
                                                  (symbol->string
                                                   (syntax-e id))))
                                  id)
            (lambda () #f))))
    (define (flatten-extended-bindings/values stxs expr)
      (define temps (generate-temporaries stxs))
      (define (remove-false-2nd l)
        (let loop ([l l] [r '()])
          (if (null? l)
            (reverse r)
            (loop (cdr l) (if (cadar l) (cons (car l) r) r)))))
      (let loop (;; tail: listof (cons extended-id, assigned-temp)
                 [tail (map cons (syntax->list stxs) temps)]
                 ;; r: listof (list extended-ids new-temps convert-expr)
                 ;;        or (list extended-id same-temp #f)
                 [r '()]
                 ;; #f if non-id scanned, otherwise #t or 'first on first pass
                 [simple? 'first]
                 ;; vbinds: listof listof listof (vars expr)
                 [vbinds (list (list (list temps expr)))])
        (if (null? tail)
          (let ([r (reverse r)])
            (if simple?
              (if (eq? simple? 'first)
                (values stxs expr)
                (values (datum->syntax-object stxs (map car r) stxs)
                        (let loop ([vbs (reverse vbinds)])
                          (if (null? vbs)
                            (if (and (pair? r) (null? (cdr r)))
                              (quasisyntax/loc stxs #,(cadar r))
                              (quasisyntax/loc stxs (values #,@(map cadr r))))
                            (quasisyntax/loc stxs
                              (let-values #,(remove-false-2nd (car vbs))
                                #,(loop (cdr vbs))))))))
              ;; saw non-identifiers, so start another iteration
              (loop (apply append (map (lambda (x)
                                         (if (caddr x)
                                           (map cons (car x) (cadr x))
                                           (list (cons (car x) (cadr x)))))
                                       r))
                    '() #t (cons (map cdr r) vbinds))))
          (syntax-case (caar tail) ()
            [var (identifier? #'var)
             (loop (cdr tail) (cons (list (caar tail) (cdar tail) #f) r)
                   simple? vbinds)]
            [(id . xs) (identifier? #'id)
             (cond
              [(id->handlers #'id) =>
               (lambda (handlers)
                 (let ([bindings (syntax->list ((car handlers) #'xs))]
                       [new-expr ((cadr handlers) (cdar tail) #'xs)])
                   (unless (list? bindings)
                     (error 'extended-binding
                            "`~s->bindings' returned a non-list value: ~s"
                            (syntax-e #'id) bindings))
                   (loop (cdr tail)
                         (cons (list bindings (generate-temporaries bindings)
                                     new-expr)
                               r)
                         #f vbinds)))]
              [else (raise-syntax-error
                     'extended-binding
                     "got a form which is not an extended binding"
                     (caar tail) #'id)])]
            [_ (raise-syntax-error
                'extended-binding "bad binding" (caar tail))]))))
    (define (_define-values stx)
      (syntax-case stx ()
        [(_ (var ...) expr)
         (let-values ([(bindings expr)
                       (flatten-extended-bindings/values #'(var ...) #'expr)])
           (quasisyntax/loc stx (define-values #,bindings #,expr)))]))
    (define (_define stx)
      (syntax-case stx (values)
        [(_ (values x ...) expr)
         (syntax/loc stx (define-values~ (x ...) expr))]
        [(_ (id . xs) expr) (id->handlers #'id)
         (syntax/loc stx (define-values~ ((id . xs)) expr))]
        [(_ (id . xs) body0 body ...)
         (syntax/loc stx (define-values~ (id) (lambda~ xs body0 body ...)))]
        [(_ x expr)
         (syntax/loc stx (define-values~ (x) expr))]))
    (define (make-let-values let-form)
      (lambda (stx)
        (syntax-case stx ()
          [(_ (binding ...) body0 body ...)
           (quasisyntax/loc stx
             (#,let-form
                 #,(map (lambda (binding)
                          (syntax-case binding ()
                            [((var ...) expr)
                             (let-values ([(bindings expr)
                                           (flatten-extended-bindings/values
                                            #'(var ...) #'expr)])
                               (quasisyntax/loc binding
                                 (#,bindings #,expr)))]))
                        (syntax->list #'(binding ...)))
               body0 body ...))])))
    (define _let-values (make-let-values #'let-values))
    (define _let*-values (make-let-values #'let*-values))
    (define _letrec-values (make-let-values #'letrec-values))
    (define (make-let let-form label?)
      (lambda (stx)
        (syntax-case stx ()
          [(_ label ((var val) ...) body0 body ...)
           (and label? (identifier? #'label))
           (quasisyntax/loc stx
             ((letrec~ ([label (lambda~ (var ...) body0 body ...)]) label)
              val ...))]
          [(_ (binding ...) body0 body ...)
           (quasisyntax/loc stx
             (#,let-form #,(map (lambda (binding)
                                  (syntax-case binding (values)
                                    [((values x ...) expr) #'((x ...) expr)]
                                    [(x expr)              #'((x)     expr)]))
                                (syntax->list #'(binding ...)))
               body0 body ...))])))
    (define _let    (make-let #'let-values~    #t))
    (define _let*   (make-let #'let*-values~   #f))
    (define _letrec (make-let #'letrec-values~ #f))
    (define (_set! stx)
      (syntax-case stx (values)
        [(_ (values x ...) expr) (syntax/loc stx (set!-values~ (x ...) expr))]
        [(_ x expr)              (syntax/loc stx (set!-values~ (x) expr))]))
    (define (_set!-values stx)
      (syntax-case stx ()
        [(_ (var ...) expr)
         (let-values ([(bindings expr)
                       (flatten-extended-bindings/values #'(var ...) #'expr)])
           (quasisyntax/loc stx
             (set!-values #,bindings #,expr)))]))
    (define (_lambda stx)
      (syntax-case stx ()
        [(_ vars body0 body ...)
         (let loop ([vs #'vars] [newvars '()] [specials '()] [restarg '()])
           (syntax-case vs ()
             [((id xs ...) . rest) (identifier? #'id)
              (let ([newvar (car (generate-temporaries #'(id)))])
                (loop #'rest (cons newvar newvars)
                      (cons (list #'(id xs ...) newvar) specials)
                      restarg))]
             [(id . rest) (identifier? #'id)
              (loop #'rest (cons #'id newvars) specials restarg)]
             [id (identifier? #'id)
              (loop #'() newvars specials #'id)]
             [() (let ([args (datum->syntax-object
                              #'vars (append (reverse newvars) restarg)
                              #'vars)])
                   (if (null? specials)
                     (quasisyntax/loc stx (lambda #,args body0 body ...))
                     (quasisyntax/loc stx
                       (lambda #,args
                         (let~ #,(reverse specials)
                           body0 body ...)))))]))]))
    (values _define-values
            _define
            _let-values
            _let*-values
            _letrec-values
            _let
            _let*
            _letrec
            _set!
            _set!-values
            _lambda)))

;; These are used as procedures for the syntax level
(provide extended-arg-keyword:list extended-arg-keyword:vector)
(define-syntax extended-arg-keyword:list
  (list (lambda (vars) vars)
        (lambda (expr vars)
          (quasisyntax/loc expr (apply values #,expr)))))
(define-syntax extended-arg-keyword:vector
  (list (lambda (vars) vars)
        (lambda (expr vars)
          (quasisyntax/loc expr (apply values (vector->list #,expr))))))

;; quote turns implicit lists and vectors to explicit ones
(provide extended-arg-keyword:quote)
(define-syntax extended-arg-keyword:quote
  (list (lambda (vars)
          (define (do-vars vs)
            (datum->syntax-object
             vs (map (lambda (v)
                       (if (identifier? v) v (quasisyntax/loc v '#,v)))
                     (syntax->list vs))
             vs))
          (do-vars (syntax-case vars ()
                     [((v ...)) #'(v ...)] [(#(v ...)) #'(v ...)])))
        (lambda (expr vars)
          (syntax-case vars ()
            [((v ...))
             (quasisyntax/loc expr (apply values #,expr))]
            [(#(v ...))
             (quasisyntax/loc expr (apply values (vector->list #,expr)))]))))

;; (define (values a (list (vector b c) (vector d) (list)) e)
;;         (values 1 (list (vector 2 3) (vector 4) (list)) 5))
;; (list a b c d e)
;; (let ([(values a (list (vector b c) (vector d) (list)) e)
;;        (values 1 (list (vector 2 3) (vector 4) (list)) 5)])
;;   (list a b c d e))
;; (let* ([(list x y) (list 1 2)] [(list x y) (list y x)]) (list x y))
;; (let ([(values a '(#(b c) #(d) ()) e)
;;        (values 1 '(#(2 3) #(4) ()) 5)])
;;   (list a b c d e))
;; (map (lambda ((list x y)) (list y x)) '((1 2) (3 4)))
;; (let loop ([(list str n) (list "foo" 10)])
;;   (if (zero? n) str (loop (list (string-append str "!") (sub1 n)))))
;;
;; (module foo mzscheme
;;   (provide (struct point (x y)) extended-arg-keyword:make-point)
;;   (define-struct point (x y))
;;   (define-syntax extended-arg-keyword:make-point
;;     (list (lambda (vars) (syntax-case vars () ((x y) vars)))
;;           (lambda (expr vars)
;;             (quasisyntax/loc expr
;;               (values (point-x #,expr) (point-y #,expr)))))))
;; (require foo)
;; (define a (make-point 1 2))
;; (let ([(make-point x y) a]) (+ x y))
