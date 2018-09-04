
(load-relative "loadtest.rktl")

(Section 'stxparam)

(require racket/stxparam
         racket/splicing)

(define-syntax-parameter tHIs (lambda (stx) #'(quote orig)))
(define-syntax-rule (inDIRECt) tHIs)

(test 'orig values tHIs)
(test 'orig values (inDIRECt))

(test 'sub values (syntax-parameterize ([tHIs (lambda (stx) #'(quote sub))])
                    tHIs))
(test 'sub values (syntax-parameterize ([tHIs (lambda (stx) #'(quote sub))])
                    (inDIRECt)))
(test 'sub values (splicing-syntax-parameterize ([tHIs (lambda (stx) #'(quote sub))])
                    (inDIRECt)))

(define-syntax-parameter tHaT #f)
;; Make sure `syntax-parameterize` works at the top level:
(syntax-parameterize ([tHaT (lambda (stx) #'(quote sub))])
  (tHaT))

(module check-splicing-stxparam-1 racket/base
  (require (for-syntax racket/base)
           racket/stxparam
           racket/splicing)
  (define-syntax-parameter sp 'orig)
  (define-syntax (m stx)
    (define v (syntax-parameter-value #'sp))
    (syntax-case stx ()
      [(_ id) #`(define id (quote #,v))]))
  (m x)

  (splicing-syntax-parameterize ([sp 'sub])
    (begin
     (define other 'other)
     (m y)))

  (begin-for-syntax (define sp-val-1 (syntax-parameter-value #'sp)))
  (define-syntax (m1 stx)
    (syntax-case stx ()
      [(_ id) #`(define id (quote #,sp-val-1))]))
  (m1 z)
  
  (splicing-syntax-parameterize ([sp 'sub2])
    (begin-for-syntax (define sp-val-2 (syntax-parameter-value #'sp))))
  (define-syntax (m2 stx)
    (syntax-case stx ()
      [(_ id) #`(define id (quote #,sp-val-2))]))
  (m2 w)

  (splicing-syntax-parameterize ([sp 'unused])
    ;; make sure that `splicing-syntax-parameterize' can
    ;; deal with a variety of compile-time forms
    (begin-for-syntax
     (require racket/base)
     (define x 11)
     (provide x)))

  (define (f)
    (splicing-syntax-parameterize ([sp 'nested])
      (define-syntax m (let ([v (syntax-parameter-value #'sp)])
                         (lambda (stx)
                           #`(quote #,v)))))
    (m))
   
  (define (g)
    (syntax-parameterize ([sp 'hidden])
      (splicing-syntax-parameterize ([sp 'also-nested])
        (define-syntax m (let ([v (syntax-parameter-value #'sp)])
                           (lambda (stx)
                             #`(quote #,v)))))
      (m)))

  ; make sure splicing-syntax-parameterize works with (module* _ #f ....)
  (splicing-syntax-parameterize ([sp 'sub-submod])
    (module* sp-submod #f
      (provide b)
      (m b)))

  ; make sure it applies to #%module-begin for submodules
  (splicing-letrec-syntax ([#%module-begin (syntax-rules ()
                                             [(_) (#%plain-module-begin (begin (provide b) (m b)))])])
    (splicing-syntax-parameterize ([sp 'begin-defined])
      (module* sp-submod2 #f)))
   
  (provide x y z w f g))

(test 'orig dynamic-require ''check-splicing-stxparam-1 'x)
(test 'sub dynamic-require ''check-splicing-stxparam-1 'y)
(test 'orig dynamic-require ''check-splicing-stxparam-1 'z)
(test 'sub2 dynamic-require ''check-splicing-stxparam-1 'w)
(test 'nested values ((dynamic-require ''check-splicing-stxparam-1 'f)))
(test 'also-nested values ((dynamic-require ''check-splicing-stxparam-1 'g)))
(test 'sub-submod dynamic-require '(submod 'check-splicing-stxparam-1 sp-submod) 'b)
(test 'begin-defined dynamic-require '(submod 'check-splicing-stxparam-1 sp-submod2) 'b)

(module check-splicing-stxparam-et racket/base
  (require (for-syntax racket/base)
           'check-splicing-stxparam-1)
  (define-syntax (m stx) (datum->syntax stx x))
  (define q (m))
  (provide q))

(test 11 dynamic-require ''check-splicing-stxparam-et 'q)

;; ----------------------------------------
;; Check interaction with internal definition contexts,
;; both at expression and module levels:

(module stxparam-interaction-with-block racket/base
  (require racket/stxparam
           racket/block
           (for-syntax racket/base))

  (define-syntax-parameter x (lambda (stx) #'10))

  (let ()
    (block
     (syntax-parameterize ([x (lambda (stx) #'11)])
       (let ()
         x))))

  (block
   (syntax-parameterize ([x (lambda (stx) #'12)])
     (let ()
       x))))

(test "11\n12\n"
      get-output-string
      (parameterize ([current-output-port (open-output-string)])
        (dynamic-require ''stxparam-interaction-with-block #f)
        (current-output-port)))

;; ----------------------------------------
;; Make sure a generated name is not ambiguous relative to
;; a directly imported or defined name:

(module stxparam-generated-name-no-conflict racket/base
  (require racket/stxparam (for-syntax racket/base))
  (define-syntax-parameter add (make-rename-transformer #'+))
  add)

;; ----------------------------------------

(let ()
  (define-syntax (slv stx)
    (syntax-case stx ()
      [(_ t)
       #`#,(syntax-local-value #'t)]))
  (define-syntax one 1)
  (define-syntax two 2)
  (define-syntax three 3)
  (define-rename-transformer-parameter num
    (make-rename-transformer #'one))
  (test #t = (slv num) 1)
  (syntax-parameterize ([num (make-rename-transformer #'two)])
    (test #t = (slv num) 2))
  (splicing-syntax-parameterize ([num (make-rename-transformer #'two)])
    (define too (slv num)))
  (test #t = too 2)
  (splicing-syntax-parameterize ([num (make-rename-transformer #'two)])
    (splicing-syntax-parameterize ([num (make-rename-transformer #'three)])
      (define trois (slv num))))
  (test #t = trois 3))

(let ()
  (define x 1)
  (define y 10)
  (define-rename-transformer-parameter num
    (make-rename-transformer #'y))
  (syntax-parameterize ([num (make-rename-transformer #'x)])
    (test #t = num 1)
    (set! num 3)
    (test #t = num 3))
  (test #t = x 3)
  (test #t = num 10))

;; ----------------------------------------

(let ()
  (define-syntax-rule (mac-zero) 0)
  (define-syntax-parameter x (make-rename-transformer #'mac-zero))
  (define-syntax-rule (mac-one) 1)
  (define-syntax y (make-rename-transformer #'x))
  (test #t = (mac-zero) 0)
  (test #t = (mac-one) 1)
  (test #t = (x) 0)
  (test #t = (y) 0)
  (test #t = (syntax-parameterize ([y (make-rename-transformer #'mac-one)]) (x)) 1)
  (test #t = (syntax-parameterize ([y (make-rename-transformer #'mac-one)]) (y)) 1))

;; ----------------------------------------

(let ([msg "syntax-parameterize: not bound as a syntax parameter"])
  (test (list msg) regexp-match
        (regexp-quote msg)
        (with-handlers ([exn:fail? (λ (x) (exn-message x))])
          (eval #'(syntax-parameterize ([x (make-rename-transformer #'f)]) 1)))))

;; ----------------------------------------
;; Check rename-transformer stx-param when used as an expression,
;; which involves calling the `prop:rename-transformer` in a
;; `syntax-transforming?` mode

(begin
  (define-syntax (slv stx)
    (syntax-case stx ()
      [(_ t)
       #`'#,(object-name (syntax-local-value #'t))]))

  (define-syntax one (procedure-rename (λ (stx) #'1) 'one))
  (define-syntax two (procedure-rename (λ (stx) #'2) 'two))

  (define-syntax-parameter normal
    (make-rename-transformer #'one))
  (test #f eq? (slv normal) 'one)
  (test #t = normal 1)
  (syntax-parameterize ([normal (make-rename-transformer #'two)])
    (test #f eq? (slv normal) 'two)
    (test #t = normal 2))

  (define-rename-transformer-parameter rt
    (make-rename-transformer #'one))
  (test #t eq? (slv rt) 'one)
  (test #t = rt 1)
  (syntax-parameterize ([rt (make-rename-transformer #'two)])
    (test #t eq? (slv rt) 'two)
    (test #t = rt 2)))

;; ----------------------------------------

(report-errs)

