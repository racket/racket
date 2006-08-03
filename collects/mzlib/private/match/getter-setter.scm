;; This library is used by match.ss

(module getter-setter mzscheme
  (provide getter setter)
  (require "coupling-and-binding.scm"
           "match-helper.ss"
           "match-error.ss"
           (lib "stx.ss" "syntax"))
  (require-for-template mzscheme
			"match-error.ss")
  
  ;;!(function setter
  ;;          (form (setter e ident let-bound) -> syntax)
  ;;          (contract (syntax syntax list) -> syntax)
  ;;          (example (setter (syntax (car x)) (syntax here) '())
  ;;                   ->
  ;;          (syntax (lambda (y) (set-car! x y)))))
  ;; This function takes an expression and returns syntax which
  ;; represents a function that is able to set the value that the
  ;; expression points to.
  (define (setter e ident let-bound)
    (define (subst e) (subst-bindings e let-bound))
    (define (mk-setter s cxt) (datum->syntax-object cxt (symbol-append 'set- s '!)))
    (syntax-case e (vector-ref unbox car cdr)
      [p
       (not (stx-pair? #'p))
       (match:syntax-err
        ident
        "set! pattern should be nested inside of a list, vector or box")]
      [(vector-ref vector index)
       #`(let ((x #,(subst #'vector)))
           (lambda (y) (vector-set! x index y)))]
      [(unbox boxed)
       #`(let ((x #,(subst #'boxed)))
           (lambda (y) (set-box! x y)))]
      [(car exp)
       #`(let ((x #,(subst #'exp)))
           (lambda (y) (set-car! x y)))]
      [(cdr exp)
       #`(let ((x #,(subst #'exp)))
           (lambda (y) (set-cdr! x y)))]
      [(acc exp)
       (let ([a (assq (syntax-object->datum #'acc) get-c---rs)])
         (if a
             #`(let ((x (#,(cadr a) #,(subst #'exp))))
                 (lambda (y) (#,(mk-setter (cddr a) #'acc) x y)))
             #`(let ((x #,(subst #'exp)))
                 (lambda (y) 
                   (#,(mk-setter (syntax-object->datum #'acc) #'acc) x y)))))]))
  
  ;;!(function getter
  ;;          (form (getter e ident let-bound) -> syntax)
  ;;          (contract (syntax syntax list) -> syntax)
  ;;          (example (getter (syntax (car x)) (syntax here) '())
  ;;                   ->
  ;;          (syntax (lambda () (car x)))))
  ;; This function takes an expression and returns syntax which
  ;; represents a function that is able to get the value that the
  ;; expression points to.
  (define (getter e ident let-bound)
    (define (subst e) (subst-bindings e let-bound))
    (syntax-case e (vector-ref unbox car cdr)
      [p
       (not (stx-pair? #'p))
       (match:syntax-err 
        ident
        "get! pattern should be nested inside of a list, vector or box")]
      [(vector-ref vector index)
       #`(let ((x #,(subst #'vector)))
           (lambda () (vector-ref x index)))]
      [(acc exp)
       #`(let ((x #,(subst #'exp)))
           (lambda () (acc x)))]))
)