#lang racket/base

(require "private/performance-hint.rkt")

;; Note: because of `define-inline''s dependencies, this module is pretty
;;  heavyweight. If this becomes a problem, we can export a
;;  `begin-encourage-inline' submodule (that only re-exports
;;  `begin-encourage-inline'), to make it available without the overhead.

(provide begin-encourage-inline
         define-inline)

(require (for-syntax syntax/parse syntax/define
                     racket/syntax racket/base)
         racket/stxparam)

(begin-for-syntax

 (define-splicing-syntax-class actual
   (pattern (~seq (~optional kw:keyword) arg:expr)
            #:with tmp (generate-temporary #'arg)
            #:attr for-aux (list (attribute kw) (attribute tmp))))

 (define-syntax-class formals
   (pattern () ; done
            #:with aux-args #'()
            #:with ids      #'()
            #:with rest-arg #'()) ; rest-arg is () for no rest arg or (id)
   (pattern rest:id
            #:with aux-args #'rest
            #:with ids      #'()
            #:with rest-arg #'(rest))
   (pattern (x:id . rest:formals)
            #:with aux-args #'(x . rest.aux-args)
            #:with ids      #'(x . rest.ids)
            #:with rest-arg #'rest.rest-arg)
   (pattern ([x:id default:expr] . rest:formals)
            ;; for aux-args, wrap defaults in syntax
            #:with aux-args #'([x #'default] . rest.aux-args)
            #:with ids      #'(x . rest.ids)
            #:with rest-arg #'rest.rest-arg)
   (pattern (kw:keyword x:id . rest:formals)
            #:with aux-args #'(kw x . rest.aux-args)
            #:with ids      #'(x . rest.ids)
            #:with rest-arg #'rest.rest-arg)
   (pattern (kw:keyword [x:id default:expr] . rest:formals)
            #:with aux-args #'(kw [x #'default] . rest.aux-args)
            #:with ids      #'(x . rest.ids)
            #:with rest-arg #'rest.rest-arg)))

(define-syntax (define-inline stx)
  (syntax-parse stx
    [(_ (name . formals) . body)
     (define-values (name lam) (normalize-definition stx #'lambda #t #t))
     (syntax-parse lam
       [(_ args . body)
        #`(define-inline-helper (#,name . args) . body)])]
    [_
     (raise-syntax-error
      'define-inline "only supports definitions with function headers" stx)]))

(define-syntax (define-inline-helper stx)
  (syntax-parse stx
    [(_ (name:id . args:formals) . body)
     (with-syntax* ([internal-name     (format-id #'name "~a-internal" #'name)]
                    [inline-name       (format-id #'name "~a-inline"   #'name)]
                    [function-aux      (format-id #'name "~a-aux"      #'name)]
                    [(arg-id ...)      #'args.ids]
                    [(rest-arg-id ...) #'args.rest-arg]
                    [(tmp-arg-id ...)  (generate-temporaries #'(arg-id ...))]
                    [(tmp-rest-arg-id ...)
                     (generate-temporaries #'(rest-arg-id ...))]
                    [body*
                     #'(let-syntax ([name (make-rename-transformer
                                           (quote-syntax internal-name))])
                         . body)])
       #`(begin
           ;; define a function version that recursive calls fall back to, to
           ;; avoid infinite expansion
           (define (internal-name . args) body*)
           (define-syntax-parameter name
             (syntax-id-rules ()
               [(_ . rest) (inline-name . rest)]
               [_ internal-name])) ; higher-order use
           (define-syntax (inline-name stx*)
             ;; generate a compile-time function that takes care of linking
             ;; formals and actuals (so we don't have to handle keyword
             ;; arguments manually)
             (define (function-aux . args.aux-args)
               ;; default values for optional arguments can refer to previous
               ;; arguments, which makes things tricky
               (with-syntax* ([tmp-arg-id      arg-id] ...
                              [tmp-rest-arg-id rest-arg-id] ...)
                 #'(let* ([arg-id tmp-arg-id] ...
                          [rest-arg-id
                           (list . tmp-rest-arg-id)] ...)
                     body*)))
             (...
              (syntax-parse stx*
                [(_ arg*:actual ...)
                 ;; let*-bind the actuals, to ensure that they're evaluated
                 ;; only once, and in order
                 #`(syntax-parameterize
                    ([name (make-rename-transformer #'internal-name)])
                    (let* ([arg*.tmp arg*.arg] ...)
                      #,(let* ([arg-entries     (attribute arg*.for-aux)]
                               [keyword-entries (filter car arg-entries)]
                               [positional-entries
                                (filter (lambda (x) (not (car x)))
                                        arg-entries)]
                               [sorted-keyword-entries
                                (sort keyword-entries
                                      string<?
                                      #:key (lambda (kw)
                                              (keyword->string
                                               (syntax-e kw))))])
                          (keyword-apply
                           function-aux
                           (map (lambda (x) (syntax-e (car x)))
                                sorted-keyword-entries)
                           (map cadr sorted-keyword-entries)
                           (map cadr positional-entries)))))])))))]))


(module+ test
  (require rackunit)

  ;; Compares output to make sure that things are evaluated the right number of
  ;; times, and in the right order.
  (define-syntax-rule (test/output expr res out)
    (let ()
      (define str (open-output-string))
      (check-equal? (parameterize ([current-output-port str]) expr) res)
      (check-equal? (get-output-string str) out)))

  (define-inline (f x)
    (+ x x))
  (test/output (f (begin (display "arg1") 1)) 2 "arg1")

  (define-inline (f2 x y)
    (+ x y))
  (test/output (f2 (begin (display "arg1") 1) (begin (display "arg2") 1))
               2 "arg1arg2")

  (define-inline (g #:x [x 0])
    (f x))
  (test/output (g #:x (begin (display "arg1") 1)) 2 "arg1")
  (test/output (g) 0 "")

  (define-inline (h #:x x)
    (f x))
  (test/output (h #:x (begin (display "arg1") 1)) 2 "arg1")

  (define-inline (i [x 0])
    (f x))
  (test/output (i (begin (display "arg1") 1)) 2 "arg1")
  (test/output (i) 0 "")

  (define-inline (j x #:y [y 0])
    (+ x y))
  (test/output (j (begin (display "arg1") 1) #:y (begin (display "arg2") 2))
               3 "arg1arg2")
  (test/output (j #:y (begin (display "arg1") 2) (begin (display "arg2") 1))
               3 "arg1arg2")
  (test/output (j (begin (display "arg1") 1)) 1 "arg1")

  (define-inline (k x [y x])
    (+ x y))
  (test/output (k (begin (display "arg1") 1) (begin (display "arg2") 2))
               3 "arg1arg2")
  (test/output (k (begin (display "arg1") 1)) 2 "arg1")

  (define-inline (l . x)
    (+ (apply + x) (apply + x)))
  (test/output (l (begin (display "arg1") 1) (begin (display "arg2") 2))
               6 "arg1arg2")

  (define-inline (l2 y . x)
    (+ y y (apply + x) (apply + x)))
  (test/output (l2 (begin (display "arg0") 3)
                   (begin (display "arg1") 1)
                   (begin (display "arg2") 2))
               12 "arg0arg1arg2")

  (define-inline (l3 y [z 0] . x)
    (+ y y z z z (apply + x) (apply + x)))
  (test/output (l3 (begin (display "arg0") 3)
                   (begin (display "arg1") 1)
                   (begin (display "arg2") 2))
               13 "arg0arg1arg2")
  (test/output (l3 (begin (display "arg0") 3))
               6 "arg0")

  (define-inline (l4 #:x [x 0] . y)
    (+ x x x (apply + y) (apply + y)))
  (test/output (l4 #:x (begin (display "arg1") 1))
               3 "arg1")
  (test/output (l4 #:x (begin (display "arg1") 1)
                   (begin (display "arg2") 2)
                   (begin (display "arg3") 3))
               13 "arg1arg2arg3")
  (test/output (l4 (begin (display "arg2") 2)
                   (begin (display "arg3") 3))
               10 "arg2arg3")

  ;; test for function fallback (recursion)
  (define-inline (sum . l)
    (if (null? l) 0 (+ (car l) (apply sum (cdr l)))))
  (check-equal? (sum 1 2 3 4) 10)

  ;; higher-order use
  (define-inline (add2 x)
    (+ x 2))
  (check-equal? (add2 3) 5)
  (check-equal? (map add2 '(1 2 3)) '(3 4 5))

  ;; currying syntax
  (define-inline ((adder x) y) (+ x y))
  (test/output (let ([add2 (adder (begin (display "arg1") 2))])
                 (+ (add2 (begin (display "arg2") 1))
                    (add2 (begin (display "arg2") 2))))
               7 "arg1arg2arg2")

  (define-inline (even? x) (if (zero? x) #t (odd?  (sub1 x))))
  (define-inline (odd? x)  (if (zero? x) #f (even? (sub1 x))))
  (check-equal? (odd? 2) #f)
  )
