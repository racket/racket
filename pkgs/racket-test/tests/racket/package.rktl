
(load-relative "loadtest.rktl")
(require scheme/package)

(Section 'packages)

(define-syntax (test-pack-seq stx)
  (syntax-case stx ()
    [(_ result form ...)
     (let loop ([forms #'(form ...)]
                [pre null])
       (syntax-case forms ()
         [([#:fail expr exn?])
          (with-syntax ([(form ...) (reverse pre)])
            #`(test-pack-seq* (list (quote-syntax form) ...)
                              (quote-syntax [#:fail expr])
                              'expr
                              exn?))]
         [(expr) 
          (with-syntax ([(form ...) (reverse pre)])
            #`(test-pack-seq* (list (quote-syntax form) ...)
                              (quote-syntax expr)
                              'expr
                              result))]
         [([#:fail expr exn?] . more)
          #`(begin
              #,(loop #'([#:fail expr exn?]) pre)
              #,(loop #'more pre))]
         [(form . more)
          (loop #'more (cons #'form pre))]))]))

(define (fail? e)
  (syntax-case e ()
    [(#:fail e) #'e]
    [_ #f]))

(define (fail-expr e)
  (or (fail? e) e))

(define (test-pack-seq* forms expr q-expr result)
  (let ([orig (current-namespace)])
    ;; top level
    (let ([ns (make-base-namespace)])
      (parameterize ([current-namespace ns])
        (namespace-attach-module orig 'scheme/package)
        (namespace-require '(for-syntax scheme/base))
        (namespace-require 'scheme/package)
        (for-each eval forms)
        (if (fail? expr)
            (err/rt-test (eval (fail-expr expr)) result)
            (test result q-expr (eval expr)))))
    ;; let
    (let ([ns (make-base-namespace)])
      (parameterize ([current-namespace ns])
        (namespace-attach-module orig 'scheme/package)
        (namespace-require '(for-syntax scheme/base))
        (namespace-require 'scheme/package)
        (let ([e `(let () (begin . ,forms) ,(fail-expr expr))])
          (if (fail? expr)
              (err/rt-test (eval e) result)
              (test result `(let ... ,q-expr) (eval e))))))
    ;; nested let
    (let ([ns (make-base-namespace)])
      (parameterize ([current-namespace ns])
        (namespace-attach-module orig 'scheme/package)
        (namespace-require '(for-syntax scheme/base))
        (namespace-require 'scheme/package)
        (let ([e (let loop ([forms forms])
                   (if (null? (cdr forms))
                       `(let () (begin . ,forms) ,(fail-expr expr))
                       `(let () ,(car forms)
                             ,(loop (cdr forms)))))])
          (if (fail? expr)
              (err/rt-test (eval e) result)
              (test result `(let ... ,q-expr) (eval e))))))
    ;; module
    (let ([ns (make-base-namespace)])
      (parameterize ([current-namespace ns])
        (namespace-attach-module orig 'scheme/package)
        (let ([m `(module m scheme/base
                    (require (for-syntax scheme/base)
                             scheme/package)
                    (begin . ,forms)
                    (define result ,(fail-expr expr))
                    (provide result))])
          (if (fail? expr)
              (err/rt-test (eval m) exn:fail:syntax?)
              (begin
                (eval m)
                (test result `(module ... ,q-expr) (dynamic-require ''m 'result)))))))))

;; ----------------------------------------

(test-pack-seq
 12
 (define-package p (x)
   (define y 5)
   (define x 12))
 [#:fail x exn:fail:contract:variable?]
 (open-package p)
 x
 [#:fail y exn:fail:contract:variable?])

(test-pack-seq
 13
 (define-package p (q)
   (define-package q (x)
     (define y 8)
     (define x 13)))
 [#:fail x exn:fail:contract:variable?]
 [#:fail (open-package q) exn:fail:syntax?]
 (open-package p)
 [#:fail x exn:fail:contract:variable?]
 (open-package q)
 x
 [#:fail y exn:fail:contract:variable?])

(test-pack-seq
 14
 (define-package p (q)
   (define-package q (r)
     (define-package r (x)
       (define x 14))))
 [#:fail x exn:fail:contract:variable?]
 [#:fail (open-package q) exn:fail:syntax?]
 [#:fail (open-package r) exn:fail:syntax?]
 (open-package p)
 (open-package q)
 (open-package r)
 x)

(test-pack-seq
 15
 (define-package p (x)
   (define x 15))
 [#:fail x exn:fail:contract:variable?]
 (define-package q #:all-defined
   (open-package p))
  [#:fail x exn:fail:contract:variable?]
 (open-package q)
 x)

(test-pack-seq
 '(16 160)
 (define-package p #:all-defined
   (define x 16)
   (define y 160))
 (open-package p)
 (list x y))

(test-pack-seq
 170
 (define-package p #:all-defined-except (x)
   (define x 17)
   (define y 170))
 (open-package p)
 [#:fail x exn:fail:contract:variable?]
 y)

;; ----------------------------------------

(test-pack-seq
 2
 (define-package p (x)
   (define* x 1)
   (define* x 2))
 (open-package p)
 x)

(test-pack-seq
 14
 (define-package p (z)
   (define* x (lambda () y))
   (define z x)
   (define* x 2)
   (define y 14))
 (open-package p)
 (z))

(test-pack-seq
 21
 (define-package p (x)
   (define* x (lambda () y))
   (define* x2 0)
   (define* x3 1)
   (define* x4 1)
   (define y 21))
 (open-package p)
 (x))

(test-pack-seq
 '(2 1)
 (define-package p (x y)
   (define* x 1)
   (define y x)
   (define* x 2))
 (open-package p)
 (list x y))

(test-pack-seq
 '(2 1)
 (define-package p (x y)
   (define* x 1)
   (define y x)
   (define* x 2))
 (open-package p)
 (list x y))

;; ----------------------------------------

(test-pack-seq
 5
 (define-package p1 #:all-defined
   (define-package p2 ()
     (define x 10))
   (open-package p2))
 (open-package p1)
 [#:fail x exn:fail:contract:variable?]
 5)

;; ----------------------------------------

(test-pack-seq
 '(17 12)
 (define-syntax-rule (mk id)
   (begin
     (define-package p (x)
       (define x 17))
     (open-package p)
     (define id x)))
 (define x 12)
 (mk z)
 (list z x))

;; ----------------------------------------

(test-pack-seq
 10
 (define-package p5 (q)
   (define* x 10)
   (define-syntax (y stx) 
     (syntax-case stx ()
       [(_ z) #'(begin (define z x))]))
   (define* x 12)
   (define* z 13)
   (y q))
 (open-package p5)
 q)

;; ----------------------------------------
;; In a submodule

(module package-in-a-submodule racket/base
  (require racket/package)
  
  (define-package pkg (foo)
    (define foo 5))
  
  (module+ main
    (open-package pkg)
    (define out foo)
    (provide out)))

(test 5 dynamic-require '(submod 'package-in-a-submodule main) 'out)

;; ----------------------------------------

(report-errs)
