#lang racket/load

(require (for-syntax racket/unit-exptime))
(require "test-harness.rkt"
         racket/unit)

(define-signature one^ (one-a one-b))
(define-signature two^ (two-a 
                        (define-values (two-v1 two-v2) (values 1 2))
                        (define-syntaxes (m) (syntax-rules () [(_) two-v2]))))
(define-signature three^ ())
(define-signature four^ extends two^ (four-z))

(define-unit one@
  (import one^ three^)
  (export two^)
  (init-depend one^)
  (define two-a 10))

(define-unit two@
  (import (tag Four four^))
  (export (tag One one^))
  (init-depend (tag Four four^))
  (define one-a 10)
  (define one-b 20))

(define-syntax (unit-info stx)
  (syntax-case stx ()
    [(_ id k) (let-values ([(ins out)
                            (unit-static-signatures #'id stx)])
                #`(k (#,ins #,out)))]))

(define-syntax (sig-info stx)
  (syntax-case stx ()
    [(_ id k) (let-values ([(super vars def-vars def-macs)
                            (signature-members #'id stx)])
                #`(k (#,super #,vars #,def-vars #,def-macs)))]))

(define-syntax (unit-dep-info stx)
  (syntax-case stx ()
    [(_ id k) (let ([deps (unit-static-init-dependencies #'id stx)])
                #`(k #,deps))]))

(test '(#f (one-a one-b) () ()) (sig-info one^ quote))
(test '(#f (two-a) (two-v1 two-v2) (m)) (sig-info two^ quote))
(test '(#f () () ()) (sig-info three^ quote))
(test '(two^ (two-a four-z) (two-v1 two-v2) (two-a two-v1 two-v2 m m)) (sig-info four^ quote))

(test '(((#f . one^) (#f . three^)) ((#f . two^))) (unit-info one@ quote))
(test '(((Four . four^)) ((One . one^))) (unit-info two@ quote))

(test '((#f . one^)) (unit-dep-info one@ quote))
(test '((Four . four^)) (unit-dep-info two@ quote))

(module m racket/base
  (require racket/unit
           (for-syntax racket/base))
  (provide x^ y^)
  (define-signature x^ (x))
  (define-signature y^ (y
                        (define-syntaxes (get-y) (lambda (stx) #'y))
                        (define-values (y2) 10))))
(require 'm)

;; based on code by Jay:
(define-syntax (as-s stx)
  (syntax-case stx ()
    [(_ e ...)
     (with-syntax ([([i-export e-export] ...)
                    (let-values ([(_1 ids _2 _3) (signature-members #'x^ stx)])
                      (for/list ([i (in-list ids)])
                        (list i (datum->syntax stx (syntax->datum i)))))]
                   [([i-import e-import] ...)
                    (let-values ([(_1 ids var-ids stx-ids) (signature-members #'y^ stx)])
                      (for/list ([i (in-list (append ids var-ids stx-ids))])
                        (list i (datum->syntax stx (syntax->datum i)))))])
       (syntax/loc stx
         (unit (import y^) (export x^)
           (define-syntax e-import (make-rename-transformer #'i-import))
           ...
           e ...
           (define i-export e-export)
           ...)))]))
(define y 'y)
(define-values/invoke-unit
  (as-s (define x (list y (get-y) y2)))
  (import y^)
  (export x^))
(test '(y y 10) x)

(displayln "tests passed")
