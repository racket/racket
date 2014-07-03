#lang racket/base

(require
  "test-utils.rkt"
  racket/base
  racket/port
  racket/promise
  rackunit
  (for-syntax
    racket/base
    racket/format
    syntax/parse))

(provide tests)
(gen-test-main)

(define-namespace-anchor anchor)

(define base-ns
  (delay
    (define ns (namespace-anchor->empty-namespace anchor))
    (parameterize ([current-namespace ns])
      (namespace-require 'typed/racket/base))
    ns))

;; get-ns: boolean? -> namespace?
;; Returns a namespace with the bindings of typed/racket/base.
;; If the argument is true, then it is a new namespace. This is slower but allows for tests that need
;; to mutate the namespace to not clash with each other.
(define (get-ns fresh)
  (if fresh
      (let ([ns (variable-reference->empty-namespace
                  (eval '(#%variable-reference) (force base-ns)))])
        (parameterize ([current-namespace ns])
          (namespace-require 'typed/racket/base)
          ns))
      (force base-ns)))

(begin-for-syntax
  (define-splicing-syntax-class fresh-kw
    (pattern (~seq) #:attr fresh #'#f)
    (pattern #:fresh #:attr fresh #'#t)))

(define-syntax (test-form-exn stx)
  (syntax-parse stx
    [(_ f:fresh-kw regexp:expr form:expr)
     (quasisyntax/loc stx
       (test-case #,(~a (syntax->datum #'form))
         (check-exn
           regexp
           (lambda ()
             (eval `(#%top-interaction .
                     ,(syntax->datum #'form)) (get-ns f.fresh))))))]))

(define-syntax (test-form-not-exn stx)
  (syntax-parse stx
    [(_ f:fresh-kw form:expr)
     (quasisyntax/loc stx
       (test-case #,(~a (syntax->datum #'form))
         (check-not-exn
           (lambda ()
             (eval `(#%top-interaction .
                     ,(syntax->datum #'form)) (get-ns f.fresh))))))]))

(define-syntax (test-form stx)
  (syntax-parse stx
    [(_ f:fresh-kw (~seq regexp:expr form:expr) ...)
     (quasisyntax/loc stx
       (test-case #,(~a (syntax->datum #'(form ...)))
         (define ns (get-ns f.fresh))
         (check-regexp-match
           regexp
           (with-output-to-string
             (lambda ()
               (eval `(#%top-interaction .
                       ,(syntax->datum #'form)) ns)))) ...))]))

;; Add 'only at the toplevel tests'
(define tests
  (test-suite "Interactive tests"

    (test-form #:fresh
      #rx"" (module test racket)
      #rx"" (define module displayln)
      #rx"racket" (module 'racket))

    (test-form (regexp-quote "String")
      "foo")
    (test-form (regexp-quote "String")
      (begin "foo"))
    (test-form (regexp-quote "String")
      (begin "foo" "bar"))
    (test-form #rx"^$"
      (begin))
    (test-form #rx"^$"
      (define x "foo"))
    (test-form #rx"^$"
      (begin (: x String)
             (define x "foo")))
    (test-form #rx"^$"
      (struct foo ()))

    ;; PR 14487
    (test-form-not-exn
      (require/typed racket/base
                     [#:opaque Evt evt?]
                     [alarm-evt (Real -> Evt)]
                     [sync (Evt -> Any)]))

    ;; PR 14380
    (test-form-not-exn (begin - (void)))

    ;; test message for undefined id
    (test-form-exn #rx"either undefined or missing a type annotation"
      (a-name-that-isnt-bound))

    (test-form #rx"1"
      (:type 1))
    (test-form (regexp-quote "(U Positive-Byte Zero)")
      (:type Byte))
    (test-form (regexp-quote "(U 0 1 Byte-Larger-Than-One")
      (:type #:verbose Byte))
    (test-form-exn #rx":type.*applied to arguments"
      :type)
    (test-form-exn #rx":type.*only valid at the top-level"
      (list (:type)))
    (test-form-exn #rx"exactly one argument"
      (:type))
    (test-form-exn #rx"exactly one argument"
      (:type 1 2))
    (test-form-exn #rx"exactly one argument"
      (:type #:verbose))

    (test-form #rx"Positive-Index"
      (:print-type (+ 1 1)))
    (test-form (regexp-quote "(values One One)")
      (:print-type (values 1 1)))
    (test-form-exn #rx":print-type.*applied to arguments"
      :print-type)
    (test-form-exn #rx":print-type.*only valid at the top-level"
      (list (:print-type)))
    (test-form-exn #rx"exactly one argument"
      (:print-type))
    (test-form-exn #rx"exactly one argument"
      (:print-type 1 2))
    (test-form (regexp-quote "has no type")
      (:print-type (begin (begin))))
    (test-form (regexp-quote "has no type")
      (:print-type (require racket/format)))

    (test-form (regexp-quote "(-> 4 Zero Zero)")
      (:query-type/args * 4 0))
    (test-form-exn #rx":query-type/args.*applied to arguments"
      :query-type/args)
    (test-form-exn #rx":query-type/args.*only valid at the top-level"
      (list (:query-type/args)))
    (test-form-exn #rx"at least one argument"
      (:query-type/args))

    (test-form (regexp-quote "(case-> (-> One One) (-> One))")
      (:query-type/result * 1))
    (test-form #rx"not in the given function's range.\n"
      (:query-type/result + String))
    (test-form-exn #rx":query-type/result.*applied to arguments"
      :query-type/result)
    (test-form-exn #rx":query-type/result.*only valid at the top-level"
      (list (:query-type/result)))
    (test-form-exn #rx"exactly two arguments"
      (:query-type/result))
    (test-form-exn #rx"exactly two arguments"
      (:query-type/result 1 2 3))
    (test-form #rx"not in the given function's range"
      (:query-type/result syntax-local-expand-expression Boolean))))
