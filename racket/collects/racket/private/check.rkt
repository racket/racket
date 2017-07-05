#lang racket/base
(require racket/stxparam
         (for-syntax racket/base))

;; Support for writing contract checks as assertions at the beginning
;; of a function. The normal usage pattern is
;;
;;   (define/who (<proc> <arg> ....)
;;      (check who <predicate> <arg>)
;;      ...
;;      <implementation>)
;;
;; where `check/who` binds `who`, but `who` is just an expression
;; for `check`, so it can be a literal symbol (discouraged) or
;; passed to a helper function that performs checks (fine).
;;
;; The `check` macro supports `#:or-false` either before or after
;; <predicate>, which allows the <arg> to be false and constructs
;; a suitable contract string -- putting `#f` in the contract before
;; or after <predicate> to match `#:or-false`.
;;
;; The `check` macro also supports `#:contract <string>` after
;; <predicate> to provide a contract string other than the automatic
;; one.
;;
;; The `procedure-arity-includes/c` export here is a macro that
;; expands to a predicate that checks `procedure?` plus
;; `procedure-arity-incudes?`.

(provide check

         procedure-arity-includes/c
         
         define/who
         who)

;; ----------------------------------------

(define-syntax (check stx)
  (syntax-case stx ()
    [(_ who pred #:contract ctc v)
     #`(unless (pred v)
         (raise-argument-error who ctc v))]
    [(_ who pred #:or-false v)
     #`(check who (lambda (x) (or (not x) (pred x))) #:contract #,(format "(or/c ~a #f)" (syntax->datum #'pred)) v)]
    [(_ who #:or-false pred v)
     #`(check who (lambda (x) (or (not x) (pred x))) #:contract #,(format "(or/c #f ~a)" (syntax->datum #'pred)) v)]
    [(_ who #:test expr #:contract ctc v)
     #`(unless expr
         (raise-argument-error who ctc v))]
    [(_ who pred v)
     #`(check who pred #:contract #,(format "~a" (syntax->datum #'pred)) v)]))

;; ----------------------------------------

(define-syntax (procedure-arity-includes/c stx)
  (syntax-case stx ()
    [(_ n)
     (exact-nonnegative-integer? (syntax-e #'n))
     #'(lambda (p)
         (and (procedure? p)
              (procedure-arity-includes? p n)))]))

;; ----------------------------------------

(define-syntax-parameter who
  (lambda (stx)
    (raise-syntax-error #f "not defined" stx)))

(define-for-syntax (make-who id)
  (lambda (stx)
    (syntax-case stx ()
      [(who . _)
       (raise-syntax-error #f "cannot apply" #'who stx)]
      [else #`'#,id])))

(define-syntax (define/who stx)
  (syntax-case stx ()
    [(_ (id . args) . body)
     (syntax/loc stx
       (define id
         (lambda args
           (syntax-parameterize ([who (make-who 'id)])
             . body))))]
    [(_ id rhs)
     (syntax/loc stx
       (define id
         (syntax-parameterize ([who (make-who 'id)])
           rhs)))]))
