#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse syntax/stx racket/match unstable/sequence unstable/syntax
         racket/dict
         (typecheck signatures)
         (types resolve union utils)
         (rep type-rep)
         (utils tc-utils)

         (for-label racket/base))


(import tc-expr^)
(export tc-app-objects^)

(define-literal-set object-literals #:for-label (list cons))


;; type-check object initialization and field operations
(define-tc/app-syntax-class (tc/app-objects expected)
  #:literal-sets (kernel-literals object-literals)
  (pattern (dmo b cl
            (#%plain-app list . pos-args)
            (#%plain-app list (#%plain-app cons (quote names) named-args) ...))
     #:declare dmo (id-from 'do-make-object 'racket/private/class-internal)
     (check-do-make-object #'b #'cl #'(names ...) #'(named-args ...)))
  (pattern (dmo . args)
     #:declare dmo (id-from 'do-make-object 'racket/private/class-internal)
     (int-err "unexpected arguments to do-make-object"))
  (pattern (gf meth obj)
     #:declare gf (id-from 'get-field/proc 'racket/private/class-internal)
     (check-get-field #'meth #'obj))
  (pattern (gf . args)
     #:declare gf (id-from 'get-field/proc 'racket/private/class-internal)
     (int-err "unexpected arguments to get-field/proc")))

;; check-do-make-object : Syntax Syntax Listof<Syntax> Listof<Syntax> -> TCResult
;; do-make-object now takes blame as its first argument, which isn't checked
;; (it's just an s-expression)
(define (check-do-make-object b cl names named-args)
  (define given-names (stx-map syntax-e names))
  (define name-assoc (for/list ([name (in-syntax names)]
                                [arg (in-syntax named-args)])
                       (list (syntax-e name) arg)))
  (match (resolve (tc-expr/t cl))
    [(Union: '()) (ret (Un))]
    [(and c (Class: _ inits fields _ _))
     (define init-names (map car inits))
     (for ([given-name given-names]
           #:unless (memq given-name init-names))
       (tc-error/delayed
        "unknown named argument ~a for class\nlegal named arguments are ~a"
        given-name (stringify init-names)))
     (for ([init inits])
       (match-define (list init-name init-type opt?) init)
       ;; stx if argument was provided, #f if it was
       ;; not provided (and if mandatory, it errors)
       (define maybe-stx
         (cond [(assq init-name name-assoc) => cadr]
               [(not opt?)
                (tc-error/delayed "value not provided for named init arg ~a"
                                  init-name)
                #f]
               [else #f]))
       (when maybe-stx
         (tc-expr/check maybe-stx (ret init-type))))
     (ret (make-Instance c))]
    [t
     (tc-error/expr #:return (ret (Un))
                    "expected a class value for object creation, got: ~a" t)]))

;; check-get-field : Syntax Syntax -> TCResult
;; type-check the `get-field` operation on objects
(define (check-get-field meth obj)
  (define maybe-meth-sym
    (syntax-parse meth [(quote m:id) (syntax-e #'m)] [_ #f]))
  (define obj-type (tc-expr obj))
  (unless maybe-meth-sym
    (tc-error/expr #:return (ret (Un))
                   "expected a symbolic method name, but got ~a" meth))
  (match obj-type
    ;; FIXME: handle unions and mu?
    [(tc-result1: (and ty (Instance: (Class: _ _ (list fields ...) _ _))))
     (cond [(assq maybe-meth-sym fields) =>
            (λ (field-entry) (ret (cadr field-entry)))]
           [else
            (tc-error/expr #:return (ret (Un))
                           "expected an object with field ~a, but got ~a"
                           maybe-meth-sym ty)])]
    [(tc-result1: t)
     (tc-error/expr #:return (ret (Un))
                    "expected an object value for get-field, got ~a" t)]))

