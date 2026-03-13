#lang racket/base
(require (for-syntax racket/base
                     syntax/parse/pre)
         racket/fixnum
         "string-convert.rkt"
         "base-pred.rkt")

(provide (protect-out
          ->
          struct
          union
          array
          system-type-case
          default_abi
          stdcall_abi
          cdecl_abi))

(begin-for-syntax
  (provide (struct-out ffi2-type)
           make-ffi2-type
           ffi2-type-compound?
           ffi2-type-pointer?
           ffi2-type-pointer-vm-type
           pointer-vm-type->gcable
           ffi2-type-immediate-pointer?
           ffi2-type-scalar?
           lookup-type
           :malloc-kind
           :tag
           :array-size
           (struct-out procedure-abi))

  (struct ffi2-type (name vm-type category predicate racket->c c->racket release))

  (struct ffi2-type/proc ffi2-type (proc)
    #:property prop:procedure 0)

  (define (make-ffi2-type name vm-type predicate
                          #:category [category #f]
                          #:procedure [proc #f]
                          #:racket->c [racket->c #'values]
                          #:c->racket [c->racket #'values]
                          #:release [release #'drop])
    (if proc
        (ffi2-type/proc name vm-type category predicate racket->c c->racket release proc)
        (ffi2-type name vm-type category predicate racket->c c->racket release)))

  (define (ffi2-type-compound? t)
    (define vm-type (ffi2-type-vm-type t))
    (and (pair? vm-type)
         (memq (car vm-type) '(struct union))
         #t))

  (define (ffi2-type-pointer? t)
    (define vm-type (ffi2-type-vm-type t))
    (or (and (pair? vm-type)
             (memq (car vm-type) '(pointer pointer/gc))
             #t)
        (eq? vm-type 'pointer)
        (eq? vm-type 'pointer/gc)))

  (define (ffi2-type-pointer-vm-type t)
    (define vm-type (ffi2-type-vm-type t))
    (cond
      [(and (pair? vm-type)
            (memq (car vm-type) '(struct union array)))
       (if (pair? (cadr vm-type))
           (list 'pointer
                 (cadr vm-type))
           'pointer)]
      [(ffi2-type-name t)
       (list 'pointer (list (string->symbol (format "~a*" (ffi2-type-name t)))))]
      [else 'pointer]))

  (define (pointer-vm-type->gcable vm-type)
    (if (pair? vm-type)
        (cons 'pointer/gc (cdr vm-type))
        'pointer/gc))

  (define (ffi2-type-immediate-pointer? t)
    (eq? (ffi2-type-category t) 'ptr))

  (define (ffi2-type-scalar? t)
    (eq? (ffi2-type-category t) 'scalar))

  (define (lookup-type stx t-id
                       #:for-return? [for-return? #f]
                       #:for-argument? [for-argument? #f])
    (define v (syntax-local-value t-id (lambda () #f)))
    (unless (ffi2-type? v)
      (raise-syntax-error #f "not an ffi2 type" stx t-id))
    (unless (or for-return? (ffi2-type-racket->c v))
      (raise-syntax-error #f "ffi2 type allowed only as a procedure return" stx t-id))
    (unless (or for-argument? for-return? (not (eq? 'racket (ffi2-type-category v))))
      (raise-syntax-error #f "ffi2 type allowed only as a procedure argument or return" stx t-id))
    v)

  (define-syntax-class :malloc-kind
    (pattern (~or #:manual #:gcable #:gcable-traced #:gcable-immobile #:gcable-traced-immobile)))

  (define-syntax-class :tag
    #:description "identifier or #f"
    (pattern #f)
    (pattern _:id))

  (define-syntax-class :array-size
    #:description "array size or `*`"
    (pattern exact-nonnegative-integer)
    (pattern (~datum *)))

  (struct procedure-abi (vm-abi))

  (void))

(define-syntax (drop stx) #'(void))

(define-for-syntax (raise-only-as-ffi-type stx)
  (raise-syntax-error #f "allowed only in an ffi2 type context" stx))

(define-syntax (-> stx) (raise-only-as-ffi-type stx))
(define-syntax (struct stx) (raise-only-as-ffi-type stx))
(define-syntax (union stx) (raise-only-as-ffi-type stx))
(define-syntax (array stx) (raise-only-as-ffi-type stx))
(define-syntax (system-type-case stx)
  (raise-syntax-error #f "allowed only in an ffi2 type or abi context" stx))

(define-syntax default_abi (procedure-abi #f))
(define-syntax stdcall_abi (procedure-abi '(__select os (windows) __stdcall #f)))
(define-syntax cdecl_abi (procedure-abi '(__select os (windows) __cdecl #f)))
