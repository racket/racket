#lang racket/base

(require (for-syntax racket/base
                     racket/struct-info
                     "helpers.rkt")
         "guts.rkt")

(provide struct/c)

#|
  as with copy-struct in struct.rkt, this first begin0
  expansion "declares" that struct/c is an expression.
  It prevents further expansion until the internal definition
  context is sorted out.
|#
(define-syntax (struct/c stx)
  (syntax-case stx ()
    [(_ . args) 
     (with-syntax ([x (syntax/loc stx (do-struct/c . args))])
       (syntax/loc stx (begin0 x)))]))

(define-syntax (do-struct/c stx)
  (syntax-case stx ()
    [(_ struct-name args ...)
     (and (identifier? (syntax struct-name))
          (struct-info? (syntax-local-value (syntax struct-name) (λ () #f))))
     (with-syntax ([(ctc-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-name-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-pred-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-proj-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-pos-proj-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-neg-proj-x ...) (generate-temporaries (syntax (args ...)))]
                   [(ctc-app-x ...) (generate-temporaries (syntax (args ...)))]
                   [(field-numbers ...)
                    (let loop ([i 0]
                               [l (syntax->list (syntax (args ...)))])
                      (cond
                        [(null? l) '()]
                        [else (cons i (loop (+ i 1) (cdr l)))]))]
                   [(type-desc-id 
                     constructor-id 
                     predicate-id 
                     (rev-selector-id ...)
                     (rev-mutator-id ...)
                     super-id)
                    (lookup-struct-info (syntax struct-name) stx)])
       (unless (= (length (syntax->list (syntax (rev-selector-id ...))))
                  (length (syntax->list (syntax (args ...)))))
         (raise-syntax-error 'struct/c 
                             (format "expected ~a contracts because struct ~a has ~a fields"
                                     (length (syntax->list (syntax (rev-selector-id ...))))
                                     (syntax-e #'struct-name)
                                     (length (syntax->list (syntax (rev-selector-id ...)))))
                             stx))
       (with-syntax ([(selector-id ...) (reverse (syntax->list (syntax (rev-selector-id ...))))]
                     [(mutator-id ...)  (reverse (syntax->list (syntax (rev-mutator-id ...))))])
         (syntax
          (let ([ctc-x (coerce-contract 'struct/c args)] ...)
            
            (unless predicate-id
              (error 'struct/c "could not determine predicate for ~s" 'struct-name))
            (unless (and selector-id ...)
              (error 'struct/c "could not determine selectors for ~s" 'struct-name))
            (unless (chaperone-contract? ctc-x)
              (error 'struct/c "expected chaperone contracts as arguments, got ~e" args))
            ...
            
            (let* ([ctc-pred-x (contract-first-order ctc-x)] 
                   ...
                   [ctc-name-x (contract-name ctc-x)]
                   ...
                   ;; To have a flat contract result, all of the contracted fields must be immutable
                   ;; and all the contracts must be flat.
                   [flat? (and (andmap not (list mutator-id ...))
                               (for/and ([c (in-list (list ctc-x ...))])
                                 (flat-contract? c)))]
                   [fo-check (λ (val)
                               (and (predicate-id val)
                                    (ctc-pred-x (selector-id val)) ...))])
              (if flat?
                  (build-flat-contract
                   (build-compound-type-name 'struct/c 'struct-name ctc-x ...)
                   fo-check)
                  (make-chaperone-contract
                   #:name (build-compound-type-name 'struct/c 'struct-name ctc-x ...)
                   #:first-order fo-check
                   #:projection
                   (let ([ctc-proj-x (contract-projection ctc-x)] ...)
                     (λ (blame)
                       (let* ([swapped-blame (blame-swap blame)]
                              [ctc-pos-proj-x (ctc-proj-x blame)] ...
                              [ctc-neg-proj-x (ctc-proj-x swapped-blame)] ...)
                         (λ (val)
                           (unless (predicate-id val)
                             (raise-blame-error blame val "expected a <~a>, got ~v" struct-name val))
                           ;; Do first order checks on values in case the struct doesn't adhere to them
                           ;; at wrapping time
                           (ctc-pos-proj-x (selector-id val)) ...
                           (apply chaperone-struct val
                                  (append (list* selector-id (λ (s v) (ctc-pos-proj-x v))
                                                 (if mutator-id
                                                     (list mutator-id (λ (s v) (ctc-neg-proj-x v)))
                                                     null)) ...)))))))))))))]
    [(_ struct-name anything ...)
     (raise-syntax-error 'struct/c "expected a struct identifier" stx (syntax struct-name))]))
