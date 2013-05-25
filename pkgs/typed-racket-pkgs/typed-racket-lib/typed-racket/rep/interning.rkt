#lang racket/base

(require syntax/id-table racket/dict (for-syntax racket/base syntax/parse))

(provide defintern hash-id)

(define-syntax (defintern stx)
  (define-splicing-syntax-class extra-kw-spec
    (pattern (~seq kw:keyword [name:id default:expr])
             #:with formal #'(kw [name default])))
  (define-splicing-syntax-class extra-spec
    (pattern ek:extra-kw-spec
             #:with e #'ek.name)
    (pattern e:expr))
  (syntax-parse stx
    [(_ name+args make-name key #:extra-args e ...)
     #'(defintern name+args (lambda () (make-hash)) make-name key #:extra-args e ...)]
    [(_ (*name:id arg:id ...) make-ht make-name key-expr #:extra-args . (~and ((~seq es:extra-spec) ...) ((~or (~seq ek:extra-kw-spec) e:expr) ...)))
     (with-syntax ([((extra-formals ...) ...) #'(ek.formal ...)])
       #'(define *name
           (let ([table (make-ht)])
             (lambda (arg ... extra-formals ... ...)
               (let ([key key-expr])
                 (hash-ref table key
                           (lambda ()
                             (let ([new (make-name (count!) es.e ... arg ...)])
                               (hash-set! table key new)
                               new))))))))]))

(define (make-count!)
  (let ([state 0])
    (lambda () (begin0 state (set! state (add1 state))))))

(define count! (make-count!))
(define id-count! (make-count!))

(define identifier-table (make-free-id-table))

(define (hash-id id)
  (dict-ref
   identifier-table
   id
   (lambda () (let ([c (id-count!)])
                (dict-set! identifier-table id c)
                c))))
