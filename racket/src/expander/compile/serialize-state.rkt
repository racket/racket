#lang racket/base
(require "../common/set.rkt")
         
(provide (struct-out serialize-state)
         make-serialize-state
         
         intern-scopes
         intern-shifted-multi-scopes
         intern-mpi-shifts
         intern-context-triple
         intern-properties
         
         push-syntax-context!
         get-syntax-context
         pop-syntax-context!)

;; A `serialize-state` record is threaded through the construction of
;; a deserialization expression

(struct serialize-state (reachable-scopes       ; the set of all reachable scopes
                         bindings-intern        ; to record pruned binding tables
                         bulk-bindings-intern   ; to record pruned bulk-binding lists
                         scopes                 ; interned scope sets
                         shifted-multi-scopes   ; interned shifted multi-scope sets
                         multi-scope-tables     ; interned phase -> scope tables
                         mpi-shifts             ; interned module path index shifts
                         context-triples        ; combinations of the previous three
                         props                  ; map full props to previously calculated
                         interned-props         ; intern filtered props
                         syntax-context         ; used to collapse encoding of syntax literals
                         sharing-syntaxes))     ; record which syntax objects are `datum->syntax` form

(define (make-serialize-state reachable-scopes)
  (define state
    (serialize-state reachable-scopes
                     (make-hasheq)   ; bindings-intern
                     (make-hasheq)   ; bulk-bindings-intern
                     (make-hash)     ; scopes
                     (make-hash)     ; shifted-multi-scopes
                     (make-hasheq)   ; multi-scope-tables
                     (make-hasheq)   ; mpi-shifts
                     (make-hasheq)   ; context-triples
                     (make-hasheq)   ; props
                     (make-hash)     ; interned-props
                     (box null)      ; syntax-context
                     (make-hasheq))) ; sharing-syntaxes
  ;; Seed intern tables for sets and hashes to use the canonical
  ;; empty version for consistent sharing:
  (define empty-seteq (seteq))
  (hash-set! (serialize-state-scopes state) empty-seteq empty-seteq)
  (hash-set! (serialize-state-shifted-multi-scopes state) empty-seteq empty-seteq)
  (hash-set! (serialize-state-interned-props state) empty-seteq empty-seteq)
  state)

(define (intern-scopes scs state)
  (or (hash-ref (serialize-state-scopes state) scs #f)
      (begin
        (hash-set! (serialize-state-scopes state) scs scs)
        scs)))

(define (intern-shifted-multi-scopes sms state)
  (or (hash-ref (serialize-state-shifted-multi-scopes state) sms #f)
      (begin
        (hash-set! (serialize-state-shifted-multi-scopes state) sms sms)
        sms)))

(define (intern-mpi-shifts mpi-shifts state)
  (cond
   [(null? mpi-shifts) null]
   [else
    (define tail (intern-mpi-shifts (cdr mpi-shifts) state))
    (define tail-table (or (hash-ref (serialize-state-mpi-shifts state) tail #f)
                           (let ([ht (make-hasheq)])
                             (hash-set! (serialize-state-mpi-shifts state) tail ht)
                             ht)))
    (or (hash-ref tail-table (car mpi-shifts) #f)
        (let ([v (cons (car mpi-shifts) tail)])
          (hash-set! tail-table (car mpi-shifts) v)
          v))]))

(define (intern-context-triple scs sms mpi-shifts state)
  (define scs-ht (or (hash-ref (serialize-state-context-triples state) scs #f)
                     (let ([ht (make-hasheq)])
                       (hash-set! (serialize-state-context-triples state) scs ht)
                       ht)))
  (define sms-ht (or (hash-ref scs-ht sms #f)
                     (let ([ht (make-hasheq)])
                       (hash-set! scs-ht sms ht)
                       ht)))
  (or (hash-ref sms-ht mpi-shifts #f)
      (let ([vec (vector-immutable scs sms mpi-shifts)])
        (hash-set! sms-ht mpi-shifts vec)
        vec)))

(define (intern-properties all-props get-preserved-props state)
  (define v (hash-ref (serialize-state-props state) all-props 'no))
  (cond
   [(eq? v 'no)
    (define preserved-props (get-preserved-props))
    (define p
      (cond
       [(zero? (hash-count preserved-props)) #f]
       [(hash-ref (serialize-state-interned-props state) preserved-props #f)
        => (lambda (p) p)]
       [else
        (hash-set! (serialize-state-interned-props state) preserved-props preserved-props)
        preserved-props]))
    (hash-set! (serialize-state-props state) all-props p)
    p]
   [else v]))

(define (push-syntax-context! state v)
  (define b (serialize-state-syntax-context state))
  (set-box! b (cons v (unbox b))))

(define (get-syntax-context state)
  (define b (serialize-state-syntax-context state))
  (if (null? (unbox b))
      #f
      (car (unbox b))))

(define (pop-syntax-context! state)
  (define b (serialize-state-syntax-context state))
  (set-box! b (cdr (unbox b))))
