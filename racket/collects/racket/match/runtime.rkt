#lang racket/base

(require racket/stxparam
         racket/unsafe/undefined
         (for-syntax racket/base))

(provide match-equality-test
         exn:misc:match?
         match:error
         fail
         matchable?
         pregexp-matcher
         match-prompt-tag
         mlist? mlist->list
         syntax-srclocs

         ;; hash pattern
         user-def
         undef

         hash-state-step
         hash-shortcut-step
         invoke-thunk

         (struct-out hash-state)
         hash-state-closed?
         hash-state-residue
         hash-pattern-optimized?)

(define match-prompt-tag (make-continuation-prompt-tag 'match)) 

(define match-equality-test (make-parameter equal? #f 'match-equality-test))

(define-struct (exn:misc:match exn:fail) (value srclocs)
  #:property prop:exn:srclocs (lambda (ex) (exn:misc:match-srclocs ex))
  #:transparent)


(define (match:error val srclocs form-name)
  (raise (make-exn:misc:match
          (format "~a: no matching clause for ~e"
                  form-name val)
          (current-continuation-marks)
          val
          srclocs)))

(define-syntax-parameter fail
  (lambda (stx)
    (raise-syntax-error
     #f "used out of context: not in match pattern" stx)))

;; can we pass this value to regexp-match?
(define (matchable? e)
  (or (string? e) (bytes? e)))

(define ((pregexp-matcher rx who) e)
  (regexp-match
   (cond
     [(or (pregexp? rx) (byte-pregexp? rx)) rx]
     [(string? rx) (pregexp rx)]
     [(bytes? rx) (byte-pregexp rx)]
     [else (raise-argument-error who "(or/c pregexp? byte-pregexp? string? bytes?)" rx)])
   e))

;; duplicated because we can't depend on `compatibility` here
(define (mlist? l)
  (cond
   [(null? l) #t]
   [(mpair? l)
    (let loop ([turtle l][hare (mcdr l)])
      (cond
       [(null? hare) #t]
       [(eq? hare turtle) #f]
       [(mpair? hare)
        (let ([hare (mcdr hare)])
          (cond
           [(null? hare) #t]
           [(eq? hare turtle) #f]
           [(mpair? hare)
            (loop (mcdr turtle) (mcdr hare))]
           [else #f]))]
       [else #f]))]
   [else #f]))

(define (mlist->list l)
  (cond
   [(null? l) null]
   [else (cons (mcar l) (mlist->list (mcdr l)))]))

(define (syntax-srclocs stx)
  (list (srcloc (syntax-source stx)
                (syntax-line stx)
                (syntax-column stx)
                (syntax-position stx)
                (syntax-span stx))))

;; Hash table patterns

(define user-def (gensym))
(define undef unsafe-undefined)

(define (user-def? v)
  (eq? user-def v))

(define (undef? v)
  (eq? unsafe-undefined v))

(struct hash-state (ht keys vals) #:prefab)

(define (hash-state-closed? state)
  (define ht (hash-state-ht state))
  (define acc-keys (hash-state-keys state))
  (define acc-vals (hash-state-vals state))
  (define seen (hash-copy-clear ht #:kind 'mutable))
  (define cnt
    (for/sum ([k (in-list acc-keys)]
              [v (in-list acc-vals)])
      (cond
        [(or (hash-has-key? seen k) (user-def? v)) 0]
        [else
         (hash-set! seen k #t)
         1])))
  (= (hash-count ht) cnt))

(define (hash-state-residue state)
  (define ht (hash-state-ht state))
  (define acc-keys (hash-state-keys state))
  (cond
    [(immutable? ht)
     (for/fold ([ht ht])
               ([k (in-list acc-keys)])
       (hash-remove ht k))]
    [else
     (define ht* (hash-copy ht))
     (for ([k (in-list acc-keys)])
       (hash-remove! ht* k))
     ht*]))

(define (error:illegal-hash-table-thunk-usage ht)
  (error 'match
         "thunk must not be invoked when matching fails; hash table value: ~e"
         ht))

(define ((hash-state-step key user-def-thunk def-id) state)
  (define ht (hash-state-ht state))
  (define acc-keys (hash-state-keys state))
  (define acc-vals (hash-state-vals state))
  (define val (hash-ref ht key def-id))
  (define new-hash-state (hash-state ht (cons key acc-keys) (cons val acc-vals)))
  (if (undef? val)
      (values #t
              (λ () (error:illegal-hash-table-thunk-usage ht))
              new-hash-state)
      (values #f
              (λ ()
                (cond
                  [(user-def? val) (user-def-thunk)]
                  [else val]))
              new-hash-state)))

(define ((hash-shortcut-step key user-def-thunk) ht)
  (define val (hash-ref ht key user-def-thunk))
  (if (undef? val)
      (values #t (λ () (error:illegal-hash-table-thunk-usage ht)))
      (values #f (λ () val))))

(define (invoke-thunk thk)
  (thk))

;; If true, optimize the hash pattern as follows:
;; - Generate simplified code for the open mode
;; - Treat #:rest _ as the open mode
(define-syntax-parameter hash-pattern-optimized? #t)
