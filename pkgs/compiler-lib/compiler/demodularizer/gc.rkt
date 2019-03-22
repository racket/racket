#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs
         "remap.rkt")

;; Prune unnused definitions,
;;  * soundly, with a simple approximation of `pure?`, by default
;;  * unsoundly, assuming all definitions are pure, optionally

(provide gc-definitions)

(define (gc-definitions body internals lifts internals-pos
                        #:assume-pure? assume-pure?)
  (define used (make-hasheqv)) ; pos -> 'used or thunk
  (define graph (make-hasheq))

  (define (used-pos! pos)
    (when (pos . >= .  internals-pos)
      (define v (hash-ref used pos #f))
      (hash-set! used pos 'used)
      (when (procedure? v)
        (v))))

  (define (used! b)
    (match b
      [(toplevel depth pos const? ready?)
       (used-pos! pos)]
      [(inline-variant direct inline)
       (used! direct)
       (used! inline)]
      [(closure code gen-id)
       (unless (hash-ref graph gen-id #f)
         (hash-set! graph gen-id #t)
         (used! code))]
      [(let-one rhs body type unused?)
       (used! rhs)
       (used! body)]
      [(let-void count boxes? body)
       (used! body)]
      [(install-value count pos boxes? rhs body)
       (used! rhs)
       (used! body)]
      [(let-rec procs body)
       (for-each used! procs)
       (used! body)]
      [(boxenv pos body)
       (used! body)]
      [(application rator rands)
       (used! rator)
       (for-each used! rands)]
      [(branch tst thn els)
       (used! tst)
       (used! thn)
       (used! els)]
      [(with-cont-mark key val body)
       (used! key)
       (used! val)
       (used! body)]
      [(beg0 forms)
       (for-each used! forms)]
      [(seq forms)
       (for-each used! forms)]
      [(varref toplevel dummy constant? unsafe?)
       (used! toplevel)
       (used! dummy)]
      [(assign id rhs undef-ok?)
       (used! id)
       (used! rhs)]
      [(apply-values proc args-expr)
       (used! proc)
       (used! args-expr)]
      [(with-immed-mark key def-val body)
       (used! key)
       (used! def-val)
       (used! body)]
      [(case-lam name clauses)
       (for-each used! clauses)]
      [_
       (cond
         [(lam? b)
          (define tl-map (lam-toplevel-map b))
          (when tl-map
            (for/set ([pos (in-set tl-map)])
              (when (pos . >= .  internals-pos)
                (used-pos! pos))))
          (used! (lam-body b))]
         [else (void)])]))

  (define (pure? b)
    (match b
      [(closure code gen-id) #t]
      [(inline-variant direct inline) #t]
      [(case-lam name clauses) #t]
      [(let-one rhs body type unused?)
       (and (pure? rhs)
            (pure? body))]
      [(seq forms)
       (for/and ([form (in-list forms)])
         (pure? form))]
      [_ (or (lam? b)
             (void? b))]))

  (for ([b (in-list body)])
    (match b
      [(def-values ids rhs)
       (define done? #f)
       (define (used-rhs!)
         (unless done?
           (set! done? #t)
           (used! rhs))
         ;; All in group are used together:
         (for-each used! ids))
       (for ([id (in-list ids)])
         (define pos (toplevel-pos id))
         (cond
           [(eq? 'used (hash-ref used pos #f))
            (used-rhs!)]
           [else
            (hash-set! used pos used-rhs!)]))
       (unless (or assume-pure?
                   (pure? rhs))
         (used-rhs!))]
      [_ (unless (pure? b)
           (used! b))]))
  
  ;; Anything not marked as used at this point can be dropped
  (define new-internals
    (for/list ([name (in-list internals)]
               [pos (in-naturals internals-pos)]
               #:when (or (eq? 'used (hash-ref used pos #f))
                          (begin
                            (log-debug "drop ~s" name)
                            #f)))
      name))

  (define lifts-pos (+ internals-pos (length internals)))
  (define new-lifts
    (for/list ([name (in-list lifts)]
               [pos (in-naturals lifts-pos)]
               #:when (or (eq? 'used (hash-ref used pos #f))
                          (begin
                            (log-debug "drop ~s" name)
                            #f)))
      name))

  (define old-pos-to-new-pos (make-hasheqv))
  (for/fold ([new-pos internals-pos]) ([name (in-list (append internals lifts))]
                                       [pos (in-naturals internals-pos)])
    (cond
      [(eq? 'used (hash-ref used pos #f))
       (hash-set! old-pos-to-new-pos pos new-pos)
       (add1 new-pos)]
      [else new-pos]))

  (define used-body
    ;; Drop unused definitions
    (for/list ([b (in-list body)]
               #:when (match b
                        [(def-values ids rhs)
                         (for/or ([id (in-list ids)])
                           (eq? 'used (hash-ref used (toplevel-pos id) #f)))]
                        [else (not (pure? b))]))
      b))

  (define new-body (remap-positions used-body
                                    (lambda (pos)
                                      (if (pos . < . internals-pos)
                                          pos
                                          (hash-ref old-pos-to-new-pos pos)))))

  (values new-body new-internals new-lifts))
