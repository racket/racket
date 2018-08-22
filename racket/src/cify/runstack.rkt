#lang racket/base
(require "out.rkt"
         "id.rkt"
         "ref.rkt"
         "state.rkt"
         "union.rkt"
         "sort.rkt"
         "debug.rkt")

(provide make-runstack
         runstack-push!
         runstack-pop!
         runstack-ref
         runstack-ref-use!
         runstack-assign
         make-runstack-assign
         runstack-stack-ref
         runstack-ref-pos
         runstack-sync!
         runstack-synced!
         runstack-max-depth
         runstack-ever-synced?

         runstack-branch-before!
         runstack-branch-other!
         runstack-branch-merge!
         runstack-branch-refs
         runstack-stage-clear-unused!
         runstack-stage-clear!)

(struct runstack (rs-state    ; shared state table
                  depth       ; curent stack depth
                  max-depth   ; max reached stack depth
                  sync-depth  ; depth that MZ_RUNSTACK currently has, `#f` if unknown
                  vars        ; list of pushed vars, newest first
                  var-depths  ; pushed var -> 'local or distance from stack start
                  need-inits  ; set of pushed vars that are not yet initialized
                  unsynced    ; pushed vars that haven't yet lived through a GC boundary
                  unsynced-refs ; per-var refs that haven't yet lived through a GC boundary
                  all-refs    ; per-var, all references encountered
                  staged-clears ; clears staged by branching
                  ever-synced?) ; whether the runstack is ever synced
  #:mutable)

(define (make-runstack state)
  (define rs-state (or (hash-ref state '#:runstack #f)
                       (let ([ht (make-hasheq)])
                         (hash-set! state '#:runstack ht)
                         ht)))
  (runstack rs-state
            0             ; depth
            0             ; max-depth
            #f            ; sync-depth
            '()           ; vars
            (make-hasheq) ; var-depths
            (make-hasheq) ; need-inits
            (make-hasheq) ; unsyned
            (make-hasheq) ; unsynced-refs
            #hasheq()     ; all-refs
            #hasheq()     ; staged-clears
            #f))          ; ever-synced?

(define (runstack-push! rs id
                        #:referenced? [referenced? #t]
                        #:local? [local? #f]
                        #:track-local? [track-local? #f])
  (set-runstack-vars! rs (cons id (runstack-vars rs)))
  (cond
    [(or local?
         (and track-local?
              referenced?
              (eq? 'local (hash-ref (runstack-rs-state rs) id #f))))
     ;; A previous pass determined that this variable will not
     ;; live across a GC boundary, so it can be stored in a C local.
     ;; Note that we're sharing a global table, even though an id
     ;; can have different extents due to closures; but only `let`
     ;; bindings are "tracked", and each of those is unique.
     (hash-set! (runstack-var-depths rs) id 'local)
     (out "Scheme_Object *~a;" (cify id))]
    [else
     (define depth (add1 (runstack-depth rs)))
     (set-runstack-depth! rs depth)
     (set-runstack-max-depth! rs (max depth (runstack-max-depth rs)))
     (hash-set! (runstack-var-depths rs) id depth)
     (hash-set! (runstack-need-inits rs) id #t)
     (hash-set! (runstack-unsynced rs) id #t)
     (out "~aconst int ~a = -~a;~a"
          (if referenced? "" "/* ")
          (cify id) depth
          (if referenced? "" " */"))]))

(define (runstack-pop! rs [n 1]
                       #:track-local? [track-local? #f])
  (define var-depths (runstack-var-depths rs))
  (let loop ([n n])
    (unless (zero? n)
      (define var (car (runstack-vars rs)))
      (unless (eq? 'local (hash-ref var-depths var #f))
        (set-runstack-depth! rs (- (runstack-depth rs) 1))
        (hash-remove! (runstack-need-inits rs) var)
        (when (hash-ref (runstack-unsynced rs) var #f)
          (hash-remove! (runstack-unsynced rs) var))
        (when (and track-local?
                   ;; If all references were pre-sync, it can be local
                   (for/and ([state (in-hash-values (hash-ref (runstack-all-refs rs) var '#hasheq()))])
                     (eq? state 'pre-sync)))
          (hash-set! (runstack-rs-state rs) var 'local))
        (let ([refs (hash-ref (runstack-unsynced-refs rs) var '())])
          (hash-remove! (runstack-unsynced-refs rs) var)
          (for ([ref (in-list refs)])
            (set-ref-last-use?! ref #f))))
      (set-runstack-vars! rs (cdr (runstack-vars rs)))
      (set-runstack-all-refs! rs (hash-remove (runstack-all-refs rs) var))
      (hash-remove! var-depths var)
      (set-runstack-staged-clears! rs (hash-remove (runstack-staged-clears rs) var))
      (loop (sub1 n)))))

(define (runstack-ref rs id #:assign? [assign? #f] #:ref [ref #f] #:values-ok? [values-ok? #f])
  (when ref
    (runstack-ref-use! rs ref)
    ;; Remember the ref, so we can clear its `last-use?` if no sync
    ;; happens before the variable is popped
    (hash-set! (runstack-unsynced-refs rs) id
               (cons ref (hash-ref (runstack-unsynced-refs rs) id '()))))
  (define s
    (cond
      [(eq? 'local (hash-ref (runstack-var-depths rs) id #f))
       (format "~a" (cify id))]
      [(and ref (ref-last-use? ref))
       (format "c_last_use(c_runbase, ~a)" (cify id))]
      [else
       (format "c_runbase[~a]"  (cify id))]))
  (if (and (current-debug) (not values-ok?) (not assign?))
      (format "c_validate(~a)" s)
      s))

(define (runstack-ref-use! rs ref)
  (set-runstack-all-refs! rs (hash-set2 (runstack-all-refs rs) (ref-id ref) ref
                                        (if (hash-ref (runstack-unsynced rs) (ref-id ref) #f)
                                            'pre-sync
                                            'post-sync))))

(define (runstack-assign rs id)
  (hash-remove! (runstack-need-inits rs) id)
  (runstack-ref rs id #:assign? #t))

(define (make-runstack-assign rs id)
  (lambda (s) (out "~a = ~a;" (runstack-assign rs id) s)))

(define (runstack-stack-ref rs)
  (format "(c_runbase-~a)" (runstack-depth rs)))

(define (runstack-ref-pos rs id)
  (hash-ref (runstack-var-depths rs) id #f))

(define (runstack-sync! rs)
  (set-runstack-ever-synced?! rs #t)
  (hash-clear! (runstack-unsynced rs))
  (hash-clear! (runstack-unsynced-refs rs))
  (runstack-generate-staged-clears! rs)
  (define vars (sort (hash-keys (runstack-need-inits rs)) symbol<?))
  (for ([var (in-list vars)])
    (out "~a = c_RUNSTACK_INIT_VAL;" (runstack-assign rs var)))
  (unless (eqv? (runstack-depth rs) (runstack-sync-depth rs))
    (out "c_current_runstack = ~a;" (runstack-stack-ref rs))
    (set-runstack-sync-depth! rs (runstack-depth rs))))

(define (runstack-synced! rs)
  (hash-clear! (runstack-need-inits rs))
  (hash-clear! (runstack-unsynced rs))
  (hash-clear! (runstack-unsynced-refs rs)))

(struct runstack-branch-state (need-inits sync-depth unsynced-refs all-refs staged-clears))

(define (runstack-branch-before! rs)
  (define unsynced-refs (runstack-unsynced-refs rs))
  (define all-refs (runstack-all-refs rs))
  (set-runstack-unsynced-refs! rs (make-hasheq))
  (set-runstack-all-refs! rs #hasheq())
  (runstack-branch-state (hash-copy (runstack-need-inits rs))
                         (runstack-sync-depth rs)
                         unsynced-refs
                         all-refs
                         (runstack-staged-clears rs)))

(define (runstack-branch-other! rs pre)
  (begin0
    (runstack-branch-state (hash-copy (runstack-need-inits rs))
                           (runstack-sync-depth rs)
                           (runstack-unsynced-refs rs)
                           (runstack-all-refs rs)
                           (runstack-staged-clears rs))
    (set-runstack-need-inits! rs (runstack-branch-state-need-inits pre))
    (set-runstack-sync-depth! rs (runstack-branch-state-sync-depth pre))
    (set-runstack-unsynced-refs! rs (make-hasheq))
    (set-runstack-all-refs! rs #hasheq())
    (set-runstack-staged-clears! rs (runstack-branch-state-staged-clears pre))))

;; Called after "then" branch, before merge:
(define (runstack-branch-refs runstack pre post)
  (values (runstack-branch-state-all-refs post)
          (runstack-all-refs runstack)))

(define (runstack-branch-merge! rs pre post)
  (for ([(k v) (in-hash (runstack-branch-state-need-inits post))])
    (hash-set! (runstack-need-inits rs) k v))
  (unless (eqv? (runstack-branch-state-sync-depth post) (runstack-sync-depth rs))
    (set-runstack-sync-depth! rs #f))
  (set-runstack-unsynced-refs! rs (union-unsynced-refs! (runstack-unsynced-refs rs)
                                                        (runstack-branch-state-unsynced-refs pre)
                                                        (runstack-branch-state-unsynced-refs post)))
  (set-runstack-all-refs! rs (union-all-refs (runstack-all-refs rs)
                                             (runstack-branch-state-all-refs pre)
                                             (runstack-branch-state-all-refs post)))
  (set-runstack-staged-clears! rs (hash-union (runstack-staged-clears rs)
                                              (runstack-branch-state-staged-clears post))))

(define union-unsynced-refs!
  (case-lambda
    [(a b c)
     (cond
       [((hash-count b) . > . (hash-count a))
        (union-unsynced-refs! b a c)]
       [((hash-count c) . > . (hash-count b))
        (union-unsynced-refs! a c b)]
       [else
        (union-unsynced-refs! a b)
        (union-unsynced-refs! a c)])]
    [(a b)
     (for ([(id l) (in-hash b)])
       (hash-set! a id (append l (hash-ref a id '()))))
     a]))

(define union-all-refs
  (case-lambda
    [(a b c)
     (cond
       [((hash-count b) . > . (hash-count a))
        (union-all-refs b a c)]
       [((hash-count c) . > . (hash-count b))
        (union-all-refs a c b)]
       [else
        (union-all-refs (union-all-refs a b) c)])]
    [(a b)
     (for/fold ([a a]) ([(id b-refs) (in-hash b)])
       (define a-refs (hash-ref a id #hasheq()))
       (hash-set a id (hash-union a-refs b-refs)))]))

(define (hash-set2 ht key key2 val)
  (hash-set ht key
            (hash-set (hash-ref ht key #hasheq())
                      key2
                      val)))

;; ----------------------------------------

;; If `other-refs` includes a last use of a variable that
;; is not referenced in `my-refs`, then stage a clear
;; operation for space safety. The clear operation is emitted
;; only if the variable is still live by the time the runstack
;; is synced.
(define (runstack-stage-clear-unused! rs my-refs other-refs state)
  (for* ([refs (in-hash-values other-refs)]
         [ref (in-hash-keys refs)])
    (define id (ref-id ref))
    (when (and (ref-last-use? ref)
               (not (hash-ref my-refs id #f)))
      (runstack-stage-clear! rs id state))))

;; A danger of lazy clearing is that we might push the same
;; clearing operation to two different branches. It would be
;; better to clear eagerly at the start of a branch if there
;; will definitely by a sync point later, but we don't currently
;; have the "sync point later?" information.
(define (runstack-stage-clear! rs id state)
  (set-runstack-staged-clears!
   rs
   (hash-set (runstack-staged-clears rs)
             id
             ;; the `get-pos` thunk:
             (lambda ()
               (cond
                 [(not (referenced? (hash-ref state id #f)))
                  ;; This can happen in we need to clear a variable that is
                  ;; otherwise only implicitly passed in a tail call:
                  (format "-~a /* ~a */" (runstack-ref-pos rs id) (cify id))]
                 [else
                  (cify id)])))))

(define (runstack-generate-staged-clears! rs)
  (for ([(id get-pos) (in-sorted-hash (runstack-staged-clears rs) symbol<?)])
    (unless (eq? (hash-ref (runstack-var-depths rs) id) 'local)
      (out "c_no_use(c_runbase, ~a);" (get-pos))))
  (set-runstack-staged-clears! rs #hasheq()))
