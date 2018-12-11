#lang racket/base
(require "intmap.rkt")

;; Stack for "interpreter.rkt"

(provide empty-stack
         stack-count
         stack-ref
         stack-set
         stack-remove
         push-stack

         (struct-out stack-info)
         stack->pos
         stack-info-branch
         stack-info-merge!
         stack-info-forget!
         stack-info-non-tail!)
         
;; ------------------------------------------------------------
;; Run-time stack

(define empty-stack empty-intmap)

(define (stack-count stack)
  (intmap-count stack))

;; Returns a `value` if `tail?` is true, and
;; returns `(values stack value)` if `tail?` is #f.
;; A box for `i` indicates that it's the last use of
;; the accessed binding, so the binding should be
;; removed.
(define (stack-ref stack i [tail? #f])
  (cond
    [(box? i)
     (let ([i (unbox* i)])
       (if tail?
           (intmap-ref stack i)
           (values (intmap-remove stack i)
                   (intmap-ref stack i))))]
    [else
     (if tail?
         (intmap-ref stack i)
         (values stack (intmap-ref stack i)))]))

(define (stack-set stack i v)
  (define s (intmap-set stack i v))
  s)

(define (stack-remove stack i)
  (intmap-remove stack i))

(define (push-stack stack pos vals mask)
  (define rest? (negative? mask))
  (define count (if rest?
                    (integer-length mask)
                    (sub1 (integer-length mask))))
  (let loop ([pos pos] [vals vals] [count count] [stack stack])
    (cond
      [(zero? count)
       (if rest?
           (stack-set stack pos vals)
           stack)]
      [else
       (loop (add1 pos) (cdr vals) (sub1 count)
             (stack-set stack pos (car vals)))])))

;; ------------------------------------------------------------
;; Compile-time stack information

(struct stack-info (capture-depth   ; boundary for the enclosing function in compile-time env
                    closure-map     ; hash table to collect variables byond boundary to capture
                    [use-map #:mutable] ; table of uses; an entry here means the binding is used later
                    [local-use-map #:mutable] ; subset of `use-map` used to tracked needed merging for branches
                    [non-tail-at-depth #:mutable])) ; stack depth at non-tail call (that needs space safety)

;; Map a compile-time environment coordinate `i` to a run-time access
;; index. If this this access is the last one --- which is the first
;; lookup of `i`, since the compiler works from the end toward earlier
;; expressions --- then probably return a box, which will trigger a
;; removal of the binding after the lookup at run time.
(define (stack->pos i stk-i #:nonuse? [nonuse? #f])
  (define capture-depth (stack-info-capture-depth stk-i))
  (define pos
    (cond
      [(not capture-depth) i]
      [(i . >= . capture-depth)
       (- i capture-depth)]
      [(hash-ref (stack-info-closure-map stk-i) i #f)
       => (lambda (pos) pos)]
      [else
       ;; Count backwards from -1 to represent closure elements
       (define cmap (stack-info-closure-map stk-i))
       (define pos (- -1 (hash-count cmap)))
       (hash-set! cmap i pos)
       pos]))
  (cond
    [nonuse? pos]
    [else
     ;; Record the use of this position. If it's the last use (i.e.,
     ;; first from the end), then box the position, which means "clear
     ;; after retreiving" and implements space safety.
     (define use-map (stack-info-use-map stk-i))
     (cond
       [(or (not use-map)
            (hash-ref use-map pos #f))
        pos]
       [else
        (when use-map
          (set-stack-info-use-map! stk-i (hash-set use-map pos #t)))
        (define local-use-map (stack-info-local-use-map stk-i))
        (when local-use-map
          (set-stack-info-local-use-map! stk-i (hash-set local-use-map pos #t)))
        ;; We only need to remove from the environment if there's a
        ;; non-tail call later where the binding would be retained
        ;; across the call
        (if (i . < . (stack-info-non-tail-at-depth stk-i))
            (box pos)
            pos)])]))

;; Create a fresh tracking record for one branch among many
(define (stack-info-branch stk-i)
  (stack-info (stack-info-capture-depth stk-i)
              (stack-info-closure-map stk-i)
              (stack-info-use-map stk-i)
              #hasheq()
              (stack-info-non-tail-at-depth stk-i)))

;; Merge branches back together, returning the set of all bindings
;; that has last uses across all branches. The returned information
;; is useful to make sure that all branches are updated to clear the
;; same set of bindings.
(define (stack-info-merge! stk-i branch-stk-is)
  (define all-clear (make-hasheq))
  (for ([branch-stk-i (in-list branch-stk-is)])
    (for ([pos (in-hash-keys (stack-info-local-use-map branch-stk-i))])
      (hash-set! all-clear pos #t)
      (define use-map (stack-info-use-map stk-i))
      (when use-map
        (set-stack-info-use-map! stk-i (hash-set use-map pos #t)))
      (define local-use-map (stack-info-local-use-map stk-i))
      (when local-use-map
        (set-stack-info-local-use-map! stk-i (hash-set local-use-map pos #t)))
      (set-stack-info-non-tail-at-depth! stk-i
                                         (max (stack-info-non-tail-at-depth stk-i)
                                              (stack-info-non-tail-at-depth branch-stk-i)))))
  all-clear)

;; Indicate that some bindings are "popped" from the stack, which
;; means that they no longer count as used, etc.
(define (stack-info-forget! stk-i stack-depth start-pos len)
  (set-stack-info-non-tail-at-depth! stk-i
                                     (min (stack-info-non-tail-at-depth stk-i)
                                          stack-depth))
  (when (stack-info-use-map stk-i)
    (for ([i (in-range len)])
      (define pos (+ start-pos i))
      (define use-map (stack-info-use-map stk-i))
      (set-stack-info-use-map! stk-i (hash-remove use-map pos))
      (define local-use-map (stack-info-local-use-map stk-i))
      (when local-use-map
        (set-stack-info-local-use-map! stk-i (hash-remove local-use-map pos))))))

;; Record the current stack depth at a non-tail call.
(define (stack-info-non-tail! stk-i stack-depth)
  (set-stack-info-non-tail-at-depth! stk-i
                                     (max (stack-info-non-tail-at-depth stk-i)
                                          stack-depth)))
