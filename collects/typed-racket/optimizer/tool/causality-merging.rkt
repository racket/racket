#lang racket/base

;; causality merging, for TR optimizations

(require racket/list
         "structs.rkt")

(provide causality-merging)

(define (causality-merging log)
  (for/fold ([res '()])
      ([new (in-list log)])
    (cond [(missed-opt-log-entry? new)
           (maybe-merge-with-parent new res)]
          [else
           (cons new res)]))) ; no merging for opts and info

;; is parent the "parent" missed optimization of child?
;; this determines whether they get reported together or not
;; currently, parents and children must be of the same kind of missed
;; optimization, and the child must be an irritant of the parent, or be a
;; merged irritant of the parent
(define (parent-of? parent child)
  (and (missed-opt-log-entry? parent) ; only applicable for missed opts
       (missed-opt-log-entry? child)
       (equal? (log-entry-kind parent)
               (log-entry-kind child))
       (member (log-entry-stx child)
               (append (missed-opt-log-entry-irritants parent)
                       (missed-opt-log-entry-merged-irritants parent)))))

;; combine reporting of two missed optimizations, increasing badness in the
;; process
(define (combine-missed-optimizations parent child)
  (missed-opt-log-entry
   (log-entry-kind        parent) ; same as child's
   (log-entry-msg         parent)
   (log-entry-stx         parent) ; we report the outermost one
   (log-entry-located-stx parent)
   (log-entry-pos         parent)
   (log-entry-provenance  parent)

   (remove-duplicates
    (append (remove (log-entry-stx child)
                    (missed-opt-log-entry-irritants parent))
            (missed-opt-log-entry-irritants child)))
   (remove-duplicates
    (append (missed-opt-log-entry-merged-irritants child)
            (missed-opt-log-entry-merged-irritants parent)
            ;; we merge child in, keep it for future merges
            (list (log-entry-stx child))))
   (+ (missed-opt-log-entry-badness parent)
      (missed-opt-log-entry-badness child))))

;; log-entry (listof log-entry) -> log-entry
;; add a new missed opt to the list, maybe replacing its parent / children
(define (maybe-merge-with-parent new log-so-far)
  ;; check if the new one is the child of an old one
  ;; for/first is ok, since we can only have one parent in the list
  ;; (if we had more, one would have to be the parent of the other, so
  ;; only one would be in the list)
  (define parent (for/first ([m (in-list log-so-far)]
                             #:when (parent-of? m new))
                   m))
  ;; do we have children in the list, if so, merge with all of them
  (define children (for/list ([m (in-list log-so-far)]
                              #:when (parent-of? new m))
                     m))
  (cond [parent
         ;; we found our parent, merge with it
         (if (member (log-entry-stx new)
                     (missed-opt-log-entry-merged-irritants
                      parent))
             ;; we have been merged in the past, do nothing
             log-so-far
             ;; do the actual merge
             (cons (combine-missed-optimizations parent new)
                   (remove parent log-so-far)))]
        [(not (null? children))
         ;; we found children, merge with them
         (let ([new (for/fold ([new new])
                        ([child children])
                      (combine-missed-optimizations new child))])
           (cons new
                 (filter (lambda (x) (not (member x children)))
                         log-so-far)))]
        [else
         ;; no related entry, just add the new one
         (cons new log-so-far)]))
