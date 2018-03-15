#lang racket/base
(require "../common/set.rkt")

;; A reference record keeps tarck of which bindings in a frame are
;; being referenced and which have been already bound so that a
;; reference doesn't count as a forward reference. This information
;; is needed for expanding internal definitions to break them into
;; suitable `let` and `letrec` sets.

(provide make-reference-record
         reference-record?
         reference-record-used!
         reference-records-all-used!
         reference-record-bound!
         reference-record-forward-references?
         reference-record-clear!)

(struct reference-record ([already-bound #:mutable]
                          [reference-before-bound #:mutable]
                          [all-referenced? #:mutable])
  #:authentic
  #:transparent)

(define (make-reference-record)
  (reference-record (seteq) (seteq) #f))

(define (reference-record-used! rr key)
  (unless (set-member? (reference-record-already-bound rr) key)
    (set-reference-record-reference-before-bound!
     rr
     (set-add (reference-record-reference-before-bound rr) key))))

(define (reference-records-all-used! rrs)
  (for ([rr (in-list rrs)]
        ;; If a reference record is already marked as all referenced,
        ;; then later records must be already marked, too
        #:break (reference-record-all-referenced? rr))
    (set-reference-record-all-referenced?! rr #t)))

(define (reference-record-bound! rr keys)
  (set-reference-record-already-bound!
   rr
   (for/fold ([ab (reference-record-already-bound rr)]) ([key (in-list keys)])
     (set-add ab key )))
  (set-reference-record-reference-before-bound!
   rr
   (for/fold ([rbb (reference-record-reference-before-bound rr)]) ([key (in-list keys)])
     (set-remove rbb key))))

(define (reference-record-forward-references? rr)
  (or (reference-record-all-referenced? rr)
      (positive? (set-count (reference-record-reference-before-bound rr)))))

(define (reference-record-clear! rr)
  (set-reference-record-already-bound! rr #f)
  (set-reference-record-reference-before-bound! rr #f))
