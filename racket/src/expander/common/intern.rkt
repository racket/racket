#lang racket/base

(provide make-weak-intern-table
         weak-intern!)

;; We can't always use Racket's weak hash tables for interning,
;; because those have a lock for `equal?` comparisons. This
;; implementation uses a box and `box-cas!` to transactionally update
;; the table after failing to find an entry (and if the transaction
;; fails, we look again for an entry).

(struct weak-intern-table (box)
  #:authentic)
(struct table (ht         ; integer[hash code] -> list of weak boxes
               count      ; number of items in the table (= sum of list lengths)
               prune-at)  ; count at which we should try to prune empty weak boxes
  #:authentic)

(define (make-weak-intern-table)
  (weak-intern-table (box (table (hasheqv) 0 128))))

(define (weak-intern! tt v)
  (define b (weak-intern-table-box tt))
  (define t (unbox b))

  (define code (equal-hash-code v))
  (define vals (hash-ref (table-ht t) code null))
  
  (or
   ;; In the table?
   (for/or ([b (in-list vals)])
     (define bv (weak-box-value b))
     (and (equal? bv v) bv))
   ;; Not in the table:
   (let* ([pruned-t (if (= (table-count t) (table-prune-at t))
                        (prune-table t)
                        t)])
     (define ht (table-ht pruned-t))
     (define new-t
       (table (hash-set ht code (cons (make-weak-box v)
                                      (hash-ref ht code null)))
              (add1 (table-count pruned-t))
              (table-prune-at pruned-t)))
     ;; Try to install the updated table, and return `v` if it
     ;; is successfully installed
     (or (and (box-cas! b t new-t)
              v)
         ;; Transaction failed, so try again
         (weak-intern! tt v)))))

;; Remove empty weak boxes from a table. Count the number
;; of remaining entries, and remember to prune again when
;; the number of entries doubles (up to at least reaches 128)
(define (prune-table t)
  (define new-ht
    (for*/hash ([(k vals) (in-hash (table-ht t))]
                [new-vals (in-value
                           (for/list ([b (in-list vals)]
                                      #:when (weak-box-value b))
                             b))]
                #:when (pair? new-vals))
      (values k new-vals)))
  (define count (for/sum ([(k vals) (in-hash new-ht)])
                  (length vals)))
  (table new-ht
         count
         (max 128 (* 2 count))))

;; ----------------------------------------

(module+ test
  (define show-status? #f)
  
  (define N 10) ; number of threads
  (define M 1000) ; number of values to intern and remember
  (define P 100) ; number of values to intern and discard
  
  (struct val (key other)
    #:transparent
    #:property prop:equal+hash (list
                                (lambda (v1 v2 eql?) (eql? (val-key v1) (val-key v2)))
                                (lambda (v1 code) (code (val-key v1)))
                                (lambda (v1 code) (code (val-key v1)))))
  
  (define tt (make-weak-intern-table))

  (define results (make-vector N #f))
  
  (define threads
    (for/list ([i (in-range N)])
      (thread
       (lambda ()
         (vector-set!
          results
          i
          (for/list ([j (in-range M)])
            (for/list ([k (in-range P)])
              (weak-intern!
               tt
               (val (format "~a + ~a" j k) i)))
            (weak-intern! tt (val (number->string j) i))))))))
  
  (define done? #f)
  (define measure-thread
    (thread
     (lambda ()
       (when show-status?
         (let loop ()
           (define t (unbox (weak-intern-table-box tt)))
           (printf "~s [~s]\n"
                   (table-count t)
                   (hash-count (table-ht t)))
           (unless done?
             (sleep 1)
             (loop)))))))
  
  (for-each sync threads)
  
  (collect-garbage)
  (collect-garbage)
  (set! done? #t)
  
  (void (sync measure-thread))
  
  (let ([t (prune-table (unbox (weak-intern-table-box tt)))])
    (printf "Final pruning: ~s [~s]\n"
            (table-count t)
            (hash-count (table-ht t)))
    (unless ((table-count t) . < . (* 3 M))
      (error "too many items are still in the table; not weak enough?")))

  (for ([i (in-range N)])
    (unless (equal? (vector-ref results i) (vector-ref results 0))
      (error "mismatch" i)))
  
  ;; Make sure the results come from different threads:
  (define representatives
    (for/fold ([ht (hasheqv)]) ([v (in-list (vector-ref results 0))])
      (hash-update ht (val-other v) add1 0)))
  (unless ((hash-count representatives) . > . (* 0.25 N))
    (error "poor representation among threads;\n something is wrong, or the test is not balanced enough")))
