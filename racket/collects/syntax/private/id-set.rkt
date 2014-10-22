#lang racket/base
(require (for-syntax racket/base)
         racket/set racket/dict
         syntax/id-table)
  
(provide 
 (rename-out [make-mutable-free-id-set mutable-free-id-set]
             [make-immutable-free-id-set immutable-free-id-set])
 free-id-set?
 mutable-free-id-set?
 immutable-free-id-set?
 free-id-set-empty?
 free-id-set-count
 free-id-set-member?
 free-id-set=?
 free-id-set-add
 free-id-set-add!
 free-id-set-remove
 free-id-set-remove!
 free-id-set-first
 free-id-set-rest
 in-free-id-set
 free-id-set->stream
 free-id-set->list
 free-id-set-copy
 free-id-set-copy-clear
 free-id-set-clear
 free-id-set-clear!
 free-id-set-union
 free-id-set-union!
 free-id-set-intersect
 free-id-set-intersect!
 )

;; TODO
;; - add contracts
;; - support generic set interface
;; - support generic sequence interface(?)
;; - better error msgs?

(struct free-id-set (table))
;; table: the internal free-id table
(struct mutable-free-id-set free-id-set ())
(struct immutable-free-id-set free-id-set ())

(define (make-mutable-free-id-set [init-set null] 
                                  #:phase [phase (syntax-local-phase-level)])
  (mutable-free-id-set 
   (make-free-id-table
    (for/hash ([x (in-set init-set)]) (values x #t))
    #:phase phase)))

(define (make-immutable-free-id-set [init-set null]
                                    #:phase [phase (syntax-local-phase-level)])
  (immutable-free-id-set 
   (make-immutable-free-id-table 
    (for/hash ([x (in-set init-set)]) (values x #t))
    #:phase phase)))

(define (free-id-set-member? s x) 
  (free-id-table-ref (free-id-set-table s) x #f))

(define (free-id-set=? s1 s2)
  (define table1 (free-id-set-table s1))
  (define table2 (free-id-set-table s2))
  (and (for/and ([id (in-dict-keys table1)]) (free-id-table-ref table2 id #f))
       (for/and ([id (in-dict-keys table2)]) (free-id-table-ref table1 id #f))))

(define (free-id-set-add s x)
  (immutable-free-id-set (free-id-table-set (free-id-set-table s) x #t)))
(define (free-id-set-add! s x)
  (free-id-table-set! (free-id-set-table s) x #t))

(define (free-id-set-remove s x)
  (immutable-free-id-set (free-id-table-remove (free-id-set-table s) x)))
(define (free-id-set-remove! s x)
  (free-id-table-remove! (free-id-set-table s) x))

(define (free-id-set-count s)
  (free-id-table-count (free-id-set-table s)))

(define (free-id-set-empty? s)
  (zero? (free-id-set-count s)))

(define (free-id-set-first s)
  (define table (free-id-set-table s))
  (free-id-table-iterate-key table (free-id-table-iterate-first table)))

;; free-id-set-rest is undefined for mutable sets
;; and thus always returns a mutable set
(define (free-id-set-rest s)
  (define table (free-id-set-table s))
  (define i (free-id-table-iterate-first table))
  (immutable-free-id-set (free-id-table-remove table (free-id-table-iterate-key table i))))

(define (in-free-id-set s)
  (in-dict-keys (free-id-set-table s)))
(define (free-id-set->stream s)
  (sequence->stream (in-free-id-set s)))
(define (free-id-set->list s)
  (for/fold ([ids '()]) ([id (in-dict-keys (free-id-set-table s))])
    (cons id ids)))

;; Can't just copy free-id-table because there's no copy function or dict-copy
(define (free-id-set-copy s)
  (if (mutable-free-id-set? s)
      (make-mutable-free-id-set (free-id-set->list s))
      (make-immutable-free-id-set (free-id-set->list s))))

(define (free-id-set-copy-clear s)
  (if (mutable-free-id-set? s)
      (make-mutable-free-id-set null)
      (make-immutable-free-id-set null)))

(define (free-id-set-clear s)
  (immutable-free-id-set (dict-clear (free-id-set-table s))))
(define (free-id-set-clear! s)
  (define table (free-id-set-table s))
  (dict-clear! table)
  (mutable-free-id-set table))
  
(define (choose-immutable cmp set0 other-sets-lst)
  (for/fold ([largest set0]) ([s (in-list other-sets-lst)])
    (if (and (immutable-free-id-set? s)
             (cmp (free-id-set-count s)
                  (free-id-set-count largest)))
        s
        largest)))
(define (choose-largest-immutable set0 other-sets-lst)
  (choose-immutable > set0 other-sets-lst))
(define (choose-smallest-immutable set0 other-sets-lst)
  (choose-immutable < set0 other-sets-lst))
  
(define (free-id-set-union set0 . ss)
  (unless (immutable-free-id-set? set0)
    (error 'free-id-set-union "expected immutable free-id set in: ~a" set0))
  (define largest-immutable (choose-largest-immutable set0 ss))
  (immutable-free-id-set
   (for/fold
     ([table (free-id-set-table largest-immutable)])
     ([s (in-list (cons set0 ss))]
      #:unless (eq? s largest-immutable))
     (for/fold ([table table]) ([id (in-dict-keys (free-id-set-table s))])
       (free-id-table-set table id #t)))))
(define (free-id-set-union! set0 . ss)
  (unless (mutable-free-id-set? set0)
    (error 'free-id-set-union! "expected mutable free-id set in: ~a" set0))
  (define table (free-id-set-table set0))
  (for ([s (in-list ss)])
    (for ([id (in-dict-keys (free-id-set-table s))])
      (free-id-table-set! table id #t))))

(define (free-id-set-intersect set0 . ss)
  (unless (immutable-free-id-set? set0)
    (error 'free-id-set-intersect "expected immutable free-id set in: ~a" set0))
  (define smallest-immutable (choose-smallest-immutable set0 ss))
  (define smallest-table (free-id-set-table smallest-immutable))
  (define all-tables-seq (in-list (map free-id-set-table (cons set0 ss))))
  (define (keep? id)
    (for/and ([table all-tables-seq] #:unless (eq? table smallest-table))
      (free-id-table-ref table id #f)))
  (immutable-free-id-set
   (for/fold ([table smallest-table])
             ([id (in-dict-keys smallest-table)] #:unless (keep? id))
     (free-id-table-remove table id))))     
(define (free-id-set-intersect! set0 . ss)
  (unless (mutable-free-id-set? set0)
    (error 'free-id-set-intersect! "expected mutable free-id set in: ~a" set0))
  (define tables-seq (in-list (map free-id-set-table ss)))
  (define (keep? id)
    (for/and ([table tables-seq])
      (printf "keep? ~a = ~a\n" id (free-id-table-ref table id #f))
      (free-id-table-ref table id #f)))
  (define table0 (free-id-set-table set0))
  ;; use dict-keys (instead of in-dict-keys) to grab all keys ahead of time
  ;; bc we will possibly be removing some of them
  (for ([id (dict-keys table0)] #:unless (keep? id))
    (free-id-table-remove! table0 id)))

;(define (free-id-set-subtract s . ss) s)
;(define (free-id-set-subtract! s . ss) (void))

;;; bound-id-set constructors
;(struct mutable-bound-id-set (table))
;(struct immutable-bound-id-set (table))
;
;(define (make-bound-id-set [init-set null] 
;                           #:phase [phase (syntax-local-phase-level)])
;  (mutable-bound-id-set 
;   (make-bound-id-table 
;    (for/hash ([x (in-set init-set)]) (values x #t))
;    phase)))
;
;(define (make-immutable-bound-id-set [init-set null]
;                                     #:phase [phase (syntax-local-phase-level)])
;  (immutable-bound-id-set 
;   (make-immutable-bound-id-table
;    (for/hash ([x (in-set init-set)]) (values x #t))
;    phase)))
;
;(define (bound-id-set? s) (or (mutable-bound-id-set? s) (immutable-bound-id-set? s)))