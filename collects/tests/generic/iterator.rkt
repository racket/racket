#lang racket/base

(require racket/private/generic racket/sequence)

;; This was designed as a higher-level interface on top of sequences,
;; but it turns out streams can do all that already (including state),
;; making iterators redundant. Kept around as extra tests.


(define-values (prop:iterator iterator? iterator-accessor)
  (make-struct-type-property
   'iterator
   #f
   ;; Iterators are automatically sequences, but don't have the full
   ;; flexibility of sequences: they are their own initial state, and
   ;; they can only look at their state to decide if iteration is over.
   ;; Given that extra field can be added to the iterator, there is no
   ;; loss of expressiveness.
   (list (cons prop:sequence
               (lambda (method-table) ; 3-vector
                 (define iterator-first     (vector-ref method-table 0))
                 (define iterator-rest      (vector-ref method-table 1))
                 (define iterator-continue? (vector-ref method-table 2))
                 (lambda (t)
                   (make-do-sequence
                    (lambda ()
                      (values iterator-first
                              iterator-rest ; needs to create a new struct
                              t
                              iterator-continue?
                              (lambda (v) #t)
                              (lambda (t v) #t))))))))))

(define-generics (iterator gen:iterator prop:iterator iterator?
                           #:defined-table dummy
                           #:prop-defined-already? iterator-accessor
                           #:define-contract #f)
  (iterator-first     iterator)
  (iterator-rest      iterator)
  (iterator-continue? iterator))

(struct list-iterator (l)
        #:methods gen:iterator
        [(define (iterator-first x) (car (list-iterator-l x)))
         (define (iterator-rest  x) (list-iterator (cdr (list-iterator-l x))))
         (define (iterator-continue? x) (not (null? (list-iterator-l x))))])

(struct vector-iterator (i v)
        #:methods gen:iterator
        [(define (iterator-first x) (vector-ref (vector-iterator-v x)
                                                (vector-iterator-i x)))
         (define (iterator-rest x) (vector-iterator (add1 (vector-iterator-i x))
                                                    (vector-iterator-v x)))
         (define (iterator-continue? x) (not (>= (vector-iterator-i x)
                                                 (vector-length
                                                  (vector-iterator-v x)))))])

(module+ test
 (require rackunit)

 (define s1 (list-iterator '(#t #t #f)))
 (check-true (sequence-ormap values s1))
 (check-false (sequence-andmap values s1))

 (define s2 (vector-iterator 0 '#(1 2 3)))
 (check-equal? (sequence-fold + 0 s2) 6)
 )
