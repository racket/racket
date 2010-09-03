#lang racket/base
;; written by ryanc
(require (for-syntax racket/base
                     unstable/wrapc)
         racket/contract/base
         racket/dict
         racket/vector)

(define make-gvector*
  (let ([make-gvector
         (lambda (#:capacity [capacity 10])
           (make-gvector (make-vector capacity #f) 0))])
    make-gvector))

(define gvector*
  (let ([gvector
         (lambda init-elements
           (let ([gv (make-gvector*)])
             (apply gvector-add! gv init-elements)
             gv))])
    gvector))

(define (check-index who index n set-to-add?)
  (unless (exact-nonnegative-integer? index)
    (raise-type-error who "exact nonnegative integer" index))
  (unless (< index n)
    (error who "index out of range ~a~a: ~s"
           (let ([max-index (if set-to-add? (- n 2) (- n 1))])
             (cond [(< max-index 0) "(empty)"]
                   [else (format "[0,~s]" max-index)]))
           (if set-to-add?
               (format " or ~s to add" (- n 1))
               "")
           index)))

(define ((bad-index-error who index))
  (raise-mismatch-error who "index out of range" index))

(define (gvector-add! gv . items)
  (let ([n (gvector-n gv)]
        [v (gvector-vec gv)]
        [item-count (length items)])
    (cond [(<= (+ n item-count) (vector-length v))
           (for ([index (in-naturals n)] [item (in-list items)])
             (vector-set! v index item))
           (set-gvector-n! gv (+ n item-count))]
          [else
           (let* ([nn (let loop ([nn n])
                        (if (<= (+ n item-count) nn) nn (loop (* 2 nn))))]
                  [nv (make-vector nn #f)])
             (vector-copy! nv 0 v)
             (for ([index (in-naturals n)] [item (in-list items)])
               (vector-set! nv index item))
             (set-gvector-vec! gv nv)
             (set-gvector-n! gv (+ n item-count)))])))

(define SHRINK-MIN 10)

;; SLOW!
(define (gvector-remove! gv index)
  (let ([n (gvector-n gv)]
        [v (gvector-vec gv)])
    (check-index 'gvector-remove! index n #f)
    (cond [(<= SHRINK-MIN (* 3 n) (vector-length v))
           (let ([nv (make-vector (floor (/ (vector-length v) 2)) #f)])
             (vector-copy! nv 0 v 0 index)
             (vector-copy! nv index v (add1 index) n)
             (set-gvector-n! gv (sub1 n))
             (set-gvector-vec! gv nv))]
          [else
           (set-gvector-n! gv (sub1 n))
           (vector-copy! v index v (add1 index) n)
           (vector-set! v (sub1 n) #f)])))

(define (gvector-count gv)
  (gvector-n gv))

(define (gvector-ref gv index
                     [default (bad-index-error 'gvector-ref index)])
  (unless (exact-nonnegative-integer? index)
    (raise-type-error 'gvector-ref "exact nonnegative integer" index))
  (if (< index (gvector-n gv))
      (vector-ref (gvector-vec gv) index)
      (if (procedure? default)
          (default)
          default)))

;; gvector-set! with index = |gv| is interpreted as gvector-add!
(define (gvector-set! gv index item)
  (let ([n (gvector-n gv)])
    (check-index 'gvector-set! index (add1 n) #t)
    (if (= index n)
        (gvector-add! gv item)
        (vector-set! (gvector-vec gv) index item))))

;; creates a snapshot vector
(define (gvector->vector gv)
  (vector-copy (gvector-vec gv) 0 (gvector-n gv)))

;; Iteration methods

;; A gvector position is represented as an exact-nonnegative-integer.

(define (gvector-iterate-first gv)
  (and (positive? (gvector-n gv)) 0))

(define (gvector-iterate-next gv iter)
  (check-index 'gvector-iterate-next iter (gvector-n gv) #f)
  (let ([n (gvector-n gv)])
    (and (< (add1 iter) n)
         (add1 iter))))

(define (gvector-iterate-key gv iter)
  (check-index 'gvector-iterate-key iter (gvector-n gv) #f)
  iter)

(define (gvector-iterate-value gv iter)
  (check-index 'gvector-iterate-value iter (gvector-n gv) #f)
  (gvector-ref gv iter))

(define (in-gvector gv)
  (unless (gvector? gv)
    (raise-type-error 'in-gvector "gvector" gv))
  (in-dict-values gv))

(define-sequence-syntax in-gvector*
  (lambda () #'in-vector)
  (lambda (stx)
    (syntax-case stx ()
      [[(var) (in-gv gv-expr)]
       (with-syntax ([gv-expr-c (wrap-expr/c #'gvector? #'gv-expr #:macro #'in-gv)])
         (syntax/loc stx
           [(var)
            (:do-in ([(gv) gv-expr-c])
                    (void) ;; outer-check; handled by contract
                    ([index 0] [vec (gvector-vec gv)] [n (gvector-n gv)]) ;; loop bindings
                    (< index n) ;; pos-guard
                    ([(var) (vector-ref vec index)]) ;; inner bindings
                    #t ;; pre-guard
                    #t ;; post-guard
                    ((add1 index) (gvector-vec gv) (gvector-n gv)))]))]
      [[(var ...) (in-gv gv-expr)]
       (with-syntax ([gv-expr-c (wrap-expr/c #'gvector? #'gv-expr #:macro #'in-gv)])
         (syntax/loc stx
           [(var ...) (in-vector gv-expr-c)]))]
      [_ #f])))

(define-syntax (for/gvector stx)
  (syntax-case stx ()
    [(_ (clause ...) . body)
     (quasisyntax/loc stx
       (let ([gv (make-gvector)])
         (for/fold/derived #,stx () (clause ...) . body)
         gv))]))

(define-syntax (for*/gvector stx)
  (syntax-case stx ()
    [(_ (clause ...) . body)
     (quasisyntax/loc stx
       (let ([gv (make-gvector)])
         (for*/fold/derived #,stx () (clause ...) . body)
         gv))]))

(define-struct gvector (vec n)
  #:mutable
  #:property prop:dict
             (vector gvector-ref
                     gvector-set!
                     #f ;; set
                     gvector-remove!
                     #f ;; remove
                     gvector-count
                     gvector-iterate-first
                     gvector-iterate-next
                     gvector-iterate-key
                     gvector-iterate-value)
  #:property prop:sequence in-gvector)

(provide/contract
 [gvector?
  (-> any/c any)]
 [rename gvector* gvector
  (->* () () #:rest any/c gvector?)]
 [rename make-gvector* make-gvector
  (->* () (#:capacity exact-positive-integer?) gvector?)]
 [gvector-ref
  (->* (gvector? exact-nonnegative-integer?) (any/c) any)]
 [gvector-set!
  (-> gvector? exact-nonnegative-integer? any/c any)]
 [gvector-add!
  (->* (gvector?) () #:rest any/c any)]
 [gvector-remove!
  (-> gvector? exact-nonnegative-integer? any)]
 [gvector-count
  (-> gvector? any)]
 [gvector->vector
  (-> gvector? vector?)])

(provide (rename-out [in-gvector* in-gvector]))
