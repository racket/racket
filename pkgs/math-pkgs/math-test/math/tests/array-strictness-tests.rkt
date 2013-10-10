#lang typed/racket

(require math/array
         typed/rackunit)

(define (check-always)
  (printf "(array-strictness) = ~v~n" (array-strictness))
  (check-true (array-strict? (make-array #(4 4) 0)))
  (check-true (array-strict? (indexes-array #(4 4))))
  (check-true (array-strict? (index-array #(4 4))))
  (check-true (array-strict? (axis-index-array #(4 4) 0)))
  (check-true (array-strict? (diagonal-array 2 6 1 0)))
  (check-true (array-strict? (list->array '(1 2 3 4))))
  (check-true (array-strict? (list->array #(2 2) '(1 2 3 4))))
  (check-true (array-strict? (list*->array 0 exact-integer?)))
  (check-true (array-strict? (list*->array '(1 2 3 4) exact-integer?)))
  (check-true (array-strict? (list*->array '((1 2) (3 4)) exact-integer?)))
  (check-true (array-strict? (vector->array #(1 2 3 4))))
  (check-true (array-strict? (vector->array #(2 2) #(1 2 3 4))))
  (check-true (array-strict? (vector*->array 0 exact-integer?)))
  (check-true (array-strict? ((inst vector*->array Integer) #(1 2 3 4) exact-integer?)))
  (check-true (array-strict? ((inst vector*->array Integer) #(#(1 2) #(3 4)) exact-integer?)))
  (check-true (array-strict? (build-simple-array #(4 4) (λ (_) 0))))
  (check-false (array-strict? (array-lazy (build-simple-array #(4 4) (λ (_) 0)))))
  )

(define nonstrict-2x2-arr
  (parameterize ([array-strictness #f])
    (build-array #(2 2) (λ (_) 0))))

(define strict-2x2-arr
  (parameterize ([array-strictness #t])
    (build-array #(2 2) (λ (_) 0))))

(define 2x2-indexes-arr
  (array #['#(0 0) '#(1 1)]))

(check-false (array-strict? nonstrict-2x2-arr))
(check-true (array-strict? strict-2x2-arr))

(parameterize ([array-strictness #t])
  (check-always)
  (check-true (array-strict? (array-list->array (list))))
  (check-true (array-strict? (array-list->array (list (array #[0 1])))))
  (check-true (array-strict? (array-list->array (list (array #[0 1]) (array #[2 3])))))
  (check-true (andmap (inst array-strict? Integer) (sequence->list (in-array-axis (array #[0 1])))))
  (check-false (array-strict? (array-broadcast nonstrict-2x2-arr ((inst vector Index) 2 2))))
  (check-true (array-strict? (array-broadcast nonstrict-2x2-arr ((inst vector Index) 2 4))))
  (check-true (array-strict? (array-broadcast strict-2x2-arr ((inst vector Index) 2 2))))
  (check-false (array-strict? (array-broadcast strict-2x2-arr ((inst vector Index) 2 4))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (array-indexes-ref arr 2x2-indexes-arr))))
  
  (for*: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
          [spec  (list '(0) 0)])
    (check-true (array-strict? (array-slice-ref arr (list (::) spec))))
    (check-true (array-strict? (array-slice-ref arr (list (::) (::new 2) spec)))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (array-transform arr #(2 2)
                                                (λ: ([js : Indexes])
                                                  (vector (vector-ref js 1) (vector-ref js 0)))))))
  
  (for: ([k  (list 0 1)])
    (check-true (array-strict? (array-append* (list nonstrict-2x2-arr strict-2x2-arr) k))))
  
  (for*: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
          [k  (list 0 1)]
          [dk  (list 0 1 2)])
    (check-true (array-strict? (array-axis-insert arr k dk))))
  
  (for*: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
          [k  (list 0 1)]
          [jk  (list 0 1)])
    (check-true (array-strict? (array-axis-ref arr k jk))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (array-axis-swap arr 0 1))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (array-axis-permute arr '(1 0)))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (array-reshape arr #(4)))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (array-flatten arr))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
         [k  (list 0 1)])
    (check-true (array-strict? (array-axis-sum arr k))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
         [k  (list 0 1)])
    (check-true (array-strict? (array-axis-count arr k even?))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
         [k  (list 0 1)])
    (check-true (array-strict? (array-axis-and (array-map even? arr) k))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
         [k  (list 0 1)])
    (check-true (array-strict? (array-axis-or (array-map even? arr) k))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (array->list-array arr))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (list-array->array (array->list-array arr)))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-true (array-strict? (array-fold arr (inst array->list-array (Listof* Integer))))))
  )

(parameterize ([array-strictness #f])
  (check-always)
  (check-false (array-strict? (array-list->array (list))))
  (check-false (array-strict? (array-list->array (list (array #[0 1])))))
  (check-false (array-strict? (array-list->array (list (array #[0 1]) (array #[2 3])))))
  (check-false (ormap (inst array-strict? Integer) (sequence->list (in-array-axis (array #[0 1])))))
  (check-false (array-strict? (array-broadcast nonstrict-2x2-arr ((inst vector Index) 2 2))))
  (check-false (array-strict? (array-broadcast nonstrict-2x2-arr ((inst vector Index) 2 4))))
  (check-true (array-strict? (array-broadcast strict-2x2-arr ((inst vector Index) 2 2))))
  (check-false (array-strict? (array-broadcast strict-2x2-arr ((inst vector Index) 2 4))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (array-indexes-ref arr 2x2-indexes-arr))))
  
  (for*: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
          [spec  (list '(0) 0)])
    (check-false (array-strict? (array-slice-ref arr (list (::) spec))))
    (check-false (array-strict? (array-slice-ref arr (list (::) (::new 2) spec)))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (array-transform arr #(2 2)
                                                 (λ: ([js : Indexes])
                                                   (vector (vector-ref js 1) (vector-ref js 0)))))))
  
  (for: ([k  (list 0 1)])
    (check-false (array-strict? (array-append* (list nonstrict-2x2-arr strict-2x2-arr) k))))
  
  (for*: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)] [k  (list 0 1)] [dk  (list 0 1 2)])
    (check-false (array-strict? (array-axis-insert arr k dk))))
  
  (for*: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
          [k  (list 0 1)]
          [jk  (list 0 1)])
    (check-false (array-strict? (array-axis-ref arr k jk))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (array-axis-swap arr 0 1))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (array-axis-permute arr '(1 0)))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (array-reshape arr #(4)))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (array-flatten arr))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
         [k  (list 0 1)])
    (check-false (array-strict? (array-axis-sum arr k))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
         [k  (list 0 1)])
    (check-false (array-strict? (array-axis-count arr k even?))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
         [k  (list 0 1)])
    (check-false (array-strict? (array-axis-and (array-map even? arr) k))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)]
         [k  (list 0 1)])
    (check-false (array-strict? (array-axis-or (array-map even? arr) k))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (array->list-array arr))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (list-array->array (array->list-array arr)))))
  
  (for: ([arr  (list nonstrict-2x2-arr strict-2x2-arr)])
    (check-false (array-strict? (array-fold arr (inst array->list-array (Listof* Integer))))))
  )
