(module sortp mzscheme
  (provide list-sorted?
           vector-sorted?)
  
  (require "vector-util.scm")
  
  ;;; The sort package -- sorted predicates
  ;;; Olin Shivers 10/98.
  ;;;
  ;;; (list-sorted? < lis) -> boolean
  ;;; (vector-sorted? < v [start end]) -> boolean
  
  (define (list-sorted? < list)
    (or (not (pair? list))
        (let lp ((prev (car list)) (tail (cdr list)))
          (or (not (pair? tail))
              (let ((next (car tail)))
                (and (not (< next prev))
                     (lp next (cdr tail))))))))
  
  (define (vector-sorted? elt< v . maybe-start+end)
    (call-with-values
     (lambda () (vector-start+end v maybe-start+end))
     (lambda (start end)
       (or (>= start end)			; Empty range
           (let lp ((i (+ start 1)) (vi-1 (vector-ref v start)))
             (or (>= i end)
                 (let ((vi (vector-ref v i)))
                   (and (not (elt< vi vi-1))
                        (lp (+ i 1) vi)))))))))
  
  ;;; Copyright and porting non-notices
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Give me a break. It's fifteen lines of code. I place this code in the
  ;;; public domain; help yourself.
  ;;;
  ;;; If your Scheme has a faster mechanism for handling optional arguments
  ;;; (e.g., Chez), you should definitely port over to it. Note that argument
  ;;; defaulting and error-checking are interleaved -- you don't have to
  ;;; error-check defaulted START/END args to see if they are fixnums that are
  ;;; legal vector indices for the corresponding vector, etc.
  
  )
