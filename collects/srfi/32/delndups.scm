(module delndups mzscheme
  (require "vector-util.scm")
  (provide list-delete-neighbor-dups
           #;list-delete-neighbor-dups!
           vector-delete-neighbor-dups
           #;vector-delete-neighbor-dups!)
  
  ;;; The sort package -- delete neighboring duplicate elts
  ;;; Copyright (c) 1998 by Olin Shivers.
  ;;; This code is open-source; see the end of the file for porting and
  ;;; more copyright information.
  ;;; Olin Shivers 11/98.
  
  ;;; Problem:
  ;;; vector-delete-neighbor-dups pushes N stack frames, where N is the number
  ;;; of elements in the answer vector. This is arguably a very efficient thing
  ;;; to do, but it might blow out on a system with a limited stack but a big
  ;;; heap. We could rewrite this to "chunk" up answers in temp vectors if we
  ;;; push more than a certain number of frames, then allocate a final answer,
  ;;; copying all the chunks into the answer. But it's much more complex code.
  
  ;;; Exports:
  ;;; (list-delete-neighbor-dups  = lis) -> list
  ;;; (list-delete-neighbor-dups! = lis) -> list
  ;;; (vector-delete-neighbor-dups  = v [start end]) -> vector
  ;;; (vector-delete-neighbor-dups! = v [start end]) -> end'
  
  ;;; These procedures delete adjacent duplicate elements from a list or
  ;;; a vector, using a given element equality procedure. The first or leftmost
  ;;; element of a run of equal elements is the one that survives. The list
  ;;; or vector is not otherwise disordered.
  ;;;
  ;;; These procedures are linear time -- much faster than the O(n^2) general 
  ;;; duplicate-elt deletors that do not assume any "bunching" of elements.
  ;;; If you want to delete duplicate elements from a large list or vector,
  ;;; sort the elements to bring equal items together, then use one of these
  ;;; procedures -- for a total time of O(n lg n). 
  
  ;;; LIST-DELETE-NEIGHBOR-DUPS
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; Below are multiple versions of the LIST-DELETE-NEIGHBOR-DUPS procedure,
  ;;; from simple to complex. RECUR's contract: Strip off any leading X's from 
  ;;; LIS, and return that list neighbor-dup-deleted.
  ;;;
  ;;; The final version
  ;;; - shares a common subtail between the input & output list, up to 1024 
  ;;;   elements;
  ;;; - Needs no more than 1024 stack frames.
  
  ;;; Simplest version. 
  ;;; - Always allocates a fresh list / never shares storage.
  ;;; - Needs N stack frames, if answer is length N.
#;  (define (list-delete-neighbor-dups = lis)
    (if (pair? lis)
        (let ((x0 (car lis)))
          (cons x0 (let recur ((x0 x0) (xs (cdr lis)))
                     (if (pair? xs)
                         (let ((x1  (car xs))
                               (x2+ (cdr xs)))
                           (if (= x0 x1)
                               (recur x0 x2+) ; Loop, actually.
                               (cons x1 (recur x1 x2+))))
                         xs))))
        lis))
  
  ;;; This version tries to use cons cells from input by sharing longest
  ;;; common tail between input & output. Still needs N stack frames, for ans
  ;;; of length N.
  (define (list-delete-neighbor-dups = lis)
    (if (pair? lis)
        (let* ((x0 (car lis))
               (xs (cdr lis))
               (ans (let recur ((x0 x0) (xs xs))
                      (if (pair? xs)
                          (let ((x1  (car xs))
                                (x2+ (cdr xs)))
                            (if (= x0 x1)
                                (recur x0 x2+)
                                (let ((ans-tail (recur x1 x2+)))
                                  (if (eq? ans-tail x2+) xs
                                      (cons x1 ans-tail)))))
                          xs))))
          (if (eq? ans xs) lis (cons x0 ans)))
        
        lis))
  
  ;;; LIST-DELETE-NEIGHBOR-DUPS!
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; This code runs in constant list space, constant stack, and also
  ;;; does only the minimum SET-CDR!'s necessary.
  
  #;
  (define (list-delete-neighbor-dups! = lis)
    (if (pair? lis)
        (let lp1 ((prev lis) (prev-elt (car lis)) (lis (cdr lis)))
          (if (pair? lis)
              (let ((lis-elt (car lis))
                    (next (cdr lis)))
                (if (= prev-elt lis-elt)
                    
                    ;; We found the first elts of a run of dups, so we know
                    ;; we're going to have to do a SET-CDR!. Scan to the end of
                    ;; the run, do the SET-CDR!, and loop on LP1.
                    (let lp2 ((lis next))
                      (if (pair? lis)
                          (let ((lis-elt (car lis))
                                (next (cdr lis)))
                            (if (= prev-elt lis-elt)
                                (lp2 next)
                                (begin (set-cdr! prev lis)
                                       (lp1 lis lis-elt next))))
                          (set-cdr! prev lis)))	; Ran off end => quit.
                    
                    (lp1 lis lis-elt next))))))
    lis)
  
  
  (define (vector-delete-neighbor-dups elt= v . maybe-start+end)
    (call-with-values
     (lambda () (vector-start+end v maybe-start+end))
     (lambda (start end)
       (if (< start end)
           (let* ((x (vector-ref v start))
                  (ans (let recur ((x x) (i start) (j 1))
                         (if (< i end)
                             (let ((y (vector-ref v i))
                                   (nexti (+ i 1)))
                               (if (elt= x y)
                                   (recur x nexti j)
                                   (let ((ansvec (recur y nexti (+ j 1))))
                                     (vector-set! ansvec j y)
                                     ansvec)))
                             (make-vector j)))))
             (vector-set! ans 0 x)
             ans)
           '#()))))
  
  
  ;;; Packs the surviving elements to the left, in range [start,end'),
  ;;; and returns END'.
  #;
  (define (vector-delete-neighbor-dups! elt= v . maybe-start+end)
    (call-with-values
     (lambda () (vector-start+end v maybe-start+end))
     (lambda (start end)
       
       (if (>= start end)
           end
           ;; To eliminate unnecessary copying (read elt i then write the value 
           ;; back at index i), we scan until we find the first dup.
           (let skip ((j start) (vj (vector-ref v start)))
             (let ((j+1 (+ j 1)))
               (if (>= j+1 end)
                   end
                   (let ((vj+1 (vector-ref v j+1)))
                     (if (not (elt= vj vj+1))
                         (skip j+1 vj+1)
                         
                         ;; OK -- j & j+1 are dups, so we're committed to moving
                         ;; data around. In lp2, v[start,j] is what we've done;
                         ;; v[k,end) is what we have yet to handle.
                         (let lp2 ((j j) (vj vj) (k (+ j 2)))
                           (let lp3 ((k k))
                             (if (>= k end)
                                 (+ j 1) ; Done.
                                 (let ((vk (vector-ref v k))
                                       (k+1 (+ k 1)))
                                   (if (elt= vj vk)
                                       (lp3 k+1)
                                       (let ((j+1 (+ j 1)))
                                         (vector-set! v j+1 vk)
                                         (lp2 j+1 vk k+1))))))))))))))))
  
  ;;; Copyright
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;; This code is
  ;;;     Copyright (c) 1998 by Olin Shivers.
  ;;; The terms are: You may do as you please with this code, as long as
  ;;; you do not delete this notice or hold me responsible for any outcome
  ;;; related to its use.
  ;;;
  ;;; Blah blah blah. Don't you think source files should contain more lines
  ;;; of code than copyright notice?
  ;;;
  ;;; Code porting
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;; If your Scheme has a faster mechanism for handling optional arguments
  ;;; (e.g., Chez), you should definitely port over to it. Note that argument
  ;;; defaulting and error-checking are interleaved -- you don't have to
  ;;; error-check defaulted START/END args to see if they are fixnums that are
  ;;; legal vector indices for the corresponding vector, etc.

  )
