(module vbinsearch mzscheme
  (provide vector-binary-search
           vector-binary-search3)
  
  (require "vector-util.scm")
  
  ;;; The sort package -- binary search			-*- Scheme -*-
  ;;; Copyright (c) 1998 by Olin Shivers.
  ;;; This code is in the public domain.
  ;;; Olin Shivers 98/11
  
  ;;; Returns the index of the matching element.
  ;;; (vector-binary-search < car 4 '#((1 . one) (3 . three)
  ;;;                                  (4 . four) (25 . twenty-five)))
  ;;;   => 2
  
  (define (vector-binary-search key< elt->key key v . maybe-start+end)
    (call-with-values
     (lambda () (vector-start+end v maybe-start+end))
     (lambda (start end)
       (let lp ((left start) (right end))	; Search V[left,right).
         (and (< left right)
              (let* ((m (quotient (+ left right) 2))
                     (elt (vector-ref v m))
                     (elt-key (elt->key elt)))
                (cond ((key< key elt-key) (lp left m))
                      ((key< elt-key key) (lp (+ m 1) right))
                      (else m))))))))
  
  (define (vector-binary-search3 compare v . maybe-start+end)
    (call-with-values
     (lambda () (vector-start+end v maybe-start+end))
     (lambda (start end)
       (let lp ((left start) (right end))	; Search V[left,right).
         (and (< left right)
              (let* ((m (quotient (+ left right) 2))
                     (sign (compare (vector-ref v m))))
                (cond ((> sign 0) (lp left m))
                      ((< sign 0) (lp (+ m 1) right))
                      (else m))))))))
  
  )
