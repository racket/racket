(module sort mzscheme
  (provide list-sort
           (rename list-sort list-sort!)
           list-stable-sort
           (rename list-stable-sort list-stable-sort!)
           vector-sort
           vector-sort!
           vector-stable-sort
           vector-stable-sort!
           ; lmsort
           list-merge
           (rename list-merge list-merge!)
           list-merge-sort
           (rename list-merge-sort list-merge-sort!)
           ; vmsort
           vector-merge
           vector-merge!
           vector-merge-sort
           vector-merge-sort!
           ; visort
           vector-insert-sort
           vector-insert-sort!
           ; vhsort
           vector-heap-sort!
           vector-heap-sort
           ; vbinsearch
           vector-binary-search
           vector-binary-search3
           ; delndups
           list-delete-neighbor-dups
           (rename list-delete-neighbor-dups list-delete-neighbor-dups!)
           vector-delete-neighbor-dups
           (rename vector-delete-neighbor-dups vector-delete-neighbor-dups!)
           ; sortp
           list-sorted?
           vector-sorted?
           )
  
  (require "lmsort.scm"
           "vmsort.scm"
           "visort.scm"
           "vhsort.scm"
           "vbinsearch.scm"
           "sortp.scm"
           "delndups.scm")
  
  ;;; The sort package -- general sort & merge procedures
  ;;;
  ;;; Copyright (c) 1998 by Olin Shivers.
  ;;; You may do as you please with this code, as long as you do not delete this
  ;;; notice or hold me responsible for any outcome related to its use.
  ;;; Olin Shivers 10/98.
  
  ;;; This file just defines the general sort API in terms of some
  ;;; algorithm-specific calls.
  
  (define (list-sort < l)		; Sort lists by converting to
    (let ((v (list->vector l)))		; a vector and sorting that.
      (vector-heap-sort! < v)
      (vector->list v)))
  
  #;(define list-sort! list-merge-sort!)
  
  (define list-stable-sort  list-merge-sort)
  #;(define list-stable-sort! list-merge-sort!)
  
  (define vector-sort  vector-heap-sort)
  (define vector-sort! vector-heap-sort!)
  
  (define vector-stable-sort  vector-merge-sort)
  (define vector-stable-sort! vector-merge-sort!)
  
  )
