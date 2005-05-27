;; --------------------------------------------------------------------------
;; list-library.ss
;; export: 
;; collect: 
;;   (A ((cons B (listof B)) (listof B) (union A C) -> (union A C))
;;    -> 
;;    ((listof B) -> (union A C)))

; #|
; (unit/sig
;   (collect filter set-minus subset?)
;   (import plt:userspace^)
; |#

  (define collect
    (lambda (base combine)
      (letrec ([C 
		(lambda (l)
		  (cond
		   ((null? l) base)
		   (else (combine l (car l) (C (cdr l))))))])
	C)))

  (define filter
    (lambda (p? l)
      [(collect null (lambda (_ x rest) (if (p? x) (cons x rest) rest))) l]))

  ;; set library
  (define set-minus
    (lambda (set1 set2)
      [(collect null (lambda (_ e1 rest) (if (member e1 set2) rest (cons e1 rest))))
       set1]))

  (define subset?
    (lambda (state1 state2)
      (cond
	((null? state1) #t)
	(else (and (member (car state1) state2)
		(subset? (cdr state1) state2))))))
; #|
;   )
; |#
