#lang scheme/base

(require mzlib/unit
         scheme/gui/base
         "hierlist/hierlist-sig.rkt"
         "hierlist/hierlist-unit.rkt")

(define-values/invoke-unit/infer hierlist@)

(provide-signature-elements hierlist^)

#|

;; Testing
(define f (make-object frame% "test"))
(define p (make-object horizontal-panel% f))
(define c (make-object (class hierarchical-list% args
			 (override
			   [on-item-opened
			    (lambda (i)
			      (let ([f (send i user-data)])
				(when f (f i))))]
			   [on-select
			    (lambda (i)
			      (printf "Selected: ~a\n"
				      (if i 
					  (send (send i get-editor) get-flattened-text)
					  i)))]
			   [on-double-select
			    (lambda (s)
			      (printf "Double-click: ~a\n"
				      (send (send s get-editor) get-flattened-text)))])
			 (sequence (apply super-init args)))
		       p))

(define a (send c new-list))
(send (send a get-editor) insert "First Item: List")
(send (send (send a new-item) get-editor) insert "Sub1")
(send (send (send a new-item) get-editor) insert "Sub2")
(define a.1 (send a new-list))
(send (send a.1 get-editor) insert "Deeper List")
(send (send (send a.1 new-item) get-editor) insert "Way Down")

(define b (send c new-item))
(send (send b get-editor) insert "Second Item")

(define d (send c new-list))
(send (send d get-editor) insert "dynamic")
(send d user-data (lambda (d)
		    (time (let loop ([i 30])
			    (unless (zero? i)
				    (send (send (send d new-item) get-editor) insert (number->string i))
				    (loop (sub1 i)))))))

(define x (send c new-list))
(send (send x get-editor) insert "x")

(define y (send c new-item))
(send (send y get-editor) insert "y")

(send f show #t)

(yield (make-semaphore))

|#
