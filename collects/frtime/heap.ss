(module heap mzscheme
  
  (require mzlib/etc
	   "base-gm.ss"
           mzlib/list
	   "dv.ss")
  
  
  (provide make-heap heap-empty? heap-size heap-insert heap-pop
	   heap-peak heap-remove heap-find
	   heap-contains heap-resort heap->list)
  
  
  
  
  (define-struct t (sorter equality data))
  
  ;; sorter: elements which have the most trueness according to 
  ;; the sorter pop out first
  (define (make-heap sorter equality)
    (let ((data (dv:make 5)))
      (dv:append data 0)
      (make-t sorter equality data)))
  
  (define (heap-size heap)
    (- (dv:length (t-data heap)) 1))
  
  (define (heap-empty? heap)
    (= (heap-size heap) 0))

  (define (heap-last heap)
    (- (dv:length (t-data heap)) 1))
  
  (define (heap-parent i) 
    (floor (/ i 2)))
  
  (define (heap-left i) (* i 2))
  
  (define (heap-right i) (+ 1 (* i 2)))
  
  (define (heap-has-right heap i) 
    (<= (heap-right i) (heap-last heap)))
  
  (define (heap-has-left heap i)
    (<= (heap-left i) (heap-last heap)))
  
  (define (heap-insert heap item)
    (let* ((sorter (t-sorter heap))
	   (data (t-data heap)))
      (dv:append data item)
      (let ((d (let loop ((prev (heap-last heap))
			  (current (heap-parent (heap-last heap))))
		 
		 (cond ((= current 0) prev)
		       ((sorter item (dv:ref data current)) 
			(dv:set! data prev (dv:ref data current))
			(loop current (heap-parent current)))
		       (#t prev)))))
	(dv:set! data d item))))
  
  (define (heap-peak heap)
    (if (= (heap-size heap) 0) (error "heap-peak: empty")
	(dv:ref (t-data heap) 1)))

  (define (heap-pop heap)
    (if (= (heap-size heap) 0) (error "heap-pop: empty")
	(begin0 (dv:ref (t-data heap) 1)
                (heap-remove-pos heap 1))))
  
  (define (heap-remove-pos heap pos)
    (let* ((data (t-data heap))
	   (sorter (t-sorter heap)))
      
      (cond ((= 0 (heap-size heap)) (error "heap: removing from empty"))
	    ((= pos (heap-last heap)) (dv:remove-last data))
	    (#t (let ((item (dv:ref data (heap-last heap))))
		  (dv:remove-last data)
		  (let loop ((current pos))
		    
		    (dv:set! data current item)
		    (let* ((left (heap-left current))
			   (right (heap-right current))
			   (best-1 (if (and (heap-has-left heap current)
					    (sorter (dv:ref data left) item)) 
				       left current))
			   
			   (best-2 (if (and (heap-has-right heap current)
					    (sorter (dv:ref data right)
						    (dv:ref data best-1)))
				       right best-1)))
		      
		      (if (not (= best-2 current))
			  (begin (dv:set! data current (dv:ref data best-2))
				 (loop best-2))))))))))
  
  ;; return false if the object is not found
  (define (heap-remove heap item)
    (let ((pos (heap-find heap item)))
      (if (not pos) false
	  (begin (heap-remove-pos heap pos) true))))

  (define (heap-contains heap item)
    (if (heap-find heap item) true false))

  (define (heap-find heap item)
    (let ((data (t-data heap))
	  (equality (t-equality heap))
	  (sorter (t-sorter heap)))
      (let loop ((current 1))
	(let ((current-item (dv:ref data current)))
	  (cond ((equality item current-item) current)
		((sorter item current-item) #f)
		(#t (or (and (heap-has-left heap current)
			     (not (sorter item (dv:ref data (heap-left current))))
			     (loop (heap-left current)))
			(and (heap-has-right heap current)
			     (not (sorter item (dv:ref data (heap-right current))))
			     (loop (heap-right current))))))))))
  
  (define (heap-resort heap item)
    (heap-remove heap item)
    (heap-insert heap item))
  
  (define (heap->list heap)
    (vector->list (t-data heap)))
  
  (define (test)
    (define f (make-heap > eq?))
    (define d (t-data f))
    (heap-insert f 99)
    (print-each "A " d) 
    (heap-remove-pos f 1)
    (print-each "B " d)
    (for-each (lambda (x) (heap-insert f x)) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14))
    (print-each "C " d)
    (heap-remove f 10) (print-each " " d)
    (heap-remove f 5) (print-each " " d)
    (heap-remove f 8) (print-each " " d)
    (heap-remove f 13) (print-each " " d)
    (print-each (heap-contains f 11))
    (print-each (heap-contains f 123))
    (heap-pop f)
    (heap-pop f)
    (heap-pop f)
    (heap-pop f) (print-each " " d)
    (print-each (heap-contains f 11))
    (print-each (heap-contains f 4))
    (print-each (heap->list f))
    (heap-remove f 2)
    (print-each (heap->list f))
    (heap-remove f 3)
    (print-each (heap->list f))
    )
  
  ;(test)

  )

