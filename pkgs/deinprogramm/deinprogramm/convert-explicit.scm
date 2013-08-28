; I HATE DEFINE-STRUCT!
(define-struct/properties :empty-list ()
  ((prop:custom-write
    (lambda (r port write?)
      (write-string "#<empty-list>" port))))
  (make-inspector))

;; essentially copied from define-record-procedures.scm
(define (write-list l port write?)
  (let ((pp? (and (pretty-printing)
		  (number? (pretty-print-columns)))))

    (write-string "#<" port)
    (write-string "list" port)

    (let-values (((ref-line ref-column ref-pos)
		  (if pp?
		      (port-next-location port)
		      (values 0 -1 0)))) ; to compensate for space
      (let ((do-element
	     (if pp?
		 (lambda (element)
		   (let* ((max-column (- (pretty-print-columns) 1)) ; > terminator
			  (tentative
			   (make-tentative-pretty-print-output-port
			    port
			    max-column
			    void)))
		     (display " " tentative)
		     ((if write? write display) element tentative)
		     (let-values (((line column pos) (port-next-location tentative)))
		       (if (< column max-column)
			   (tentative-pretty-print-port-transfer tentative port)
			   (begin
			     (tentative-pretty-print-port-cancel tentative)
			     (let ((count (pretty-print-newline port max-column)))
			       (write-string (make-string (max 0 (- (+ ref-column 1) count)) #\space) 
					     port)
			       ((if write? write display) element port)))))))
		 (lambda (element)
		   (display " " port)
		   ((if write? write display) element port)))))
	(let loop ((elements (:list-elements l)))
	  (cond
	   ((pair? elements)
	    (do-element (car elements))
	    (loop (cdr elements)))
	   ((not (null? elements))
	    (write-string " ." port)
	    (do-element elements))))))
      
    (write-string ">" port)))

;; might be improper
(define-struct/properties :list (elements)
  ((prop:custom-write write-list))
  (make-inspector))

(define (convert-explicit v)
  (let ((hash (make-hasheq)))
    (let recur ((v v))
      (cond
       ((null? v) (make-:empty-list)) ; prevent silly printing of sharing
       ((pair? v)
	(make-:list
	 (let list-recur ((v v))
	   (cond
	    ((null? v)
	     v)
	    ((not (pair? v))
	     (recur v))
	    (else
	     (cons (recur (car v))
		   (list-recur (cdr v))))))))
       ((struct? v)
	(or (hash-ref hash v #f)
	    (let-values (((ty skipped?) (struct-info v)))
	      (cond
	       ((and ty (lazy-wrap? ty))
		(let ((lazy-wrap-info (lazy-wrap-ref ty)))
		  (let ((constructor (lazy-wrap-info-constructor lazy-wrap-info))
			(raw-accessors (lazy-wrap-info-raw-accessors lazy-wrap-info)))
		    (let ((val (apply constructor (map (lambda (raw-accessor)
							 (recur (raw-accessor v)))
						       raw-accessors))))
		      (hash-set! hash v val)
		      val))))
	       (else v)))))
       (else
	v)))))

