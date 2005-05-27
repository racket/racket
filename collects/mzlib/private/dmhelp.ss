
(module dmhelp mzscheme
  (require (lib "stx.ss" "syntax"))

  (provide dm-syntax->datum
	   dm-subst)

  ;; `dm->syntax-datum' is like syntax-object->datum, but it also
  ;; builds a hash table that maps generated data to original syntax
  ;; objects. The hash table can then be used with `dm-subst' to
  ;; replace each re-used, unmodified datum with the original syntax
  ;; object.

  (define (dm-syntax->datum stx ht)
    (define cycle-ht #f)
    ;; Easiest to handle cycles by letting `syntax-object->datum'
    ;;  do all the work.
    (let ([v (syntax-object->datum stx)])
      (let loop ([stx stx][v v])
	(unless (and cycle-ht (hash-table-get cycle-ht v (lambda () #f)))
	  (when (and (syntax? stx) (syntax-graph? stx))
	    (unless cycle-ht
	      (set! cycle-ht (make-hash-table)))
	    (hash-table-put! cycle-ht v #t))
	  (let ([already (hash-table-get ht v (lambda () #f))])
	    (if already
		(hash-table-put! ht v #t) ;; not stx => don't subst later
		(hash-table-put! ht v stx))
	    (cond
	     [(stx-pair? stx)
	      (loop (stx-car stx) (car v))
	      (loop (stx-cdr stx) (cdr v))]
	     [(stx-null? stx) null]
	     [(vector? (syntax-e stx))
	      (for-each
	       loop
	       (vector->list
		(syntax-e stx))
	       (vector->list v))]
	     [(box? (syntax-e stx))
	      (loop (unbox (syntax-e stx))
		    (unbox v))]
	     [else (void)]))))
      v))
  
  (define (dm-subst ht v)
    (define cycle-ht (make-hash-table))
    (let loop ([v v])
      (if (hash-table-get cycle-ht v (lambda () #f))
	  v
	  (begin
	    (hash-table-put! cycle-ht v #t)
	    (let ([m (hash-table-get ht v (lambda () #f))])
	      (cond
	       [(syntax? m) m] ;; subst back!
	       [(pair? v) (cons (loop (car v))
				(loop (cdr v)))]
	       [(vector? v) (list->vector
			     (map
			      loop
			      (vector->list v)))]
	       [(box? v) (box (loop (unbox v)))]
	       [else v])))))))



	  
