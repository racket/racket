(module generator mzscheme
  (require "private/matcher.ss")

  (provide lang->generator-table
           for-each-generated
           for-each-generated/size)
  
  (define (lang->generator-table lang
				 nums
				 vars
				 strs
				 skip-kws
				 cache-limit)

    ;; -------------------- Cache implementation --------------------
    ;; Cache is currently disabled. It's not clear that it's useful.
    (define (cache-small gen) gen)
    
    ;; -------------------- Build table --------------------
    ;; The `gens' table maps non-terminal symbols to
    ;; generator functions. A generator function conumes:
    ;;   * the min acceptable size of a generated element
    ;;   * the max acceptable size of a generated element
    ;;   * a sucess continuation proc that accepts
    ;;        - the generated value
    ;;        - the value's size
    ;;        - a generator proc that produces the next value;
    ;;          this proc expects to be given the same min, max,
    ;;          and fail continuation proc as before
    ;;   * a failure continuation thunk
    ;;
    (let ([nts (compiled-lang-lang lang)]
          [nt-map (make-hash-table)])
      ;; nt-map tells us which symbols are non-terminals; it also 
      ;; provides conservative min-size and max-size thunks that are
      ;; refined as table generation proceeds
      (for-each (lambda (nt) (hash-table-put! nt-map (nt-name nt) 
                                              (cons (lambda () 1)
                                                    (lambda () +inf.0))))
                nts)
      ;; gens is the main hash table
      (let ([gens (make-hash-table)]
            [atomic-alts (lambda (l size)
                           (values
                            (lambda (min-size max-size result-k fail-k)
			      (let loop ([l l][result-k result-k][max-size max-size][fail-k fail-k])
				(if (<= min-size size max-size)
                                    (if (null? l)
                                        (fail-k)
                                        (result-k (car l)
						  size
						  (lambda (s xs result-k fail-k)
						    (loop (cdr l) result-k xs fail-k))))
				    (fail-k))))
                            (lambda () size)
                            (lambda () size)))]
            [to-do nts])
        (letrec ([make-gen/get-size
                  (lambda (p)
                    (cond
                      [(hash-table-get nt-map p (lambda () #f))
                       => (lambda (get-sizes)
                            (values
                             (lambda (min-size max-size result-k fail-k)
                               ((hash-table-get gens p) min-size max-size result-k fail-k))
                             (car get-sizes)
                             (cdr get-sizes)))]
                      [(eq? 'number p) (atomic-alts nums 1)]
                      [(eq? 'string p) (atomic-alts strs 1)]
                      [(eq? 'any p) (atomic-alts (append nums strs vars) 1)]
                      [(or (eq? 'variable p)
                           (and (pair? p)
                                (eq? (car p) 'variable-except)))
                       (atomic-alts vars 1)]
                      [(symbol? p) ; not a non-terminal, because we checked above
		       (if (memq p skip-kws)
			   (values
			    (lambda (min-size max-size result-k fail-k)
			      (fail-k))
			    (lambda () +inf.0)
			    (lambda () -1))
			   (atomic-alts (list p) 0))]
                      [(null? p) (atomic-alts (list null) 0)]
                      [(and (pair? p)
                            (or (not (pair? (cdr p)))
                                (not (eq? '... (cadr p)))))
                       (make-pair-gen/get-size p cons)]
                      [(and (pair? p) (pair? (cdr p)) (eq? '... (cadr p)))
		       (let-values ([(just-rest just-rest-min-size just-rest-max-size)
				     (make-gen/get-size (cddr p))]
				    [(both both-min-size both-max-size)
				     (make-pair-gen/get-size (cons (kleene+ (car p)) (cddr p)) append)])
			 (values
			  (lambda (min-size max-size result-k fail-k)
			    (let loop ([both both][result-k result-k][max-size max-size][fail-k fail-k])
			      (both min-size max-size
				    (lambda (v size next-both)
				      (result-k v size
						(lambda (ns xs result-k fail-k)
						  (loop next-both result-k xs fail-k))))
				    (lambda ()
				      (just-rest min-size max-size result-k fail-k)))))
			  just-rest-min-size
			  (lambda () +inf.0)))]
                      [else
                       (error 'make-gen "unrecognized pattern: ~e" p)]))]
                 [make-pair-gen/get-size
                  (lambda (p combiner)
                    (let*-values ([(first first-min-size first-max-size) 
                                   (make-gen/get-size (car p))]
                                  [(rest rest-min-size rest-max-size) 
                                   (make-gen/get-size (cdr p))]
                                  [(this-min-size) (let ([v #f])
						     (lambda ()
						       (unless v
							 (set! v (+ (first-min-size)
								    (rest-min-size))))
						       v))]
                                  [(this-max-size) (let ([v #f])
						     (lambda ()
						       (unless v
							 (set! v (+ (first-max-size)
								    (rest-max-size))))
						       v))])
                      (values
                       (cache-small
                        (lambda (min-size max-size result-k fail-k)
			  (if (min-size . > . (this-max-size))
			      (fail-k)
			      (let rloop ([rest rest][result-k result-k][max-size max-size][fail-k fail-k][failed-size +inf.0])
				(if (max-size . < . (this-min-size))
				    (fail-k)
				    (rest
				     (max 0 (- min-size (first-max-size)))
				     (min (sub1 failed-size) (- max-size (first-min-size)))
				     (lambda (rest rest-size next-rest)
				       (if (rest-size . >= . failed-size)
					   (rloop next-rest result-k max-size fail-k failed-size)
					   (let floop ([first first]
						       [result-k result-k]
						       [max-size max-size]
						       [fail-k fail-k] 
						       [first-fail-k (lambda ()
								       (rloop next-rest result-k max-size fail-k rest-size))])
					     (first (max 0 (- min-size rest-size))
						    (- max-size rest-size)
						    (lambda (first first-size next-first)
						      (result-k 
						       (combiner first rest)
						       (+ first-size rest-size)
						       (lambda (ns xs result-k fail-k)
							 (floop next-first result-k xs fail-k
								(lambda ()
								  (rloop next-rest result-k xs fail-k failed-size))))))
						    first-fail-k))))
				     fail-k))))))
                       this-min-size
                       this-max-size)))]
                 [kleene+ (lambda (p)
			    (let ([n (gensym)])
			      (hash-table-put! nt-map n (cons (lambda () 1)
							      (lambda () +inf.0)))
			      (set! to-do (cons (make-nt 
						 n 
						 (list (make-rhs (cons p '()))
						       (make-rhs (cons p n))))
						to-do))
			      n))])
          (let to-do-loop ([nts (reverse to-do)])
            (set! to-do null)
            (for-each (lambda (nt)
                        (hash-table-put!
                         gens
                         (nt-name nt)
                         (let* ([gens+sizes
                                 (map (lambda (rhs)
                                        (let-values ([(gen get-min-size get-max-size)
                                                      (make-gen/get-size 
                                                       (rhs-pattern rhs))])
					  (cons gen (cons get-min-size get-max-size))))
                                      (nt-rhs nt))]
                                [get-min-size
                                 (let ([get-min-sizes (map cadr gens+sizes)])
                                   (let ([v #f])
                                     (lambda ()
                                       (unless v
                                         (set! v (add1
						  (apply min (map (lambda (gs) (gs))
								  get-min-sizes)))))
				       v)))]
                                [get-max-size
                                 (let ([get-max-sizes (map cddr gens+sizes)])
                                   (let ([v #f])
                                     (lambda ()
                                       (unless v
                                         (set! v (add1
						  (apply max (map (lambda (gs) (gs))
								  get-max-sizes)))))
                                       v)))])
                           (hash-table-put! nt-map (nt-name nt)
                                            (cons get-min-size get-max-size))
                           (cache-small
                            (lambda (min-size max-size result-k fail-k)
			      (if (min-size . > . (get-max-size))
                                  (fail-k)
                                  (let loop ([l (map car gens+sizes)][result-k result-k][max-size max-size][fail-k fail-k])
				    (if (max-size . < . (get-min-size))
					(fail-k)
					(if (null? l)
					    (fail-k)
					    (let iloop ([alt-next (car l)]
							[result-k result-k]
							[max-size max-size]
							[fail-k fail-k])
					      (alt-next
					       (max 0 (sub1 min-size))
					       (sub1 max-size)
					       (lambda (alt a-size alt-next)
						 (result-k
						  alt
						  (add1 a-size)
						  (lambda (ns xs result-k fail-k)
						    (iloop alt-next result-k xs fail-k))))
					       (lambda ()
						 (loop (cdr l) result-k max-size fail-k)))))))))))))
                      nts)
            (unless (null? to-do)
              (to-do-loop to-do))))
	gens)))
  
  (define (for-each-generated/size proc gens min-size max-size nonterm)
    (let ([gen (hash-table-get gens nonterm)])
      (let loop ([gen gen])
        (gen
         min-size
         max-size
         (lambda (val z1 gen-next)
           (proc val z1)
           (loop gen-next))
         void))))
  
  (define (for-each-generated proc gens nonterm)
    (let loop ([i 0])
      (for-each-generated/size proc gens i i nonterm)
      (loop (add1 i)))))
