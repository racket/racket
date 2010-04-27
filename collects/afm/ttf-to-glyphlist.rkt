
(module ttf-to-glyphlist mzscheme

  ;; http://developer.apple.com/fonts/TTRefMan/RM06/Chap6.html
  ;; http://www.microsoft.com/typography/otspec/otff.htm
  
  (require mzlib/etc
	   mzlib/list
	   mzlib/cmdline)
  
  (define (read-fixed p)
    (integer-bytes->integer (read-bytes 4 p) #f #t))
  (define (read-short p)
    (integer-bytes->integer (read-bytes 2 p) #t #t))
  (define (read-ushort p)
    (integer-bytes->integer (read-bytes 2 p) #f #t))
  (define (read-ulong p)
    (integer-bytes->integer (read-bytes 4 p) #f #t))
  
  (define (parse-ttf p)
    (begin-with-definitions

     (let ([v (read-fixed p)])
       (unless (or (= v #x10000)
		   (= v (integer-bytes->integer #"true" #f #t)))
	 (error "Doesn't start with snft version #x10000 or 'true'; got #x~x" v)))
     
     (define num-tables (read-short p))
     (read-short p)
     (read-short p)
     (read-short p)
     
     (define-struct table (tag checksum offset length))
     
     (define (read-table)
       (make-table (read-bytes 4 p)
		   (read-ulong p)
		   (read-ulong p)
		   (read-ulong p)))

     (define tables
       (build-list num-tables (lambda (_) (read-table))))

     (define cmaps
       (filter (lambda (t) (bytes=? #"cmap" (table-tag t)))
	       tables))

     (define posts
       (filter (lambda (t) (bytes=? #"post" (table-tag t)))
	       tables))

     (define-struct encoding (platform encoding offset))
     (define-struct segment (start end delta offset) #f)
     (print-struct #t)

     (define (read-cmap t)
       (file-position p (table-offset t))
       (unless (= (read-ushort p) 0)
	 (error "cmap table is not version 0"))
       (let ([count (read-ushort p)])
	 (let ([encodings (build-list
			   count
			   (lambda (_)
			     (make-encoding (read-ushort p)
					    (read-ushort p)
					    (read-ulong p))))])
	   (for-each (lambda (e)
		       (file-position p (+ (table-offset t) (encoding-offset e)))
		       ;; (printf "~a ~a\n" (encoding-platform e) (encoding-encoding e))
		       (cond
			[(or (and (= (encoding-platform e) 0)
				  (= (encoding-encoding e) 3))
			     (and (= (encoding-platform e) 3)
				  (= (encoding-encoding e) 1)))
			 (let ([format (read-ushort p)])
			   (unless (= format 4)
			     (error "mapping sub-table is not format 4"))
			   (read-ushort p) ; length in bytes
			   (read-ushort p) ; version
			   (let ([count (/ (read-ushort p) 2)])
			     (read-ushort p) ; searchrange
			     (read-ushort p) ; entry selector
			     (read-ushort p) ; rangeshift

			     (let ([ends (build-list count
						     (lambda (_) (read-ushort p)))]
				   [_ (read-ushort p)]
				   [starts (build-list count
						       (lambda (_) (read-ushort p)))]
				   [deltas (build-list count
						       (lambda (_) (read-short p)))]
				   [offsets (build-list count
							(lambda (_) (read-ushort p)))])
			       (let ([segments (map make-segment starts ends deltas offsets)])
				 (let loop ([segments segments][prev-end 0])
				   (unless (null? segments)
				     (let ([s (car segments)])
				       ;; (printf "~s\n" s)
				       (unless (zero? (segment-offset s))
					 (file-position p
							(+ (table-offset t) (encoding-offset e)
							   (segment-offset s))))
				       (let loop ([pos (max (add1 prev-end) (segment-start s))])
					 (unless (pos . > . (segment-end s))
					   (let ([char pos])
					     (map-unicode char
							  (modulo
							   (+ (if (zero? (segment-offset s))
								  char
								  (read-ushort p)) 
							      (segment-delta s))
							   65536)))
					   (loop (add1 pos))))
				       (loop (cdr segments) (segment-end s)))))))))]))
		     encodings)
	   (void))))

     (define name-map (make-hash-table 'equal))



     (define (read-name pos names)
       (or (vector-ref names pos)
	   (begin
	     (when (positive? pos)
	       (read-name (sub1 pos) names))
	     (vector-set! names pos (let ([n (read-byte p)])
				      (read-bytes n p)))
	     (vector-ref names pos))))

     (define (read-post t)
       (file-position p (table-offset t))
       (if (= (read-fixed p) #x20000)
	   (begin
	     (read-bytes 28 p) ; header
	     (let ([count (read-ushort p)])
	       (let ([idxs (build-list count
				       (lambda (_) (read-ushort p)))]
		     [names (make-vector count #f)])
		 (let loop ([idxs idxs][glyph 0])
		   (unless (null? idxs)
		     (let ([idx (car idxs)])
		       (when (idx . > . 257)
			 (let ([name (read-name (- idx 258) names)])
			   (hash-table-put! name-map glyph name))))
		     (loop (cdr idxs) (add1 glyph)))))))
	   (error "post table is not version 2")))

     (for-each read-post posts)

     (define rename-table (make-hash-table 'equal))

     (define (map-unicode u g)
       (let ([name (hash-table-get name-map g (lambda () #f))])
	 (when name
	   (printf "~a;~x\n" name u)
	   (hash-table-put! rename-table name u))))

     (for-each read-cmap cmaps)))

  (define filename
    (command-line
     "ttf-to-glyphlist"
     (current-command-line-arguments)
     [args (ttf-file)
	   ttf-file]))

  (let ([p (open-input-file filename)])
    (dynamic-wind
	void
	(lambda () (parse-ttf p))
	(lambda () (close-input-port p)))))

		


     
