
(module winicon mzscheme
  (require (lib "list.ss"))
  (provide install-icon
	   extract-icons
	   parse-icon
	   build-icon)

  (define (byte->integer p)
    (read-byte p))
  (define (word->integer p)
    (integer-bytes->integer (read-bytes 2 p) #f #f))
  (define (dword->integer p)
    (integer-bytes->integer (read-bytes 4 p) #f #f))
  
  ;; The 0 added in the alpha position apparently means "ignore the alpha
  ;;  and use the mask, instead"
  (define (3/2word->integer p)
    (integer-bytes->integer (bytes-append (read-bytes 3 p) #"\0") #f #f))

  (define (integer->word i p)
    (display (integer->integer-bytes i 2 #f #f) p))
  (define (integer->dword i p)
    (display (integer->integer-bytes i 4 #f #f) p))

  (define (flag v)
    (positive? (bitwise-and #x80000000 v)))
  (define (value v)
    (bitwise-and #x7FFFFFFF v))

  (define (skip-to-image-headers-after-signature p)
    ;; p is expected to be a file port
    (file-position p 60)
    (let ([pos (word->integer p)])
      ;; pos points to IMAGE_NT_HEADERS
      (file-position p pos)
      (unless (= #x4550 (dword->integer p))
	      (error "bad signature"))
      pos))

  (define (get-image-base p)
    (let ([pos (skip-to-image-headers-after-signature p)])
      (file-position p (+ 4
			  20
			  28))
      (dword->integer p)))

  (define (find-section p find-name)
    (let ([pos (skip-to-image-headers-after-signature p)])
      (word->integer p) ; skip machine
      (let ([num-sections (word->integer p)]
	    [_ (begin (dword->integer p)
		      (dword->integer p)
		      (dword->integer p))]
	    [size (word->integer p)])
	(let ([pos (+ pos
		      4       ; Signature : DWORD
		      20      ; FileHeader: IMAGE_FILE_HEADER
		      size)]) ; "optional" header
	  (let sloop ([section 0][section-pos pos])
	    (if (= section num-sections)
		(error 'find-section "can't find section: ~e" find-name)
		(begin
		  (file-position p section-pos)
		  ;; p points to an IMAGE_SECTION_HEADER
		  (let ([name (read-bytes 8 p)])
		    (if (bytes=? find-name name)
			(let ([_ (dword->integer p)]) ; skip
			  (values (dword->integer p)  ; virtual address
				  (dword->integer p)  ; length
				  (dword->integer p))); file pos
			(sloop (add1 section) (+ section-pos 40)))))))))))
  
  (define (find-rsrc-start p re:rsrc)
    (let-values ([(rsrc-virtual-addr rsrc-len rsrc-pos)
		  (find-section p #".rsrc\0\0\0")])
      (let loop ([dir-pos 0][path ""])
	(file-position p (+ rsrc-pos dir-pos 12))
	(let ([num-named (word->integer p)]
	      [num-ided (word->integer p)])
	  (let iloop ([i 0])
	    (if (= i (+ num-ided num-named))
		#f
		(let ([name-delta (dword->integer p)]
		      [data-delta (dword->integer p)]
		      [next (file-position p)])
		  (or (let ([name (if (flag name-delta)
				      (begin
					(file-position p (+ rsrc-pos (value name-delta)))
					(let* ([len (word->integer p)])
					  ;; len is in unicode chars...
					  (let ([unistr (read-bytes (* 2 len) p)])
					    ;; Assume it fits into ASCII...
					    (regexp-replace* "\0" 
							     (bytes->string/latin-1 unistr)
							     ""))))
				      (value name-delta))])
			;;(printf "Name: ~a~a = ~a~n" path name (+ rsrc-pos (value data-delta)))
			(let ([full-name (format "~a~a" path name)])
			  (if (flag data-delta)
			      (loop (value data-delta) (string-append full-name "."))
			      ;; Found the icon?
			      (and (regexp-match re:rsrc full-name)
				   ;; Yes, so read IMAGE_RESOURCE_DATA_ENTRY
				   (begin
				     (file-position p (+ rsrc-pos (value data-delta)))
				     (cons
				      (+ (dword->integer p)           ; offset (an RVA)
					 (- rsrc-pos
					    rsrc-virtual-addr))
				      (dword->integer p)))))))        ; size
		      (begin
			(file-position p next)
			(iloop (add1 i)))))))))))

  ;; >>> Probably doesn't work <<<
  (define (find-import-names p)
    (let-values ([(seg-virtual-addr seg-len seg-pos)
		  (find-section p #".idata\0\0")])
      (let loop ([pos seg-pos])
	;; pos points to an IMAGE_IMPORT_DESCRIPTOR;
	;; skip first 4 fields
	(file-position p pos)
	(if (zero? (dword->integer p)) ; 0 is terminator
	    null
	    (begin
	      (dword->integer p)
	      (dword->integer p)
	      ;; next field is name
	      (let ([name-pos (+ (dword->integer p) ; RVA to nul-terminated name
				 (- seg-pos seg-virtual-addr))])
		(file-position p name-pos)
		(let ([name (regexp-match "^[^\0]*" p)])
		  (cons (cons (car name) name-pos)
			(loop (+ pos 20))))))))))

  ;; >>> Doesn't work <<<
  (define (find-delay-loads p)
    (let-values ([(seg-virtual-addr seg-len seg-pos)
		  (find-section p #".text\0\0")])
    (let ([pos (skip-to-image-headers-after-signature p)]
	  [image-base (get-image-base p)])
      (let ([pos (+ pos
		    4       ; Signature : DWORD
		    20      ; FileHeader: IMAGE_FILE_HEADER
		    96      ; IMAGE_OPTIONAL_HEADER up to directory
		    104)]   ; skip 13 directory entries
	    [vdelta image-base])
	(file-position p pos)
	(let loop ([delay-pos (dword->integer p)])
	  (printf "~a ~a~n" delay-pos vdelta)
	  (file-position p (+ delay-pos vdelta))
	  (dword->integer p) ; skip attributes
	  (let ([name-pos (dword->integer p)])
	    (printf "~a ~a~n" name-pos vdelta)
	    (file-position p (+ name-pos vdelta))
	    (let ([name (regexp-match "^[^\0]*" p)])
	      (printf "~a~n" name))))))))

  (define-struct icon (desc data))
  ;; desc is (list width height colors 0 planes bitcount)
  ;; data is (cons pos string)
  
  (define (num-colors l)
    (let ([n (caddr l)])
      (if (zero? n)
          (arithmetic-shift 1 (list-ref l 5))
          n)))
  
  (define (install-icon exe-file ico-file . extra-icons)
    (let ([ico-icons (append (if ico-file 
                                 (extract-icons ico-file)
                                 null)
                             extra-icons)]
	  [exe-icons (extract-icons exe-file)])
      (let ([p (open-output-file exe-file 'update)])
	(dynamic-wind
	 void
	 (lambda ()
	   (for-each (lambda (exe-icon)
                       (let ([best-ico-icon
                              ;; Find exact match?
                              (ormap (lambda (ico-icon)
                                       (let ([le (icon-desc exe-icon)]
                                             [li (icon-desc ico-icon)])
                                         (and (= (car li) (car le))
                                              (= (cadr li) (cadr le))
                                              (= (num-colors li) (num-colors le))
                                              (= (bytes-length (cdr (icon-data exe-icon)))
                                                 (bytes-length (cdr (icon-data ico-icon))))
                                              ico-icon)))
                                     ico-icons)])
                         (let ([ico-icon (or best-ico-icon
                                             ;; Look for a conversion, if we
                                             ;; need a 16x16, 32x32, or 48x48
                                             ;; icon
                                             (and
                                              (= (car (icon-desc exe-icon))
                                                 (cadr (icon-desc exe-icon)))
                                              (memq (car (icon-desc exe-icon))
                                                    '(16 32 48))
                                              (let ([biggest-colorest #f])
                                                (for-each 
						 (lambda (ico-icon)
						   (let ([w (car (icon-desc ico-icon))]
							 [exew (car (icon-desc exe-icon))])
						     (when (and
							    (= w
							       (cadr (icon-desc ico-icon)))
							    (memq w '(16 32 48))
							    (or
							     (not biggest-colorest)
							     (and (= w exew)
								  (not (= exew (car (icon-desc biggest-colorest)))))
							     (and (= w exew)
								  (> (num-colors (icon-desc ico-icon))
								     (num-colors (icon-desc biggest-colorest))))
							     (and (not (= exew (car (icon-desc biggest-colorest))))
								  (or (> w (car (icon-desc biggest-colorest)))
								      (> (num-colors (icon-desc ico-icon))
									 (num-colors (icon-desc biggest-colorest)))))))
							   (set! biggest-colorest ico-icon))))
						 ico-icons)
                                                (and
                                                 biggest-colorest
                                                 ;; Convert...
                                                 (let* ([src-size (car (icon-desc biggest-colorest))]
							[dest-size (car (icon-desc exe-icon))]
							[src (parse-icon biggest-colorest)]
							[image (list-ref src 3)]
							[mask (list-ref src 4)]
							[has-alpha? (<= 256 (num-colors (icon-desc biggest-colorest)))])
						   (if (= src-size dest-size)
						       (build-icon exe-icon
								   (if has-alpha?
								       image
								       (mask->alpha image mask))
								   mask)
						       (let ([cvt
							      (cond
							       [(and (= src-size 32) (= dest-size 16))
								(lambda (i) (48->16 (32->48 i)))]
							       [(and (= src-size 32) (= dest-size 48)) 
								32->48]
							       [(and (= src-size 48) (= dest-size 16)) 
								48->16]
							       [(and (= src-size 48) (= dest-size 32))
								48->32]
							       [(and (= src-size 16) (= dest-size 32)) 
								16->32]
							      [(and (= src-size 16) (= dest-size 48)) 
							       (lambda (i) (32->48 (16->32 i)))])])
							 (let ([mask (cvt mask)])
							   (build-icon exe-icon 
								       (if has-alpha?
									   image
									   (mask->alpha (cvt image) mask))
								       mask)))))))))])
			   (unless ico-icon (printf "no! ~a~n" (icon-desc exe-icon)))
                           (when ico-icon
                             (file-position p (car (icon-data exe-icon)))
                             (display (cdr (icon-data ico-icon)) p)))))
		     exe-icons))
	 (lambda () (close-output-port p))))))

  ;; ------------------------------
  ;;  Image parsing
  ;; ------------------------------

  (define (get-icons file res?)
    (let ([p (if (input-port? file)
		 file
		 (open-input-file file))])
      (dynamic-wind
       void
       (lambda ()
	 (unless (= 0 (word->integer p))
		 (error 'get-icons "~a doesn't start with 0" file))
	 (unless (= 1 (word->integer p))
		 (error "type isn't 1"))
	 (let ([cnt (word->integer p)])
	   (let ([icons (let loop ([i 0])
			  (if (= i cnt)
			      null
			      (cons
			       (make-icon
				(list (byte->integer p)   ; w
				      (byte->integer p)   ; h
				      (byte->integer p)   ; colors
				      (byte->integer p)   ; 0
				      (word->integer p)   ; planes
				      (word->integer p))  ; bitcount
				(list (dword->integer p)  ; bytes
				      ((if res?           ; where or icon id
					   word->integer 
					   dword->integer)
				       p)))
			       (loop (add1 i)))))])
	     ;; (printf "~a~n" icons)
	     (for-each (lambda (icon)
			 (set-icon-data!
			  icon
			  (let ([size (car (icon-data icon))]
				[where (cadr (icon-data icon))])
			    (let ([icon-pos (if res?
						;; last number is icon id:
						(car (find-rsrc-start p (format "^3[.]~a[.]" where)))
						;; last number is file position:
						where)])
			      (file-position p icon-pos)
			      (cons icon-pos
				    (read-bytes size p)))))
                         ;; If colors, planes, and bitcount are all 0,
                         ;;  get the info from the DIB data
                         (let ([desc (icon-desc icon)])
                           (when (and (zero? (list-ref desc 2))
                                      (zero? (list-ref desc 4))
                                      (zero? (list-ref desc 5)))
                             (let ([bi (bitmapinfo icon)])
                               (set-car! (list-tail desc 4)
                                         (list-ref bi 3))
                               (set-car! (list-tail desc 5)
                                         (list-ref bi 4))))))
		       icons)
	     icons)))
       (lambda ()
	 (when (path-string? file)
	       (close-input-port p))))))

  (define (bitmapinfo icon)
    (let ([p (open-input-bytes (cdr (icon-data icon)))])
      (list (dword->integer p)    ; size == 40 in practice
	    (dword->integer p)    ; width
	    (dword->integer p)    ; height
	    (word->integer p)     ; planes
	    (word->integer p)     ; bitcount
	    (dword->integer p)    ; compression == 0
	    (dword->integer p)    ; size image
	    (dword->integer p)    ; x pixels per meter == 0
	    (dword->integer p)    ; y pixels per meter == 0
	    (dword->integer p)    ; used == 0
	    (dword->integer p)))) ; important == 0

  ;; Assumes that bits-per-pixel is 1, 2, 4, 8, 24, or 32.
  ;; Also assumes that (bits-per-pixel * width) is a multiple of 8.
  (define (parse-dib icon)
    (let* ([bi (bitmapinfo icon)]
	   [header-size (list-ref bi 0)]
	   [num-colors (caddr (icon-desc icon))]
	   [w (list-ref bi 1)]
	   [h (/ (list-ref bi 2) 2)]
	   [bits-per-pixel (list-ref bi 4)])
      (let ([p (open-input-bytes (cdr (icon-data icon)))])
	;; Skip header
	(read-bytes header-size p)
	(let* ([read-n
		(lambda (n read-one combine)
		  (let loop ([i n][r null])
		    (if (= i 0)
			(reverse! r)
			(loop (sub1 i)
			      (combine (read-one p) r)))))]
	       [read-lines 
		(lambda (w h read-one combine)
		  (if (zero? (modulo w 4))
		      (read-n (* w h) read-one combine)
		      (let loop ([h h])
			(if (zero? h)
			    null
			    (append (read-n w read-one combine)
				    (begin
				      ;; pad line to dword:
				      (read-n (- 4 (modulo w 4)) byte->integer cons)
				      ;; read next line:
				      (loop (sub1 h))))))))]
	       [split-bits (lambda (b)
			     (list
			      (bitwise-and b 1)
			      (arithmetic-shift (bitwise-and b 2) -1)
			      (arithmetic-shift (bitwise-and b 4) -2)
			      (arithmetic-shift (bitwise-and b 8) -3)
			      (arithmetic-shift (bitwise-and b 16) -4)
			      (arithmetic-shift (bitwise-and b 32) -5)
			      (arithmetic-shift (bitwise-and b 64) -6)
			      (arithmetic-shift (bitwise-and b 128) -7)))])
	  (let ([main-image
		 (cond
                   [(= bits-per-pixel 32)
                    ;; RGB mode:
                    (read-n (* w h) dword->integer cons)]
                   [(= bits-per-pixel 24)
                    ;; RGB mode:
                    (read-n (* w h) 3/2word->integer cons)]
                   [else
                    ;; Index mode:
                    (let ([color-table (list->vector
                                        (read-n (if (zero? num-colors)
                                                    (arithmetic-shift 1 bits-per-pixel)
                                                    num-colors)
                                                dword->integer cons))]
                          [image (read-lines (/ w (/ 8 bits-per-pixel))
                                             h
                                             (lambda (p)
                                               (let ([b (byte->integer p)])
                                                 (case bits-per-pixel
                                                   [(1) (split-bits b)]
                                                   [(2)
                                                    (list
                                                     (bitwise-and b 3)
                                                     (arithmetic-shift (bitwise-and b 12) -2)
                                                     (arithmetic-shift (bitwise-and b 48) -4)
                                                     (arithmetic-shift (bitwise-and b 192) -6))]
                                                   [(4)
                                                    (list
                                                     (bitwise-and b 15)
                                                     (arithmetic-shift (bitwise-and b 240) -4))]
                                                   [(8) (list b)])))
                                             append)])
                      (map (lambda (i) (vector-ref color-table i)) image))])])
	    (let ([mask (read-lines (/ w 8)
				    h
				    (lambda (p) (split-bits (byte->integer p)))
				    append)])
	      (unless (eof-object? (read-char p))
		 (error 'parse-dib "not extactly at end"))
	      (list main-image mask)))))))

  ;; rgb->indexed
  ;;  The color-reduction strategy isn't great, and because it
  ;;  depends on hash-table order, it's non-deterministic in
  ;;  principle. But the actual hash-table implementatin is
  ;;  deterministic, of course. Also, the re-ordering of the
  ;;  image via the hash tables tends to produce better
  ;;  (pseudo-random) representatives of the image for colors.
  (define (rgb->indexed image num-colors)
    (let ([image (map (lambda (i) (bitwise-and #xFFFFFF i)) image)] ; drop alphas, if any
	  [table (make-vector num-colors 0)]
	  [ht (make-hash-table 'equal)]
	  [map-ht (make-hash-table 'equal)]
	  [color-dist (lambda (a b)
			(sqrt (+ (expt (- (bitwise-and #xFF a)
					  (bitwise-and #xFF b))
				       2)
				 (expt (- (arithmetic-shift (bitwise-and #xFF00 a) -8)
					  (arithmetic-shift (bitwise-and #xFF00 b) -8))
				       2)
				 (expt (- (arithmetic-shift (bitwise-and #xFF0000 a) -16)
					  (arithmetic-shift (bitwise-and #xFF0000 b) -16))
				       2))))])
      (for-each (lambda (c)
		  (hash-table-put! 
		   ht 
		   c
		   (add1
		    (hash-table-get ht c (lambda () 0)))))
		image)
      (let ([kv-sorted (sort (hash-table-map ht cons)
                             (lambda (a b) (< (cdr a) (cdr b))))])
        (let ([n 0])
          (for-each (lambda (kv)
                      (let ([key (car kv)])
                        (let ([n (if (< n (sub1 num-colors))
                                     n
                                     ;; Find closest match:
                                     (let ([n 0])
                                       (let loop ([i 1])
                                         (unless (= i num-colors)
                                           (when (< (color-dist key (vector-ref table i))
                                                    (color-dist key (vector-ref table n)))
                                             (set! n i))
                                           (loop (add1 i))))
                                       n))])
                          (vector-set! table n key)
                          (hash-table-put! map-ht key n))
                        (when (< n (sub1 num-colors))
                          (set! n (add1 n)))))
                    kv-sorted)))
      (values (vector->list table)
              (map (lambda (c) (hash-table-get map-ht c)) image))))
    
  ;; Assumes that bits-per-pixel is 1, 2, 4, 8, or 32.
  ;; Also assumes that (bits-per-pixel * width) is a multiple of 8.
  (define (build-dib icon image mask)
    (let* ([bi (bitmapinfo icon)]
	   [header-size (list-ref bi 0)]
	   [num-colors (caddr (icon-desc icon))]
	   [w (list-ref bi 1)]
	   [h (/ (list-ref bi 2) 2)]
	   [bits-per-pixel (list-ref bi 4)])
      (let ([orig-p (open-input-bytes (cdr (icon-data icon)))]
	    [result-p (open-output-bytes)])
	;; Copy header:
	(display (read-bytes header-size orig-p) result-p)
	(let ([get-lines (lambda (image bits-per-pixel)
			   (map (lambda (line)
				  ;; pad line to dword boundary
				  (let ([line-bytes (/ (* w bits-per-pixel) 8)])
				    (if (zero? (modulo line-bytes 4))
					line
					(append line 
						(vector->list
						 (make-vector (* (- 4 (modulo line-bytes 4)) 
								 (/ 8 bits-per-pixel))
							      0))))))
				;; break out lines
				(let loop ([l image])
				  (if (null? l)
				      null
				      (cons (let loop ([l l][i 0])
					      (if (= i w)
						  null
						  (cons (car l) (loop (cdr l) (add1 i)))))
					    (loop (list-tail l w)))))))]
	      [bits->dwords (lambda (l bpp)
			      (let ([chunk-size (/ 32 bpp)]
				    [1byte (lambda (l)
					     (bitwise-ior
					      (arithmetic-shift (list-ref l 0) 7)
					      (arithmetic-shift (list-ref l 1) 6)
					      (arithmetic-shift (list-ref l 2) 5)
					      (arithmetic-shift (list-ref l 3) 4)
					      (arithmetic-shift (list-ref l 4) 3)
					      (arithmetic-shift (list-ref l 5) 2)
					      (arithmetic-shift (list-ref l 6) 1)
					      (arithmetic-shift (list-ref l 7) 0)))]
				    [2byte (lambda (l)
					     (bitwise-ior
					      (arithmetic-shift (list-ref l 0) 6)
					      (arithmetic-shift (list-ref l 1) 4)
					      (arithmetic-shift (list-ref l 2) 2)
					      (arithmetic-shift (list-ref l 3) 0)))]
				    [4byte (lambda (l)
					     (bitwise-ior
					      (arithmetic-shift (list-ref l 0) 4)
					      (arithmetic-shift (list-ref l 1) 0)))])
				(let loop ([l l])
				  (if (null? l)
				      null
				      (cons (case bpp
					      [(1) (bitwise-ior
						    (arithmetic-shift (1byte (list-tail l 0)) 0)
						    (arithmetic-shift (1byte (list-tail l 8)) 8)
						    (arithmetic-shift (1byte (list-tail l 16)) 16)
						    (arithmetic-shift (1byte (list-tail l 24)) 24))]
					      [(2) (bitwise-ior
						    (2byte l)
						    (arithmetic-shift (2byte (list-tail l 4)) 8)
						    (arithmetic-shift (2byte (list-tail l 8)) 16)
						    (arithmetic-shift (2byte (list-tail l 12)) 24))]
					      [(4) (bitwise-ior
						    (4byte l)
						    (arithmetic-shift (4byte (list-tail l 2)) 8)
						    (arithmetic-shift (4byte (list-tail l 4)) 16)
						    (arithmetic-shift (4byte (list-tail l 6)) 24))]
					      [(8) (bitwise-ior
						    (car l)
						    (arithmetic-shift (list-ref l 1) 8)
						    (arithmetic-shift (list-ref l 2) 16)
						    (arithmetic-shift (list-ref l 3) 24))])
					    (loop (list-tail l chunk-size)))))))])
	  (if (= bits-per-pixel 32)
	      (for-each (lambda (col) (integer->dword col result-p))
			image)
	      (let-values ([(colors indexed-image) (rgb->indexed image (arithmetic-shift 1 bits-per-pixel))])
  	        ;; color table
	        (for-each (lambda (col) (integer->dword col result-p))
			  colors)
		(let* ([lines (get-lines indexed-image bits-per-pixel)]
		       [dwords (apply append (map (lambda (l) (bits->dwords l bits-per-pixel)) 
                                                 lines))])
                  (for-each (lambda (col) (integer->dword col result-p))
			    dwords))))
	  (let* ([lines (get-lines mask 1)]
		 [dwords (apply append (map (lambda (l) (bits->dwords l 1)) lines))])
	    (for-each (lambda (col) (integer->dword col result-p))
		      dwords))
	  (let ([s (get-output-bytes result-p)])
	    (unless (= (bytes-length s) (bytes-length (cdr (icon-data icon))))
	      (error 'build-dib "bad result size ~a != ~a"
                     (bytes-length s) (bytes-length (cdr (icon-data icon)))))
	    s)))))

  (define (parse-icon icon)
    (let ([image (parse-dib icon)])
      (list (car (icon-desc icon))
	    (cadr (icon-desc icon))
	    (let ([cols (caddr (icon-desc icon))])
	      (if (zero? cols)
		  (expt 2 (list-ref (icon-desc icon) 5))
		  cols))
	    (car image)
	    (cadr image))))

  (define (build-icon base-icon image mask)
    (make-icon (icon-desc base-icon)
	       (cons (car (icon-desc base-icon))
		     (build-dib base-icon image mask))))

  (define (extract-icons file)
    (if (regexp-match #rx"[.]ico$" (if (path? file)
				       (path->string file)
				       file))
	(get-icons-in-ico file)
	(get-icons-in-exe file)))

  (define (get-icons-in-ico ico-file)
    (get-icons ico-file #f))

  (define (get-icons-in-exe exe-file)
    (let ([p (open-input-file exe-file)])
      (dynamic-wind
       void
       (lambda ()
	 (let ([pos+size (find-rsrc-start p "^14[.]")])
	   (file-position p (car pos+size))
	   (get-icons p #t)))
       (lambda () (close-input-port p)))))

  ;; The following is useful for bitmap->icon,
  ;;  but it uses MrEd, and this module is used by
  ;;  Setup PLT. Probably this code should just be
  ;;  moved somewhere else.
  #;
  (begin
    (require (lib "mred.ss" "mred")
             (lib "class.ss"))
    (define (bitmap%->icon bm)
      (let* ([w (send bm get-width)]
             [h (send bm get-height)]
             [argb (make-bytes (* w h 4))]
             [mdc (make-object bitmap-dc% bm)])
        (send mdc get-argb-pixels 0 0 w h argb)
        (send mdc set-bitmap #f)
        ;; Get mask (inverse alpha), if any:
        (let ([mask-argb (make-bytes (* w h 4) #o377)]
              [mbm (send bm get-loaded-mask)])
          (when mbm
            (send mdc set-bitmap mbm)
            (send mdc get-argb-pixels 0 0 w h mask-argb)
            (send mdc set-bitmap #f))
          (bitmap->icon w h argb mask-argb)))))

  (define (bitmap->icon w h argb mask-argb)
    (let ([o (open-output-bytes)])
      (integer->dword 40 o) ; size
      (integer->dword w o)  ; width
      (integer->dword (* 2 h) o)  ; height
      (integer->word 1 o)   ; planes
      (integer->word 32 o)  ; bitcount
      (integer->dword 0 o)  ; compression
      (integer->dword 0 o)  ; size image
      (integer->dword 0 o)  ; x pixels per meter
      (integer->dword 0 o)  ; y pixels per meter
      (integer->dword 0 o)  ; used
      (integer->dword 0 o)  ; important
      ;; Got ARGB, need BGRA
      (let* ([flip-pixels (lambda (s)
                            (let ([s (bytes-copy s)])
                              (let loop ([p 0])
                                (unless (= p (bytes-length s))
                                  (let ([a (bytes-ref s p)]
                                        [r (bytes-ref s (+ p 1))]
                                        [g (bytes-ref s (+ p 2))]
                                        [b (bytes-ref s (+ p 3))])
                                    (bytes-set! s p b)
                                    (bytes-set! s (+ p 1) g)
                                    (bytes-set! s (+ p 2) r)
                                    (bytes-set! s (+ p 3) a)
                                    (loop (+ p 4)))))
                              s))]
             [rgba (flip-pixels argb)]
             [mask-rgba (flip-pixels mask-argb)]
             [row-size (if (zero? (modulo w 32))
                           w
                           (+ w (- 32 (remainder w 32))))]
             [mask (make-bytes (* h row-size 1/8) 0)])
        (let loop ([i (* w h 4)])
          (unless (zero? i)
            (let ([mr (bytes-ref mask-rgba (- i 2))]
                  [mg (bytes-ref mask-rgba (- i 3))]
                  [mb (bytes-ref mask-rgba (- i 4))]
                  [a (- i 1)])
              (let ([alpha (- 255
                              (floor (/ (+ mr mg mb)
                                        3)))])
                (if (< alpha 10)
                    ;; white mask -> zero alpha; add white pixel to mask
                    (begin
                      (bytes-set! rgba a 0)
                      (let ([pos (+ (* (quotient (sub1 (/ i 4)) w) row-size)
                                    (remainder (sub1 (/ i 4)) w))])
                        (bytes-set! mask 
				    (quotient pos 8)
				    (bitwise-ior
				     (arithmetic-shift 1 (- 7 (remainder pos 8)))
				     (bytes-ref mask (quotient pos 8))))))
                    ;; non-white mask -> non-zero alpha
                    (bytes-set! rgba a alpha))))
            (loop (- i 4))))
        ;; Windows icons are upside-down:
        (let ([flip (lambda (str row-width)
                      (apply
                       bytes-append
                       (reverse
                        (let loop ([pos 0])
                          (if (= pos (bytes-length str))
                              null
                              (cons (subbytes str pos (+ pos row-width))
                                    (loop (+ pos row-width))))))))])                      
          (display (flip rgba (* w 4)) o)
          (display (flip mask (/ row-size 8)) o))
        (make-icon (list w h 0 0 1 32)
                   (cons 0 (get-output-bytes o))))))
  
  ;; ------------------------------
  ;;  Image conversion
  ;; ------------------------------
  
  (define (mask->alpha image mask)
    (map (lambda (i m)
           (if (zero? m)
               (bitwise-ior #xFF000000 i)
               m))
         image mask))
  
  (define (first-n n l)
    (let loop ([l l][i n])
      (if (zero? i)
          null
          (cons (car l) (loop (cdr l) (sub1 i))))))
  
  (define (16->32 l)
    (let loop ([l l])
      (if (null? l) 
          null
          (let ([l2 (let loop ([l (first-n 16 l)])
                      (if (null? l)
                          null
                          (list* (car l) (car l) (loop (cdr l)))))])
            (append l2 l2
                    (loop (list-tail l 16)))))))
  
  (define (32->48 l)
    (let loop ([l l][dup? #t])
      (if (null? l) 
          null
          (let ([l2 (let loop ([l (first-n 32 l)])
                      (if (null? l)
                          null
                          (list* (car l) (car l) (cadr l)
                                 (loop (cddr l)))))])
            (append l2
                    (if dup? l2 null)
                    (loop (list-tail l 32) (not dup?)))))))
  
  (define (48->16 l)
    (let loop ([l l])
      (if (null? l) 
          null
          (let ([l2 (let loop ([l (first-n 48 l)])
                      (if (null? l)
                          null
                          (cons (car l) (loop (cdddr l)))))])
            (append l2
                    (loop (list-tail l 144)))))))
  
  (define (48->32 l)
    (let loop ([l l][step 0])
      (if (null? l) 
          null
          (let ([l2 (let loop ([l (first-n 48 l)][step 0])
                      (if (null? l)
                          null
                          (if (= 1 (modulo step 3))
                              (loop (cdr l) 2)
                              (cons (car l) (loop (cdr l) (add1 step))))))])
            (append (if (= 1 (modulo step 3)) null l2)
                    (loop (list-tail l 48) (add1 step))))))))

#|

;; ----------------------------------------
;; Test code

(define icons (extract-icons "e:/matthew/plt/mred.exe"))

(define (show-icon w h col-count image mask)
  (let* ([f (make-object frame% (format "~a x ~a (~a) Image" w h col-count))]
         [bm (make-object bitmap% w h)]
         [mbm (make-object bitmap% w h)]
         [c (instantiate canvas% (f)
              [paint-callback (lambda (c dc)
                                (send dc draw-bitmap bm 0 0)
                                (send dc draw-bitmap mbm w 0))])])
    (let ([mdc (make-object bitmap-dc% bm)]
          [col (make-object color%)])
      (let loop ([l image][i 0][j 0])
        (unless (= j h)
          (let ([v (car l)])
            (send col set
                  (bitwise-and v #xFF)
                  (arithmetic-shift (bitwise-and v #xFF00) -8)
                  (arithmetic-shift (bitwise-and v #xFF0000) -16))
            (send mdc set-pixel i j col)
            (if (= (add1 i) w)
                (loop (cdr l) 0 (add1 j))
                (loop (cdr l) (add1 i) j)))))
      (send mdc set-bitmap mbm)
      (let loop ([l (if (col-count . > . 256) image mask)][i 0][j 0])
        (unless (= j h)
          (let ([v (if (col-count . > . 256)
                       (- 255 (arithmetic-shift (bitwise-and (car l) #xFF000000) -24))
                       (if (zero? (car l))
                           0
                           255))])
            (send col set v v v)
            (send mdc set-pixel i j col)
            (if (= (add1 i) w)
                (loop (cdr l) 0 (add1 j))
                (loop (cdr l) (add1 i) j)))))
      
      (send mdc set-bitmap #f))
    (send c min-client-width (* 2 w))
    (send c min-client-height h)
    (send c stretchable-width #f)
    (send c stretchable-height #f)
    
    (send f show #t)))

(define (find-icon icons w h colors)
  (ormap (lambda (i)
           (let ([p (parse-icon i)])
             (and (= w (car p))
                  (= h (cadr p))
                  (= colors (caddr p))
                  i)))
         icons))

(let ([orig (find-icon icons 48 48 (expt 2 32))]
      [target (find-icon icons 32 32 256)])
  (apply show-icon (parse-icon orig))
  (apply show-icon (parse-icon target))
  (apply show-icon 
         (parse-icon
          (let* ([p (parse-icon orig)]
                 [mask (48->32(list-ref p 4))]
                 [image (mask->alpha (48->32 (list-ref p 3)) mask)])
            (build-icon target image mask)))))

;; ----------------------------------------
;; End test code

|#
