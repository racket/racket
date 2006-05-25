(module mach-o mzscheme

  (provide add-plt-segment
	   get/set-dylib-path)
  
  (define (read-ulong p)
    (integer-bytes->integer (read-bytes 4 p) #f))

  (define (write-ulong v out)
    (display (integer->integer-bytes v 4 #f) out))
  
  (define (check-same a b)
    (unless (= a b)
      (error 'check-same "not: ~e ~e" a b)))

  (define (round-up-page v)
    (bitwise-and #xFFFFF000 (+ v #xFFF)))

  (define (mult-of-4 n)
    (let ([m (modulo n 4)])
      (if (zero? m)
	  n
	  (+ n (- 4 m)))))

  (define move-link-edit? #t)

  (define (add-plt-segment file segdata)
    (let-values ([(p out) (open-input-output-file file 'update)])
      (dynamic-wind
	  void
	  (lambda ()
	    (file-stream-buffer-mode out 'none)
	    (check-same #xFeedFace (read-ulong p))
	    (read-ulong p)
	    (read-ulong p)
	    (check-same #x2 (read-ulong p))
	    (let* ([cnt (read-ulong p)]
		   [cmdssz (read-ulong p)]
		   [min-used (round-up-page cmdssz)]
		   [sym-tab-pos 0]
		   [dysym-pos 0]
		   [hints-pos 0]
		   [link-edit-pos 0]
		   [link-edit-addr 0]
		   [link-edit-offset 0]
		   [link-edit-len 0])
	      ;; (printf "~a cmds, length 0x~x\n" cnt cmdssz)
	      (read-ulong p)
	      (let loop ([cnt cnt])
		(unless (zero? cnt)
		  (let ([pos (file-position p)]
			[cmd (read-ulong p)]
			[sz (read-ulong p)])
		    ;; (printf "~x (~a)\n" cmd sz)
		    (case cmd
		      [(1)
		       ;; Segment
		       (let ([segname (read-bytes 16 p)]
			     [vmaddr (read-ulong p)]
			     [vmlen (read-ulong p)]
			     [offset (read-ulong p)]
			     [len (read-ulong p)])
			 ;; (printf "~s\n" segname)
			 (when (equal? segname #"__LINKEDIT\0\0\0\0\0\0")
			   (set! link-edit-pos pos)
			   (set! link-edit-addr vmaddr)
			   (set! link-edit-offset offset)
			   (set! link-edit-len len))
			 ;; (printf "  0x~x 0x~x -> 0x~x 0x~x\n" offset len vmaddr vmlen)
			 (read-ulong p)
			 (read-ulong p)
			 (let ([nsects (read-ulong p)])
			   (read-ulong p)
			   (let loop ([nsects nsects])
			     (unless (zero? nsects)
			       (let ([sect (read-bytes 16 p)]
				     [seg (read-bytes 16 p)]
				     [vmaddr (read-ulong p)]
				     [vmsz (read-ulong p)]
				     [offset (read-ulong p)]
				     [align (read-ulong p)]
				     [reloff (read-ulong p)]
				     [nreloc (read-ulong p)]
				     [flags (read-ulong p)])
				 (when ((+ offset vmsz) . > . (+ cmdssz 28))
				   (when (offset . < . min-used)
				     ;; (printf "   new min!\n")
				     (set! min-used offset)))
				 ;; (printf "    ~s,~s 0x~x 0x~x\n" seg sect offset vmsz)
				 (read-ulong p) (read-ulong p))
			       (loop (sub1 nsects))))))]
		      [(2)
		       ;; Symbol table
		       (set! sym-tab-pos pos)]
		      [(#xB)
		       ;; Dysym
		       (set! dysym-pos pos)]
		      [(#x16)
		       ;; 2-level hints table
		       (set! hints-pos pos)]
		      [else
		       (void)])
		    (file-position p (+ pos sz))
		    (loop (sub1 cnt)))))
	      ;; (printf "Start offset: 0x~x\n" min-used)
	      (let ([end-cmd (+ cmdssz 28)]
		    [new-cmd-sz 56]
		    [outlen (round-up-page (bytes-length segdata))]
		    [out-offset (if move-link-edit?
				    link-edit-offset
				    (+ link-edit-offset (round-up-page link-edit-len)))]
		    [out-addr (+ link-edit-addr (round-up-page link-edit-len))])
		(unless ((+ end-cmd new-cmd-sz) . < . min-used)
		  (error 'check-header "no room for a new section load command"))
		;; Shift commands after link-edit segment:
		(file-position p link-edit-pos)
		(let ([s (read-bytes (- end-cmd link-edit-pos) p)])
		  (file-position out (+ link-edit-pos 56))
		  (display s out))
		(file-position out 16)
		;; The segment:
		(write-ulong (+ cnt 1) out)
		(write-ulong (+ cmdssz new-cmd-sz) out)
		(file-position out link-edit-pos)
		(write-ulong 1 out) ; LC_SEGMENT
		(write-ulong new-cmd-sz out)
		(display #"__PLTSCHEME\0\0\0\0\0" out)
		(write-ulong out-addr out)
		(write-ulong outlen out)
		(write-ulong out-offset out)
		(write-ulong outlen out)
		(write-ulong 0 out)
		(write-ulong 0 out)
		(write-ulong 0 out)
		(write-ulong 4 out) ; 4 means SG_NORELOC
		;; Shift command positions
		(when (sym-tab-pos . > . link-edit-pos)
		  (set! sym-tab-pos (+ sym-tab-pos 56)))
		(when (dysym-pos . > . link-edit-pos)
		  (set! dysym-pos (+ dysym-pos 56)))
		(when (hints-pos . > . link-edit-pos)
		  (set! hints-pos (+ hints-pos 56)))
		(set! link-edit-pos (+ link-edit-pos 56))
		(when move-link-edit?
		  ;; Update link-edit segment entry:
		  (file-position out (+ link-edit-pos 32))
		  ;; (printf "Update to ~a\n" (+ out-offset outlen))
		  (write-ulong (+ out-offset outlen) out)
		  ;; Read link-edit segment:
		  (file-position p link-edit-offset)
		  (let ([link-edit (read-bytes link-edit-len p)])
		    ;; Write link-edit data in new location:
		    (file-position out (+ link-edit-offset outlen))
		    (display link-edit out))
		  ;; Shift symbol-table pointer:
		  (file-position p (+ sym-tab-pos 8))
		  (let ([symtab-offset (read-ulong p)]
			[_ (read-ulong p)]
			[symstr-offset (read-ulong p)])
		    (file-position out (+ sym-tab-pos 8))
		    (write-ulong (+ symtab-offset outlen) out)
		    (file-position out (+ sym-tab-pos 16))
		    (write-ulong (+ symstr-offset outlen) out))
		  ;; Shift dysym pointers:
		  (for-each (lambda (delta)
			      (file-position p (+ dysym-pos delta))
			      (let ([offset (read-ulong p)])
				(unless (zero? offset)
				  (file-position out (+ dysym-pos delta))
				  (write-ulong (+ offset outlen) out))))
			    '(32 40 48 56 64 72))
		  ;; Shift hints pointer:
		  (file-position p (+ hints-pos 8))
		  (let ([hints-offset (read-ulong p)])
		    (file-position out (+ hints-pos 8))
		    (write-ulong (+ hints-offset outlen) out)))
		;; Write segdata to former link-data offset:
		(file-position out out-offset)
		(display segdata out)
		(display (make-bytes (- outlen (bytes-length segdata)) 0) out)
		;; Result is offset where data was written:
		out-offset)))
	  (lambda ()
	    (close-input-port p)
	    (close-output-port out)))))

  (define (fix-offset p pos out d base delta)
    (when (and out (not (zero? delta)))
      (file-position p (+ pos d))
      (let ([offset (read-ulong p)])
	(when (offset . > . base)
	  (file-position out (+ pos d))
	  (write-ulong (+ offset delta) out)
	  (flush-output out)))))
    
  (define (get/set-dylib-path file rx new-path)
    (let-values ([(p out) (if new-path
			      (open-input-output-file file 'update)
			      (values (open-input-file file)
				      #f))])
      (dynamic-wind
	  void
	  (lambda ()
	    (check-same #xFeedFace (read-ulong p))
	    (read-ulong p)
	    (read-ulong p)
	    (check-same #x2 (read-ulong p))
	    (let* ([cnt (read-ulong p)]
		   [cmdssz (read-ulong p)])
	      (read-ulong p)
	      (let loop ([cnt cnt][base 0][delta 0][result #f])
		(if (zero? cnt)
		    result
		    (let ([pos (file-position p)]
			  [cmd (read-ulong p)]
			  [sz (read-ulong p)])
		      (case cmd
			[(#xC)
			 ;; LC_LOAD_DYLIB
			 (let ([offset (read-ulong p)])
			   (file-position p (+ pos offset))
			   (let* ([namelen (- sz offset)]
				  [segname (read-bytes namelen p)]
				  [segname (car (regexp-match #rx#"^[^\0]*" segname))])
			     (if (regexp-match rx segname)
				 (let* ([newnamelen (and out
							 (mult-of-4 (+ 1 (bytes-length new-path))))]
					[delta (if out
						   (-  newnamelen namelen)
						   0)])
				   (when out
				     (unless (zero? delta)
				       ;; We assume that there's enough header room to
				       ;; extend this load command, because the binary
				       ;; was linked with -headerpad_max_install_names
				       (file-position out (+ pos 4))
				       (write-ulong (+ sz delta) out)
				       (flush-output out)
				       ;; Shift rest of load commands by delta
				       (let ([end (+ cmdssz 56)])
					 (file-position p (+ pos sz))
					 (let ([s (read-bytes (- end (+ pos sz)) p)])
					   (file-position out (+ pos sz delta))
					   (write-bytes s out)
					   (when (negative? delta)
					     ;; zero-out now-unneeded space:
					     (write-bytes (make-bytes (- delta) 0) out))
					   (flush-output out))
					 ;; Change load-commands size in header:
					 (file-position out 20)
					 (write-ulong (+ cmdssz delta) out)
					 (flush-output out)))
				     (file-position out (+ pos offset))
				     (write-bytes new-path out)
				     (write-bytes (make-bytes (- newnamelen (bytes-length new-path)) 0) out)
				     (flush-output out))
				   (file-position p (+ pos sz delta))
				   (loop (sub1 cnt) pos delta segname))
				 (begin
				   (file-position p (+ pos sz))
				   (loop (sub1 cnt) base delta result)))))]
			[else
			 (file-position p (+ pos sz))
			 (loop (sub1 cnt) base delta result)]))))))
	  (lambda ()
	    (close-input-port p)
	    (when out
	      (close-output-port out)))))))
