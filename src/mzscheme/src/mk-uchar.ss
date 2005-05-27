
;; This script parses UnicodeData.txt (the standard Unicode database,
;; available from the web), and produces schuchar.inc, which is 
;; used by scheme_isalpha, etc., and thus `char-alphabetic?', etc.

;; Run as
;;   mzscheme -r mk-uchar.ss
;; in the script's directory, and have a copy of UnicodeData.txt
;; in the same directory. The file schuchar.inc will be
;; overwritten.

(require (lib "list.ss"))

(define mark-cats '("Mn" "Mc" "Me"))
(define letter-cats '("Lu" "Ll" "Lt" "Lm" "Lo"))
(define digit-cats '("Nd"))
(define space-cats '("Zl" "Zs" "Zp"))
(define punc-cats '("Pc" "Pd" "Ps" "Pe" "Pi" "Pf" "Po"))
(define sym-cats '("Sm" "Sc" "Sk" "So"))
(define graphic-cats (append mark-cats
			     letter-cats
			     digit-cats
			     punc-cats
			     sym-cats))

(define ups (cons (make-hash-table 'equal) (box 0)))
(define downs (cons (make-hash-table 'equal) (box 0)))
(define titles (cons (make-hash-table 'equal) (box 0)))

(define (indirect t v)
  (let ([r (hash-table-get (car t) v (lambda () #f))])
    (or r
	(let ([r (unbox (cdr t))])
	  (set-box! (cdr t) (add1 r))
	  (hash-table-put! (car t) v r)
	  (when (r . > . 63)
	    (error "too many indirects"))
	  r))))

(define (combine up down title . l)  
  ;; The scheme_is...() macros in scheme.h must match
  ;;  the bit layout produced here
  (bitwise-ior
   (arithmetic-shift (indirect ups up) 12)
   (arithmetic-shift (indirect downs down) 18)
   (arithmetic-shift (indirect titles title) 24)
   (let loop ([l l][v 0])
     (if (null? l)
	 v
	 (loop (cdr l) (bitwise-ior (arithmetic-shift v 1)
				    (if (car l)
					1
					0)))))))

(define hexes (map char->integer (string->list "0123456789abcdefABCDEF")))

;; In principle, adjust this number to tune the result, but
;;  the macros for accessing the table (in scheme.h) need to
;;  be updated accordingly.
;; In practice, it's unlikely that anything will ever work
;;  much better than 8. (At the time this was implemented,
;;  9 produced a table 10% smaller, but I left it at 8 
;;  because it feels more intuitively correct.)
(define low-bits 8)

(define low (sub1 (expt 2 low-bits)))
(define hi-count (expt 2 (- 21 low-bits)))
(define hi (arithmetic-shift (sub1 hi-count) low-bits))

(define top (make-vector hi-count #f))

(define range-bottom 0)
(define range-top -1)
(define range-v -1)
(define ranges null)

(define ccount 0)

(define (map1 c v)
  (set! ccount (add1 ccount))
  (if (= c (add1 range-top))
      (begin
	(unless (= v range-v)
	  (set! range-v -1))
	(set! range-top c))
      (begin
	;; Drop surrogate from range.
	;;  At the time of implemenation, the following
	;;  was never executed, because #D7FF wasn't mapped:
	(when (and (< range-bottom #xD800)
		   (> range-top #xD800))
	  (set! ranges (cons (list range-bottom #xD7FF (range-v . > . -1))
			     ranges))
	  (set! range-bottom #xE000))
	;; ... but this one was executed.
	(when (= range-bottom #xD800)
	  (set! range-bottom #xE000))
	(set! ranges (cons (list range-bottom range-top (range-v . > . -1)) 
			   ranges))
	(set! range-bottom c)
	(set! range-top c)
	(set! range-v v)))
  (let ([top-index (arithmetic-shift c (- low-bits))])
    (let ([vec (vector-ref top top-index)])
      (unless vec
	(vector-set! top top-index (make-vector (add1 low))))
      (let ([vec (vector-ref top top-index)])
	(vector-set! vec (bitwise-and c low) v)))))

(define (mapn c from v)
  (if (= c from)
      (map1 c v)
      (begin
	(map1 from v)
	(mapn c (add1 from) v))))

(call-with-input-file "UnicodeData.txt"
  (lambda (i)
    (let loop ([prev-code 0])
      (let ([l (read-line i)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"^([0-9A-F]+);([^;]*);([^;]*);[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;[^;]*;([^;]*);([^;]*);([^;]*)"
				 l)])
	    (unless m
	      (printf "no match: ~a~n" l))
	    (let ([code (string->number (cadr m) 16)]
		  [name (caddr m)]
		  [cat (cadddr m)]
		  [up (string->number (cadddr (cdr m)) 16)]
		  [down (string->number (cadddr (cddr m)) 16)]
		  [title (string->number (cadddr (cdddr m)) 16)])
	      (mapn code
		    (if (regexp-match #rx", Last>" name)
			(add1 prev-code)
			code)
		    (combine
		     (if up (- up code) 0)
		     (if down (- down code) 0)
		     (if title (- title code) 0)

		     ;; graphic
		     (member cat graphic-cats)
		     ;; lowercase:
		     (and (not (<= #x2000 code #x2FFF))
			  (not down)
			  (or up
			      (regexp-match #rx"SMALL LETTER" name)
			      (regexp-match #rx"SMALL LIGATURE" name)))
		     ;; uppercase;
		     (and (not (<= #x2000 code #x2FFF))
			  (not up)
			  (or down
			      (regexp-match #rx"CAPITAL LETTER" name)
			      (regexp-match #rx"CAPITAL LIGATURE" name)))
		     ;; titlecase:
		     (string=? cat "Lt")
		     ;; letter
		     (member cat letter-cats)
		     ;; digit
		     (member cat digit-cats)
		     ;; hex digit
		     (member code hexes)
		     ;; whitespace
		     (or (member cat space-cats)
			 (member code '(#x9 #xa #xb #xc #xd)))
		     ;; control
		     (or (<= #x0000 code #x001F)
			 (<= #x007F code #x009F))
		     ;; punctuation
		     (member cat punc-cats)
		     ;; symbol
		     (member cat sym-cats)
		     ;; blank
		     (or (string=? cat "Zs")
			 (= code #x9))))
	      (loop code))))))))

(define vectors (make-hash-table 'equal))

(define pos 0)

(current-output-port (open-output-file "schuchar.inc" 'truncate/replace))

(let loop ([i 0])
  (unless (= i hi-count)
    (let ([vec (vector-ref top i)])
      (when vec
	(unless (hash-table-get vectors vec (lambda () #f))
	  (set! pos (add1 pos))
	  (hash-table-put! vectors vec pos)))
      (loop (add1 i)))))

(define world-count (expt 2 10))

(printf "/* Generated by mk-uchar.ss */~n~n")

(printf "/* Character count: ~a */~n" ccount)
(printf "/* Table size: ~a */~n~n" 
	(+ (* (add1 low) 
	      (add1 (length (hash-table-map vectors cons))))
	   (* 2 hi-count)
	   world-count))

(printf "unsigned int **scheme_uchar_table[~a];~n~n" world-count)
(printf "static unsigned int *main_table[~a], *zero_table[~a];~n~n"
	hi-count hi-count)

(define print-row
 (lambda (vec name)
   (printf " /* ~a */~n" name)
   (let loop ([i 0])
     (printf " ~a~a" 
	     (or (vector-ref vec i) "0")
	     (if (and (= name pos)
		      (= i low))
		 "" ","))
     (when (zero? (modulo (add1 i) 16))
	 (newline))
     (unless (= i low)
       (loop (add1 i))))))

(printf "static unsigned int udata[] = {~n")

(print-row (make-vector (add1 low) 0) 0)

(map (lambda (p)
       (print-row (car p) (cdr p)))
     (quicksort
      (hash-table-map vectors cons)
      (lambda (a b) (< (cdr a) (cdr b)))))
(printf "};~n")

(define (print-shift t end name)
  (printf "~nint scheme_uchar_~a[] = {~n" name)
  (for-each (lambda (p)
	      (printf " ~a~a" 
		      (car p)
		      (if (= (cdr p) (sub1 end))
			  ""
			  ","))
	      (when (zero? (modulo (add1 (cdr p)) 16))
		(newline)))
	    (quicksort (hash-table-map t cons)
		       (lambda (a b) (< (cdr a) (cdr b)))))
  (printf " };~n"))

(print-shift (car ups) (unbox (cdr ups)) "ups")
(print-shift (car downs) (unbox (cdr downs)) "downs")
(print-shift (car titles) (unbox (cdr titles)) "titles")

(set! ranges (cons (list range-bottom range-top (range-v . > . -1))
		   ranges))

(printf "~n#define NUM_UCHAR_RANGES ~a~n" (length ranges))
(printf "~n#define URANGE_VARIES 0x40000000~n")
(printf "static int mapped_uchar_ranges[] = {~n")
(for-each (lambda (r)
	    (printf "0x~x, 0x~x~a~a~n"
		    (car r) 
		    (cadr r)
		    (if (caddr r) "" " | URANGE_VARIES")
		    (if (= (cadr r) range-top)
			""
			",")))
	  (reverse ranges))
(printf "};~n")

(printf "~nstatic void init_uchar_table(void)~n{~n")
(printf "  int i;~n~n")
(printf "  scheme_uchar_table[0] = main_table;~n")
(printf "  for (i = 1; i < ~a; i++) {~n" world-count)
(printf "    scheme_uchar_table[i] = zero_table;~n")
(printf "  }~n~n")
(printf "  for (i = 0; i < ~a; i++) { ~n" hi-count)
(printf "    main_table[i] = udata;~n")
(printf "    zero_table[i] = udata;~n")
(printf "  }~n")
(printf "~n")
(let loop ([i 0])
  (unless (= i hi-count)
    (let ([vec (vector-ref top i)])
      (if vec
	  (let ([same-count (let loop ([j (add1 i)])
			      (if (equal? vec (vector-ref top j))
				  (loop (add1 j))
				  (- j i)))]
		[vec-pos (* (add1 low) (hash-table-get vectors vec))])
	    (if (> same-count 4)
		(begin
		  (printf "  for (i = ~a; i < ~a; i++) {~n"
			  i (+ i same-count))
		  (printf "    main_table[i] = udata + ~a;~n"
			  vec-pos)
		  (printf "  }~n")
		  (loop (+ same-count i)))
		(begin
		  (printf "  main_table[~a] = udata + ~a;~n"
			  i
			  vec-pos)
		  (loop (add1 i)))))
	  (loop (add1 i))))))
(printf "}~n")
