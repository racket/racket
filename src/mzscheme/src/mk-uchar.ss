
;; This script parses UnicodeData.txt (the standard Unicode database,
;; available from the web) and other such files, and it produces
;; "schuchar.inc" and "schustr.inc". The former is used by
;; scheme_isalpha, etc., and thus `char-alphabetic?', etc. The latter
;; is used for string operations.

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

(define cases (cons (make-hash-table 'equal) (box 0)))

(define (indirect t v)
  (let ([r (hash-table-get (car t) v (lambda () #f))])
    (or r
	(let ([r (unbox (cdr t))])
	  (set-box! (cdr t) (add1 r))
	  (hash-table-put! (car t) v r)
	  (when (r . > . 255)
	    (error "too many indirects"))
	  r))))

(define (combine . l)  
  ;; The scheme_is...() macros in scheme.h must match
  ;;  the bit layout produced here
  (let loop ([l l][v 0])
    (if (null? l)
	v
	(loop (cdr l) (bitwise-ior (arithmetic-shift v 1)
				   (if (car l)
				       1
				       0))))))

(define (combine-case up down title fold)
  (indirect cases (list up down title fold)))

(define hexes (map char->integer (string->list "0123456789abcdefABCDEF")))

;; In principle, adjust this number to tune the result, but
;;  the macros for accessing the table (in scheme.h) need to
;;  be updated accordingly.
;; In practice, it's unlikely that anything will ever work
;;  much better than 8.
(define low-bits 8)

(define low (sub1 (expt 2 low-bits)))
(define hi-count (expt 2 (- 21 low-bits)))
(define hi (arithmetic-shift (sub1 hi-count) low-bits))

(define top (make-vector hi-count #f))
(define top2 (make-vector hi-count #f))

(define range-bottom 0)
(define range-top -1)
(define range-v -1)
(define range-v2 -1)
(define ranges null)

(define ccount 0)

(define (map1 c v v2)
  (set! ccount (add1 ccount))
  (if (= c (add1 range-top))
      (begin
	(unless (and (= v range-v)
		     (= v2 range-v2))
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
	(set! range-v v)
	(set! range-v2 v2)))
  (let ([top-index (arithmetic-shift c (- low-bits))])
    (let ([vec (vector-ref top top-index)]
	  [vec2 (vector-ref top2 top-index)])
      (unless vec
	(vector-set! top top-index (make-vector (add1 low))))
      (unless vec2
	(vector-set! top2 top-index (make-vector (add1 low))))
      (let ([vec (vector-ref top top-index)]
	    [vec2 (vector-ref top2 top-index)])
	(vector-set! vec (bitwise-and c low) v)
	(vector-set! vec2 (bitwise-and c low) v2)))))

(define (mapn c from v v2)
  (if (= c from)
      (map1 c v v2)
      (begin
	(map1 from v v2)
	(mapn c (add1 from) v v2))))

(define midletters
  (call-with-input-file "WordBreakProperty.txt"
    (lambda (i)
      (let loop ()
	(let ([re (regexp-match #rx"\n([0-9A-F]+)  *; *MidLetter" i)])
	  (if re
	      (cons (string->number (bytes->string/latin-1 (cadr re)) 16)
		    (loop))
	      null))))))

(define (string->codes s)
  (let ([m (regexp-match #rx"^[^0-9A-F]*([0-9A-F]+)" s)])
    (if m
	(cons (string->number (cadr m) 16) 
	      (string->codes (substring s (string-length (car m)))))
	null)))

;; This code assumes that Final_Sigma is the only condition that we care about:
(define case-foldings (make-hash-table 'equal))
(define special-case-foldings (make-hash-table 'equal))
(call-with-input-file "CaseFolding.txt"
  (lambda (i)
    (let loop ()
      (let ([l (read-line i)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"^([0-9A-F]+); *([CSF]) *;([^;]*)" l)])
	    (when m
	      (let ([code (string->number (cadr m) 16)]
		    [variant (list-ref m 2)]
		    [folded (string->codes (list-ref m 3))])
		(if (string=? variant "F")
		    (hash-table-put! special-case-foldings code folded)
		    (hash-table-put! case-foldings code (car folded))))))
	  (loop))))))

;; This code assumes that Final_Sigma is the only condition that we care about:
(define special-casings (make-hash-table 'equal))
(define-struct special-casing (lower upper title folding final-sigma?))
(call-with-input-file "SpecialCasing.txt"
  (lambda (i)
    (let loop ()
      (let ([l (read-line i)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"^([0-9A-F]+);([^;]*);([^;]*);([^;]*);([^;]*)" l)])
	    (when (and m
		       (regexp-match #rx"^(?:(?: *Final_Sigma *)|(?: *))(?:$|[;#].*)" (list-ref m 5)))
	      (let ([code (string->number (cadr m) 16)]
		    [lower (string->codes (list-ref m 2))]
		    [upper (string->codes (list-ref m 4))]
		    [title (string->codes (list-ref m 3))]
		    [final-sigma? (and (regexp-match #rx"Final_Sigma" (list-ref m 5)) #t)])
		(let ([folding (list (hash-table-get case-foldings code (lambda () code)))])
		  (hash-table-put! special-casings code (make-special-casing lower upper title folding final-sigma?))))))
	  (loop))))))

(define lower-case  (make-hash-table 'equal))
(define upper-case  (make-hash-table 'equal))

(with-input-from-file "DerivedCoreProperties.txt"
  (lambda ()
    (let loop ()
      (let ([l (read-line)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"^([0-9A-F.]+) *; (Lower|Upper)case" l)])
	    (when m
	      (let* ([start (string->number (car (regexp-match #rx"^[0-9A-F]+" (car m))) 16)]
		     [end (let ([m (regexp-match #rx"^[0-9A-F]+[.][.]([0-9A-F]+)" (car m))])
			    (if m
				(string->number (cadr m) 16)
				start))]
		     [t (if (string=? (caddr m) "Lower") lower-case upper-case)])
		(let loop ([i start])
		  (hash-table-put! t i #t)
		  (unless (= i end)
		    (loop (add1 i)))))))
	  (loop))))))


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
		    ;; The booleans below are in most-siginficant-bit-first order
		    (combine
		     ;; special-casing
		     (or (hash-table-get special-casings code (lambda () #f))
			 (hash-table-get special-case-foldings code (lambda () #f)))
		     ;; case-ignoreable
		     (or (member code midletters)
			 (member cat '("Mn" "Me" "Cf" "Lm" "Sk")))
		     ;; graphic
		     (member cat graphic-cats)
		     ;; lowercase:
		     (hash-table-get lower-case code (lambda () #f))
		     #;
		     (and (not (<= #x2000 code #x2FFF))
			  (not down)
			  (or up
			      (regexp-match #rx"SMALL LETTER" name)
			      (regexp-match #rx"SMALL LIGATURE" name)))
		     ;; uppercase;
		     (hash-table-get upper-case code (lambda () #f))
		     #;
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
			 (= code #x9)))
		    ;; Cases
		    (combine-case
		     (if up (- up code) 0)
		     (if down (- down code) 0)
		     (if title (- title code) 0)
		     (let ([case-fold (hash-table-get case-foldings code (lambda () #f))])
		       (if case-fold (- case-fold code) 0))))
	      (loop code))))))))

(define vectors (make-hash-table 'equal))
(define vectors2 (make-hash-table 'equal))

(define pos 0)
(define pos2 0)

(current-output-port (open-output-file "schuchar.inc" 'truncate/replace))

(define (hash-vectors! top vectors get-pos set-pos!)
  (let loop ([i 0])
    (unless (= i hi-count)
      (let ([vec (vector-ref top i)])
	(when vec
	  (unless (hash-table-get vectors vec (lambda () #f))
	    (set-pos! (add1 (get-pos)))
	    (hash-table-put! vectors vec (get-pos))))
	(loop (add1 i))))))

(hash-vectors! top vectors (lambda () pos) (lambda (v) (set! pos v)))
(hash-vectors! top2 vectors2 (lambda () pos2) (lambda (v) (set! pos2 v)))

;; copy folding special cases to the special-cases table, if not there already:
(hash-table-for-each special-case-foldings
		     (lambda (k v)
		       (let ([sc (hash-table-get special-casings k (lambda ()
								     (let ([sc (make-special-casing
										(list k)
										(list k)
										(list k)
										(list k)
										#f)])
								       (hash-table-put! special-casings k sc)
								       sc)))])
			 (set-special-casing-folding! sc v))))

(define world-count (expt 2 10))

(printf "/* Generated by mk-uchar.ss */~n~n")

(printf "/* Character count: ~a */~n" ccount)
(printf "/* Table size: ~a */~n~n" 
	(+ (* (add1 low) 
	      (* 2 (add1 (length (hash-table-map vectors cons)))))
	   (* (add1 low) 
	      (* 1 (add1 (length (hash-table-map vectors2 cons)))))
	   (* 4 4 (unbox (cdr cases)))
	   (* 4 (* 2 hi-count))))

(printf "unsigned short *scheme_uchar_table[~a];~n" hi-count)
(printf "unsigned char *scheme_uchar_cases_table[~a];~n~n" hi-count)

(define print-row
 (lambda (vec name pos hex?)
   (printf " /* ~a */~n" name)
   (let loop ([i 0])
     (printf (if hex? " 0x~x~a" " ~a~a")
	     (or (vector-ref vec i) "0")
	     (if (and (= name pos)
		      (= i low))
		 "" ","))
     (when (zero? (modulo (add1 i) 16))
	 (newline))
     (unless (= i low)
       (loop (add1 i))))))

(define (print-table type suffix vectors pos hex?)
  (printf "static unsigned ~a udata~a[] = {~n" type suffix)
  (print-row (make-vector (add1 low) 0) 0 pos hex?)
  (map (lambda (p)
	 (print-row (car p) (cdr p) pos hex?))
       (quicksort
	(hash-table-map vectors cons)
	(lambda (a b) (< (cdr a) (cdr b)))))
  (printf "};~n"))
(print-table "short" "" vectors pos #t)
(printf "\n")
(print-table "char" "_cases" vectors2 pos2 #f)

(printf "~n/* Case mapping size: ~a */~n" (hash-table-count (car cases)))
  
(define (print-shift t end select name)
  (printf "~nint scheme_uchar_~a[] = {~n" name)
  (for-each (lambda (p)
	      (printf " ~a~a" 
		      (select (car p))
		      (if (= (cdr p) (sub1 end))
			  ""
			  ","))
	      (when (zero? (modulo (add1 (cdr p)) 16))
		(newline)))
	    (quicksort (hash-table-map t cons)
		       (lambda (a b) (< (cdr a) (cdr b)))))
  (printf " };~n"))

(print-shift (car cases) (unbox (cdr cases)) car "ups")
(print-shift (car cases) (unbox (cdr cases)) cadr "downs")
(print-shift (car cases) (unbox (cdr cases)) caddr "titles")
(print-shift (car cases) (unbox (cdr cases)) cadddr "folds")

(set! ranges (cons (list range-bottom range-top (range-v . > . -1))
		   ranges))

(printf "~n#define NUM_UCHAR_RANGES ~a~n" (length ranges))
(printf "~n#define URANGE_VARIES 0x40000000~n")
(printf "static int mapped_uchar_ranges[] = {~n")
(for-each (lambda (r)
	    (printf "  0x~x, 0x~x~a~a~n"
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
(printf "  for (i = 0; i < ~a; i++) { ~n" hi-count)
(printf "    scheme_uchar_table[i] = udata;~n")
(printf "    scheme_uchar_cases_table[i] = udata_cases;~n")
(printf "  }~n")
(printf "~n")
(define (print-init top vectors suffix)
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
		    (printf "    scheme_uchar~a_table[i] = udata~a + ~a;~n"
			    suffix suffix
			    vec-pos)
		    (printf "  }~n")
		    (loop (+ same-count i)))
		  (begin
		    (printf "  scheme_uchar~a_table[~a] = udata~a + ~a;~n"
			    suffix
			    i
			    suffix
			    vec-pos)
		    (loop (add1 i)))))
	    (loop (add1 i)))))))
(print-init top vectors "")
(print-init top2 vectors2 "_cases")
(printf "}~n")

;; ----------------------------------------

(current-output-port (open-output-file "schustr.inc" 'truncate/replace))

(printf "/* Generated by mk-uchar.ss */~n~n")

(define specials null)
(define special-count 0)
(define (register-special l)
  (let ([l (reverse l)])
    (unless (let loop ([l l][specials specials])
	      (cond
	       [(null? l) #t]
	       [(null? specials) #f]
	       [(= (car l) (car specials)) (loop (cdr l) (cdr specials))]
	       [else #f]))
      (set! specials (append l specials))
      (set! special-count (+ special-count (length l))))
    (- special-count (length l))))

(printf "#define NUM_SPECIAL_CASINGS ~a\n\n" (hash-table-count special-casings))
(printf "static int uchar_special_casings[] = {\n")
(printf "  /* code,  down len, off,  up len, off,  title len, off,  fold len, off,  final-sigma? */\n")
(let ([n (hash-table-count special-casings)])
  (for-each (lambda (p)
	      (set! n (sub1 n))
	      (let ([code (car p)]
		    [sc (cdr p)])
		(let ([lower-start (register-special (special-casing-lower sc))]
		      [upper-start (register-special (special-casing-upper sc))]
		      [title-start (register-special (special-casing-title sc))]
		      [folding-start (register-special (special-casing-folding sc))])
		  (printf "  ~a,  ~a, ~a,  ~a, ~a,  ~a, ~a,  ~a, ~a,  ~a~a"
			  code
			  (length (special-casing-lower sc)) lower-start
			  (length (special-casing-upper sc)) upper-start
			  (length (special-casing-title sc)) title-start
			  (length (special-casing-folding sc)) folding-start
			  (if (special-casing-final-sigma? sc) 1 0)
			  (if (zero? n) " " ",\n")))))
	    (quicksort (hash-table-map special-casings cons)
		       (lambda (a b) (< (car a) (car b))))))
(printf "};\n")
(printf "\n/* Offsets in scheme_uchar_special_casings point into here: */\n")
(printf "static int uchar_special_casing_data[] = {\n  ")
(let ([n 0])
  (for-each (lambda (v)
	      (printf
	       (cond
		[(zero? n) "~a"]
		[(zero? (modulo n 16)) ",\n  ~a"]
		[else ", ~a"])
	       v)
	      (set! n (add1 n)))
	    (reverse specials)))
(printf " };~n")

(printf "\n#define SPECIAL_CASE_FOLD_MAX ~a\n" (apply 
						max 
						(hash-table-map 
						 special-casings
						 (lambda (k v)
						   (length (special-casing-folding v))))))


