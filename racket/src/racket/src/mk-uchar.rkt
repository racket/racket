#lang racket/base

;; This script parses UnicodeData.txt (the standard Unicode database,
;; available from the web) and other such files, and it produces
;; "schuchar.inc" and "schustr.inc". The former is used by
;; scheme_isalpha, etc., and thus `char-alphabetic?', etc. The latter
;; is used for string operations.

;; Run as
;;   racket mk-uchar.rkt
;; in the script's directory, and have a copy of UnicodeData.txt, etc.
;; in the "Unicode" directory. The file schuchar.inc will be
;; overwritten.

(require racket/list)

(define mark-cats '("Mn" "Mc" "Me"))
(define letter-cats '("Lu" "Ll" "Lt" "Lm" "Lo"))
(define digit-cats '("Nd" "No" "Nl"))
(define space-cats '("Zl" "Zs" "Zp"))
(define punc-cats '("Pc" "Pd" "Ps" "Pe" "Pi" "Pf" "Po"))
(define sym-cats '("Sm" "Sc" "Sk" "So"))
(define sympart-non-cats '("Ps" "Pe" "Pi" "Pf" "Zl" "Zs" "Zp"))
(define graphic-cats (append mark-cats
			     letter-cats
			     digit-cats
			     punc-cats
			     sym-cats))

(define cases (cons (make-hash) (box 0)))

(define (indirect t v limit)
  (let ([r (hash-ref (car t) v (lambda () #f))])
    (or r
	(let ([r (unbox (cdr t))])
	  (set-box! (cdr t) (add1 r))
	  (hash-set! (car t) v r)
	  (when (r . > . limit)
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

(define (combine-case up down title fold combining)
  (indirect cases (list up down title fold combining) 256))

(define general-categories (make-hash))
(define (combine-cat cat)
  (hash-ref general-categories cat
            (lambda ()
              (let ([v (hash-count general-categories)])
                (hash-set! general-categories cat v)
                v))))
;; So they're in order:
(with-input-from-file "schgencat.h"
  (lambda ()
    (let loop ()
      (let ([l (read-line)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"mzu_([A-Z][a-z])" l)])
	    (when m
	      (combine-cat (cadr m))))
	  (loop))))))

(define hexes (map char->integer (string->list "0123456789abcdefABCDEF")))

(define combining-class-ht (make-hasheq))

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
(define top3 (make-vector hi-count #f))

(define range-bottom 0)
(define range-top -1)
(define range-v -1)
(define range-v2 -1)
(define range-v3 -1)
(define ranges null)

(define ccount 0)

(define (map1 c v v2 v3 cc)
  (hash-set! combining-class-ht c cc)
  (set! ccount (add1 ccount))
  (if (= c (add1 range-top))
      (begin
	(unless (and (= v range-v)
		     (= v2 range-v2)
		     (= v3 range-v3))
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
	(set! range-v2 v2)
	(set! range-v3 v3)))
  (let ([top-index (arithmetic-shift c (- low-bits))])
    (let ([vec (vector-ref top top-index)]
	  [vec2 (vector-ref top2 top-index)]
	  [vec3 (vector-ref top3 top-index)])
      (unless vec
	(vector-set! top top-index (make-vector (add1 low))))
      (unless vec2
	(vector-set! top2 top-index (make-vector (add1 low))))
      (unless vec3
	(vector-set! top3 top-index (make-vector (add1 low))))
      (let ([vec (vector-ref top top-index)]
	    [vec2 (vector-ref top2 top-index)]
	    [vec3 (vector-ref top3 top-index)])
	(vector-set! vec (bitwise-and c low) v)
	(vector-set! vec2 (bitwise-and c low) v2)
	(vector-set! vec3 (bitwise-and c low) v3)))))

(define (mapn c from v v2 v3 cc)
  (if (= c from)
      (map1 c v v2 v3 cc)
      (begin
	(map1 from v v2 v3 cc)
	(mapn c (add1 from) v v2 v3 cc))))

(define (set-compose-initial! c)
  (let ([top-index (arithmetic-shift c (- low-bits))])
    (let ([vec (vector-ref top top-index)]
	  [i (bitwise-and c low) ])
      (vector-set! vec i (bitwise-ior #x8000 (vector-ref vec i))))))

(define (string->codes s)
  (let ([m (regexp-match #rx"^[^0-9A-F]*([0-9A-F]+)" s)])
    (if m
	(cons (string->number (cadr m) 16) 
	      (string->codes (substring s (string-length (car m)))))
	null)))

;; This code assumes that Final_Sigma is the only condition that we care about:
(define case-foldings (make-hash))
(define special-case-foldings (make-hash))
(call-with-input-file "Unicode/CaseFolding.txt"
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
		    (hash-set! special-case-foldings code folded)
		    (hash-set! case-foldings code (car folded))))))
	  (loop))))))

;; This code assumes that Final_Sigma is the only condition that we care about:
(define special-casings (make-hash))
(define-struct special-casing (lower upper title folding final-sigma?) #:mutable)
(call-with-input-file "Unicode/SpecialCasing.txt"
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
		(let ([folding (list (hash-ref case-foldings code (lambda () code)))])
		  (hash-set! special-casings code (make-special-casing lower upper title folding final-sigma?))))))
	  (loop))))))

(define lower-case  (make-hash))
(define upper-case  (make-hash))
(define alphabetic  (make-hash))
(define case-ignorable (make-hash))

(with-input-from-file "Unicode/DerivedCoreProperties.txt"
  (lambda ()
    (let loop ()
      (let ([l (read-line)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"^([0-9A-F.]+) *; ((Lower|Upper)case|Alphabetic|Case_Ignorable)" l)])
	    (when m
	      (let* ([start (string->number (car (regexp-match #rx"^[0-9A-F]+" (car m))) 16)]
		     [end (let ([m (regexp-match #rx"^[0-9A-F]+[.][.]([0-9A-F]+)" (car m))])
			    (if m
				(string->number (cadr m) 16)
				start))]
		     [t (cond
                         [(string=? (caddr m) "Lowercase") lower-case]
                         [(string=? (caddr m) "Uppercase") upper-case]
                         [(string=? (caddr m) "Alphabetic") alphabetic]
                         [(string=? (caddr m) "Case_Ignorable") case-ignorable]
                         [else (error "unknown property section")])])
		(let loop ([i start])
		  (hash-set! t i #t)
		  (unless (= i end)
		    (loop (add1 i)))))))
	  (loop))))))

(define white_spaces (make-hash))

(with-input-from-file "Unicode/PropList.txt"
  (lambda ()
    (let loop ()
      (let ([l (read-line)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"^([0-9A-F.]+) *; White_Space" l)])
	    (when m
	      (let* ([start (string->number (car (regexp-match #rx"^[0-9A-F]+" (car m))) 16)]
		     [end (let ([m (regexp-match #rx"^[0-9A-F]+[.][.]([0-9A-F]+)" (car m))])
			    (if m
				(string->number (cadr m) 16)
				start))])
		(let loop ([i start])
		  (hash-set! white_spaces i #t)
		  (unless (= i end)
		    (loop (add1 i)))))))
	  (loop))))))

(define decomp-ht (make-hasheq))
(define k-decomp-ht (make-hasheq))
(define compose-initial-ht (make-hasheq))
(define compose-map (make-hash))
(define do-not-compose-ht (make-hash))

(with-input-from-file "Unicode/CompositionExclusions.txt"
  (lambda ()
    (let loop ()
      (let ([l (read-line)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"^([0-9A-F.]+)" l)])
	    (when m
	      (let ([code (string->number (car m) 16)])
		(hash-set! do-not-compose-ht code #t))))
	  (loop))))))

(define (composition-key a b)
  ;; If `a` and `b` are both in the BMP (i.e., both fit in 16 bits),
  ;; map to a 32-bit key.
  (bitwise-ior (arithmetic-shift (bitwise-and a #xFFFF) 16)
               (bitwise-and b #xFFFF)
               (arithmetic-shift 
                (bitwise-ior (arithmetic-shift (arithmetic-shift a -16)
                                               5)
                             (arithmetic-shift b -16))
                32)))

(define (composition-key-first k)
  (bitwise-ior (bitwise-and (arithmetic-shift k -16) #xFFFF)
               (arithmetic-shift (arithmetic-shift k -37) 16)))

(define (extract-decomp decomp code)
  (if (string=? decomp "")
      #f
      (let ([m (regexp-match #rx"^([0-9A-F]+) ?([0-9A-F]*)$" decomp)])
	(if m
	    ;; Canonical decomp
	    (let ([a (string->number (cadr m) 16)]
		  [b (if (string=? "" (caddr m))
			 0
			 (string->number (caddr m) 16))])
	      ;; Canonical composition?
	      (when (and (positive? b)
			 (not (hash-ref do-not-compose-ht
                                        code
                                        (lambda () #f))))
		(hash-set! compose-initial-ht a #t)
		(let ([key (composition-key a b)])
		  (when (hash-ref compose-map key (lambda () #f))
		    (error 'decomp "composition already mapped: ~x for: ~x" key code))
		  (hash-set! compose-map key code)))
	      (hash-set! decomp-ht code (cons a b))
	      #t)
	    ;; Compatibility decomp
	    (let ([seq
		   (let loop ([str (cadr (regexp-match #rx"^<[^>]*> *(.*)$" decomp))])
		     (let ([m (regexp-match #rx"^([0-9A-F]+) *(.*)$" str)])
		       (if m
			   (cons (string->number (cadr m) 16)
				 (loop (caddr m)))
			   null)))])
	      (hash-set! k-decomp-ht code seq)
	      #t)))))

(call-with-input-file "Unicode/UnicodeData.txt"
  (lambda (i)
    (let loop ([prev-code 0])
      (let ([l (read-line i)])
	(unless (eof-object? l)
	  (let ([m (regexp-match #rx"^([0-9A-F]+);([^;]*);([^;]*);([^;]*);[^;]*;([^;]*);[^;]*;([^;]*);[^;]*;[^;]*;[^;]*;[^;]*;([^;]*);([^;]*);([^;]*)"
				 l)])
	    (unless m
	      (printf "no match: ~a\n" l))
	    (let ([code (string->number (cadr m) 16)]
		  [name (caddr m)]
		  [cat (cadddr m)]
		  [combining (string->number (cadddr (cdr m)))]
		  [decomp (cadddr (cddr m))]
                  [numeric (cadddr (cdddr m))]
		  [up (string->number (cadddr (cddddr m)) 16)]
		  [down (string->number (cadddr (cddddr (cdr m))) 16)]
		  [title (string->number (cadddr (cddddr (cddr m))) 16)])
              (let ([alphabetic? (hash-ref alphabetic code #f)]
                    [numeric? (not (string=? numeric ""))]
                    [symbolic? (member cat sym-cats)]
                    [punctuation? (member cat punc-cats)])
                (mapn code
                      (if (regexp-match #rx", Last>" name)
                          (add1 prev-code)
                          code)
                      ;; The booleans below are in most-siginficant-bit-first order
                      (combine
                       ;; Decomposition
                       (extract-decomp decomp code)
                       ;; special-casing
                       (or (hash-ref special-casings code (lambda () #f))
                           (hash-ref special-case-foldings code (lambda () #f)))
                       ;; case-ignoreable
                       (hash-ref case-ignorable code #f)
                       ;; graphic
                       (or alphabetic?
                           numeric?
                           symbolic?
                           punctuation?
                           (member cat graphic-cats))
                       ;; lowercase:
                       (hash-ref lower-case code (lambda () #f))
                       #;
                       (and (not (<= #x2000 code #x2FFF))
                            (not down)
                            (or up
                                (regexp-match #rx"SMALL LETTER" name)
                                (regexp-match #rx"SMALL LIGATURE" name)))
                       ;; uppercase;
                       (hash-ref upper-case code (lambda () #f))
                       #;
                       (and (not (<= #x2000 code #x2FFF))
                            (not up)
                            (or down
                                (regexp-match #rx"CAPITAL LETTER" name)
                                (regexp-match #rx"CAPITAL LIGATURE" name)))
                       ;; titlecase:
                       (string=? cat "Lt")
                       ;; letter
                       alphabetic?
                       #;
                       (member cat letter-cats)
                       ;; digit
                       numeric?
                       #;
                       (member cat digit-cats)
                       ;; SOMETHING - this bit not yet used
                       #f
                       ;; whitespace
                       (hash-ref white_spaces code #f)
                       #;
                       (or (member cat space-cats)
                       (member code '(#x9 #xa #xb #xc #xd #x85)))
                       ;; control
                       (or (<= #x0000 code #x001F)
                           (<= #x007F code #x009F))
                       ;; punctuation
                       punctuation?
                       ;; symbol
                       symbolic?
                       ;; blank
                       (or (string=? cat "Zs")
                           (= code #x9)))
                      ;; Cases
                      (combine-case
                       (if up (- up code) 0)
                       (if down (- down code) 0)
                       (if title (- title code) 0)
                       (let ([case-fold (hash-ref case-foldings code (lambda () #f))])
                         (if case-fold (- case-fold code) 0))
                       combining)
                      ;; Category
                      (combine-cat cat)
                      ;; Combining class - used again to filter initial composes
                      combining))
	      (loop code))))))))

(hash-for-each compose-initial-ht
               (lambda (k v)
                 ;; A canonical decomposition that starts with a non-0 combining
                 ;;  class is not re-created in a canonical composition. There
                 ;;  are only two such leading character as of Unicode 4.0:
                 ;;  U+0308 and U+0F71.
                 (when (zero? (hash-ref combining-class-ht k))
                   (set-compose-initial! k))))

;; Remove compositions from compose map that start with
;;  a character whose combining class is not 0. As of Unicode
;;  4.0, there are only four of these: U+0344, U+0F73,
;;  U+0F75, and U+0F81.
(for-each (lambda (k)
	    (let ([a (composition-key-first k)])
	      (unless (zero? (hash-ref combining-class-ht a))
		(hash-remove! compose-map k))))
	  (hash-map compose-map (lambda (k v) k)))

(define k-decomp-map-ht (make-hasheq))
(define k-decomp-strs-ht (make-hash))
(define k-decomp-strs-len 0)
(define k-decomp-strs null)

(define (fold-decomp s)
  (cond
   [(empty? s) empty]
   [(empty? (cdr s))
    (let ([code (car s)])
      (let ([v (hash-ref decomp-ht code (lambda () #f))])
	(if v
	    (if (zero? (cdr v))
		(fold-decomp (list (car v)))
		(fold-decomp (list (car v) (cdr v))))
	    (let ([v (hash-ref k-decomp-ht code (lambda () #f))])
	      (if v
		  (fold-decomp v)
		  (list code))))))]
   [else (append (fold-decomp (list (car s)))
		 (fold-decomp (cdr s)))]))

(for-each
 (lambda (p)
   (let* ([code (car p)]
	  [seq (fold-decomp (cdr p))]
	  [pos (hash-ref k-decomp-strs-ht seq
                         (lambda ()
                           (begin0
                            k-decomp-strs-len
                            (hash-set! k-decomp-strs-ht seq
                                       k-decomp-strs-len)
                            (set! k-decomp-strs
                                  (append (reverse seq) k-decomp-strs))
                            (set! k-decomp-strs-len (+ k-decomp-strs-len
                                                       (length seq))))))])
     (hash-set! k-decomp-map-ht code (cons pos (length seq)))))
 ;; Sort to keep it deterministic:
 (sort (hash-map k-decomp-ht cons)
       (lambda (a b) (< (car a) (car b)))))


(define vectors (make-hash))
(define vectors2 (make-hash))
(define vectors3 (make-hash))

(define pos 0)
(define pos2 0)
(define pos3 0)

(current-output-port (open-output-file "schuchar.inc" #:exists 'truncate/replace))

(define (hash-vectors! top vectors get-pos set-pos!)
  (let loop ([i 0])
    (unless (= i hi-count)
      (let ([vec (vector-ref top i)])
	(when vec
	  (unless (hash-ref vectors vec (lambda () #f))
	    (set-pos! (add1 (get-pos)))
	    (hash-set! vectors vec (get-pos))))
	(loop (add1 i))))))

(hash-vectors! top vectors (lambda () pos) (lambda (v) (set! pos v)))
(hash-vectors! top2 vectors2 (lambda () pos2) (lambda (v) (set! pos2 v)))
(hash-vectors! top3 vectors3 (lambda () pos3) (lambda (v) (set! pos3 v)))

;; copy folding special cases to the special-cases table, if not there already:
(hash-for-each special-case-foldings
               (lambda (k v)
                 (let ([sc (hash-ref special-casings k (lambda ()
                                                         (let ([sc (make-special-casing
                                                                    (list k)
                                                                    (list k)
                                                                    (list k)
                                                                    (list k)
                                                                    #f)])
                                                           (hash-set! special-casings k sc)
                                                           sc)))])
                   (set-special-casing-folding! sc v))))

(define world-count (expt 2 10))

(printf "/* Generated by mk-uchar.rkt */\n\n")

(printf "/* Character count: ~a */\n" ccount)
(printf "/* Total bytes for all tables: ~a */\n\n" 
	(+ (* (add1 low) 
	      (* 2 (add1 (length (hash-map vectors cons)))))
	   (* (add1 low) 
	      (* 1 (add1 (length (hash-map vectors2 cons)))))
	   (* (add1 low) 
	      (* 1 (add1 (length (hash-map vectors3 cons)))))
	   (* (hash-count decomp-ht)
	      8)
	   (* (hash-count compose-map)
	      2)
	   (* (hash-count k-decomp-map-ht) (+ 4 1 2))
	   (* 2 k-decomp-strs-len)
	   (* 4 4 (unbox (cdr cases)))
	   (* 4 (* 2 hi-count))))

(printf (string-append
	 "/* Each of the following maps a character to a value\n"
	 "   via the scheme_uchar_find() macro in scheme.h. */\n\n"))

(printf "/* Character properties: */\n")
(printf "READ_ONLY unsigned short *scheme_uchar_table[~a];\n" hi-count)

(printf "\n/* Character case mapping as index into scheme_uchar_ups, etc.: */\n")
(printf "READ_ONLY unsigned char *scheme_uchar_cases_table[~a];\n" hi-count)

(printf "\n/* Character general categories: */\n")
(printf "READ_ONLY unsigned char *scheme_uchar_cats_table[~a];\n" hi-count)

(printf "\n/* The udata... arrays are used by init_uchar_table to fill the above mappings.*/\n\n")

(define print-row
 (lambda (vec name pos hex?)
   (printf " /* ~a */\n" name)
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
  (printf "READ_ONLY static unsigned ~a udata~a[] = {\n" type suffix)
  (print-row (make-vector (add1 low) 0) 0 pos hex?)
  (map (lambda (p)
	 (print-row (car p) (cdr p) pos hex?))
       (sort (hash-map vectors cons)
             (lambda (a b) (< (cdr a) (cdr b)))))
  (printf "};\n"))
(print-table "short" "" vectors pos #t)
(printf "\n")
(print-table "char" "_cases" vectors2 pos2 #f)
(print-table "char" "_cats" vectors3 pos3 #f)

(printf "\n/* Case mapping size: ~a */\n" (hash-count (car cases)))
(printf "/* Find an index into the ups, downs, etc. table for a character\n")
(printf "   by using scheme_uchar_cases_table; then, the value at the index\n")
(printf "   is relative to the original character (except for combining class,\n")
(printf "   of course). */\n")
  
(define (print-shift t end select type name)
  (printf "\nREAD_ONLY ~a scheme_uchar_~a[] = {\n" type name)
  (for-each (lambda (p)
	      (printf " ~a~a" 
		      (select (car p))
		      (if (= (cdr p) (sub1 end))
			  ""
			  ","))
	      (when (zero? (modulo (add1 (cdr p)) 16))
		(newline)))
	    (sort (hash-map t cons)
                  (lambda (a b) (< (cdr a) (cdr b)))))
  (printf " };\n"))

(print-shift (car cases) (unbox (cdr cases)) car "int" "ups")
(print-shift (car cases) (unbox (cdr cases)) cadr "int" "downs")
(print-shift (car cases) (unbox (cdr cases)) caddr "int" "titles")
(print-shift (car cases) (unbox (cdr cases)) cadddr "int" "folds")
(print-shift (car cases) (unbox (cdr cases)) (lambda (x) (cadddr (cdr x))) "unsigned char" "combining_classes")

(let ([l (sort (hash-map general-categories cons)
               (lambda (a b) (< (cdr a) (cdr b))))])
  (printf "\n#define NUM_GENERAL_CATEGORIES ~a\n" (length l))
  (printf "READ_ONLY static const char *general_category_names[] = {")
  (for-each (lambda (c)
	      (printf (if (zero? (cdr c))
			  "\n  ~s"
			  ",\n  ~s")
		      (string-downcase (car c))))
	    l)
  (printf "\n};\n"))

(set! ranges (cons (list range-bottom range-top (range-v . > . -1))
		   ranges))

(printf "\n#define NUM_UCHAR_RANGES ~a\n" (length ranges))
(printf "\n#define URANGE_VARIES 0x40000000\n")
(printf "READ_ONLY static int mapped_uchar_ranges[] = {\n")
(for-each (lambda (r)
	    (printf "  0x~x, 0x~x~a~a\n"
		    (car r) 
		    (cadr r)
		    (if (caddr r) "" " | URANGE_VARIES")
		    (if (= (cadr r) range-top)
			""
			",")))
	  (reverse ranges))
(printf "};\n")

(printf "\nstatic void init_uchar_table(void)\n{\n")
(printf "  int i;\n\n")
(printf "  for (i = 0; i < ~a; i++) { \n" hi-count)
(printf "    scheme_uchar_table[i] = udata;\n")
(printf "    scheme_uchar_cases_table[i] = udata_cases;\n")
(printf "    scheme_uchar_cats_table[i] = udata_cats;\n")
(printf "  }\n")
(printf "\n")
(define (print-init top vectors suffix)
  (let loop ([i 0])
    (unless (= i hi-count)
      (let ([vec (vector-ref top i)])
	(if vec
	    (let ([same-count (let loop ([j (add1 i)])
				(if (equal? vec (vector-ref top j))
				    (loop (add1 j))
				    (- j i)))]
		  [vec-pos (* (add1 low) (hash-ref vectors vec))])
	      (if (> same-count 4)
		  (begin
		    (printf "  for (i = ~a; i < ~a; i++) {\n"
			    i (+ i same-count))
		    (printf "    scheme_uchar~a_table[i] = udata~a + ~a;\n"
			    suffix suffix
			    vec-pos)
		    (printf "  }\n")
		    (loop (+ same-count i)))
		  (begin
		    (printf "  scheme_uchar~a_table[~a] = udata~a + ~a;\n"
			    suffix
			    i
			    suffix
			    vec-pos)
		    (loop (add1 i)))))
	    (loop (add1 i)))))))
(print-init top vectors "")
(print-init top2 vectors2 "_cases")
(print-init top3 vectors3 "_cats")
(printf "}\n")

;; ----------------------------------------

(current-output-port (open-output-file "schustr.inc" #:exists 'truncate/replace))

(printf "/* Generated by mk-uchar.rkt */\n\n")

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

(printf "#define NUM_SPECIAL_CASINGS ~a\n\n" (hash-count special-casings))
(printf "READ_ONLY static int uchar_special_casings[] = {\n")
(printf "  /* code,  down len, off,  up len, off,  title len, off,  fold len, off,  final-sigma? */\n")
(let ([n (hash-count special-casings)])
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
	    (sort (hash-map special-casings cons)
                  (lambda (a b) (< (car a) (car b))))))
(printf "};\n")
(printf "\n/* Offsets in scheme_uchar_special_casings point into here: */\n")
(printf "READ_ONLY static int uchar_special_casing_data[] = {\n  ")
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
(printf " };\n")

(printf "\n#define SPECIAL_CASE_FOLD_MAX ~a\n" (apply 
						max 
						(hash-map 
						 special-casings
						 (lambda (k v)
						   (length (special-casing-folding v))))))




(let ()
  (define (make-composes-table ps)
    (list->vector (sort ps (lambda (a b) (< (car a) (car b))))))
    
  (define canon-composes
    (make-composes-table (for/list ([(k v) (in-hash compose-map)]
                                    #:when (k . <= . #xFFFFFFFF))
                           (cons k v))))
  (define count (vector-length canon-composes))

  (define long-canon-composes
    (make-composes-table (for/list ([(k v) (in-hash compose-map)]
                                    #:when (k . > . #xFFFFFFFF))
                           (cons k v))))
  (define long-count (vector-length long-canon-composes))

  (define-values (all-composes decomp-vector long-composes)
    (let ([decomp-pos-ht (make-hasheq)]
	  [counter count]
	  [extra null]
	  [long-counter 0]
	  [longs null])
      (hash-for-each decomp-ht
                     (lambda (k v)
                       ;; Use table of composed shorts:
                       (let ([key (composition-key (car v) (cdr v))])
                         (let ([pos 
                                (if (and ((car v) . <= . #xFFFF)
                                         ((cdr v) . <= . #xFFFF))
                                    (if (hash-ref compose-map key (lambda () #f))
                                        ;; Find index in comp vector:
                                        (let loop ([i 0])
                                          (if (= key (car (vector-ref canon-composes i)))
                                              i
                                              (loop (add1 i))))
                                        ;; Add to compose table:
                                        (begin0
                                         counter
                                         (set! extra (cons (cons key #f) extra))
                                         (set! counter (add1 counter))))
                                    ;; Use table of long+long sequences:
                                    (begin
                                      (set! long-counter (add1 long-counter))
                                      (set! longs (cons (cdr v) (cons (car v) longs)))
                                      (- long-counter)))])
                           (hash-set! decomp-pos-ht k pos)))))
      (values
       (list->vector (append (vector->list canon-composes)
			     (reverse extra)))
       (list->vector
	(sort (hash-map decomp-pos-ht cons)
              (lambda (a b) (< (car a) (car b)))))
       (list->vector (reverse longs)))))

  (printf "\n/* Subset of ~a decompositions used for canonical composition: */\n"
	  (vector-length all-composes))
  (printf "#define COMPOSE_TABLE_SIZE ~a\n\n" count)

  (let ([print-compose-data
	 (lambda (type suffix which composes count hex? row-len)
	   (printf "READ_ONLY static ~a utable_~a[] = {\n"
		   type suffix)
	   (let loop ([i 0])
	     (let ([v (which (vector-ref composes i))])
	       (if (= i (sub1 count))
		   (printf (format " ~a\n};\n" (if hex? "0x~x" "~a")) v)
		   (begin
		     (printf (format " ~a," (if hex? "0x~x" "~a")) v)
		     (when (zero? (modulo (add1 i) row-len))
		       (newline))
		     (loop (add1 i)))))))])
    (printf "/* utable_compose_pairs contains BMP pairs that form a canonical decomposition.\n")
    (printf "   The first COMPOSE_TABLE_SIZE are also canonical compositions, and they are\n")
    (printf "   sorted, so that a binary search can find the pair; the utable_compose_result\n")
    (printf "   table is in parallel for those COMPOSE_TABLE_SIZE to indicate the composed\n")
    (printf "   characters. Use scheme_needs_maybe_compose() from scheme.h to check whether\n")
    (printf "   a character might start a canonical decomposition. A zero as the second element\n")
    (printf "   of a composition means that it is a singleton decomposition.\n")
    (printf "   The entire utable_compose_pairs table is referenced by utable_decomp_indices\n")
    (printf "   to map characters to canonical decompositions.\n")
    (printf "   None of the [de]composition tables includes Hangol. */\n")
    (print-compose-data "unsigned int" "compose_pairs" car all-composes (vector-length all-composes) #t 8)
    (print-compose-data "unsigned int" "compose_result" cdr canon-composes count #t 8)
    (printf "\n")
    (printf "/* utable_compose_long_pairs contains a sequence of character pairs where at\n")
    (printf "   least one is outside the BMP, so it doesn't fit in utable_compose_pairs.\n")
    (printf "   Negative values in utable_decomp_indices map to this table; add one to\n")
    (printf "   the mapped index, negate, then multiply by 2 to find the pair. */\n")
    (print-compose-data "unsigned int" "compose_long_pairs" values long-composes (vector-length long-composes) #t 8)
    (printf "\n")
    (printf "/* utable_canon_compose_long_pairs repeats information from utable_compose_long_pairs,\n")
    (printf "   but for canonical compositions only. The two characters are combined by putting the\n")
    (printf "   lower 16 bits of the combined numbers in the low 32 bits, and then the next higher 10\n")
    (printf "   bits provide the remaining 5 bits of each character, and the array is sorted. The\n")
    (printf "   canon_compose_long_result array provides in parellel the composed character. */\n")
    (printf "#define LONG_COMPOSE_TABLE_SIZE ~a\n\n" long-count)
    (print-compose-data "mzlonglong" "canon_compose_long_pairs" car long-canon-composes long-count #t 8)
    (print-compose-data "unsigned int" "canon_compose_long_result" cdr long-canon-composes long-count #t 8)
    (printf "\n")
    (printf "/* utable_decomp_keys identifies characters that have a canonical decomposition;\n")
    (printf "   it is sorted, so binary search can be used, but use scheme_needs_decompose()\n")
    (printf "   from scheme.h to first determine whether a character may have a mapping in this table.\n")
    (printf "   (If scheme_needs_decompose(), may instead have a mapping in the kompat table.).\n")
    (printf "   The parallel utable_decomp_indices maps the corresponding character in this table\n")
    (printf "   to a composition pair in either utable_compose_pairs (when the index is positive) or\n")
    (printf "   utable_long_compose_pairs (when the index is negative). */\n")
    (printf "#define DECOMPOSE_TABLE_SIZE ~a\n\n" (vector-length decomp-vector))
    (print-compose-data "unsigned int" "decomp_keys" car decomp-vector (vector-length decomp-vector) #t 8)
    (print-compose-data "short" "decomp_indices" cdr decomp-vector (vector-length decomp-vector) #f 8)

    (let ([k-decomp-vector
	   (list->vector
	    (sort (hash-map k-decomp-map-ht cons)
                  (lambda (a b) (< (car a) (car b)))))])
      (printf "\n")
      (printf "/* utable_kompat_decomp_keys identifies characters that have a compatibility decomposition;\n")
      (printf "   it is sorted, and scheme_needs_decompose() is true for every key (but a character\n")
      (printf "   with scheme_needs_decompose(), may instead have a mapping in the canonical table.).\n")
      (printf "   The parallel utable_kompat_decomp_indices maps the corresponding character in this table\n")
      (printf "   to a composition string in kompat_decomp_strs with a length determined by the\n")
      (printf "   utable_kompat_decomp_lens table. The decomposition never contains characters that need\n")
      (printf "   further decomposition. */\n")
      (printf "\n#define KOMPAT_DECOMPOSE_TABLE_SIZE ~a\n\n"  (vector-length k-decomp-vector))
      (print-compose-data "unsigned int" "kompat_decomp_keys" car k-decomp-vector (vector-length k-decomp-vector) #t 8)
      (print-compose-data "char" "kompat_decomp_lens" cddr
			  k-decomp-vector (vector-length k-decomp-vector) #f 24)
      (print-compose-data "short" "kompat_decomp_indices" cadr
			  k-decomp-vector (vector-length k-decomp-vector) #f 16)
      (let ([l (list->vector (reverse k-decomp-strs))])
	(print-compose-data "unsigned short" "kompat_decomp_strs" values l (vector-length l) #t 8)))))
