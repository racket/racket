
;; When autoconf produces `configure', it includes many
;;  options that do not apply to PLT software. We want to
;;  get rid of them, so that `configure --help' produces
;;  valid information.

(let loop ([skip #f])
  (let ([l (read-line)])
    (unless (eof-object? l)
      (cond
       [(regexp-match "^Installation directories:" l)
	(printf "~a~n" l)
	;; start skipping lines, except --prefix:
	(loop 1)]
       [(and skip (regexp-match "^(_AC)?EOF$" l))
	;; Done skipping. Output line, then add blank lines
	;; to keep the rest of the file in sync.
	(printf "~a~n" l)
	(let loop ([n skip])
	  (unless (zero? n)
	    (newline)
	    (loop (sub1 n))))
	(loop #f)]
       [(and skip (regexp-match "^ *--prefix=" l))
	(printf "  --prefix=TARGETDIR      install to TARGETDIR (usually ..../plt)~n")
	(loop skip)]
       [skip
	(loop (add1 skip))]
       [else
	(printf "~a~n" l)
	(loop skip)]))))
