;pregexp.ss
;; Originally:
;;   ;Portable regular expressions for Scheme
;;   ;Dorai Sitaram
;;   ;http://www.ccs.neu.edu/~dorai/pregexp/pregexp.html
;; but `pregexp' functionality is now built into MzScheme, so
;; this is mostly a wrapper module.

(module pregexp mzscheme
  (require (lib "string.ss")
	   (lib "kw.ss"))
  (provide pregexp
	   pregexp-match-positions
	   pregexp-match
	   pregexp-split
	   pregexp-replace
	   pregexp-replace*
	   (rename regexp-quote pregexp-quote))

  ;; Most of this code just checks arguments, so that errors are reported as
  ;;  from `pregexp...' instead of `regexp...'. We need a better way to
  ;;  do that than just writing the checks again.

  (define (pattern->pregexp who pattern)
    (cond
     [(bytes? pattern) (byte-pregexp pattern)]
     [(string? pattern) (pregexp pattern)]
     [(regexp? pattern) pattern]
     [(byte-regexp? pattern) pattern]
     [else (raise-type-error who "regexp, byte-regexp, string, or byte string"
			     pattern)]))

  (define (check-input who input)
    (unless (or (string? input) (bytes? input) (input-port? input))
      (raise-type-error who "string, byte string, or input port" input)))

  (define (check-start-end-k who input start-k end-k)
    (unless (and (number? start-k) (exact? start-k) (start-k . >= . 0))
      (raise-type-error who "exact non-negative integer" start-k))
    (when end-k
      (unless (and (number? end-k) (exact? end-k) (end-k . >= . 0))
	(raise-type-error who "exact non-negative integer or #f" end-k))
      (unless (start-k . <= . end-k)
	(raise-mismatch-error who 
			      (format "starting index ~a is not less than ending index: "
				      start-k)
			      end-k)))
    (let ([len (cond
		[(bytes? input) (bytes-length bytes)]
		[(string? input) (string-length input)]
		[else #f])])
      (when len
	(unless (start-k . <= . len)
	  (raise-mismatch-error who (format "starting index ~a is out of range [0,~a] for input: "
					    start-k
					    len)
				input))
	(when end-k
	  (unless (end-k . <= . len)
	    (raise-mismatch-error who (format "ending index ~a is out of range [~a,~a] for input: "
					      end-k
					      start-k
					      len)
				  input))))))
  
  (define (check-output who output)
    (when output
      (unless (or (output-port? output))
	(raise-type-error who "output port or #f" output))))

  (define (check-insert who input insert)
    (unless (or (string? insert) (bytes? insert))
      (raise-type-error who "string or byte string" insert))
    (when (and (bytes? insert) (string? input))
      (raise-mismatch-error who "cannot replace a string with a byte string: " insert)))


  (define/kw (pregexp-match pattern input #:optional [start-k 0] [end-k #f] [output-port #f])
    (let ([pattern (pattern->pregexp 'pregexp-match pattern)])
      (check-input 'pregexp-match input)
      (check-start-end-k 'pregexp-match input start-k end-k)
      (check-output 'pregexp-match output-port)
      (regexp-match pattern input start-k end-k output-port)))

  (define/kw (pregexp-match-positions pattern input #:optional [start-k 0] [end-k #f] [output-port #f])
    (let ([pattern (pattern->pregexp 'pregexp-match-positions pattern)])
      (check-input 'pregexp-match-positions input)
      (check-start-end-k 'pregexp-match-positions input start-k end-k)
      (check-output 'pregexp-match-positions output-port)
      (regexp-match pattern input start-k end-k output-port)))
  
  (define/kw (pregexp-split pattern string #:optional [start 0] [end #f])
    (let ([pattern (pattern->pregexp 'pregexp-split pattern)])
      (check-input 'pregexp-split string)
      (check-start-end-k 'pregexp-split string start end)
      (regexp-split pattern string start end)))

  (define/kw (pregexp-replace pattern input insert)
    (let ([pattern (pattern->pregexp 'regexp-replace pattern)])
      (check-input 'pregexp-replace input)
      (check-insert 'pregexp-replace input insert)
      (regexp-replace pattern input insert)))

  (define/kw (pregexp-replace* pattern input insert)
    (let ([pattern (pattern->pregexp 'regexp-replace* pattern)])
      (check-input 'pregexp-replace* input)
      (check-insert 'pregexp-replace* input insert)
      (regexp-replace* pattern input insert))))
