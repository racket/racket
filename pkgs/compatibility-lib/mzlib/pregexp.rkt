;; pregexp.rkt
;; Originally:
;;   ;Portable regular expressions for Scheme
;;   ;Dorai Sitaram
;;   ;http://www.ccs.neu.edu/~dorai/pregexp/pregexp.html
;; but `pregexp' functionality is now built into Racket, so
;; this is mostly a wrapper module.

(module pregexp mzscheme
  (require (only racket/base regexp-quote regexp-split)
	   mzlib/kw)
  (provide pregexp
	   pregexp-match-positions
	   pregexp-match
	   pregexp-split
	   pregexp-replace
	   pregexp-replace*
	   (rename regexp-quote pregexp-quote))

  (define (pattern->pregexp who pattern)
    (cond
     [(bytes? pattern) (byte-pregexp pattern)]
     [(string? pattern) (pregexp pattern)]
     [(regexp? pattern) pattern]
     [(byte-regexp? pattern) pattern]
     [else (raise-argument-error who "(or/c regexp? byte-regexp? string? bytes?)"
			     pattern)]))

  (define/kw (pregexp-match pattern input #:optional [start-k 0] [end-k #f] [output-port #f])
    (let ([pattern (pattern->pregexp 'pregexp-match pattern)])
      (regexp-match pattern input start-k end-k output-port)))

  (define/kw (pregexp-match-positions pattern input #:optional [start-k 0] [end-k #f] [output-port #f])
    (let ([pattern (pattern->pregexp 'pregexp-match-positions pattern)])
      (regexp-match-positions pattern input start-k end-k output-port)))
  
  (define/kw (pregexp-split pattern string #:optional [start 0] [end #f])
    (let ([pattern (pattern->pregexp 'pregexp-split pattern)])
      (regexp-split pattern string start end)))

  (define/kw (pregexp-replace pattern input insert)
    (let ([pattern (pattern->pregexp 'regexp-replace pattern)])
      (regexp-replace pattern input insert)))

  (define/kw (pregexp-replace* pattern input insert)
    (let ([pattern (pattern->pregexp 'regexp-replace* pattern)])
      (regexp-replace* pattern input insert))))
