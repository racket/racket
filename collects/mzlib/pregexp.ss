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

  (define (pattern->pregexp pattern)
    (cond
     [(bytes? pattern) (byte-pregexp pattern)]
     [(string? pattern) (pregexp pattern)]
     [else pattern]))

  (define/kw (pregexp-match pattern input #:optional [start-k 0] [end-k #f] [output-port #f])
    (regexp-match (pattern->pregexp pattern) input start-k end-k output-port))

  (define/kw (pregexp-match-positions pattern input #:optional [start-k 0] [end-k #f] [output-port #f])
    (regexp-match (pattern->pregexp pattern) input start-k end-k output-port))
  
  (define/kw (pregexp-split pattern string #:optional [start 0] [end #f])
    (pregexp-split (pattern->pregexp pattern) string start end))

  (define/kw (pregexp-replace pattern input insert)
    (regexp-replace (pattern->pregexp pattern) input insert))

  (define/kw (pregexp-replace* pattern input insert)
    (regexp-replace* (pattern->pregexp pattern) input insert)))
