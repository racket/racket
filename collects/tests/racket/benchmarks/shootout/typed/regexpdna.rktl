;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Based on a version by by Anthony Borla

(require racket/port)

;; -------------------------------

(define VARIANTS
  '(#"agggtaaa|tttaccct" #"[cgt]gggtaaa|tttaccc[acg]" #"a[act]ggtaaa|tttacc[agt]t"
    #"ag[act]gtaaa|tttac[agt]ct" #"agg[act]taaa|ttta[agt]cct" #"aggg[acg]aaa|ttt[cgt]ccct"
    #"agggt[cgt]aa|tt[acg]accct" #"agggta[cgt]a|t[acg]taccct" #"agggtaa[cgt]|[acg]ttaccct"))


(define IUBS
  '((#"B" #"(c|g|t)") (#"D" #"(a|g|t)") (#"H" #"(a|c|t)")
    (#"K" #"(g|t)") (#"M" #"(a|c)") (#"N" #"(a|c|g|t)")
    (#"R" #"(a|g)") (#"S" #"(c|g)") (#"V" #"(a|c|g)")
    (#"W" #"(a|t)") (#"Y" #"(c|t)")))

;; -------------------------------

(: ci-byte-regexp (Bytes -> Byte-Regexp))
(define (ci-byte-regexp s)
  (byte-regexp (bytes-append #"(?i:" s #")")))

;; -------------------------------

(: match-count (Bytes Byte-Regexp Integer Integer -> Integer))
(define (match-count str rx offset cnt)
  (let ([m (regexp-match-positions rx str offset)])
    (if m
        (match-count str rx (cdr (assert (car m))) (add1 cnt))
        cnt)))

;; -------------------------------

;; Load sequence and record its length
(let* ([orig (port->bytes)]
       [filtered (regexp-replace* #rx#"(?:>.*?\n)|\n" orig #"")])

  ;; Perform regexp counts
  (for ([i (in-list VARIANTS)])
    (printf "~a ~a\n" i (match-count filtered (ci-byte-regexp i) 0 0)))

  ;; Perform regexp replacements, and record sequence length
  (let ([replaced
         (for/fold: : Bytes
                    ([sequence : Bytes filtered])
                    ([IUB : (List Bytes Bytes) IUBS])
           (regexp-replace* (byte-regexp (car IUB)) sequence (cadr IUB)))])
    ;; Print statistics
    (printf "\n~a\n~a\n~a\n" 
            (bytes-length orig)
            (bytes-length filtered)
            (bytes-length replaced))))
