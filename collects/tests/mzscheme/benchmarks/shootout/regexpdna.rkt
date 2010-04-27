;; ---------------------------------------------------------------------
;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; Tested with PCRE [compiler must be built with PCRE already installed
;; else other regex routines (with different behaviours) will be used].
;; Regex performance appears reasonable, but file loading [of 'large'
;; files] performance requires tweaking to effect a significant improvement.
;;
;; Contributed by Anthony Borla
;; ---------------------------------------------------------------------

#lang scheme/base
(require scheme/port)

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

(define (ci-byte-regexp s)
  (byte-regexp (bytes-append #"(?i:" s #")")))

;; -------------------------------

(define (match-count str rx offset cnt)
  (let ([m (regexp-match-positions rx str offset)])
    (if m
        (match-count str rx (cdar m) (add1 cnt))
        cnt)))

;; --------------

(define (replace-all rx str new)
  (let ([out (open-output-bytes)])
    (let loop ([pos 0])
      (let ([m (regexp-match-positions rx str pos)])
        (if m
            (begin
              (write-bytes str out pos (caar m))
              (write-bytes new out)
              (loop (cdar m)))
            (write-bytes str out pos))))
    (get-output-bytes out)))

;; -------------------------------

(define (input->bytes)
  (let ([b (open-output-bytes)])
    (copy-port (current-input-port) b)
    (get-output-bytes b)))

;; -------------------------------

;; Load sequence and record its length
(let* ([orig (input->bytes)]
       [filtered (replace-all #rx#"(>.*?\n)|\n" orig #"")])

  ;; Perform regexp counts
  (for-each
   (lambda (i)
     (printf "~a ~a\n" i (match-count filtered (ci-byte-regexp i) 0 0)))
   VARIANTS)

  ;; Perform regexp replacements, and record sequence length
  (let ([replaced
         (let loop ([sequence filtered]
                    [IUBS IUBS])
           (if (null? IUBS)
               sequence
               (loop (replace-all (byte-regexp (caar IUBS)) sequence (cadar IUBS))
                     (cdr IUBS))))])
    ;; Print statistics
    (printf "~%~A~%~A~%~A~%" 
            (bytes-length orig)
            (bytes-length filtered)
            (bytes-length replaced))))
