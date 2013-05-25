#lang racket/base

;; The Computer Language Benchmarks Game
;; http://shootout.alioth.debian.org/
;;
;; Based on a version by by Anthony Borla
;; Parallelize by Sam Tobin-Hochstadt

(require racket/port racket/place)

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

(define (main . _)
  ;; Load sequence and record its length
  (define orig (port->bytes))
  (define filtered (regexp-replace* #rx#"(?:>.*?\n)|\n" orig #""))
  ;; Perform regexp counts
  (define pls
    (for/list ([i (in-list VARIANTS)])
      (define p (place ch
                       (define seq (place-channel-get ch))
                       (define i (place-channel-get ch))
                       (place-channel-put ch (length (regexp-match-positions* (byte-regexp i) seq)))))
      (place-channel-put p filtered)
      (place-channel-put p i)
      p))

  ;; Perform regexp replacements, and record sequence length
  (define replaced
    (for/fold ([sequence filtered]) ([IUB IUBS])
      (regexp-replace* (byte-regexp (car IUB)) sequence (cadr IUB))))
  (for ([p pls] [i VARIANTS]) (printf "~a ~a\n" i (place-channel-get p)))

  ;; Print statistics
  (printf "\n~a\n~a\n~a\n"
          (bytes-length orig)
          (bytes-length filtered)
          (bytes-length replaced)))
(provide main)
