#lang racket/base

;;   The Computer Language Shootout
;;   http://shootout.alioth.debian.org/
(require racket/future racket/require (for-syntax racket/base) racket/port
         racket/place
         (filtered-in (λ (name) (regexp-replace #rx"unsafe-" name ""))
                      racket/unsafe/ops))

(define (all-counts len dna)
  (define table (make-hasheq))
  (define seq (make-bytes len))
  (for ([s (in-range (- (bytes-length dna) len) -1 -1)])
    (bytes-copy! seq 0 dna s (+ s len))
    (define key (string->symbol (bytes->string/utf-8 seq)))
    (define cnt (hash-ref table key 0))
    (hash-set! table key (add1 cnt)))
  table)

(define (write-freqs table port)
  (define content (hash-map table cons))
  (define total (exact->inexact (apply + (map cdr content))))
  (for ([a (sort content > #:key cdr)])
    (fprintf port "~a ~a\n"
             (car a)
             (real->decimal-string (fl* 100. (fl/ (fx->fl (cdr a)) total)) 3))))

(define (write-one-freq table key port)
  (define cnt (hash-ref table key 0))
  (fprintf port "~a\t~a\n" cnt key))

(define-syntax-rule (at-place (var ...) body ...)
  (let ()
    (define p (place ch
                     (define var (place-channel-get ch)) ...
                     (place-channel-put ch (let () body ...))))
    (place-channel-put p var) ...
    p))

(define (main . _)    
  (define dna-shared
    (let ()
      (define in (current-input-port))
      ;; Skip to ">THREE ..."          
      (regexp-match #rx#"(?m:^>THREE.*$)" in)
      (define s (open-output-bytes))
      ;; Copy everything but newlines to s:            
      (for ([l (in-bytes-lines in)])
        (write-bytes l s))
      ;; Extract the bytes from s:            
      (define dna (get-output-bytes s))
      (define sb (make-shared-bytes (bytes-length dna)))
      (for ([i (in-range (bytes-length dna))])
        (bytes-set! sb i (fx- (bytes-ref dna i) 32)))
      sb))
  (define l
    (append
     ;; 1/2-nucleotide counts:   
     (for/list ([i '(1 2)])
       (at-place (i dna-shared)
         (define pr (open-output-bytes))
         (write-freqs (all-counts i dna-shared) pr)
         (newline pr)
         (get-output-bytes pr)))
     
     ;; Specific sequences:
     (for/list ([seq '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT")])
       (at-place (seq dna-shared)
         (define pr (open-output-bytes))
         (write-one-freq (all-counts (string-length seq) dna-shared)
                         (string->symbol seq) pr)
         (get-output-bytes pr)))))

  (for ([p l]) (write-bytes (place-channel-get p))))
(provide main)




