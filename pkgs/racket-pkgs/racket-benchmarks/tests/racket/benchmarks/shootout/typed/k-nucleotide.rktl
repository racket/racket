;;   The Computer Language Shootout
;;   http://shootout.alioth.debian.org/

(: all-counts (Integer String -> (HashTable Symbol Integer)))
(define (all-counts len dna)
  (let: ([table : (HashTable Symbol Integer) (make-hasheq)]
         [seq : String (make-string len)])
    (for: ([s : Integer (in-range (- (string-length dna) len) -1 -1)])
      (string-copy! seq 0 dna s (+ s len))
      (let ([key (string->symbol seq)])
        (let ([cnt (hash-ref table key (lambda () 0))])
          (hash-set! table key (add1 cnt)))))
    table))

(: write-freqs ((HashTable Symbol Integer) -> Void))
(define (write-freqs table)
  (let*: ([content : (Listof (Pair Symbol Integer)) (hash-map table (inst cons Symbol Integer))]
          [total : Float (exact->inexact (apply + (map (inst cdr Symbol Integer) content)))])
    (for: ([a : (Pair Symbol Integer) ((inst sort (Pair Symbol Integer) Integer) content > #:key cdr)])
      (printf "~a ~a\n" 
              (car a) 
              (real->decimal-string (* 100 (/ (cdr a) total)) 3)))))

(: write-one-freq ((HashTable Symbol Integer) Symbol -> Void))
(define (write-one-freq table key)
  (let ([cnt (hash-ref table key (lambda () 0))])
    (printf "~a\t~a\n" cnt key)))

(define dna
  (let ([in (current-input-port)])
    ;; Skip to ">THREE ..."
    (regexp-match #rx#"(?m:^>THREE.*$)" in)
    (let ([s (open-output-string)])
      ;; Copy everything but newlines to s:
      (for ([l (in-bytes-lines in)])
        (write-bytes l s))
      ;; Extract the string from s:
      (string-upcase (get-output-string s)))))

;; 1-nucleotide counts:
(write-freqs (all-counts 1 dna))
(newline)

;; 2-nucleotide counts:
(write-freqs (all-counts 2 dna))
(newline)

;; Specific sequences:
(for: : Void
      ([seq : String '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT")]) 
  (write-one-freq (all-counts (string-length seq) dna)
                  (string->symbol seq)))
