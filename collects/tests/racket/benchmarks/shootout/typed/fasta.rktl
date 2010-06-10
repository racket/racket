;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; fasta - benchmark
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla

(require racket/cmdline)

(define-type CumulativeTable (Listof (Pair Natural Float)))

(define +alu+
  (bytes-append
   #"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
   #"GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
   #"CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
   #"ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
   #"GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
   #"AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
   #"AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))

(define +iub+
  (list
   '(#\a . 0.27) '(#\c . 0.12) '(#\g . 0.12) '(#\t . 0.27) '(#\B . 0.02)
   '(#\D . 0.02) '(#\H . 0.02) '(#\K . 0.02) '(#\M . 0.02) '(#\N . 0.02)
   '(#\R . 0.02) '(#\S . 0.02) '(#\V . 0.02) '(#\W . 0.02) '(#\Y . 0.02)))

(define +homosapien+
  (list
   '(#\a . 0.3029549426680) '(#\c . 0.1979883004921)
   '(#\g . 0.1975473066391) '(#\t . 0.3015094502008)))

;; -------------

(define +line-size+ 60)

;; -------------------------------

(: make-random (Integer -> (Real -> Real)))
(define (make-random seed)
  (let* ((ia 3877) (ic 29573) (im 139968) (last seed))
    (lambda: ((max : Real))
      (set! last (modulo (+ ic (* last ia)) im))
      (/ (* max last) im))))

;; -------------------------------

(: make-cumulative-table ((Listof (Pair Char Float)) -> CumulativeTable))
(define (make-cumulative-table frequency-table)
  (let ([cumulative 0.0])
    (for/list: : CumulativeTable
               ([x : (Pair Char Float) frequency-table])
      (set! cumulative (+ cumulative (cdr x))) 
      (cons (char->integer (car x)) cumulative))))

;; -------------

(define random-next (make-random 42))
(define +segmarker+ ">")

;; -------------

(: select-random (CumulativeTable -> Natural))
(define (select-random cumulative-table)
  (let ((rvalue (random-next 1.0)))
    (let select-over-threshold ([table cumulative-table])
      (if (<= rvalue (cdar table))
          (caar table)
          (select-over-threshold (cdr table))))))

;; -------------

(: repeat-fasta (String String Integer Bytes Integer -> Void))
(define (repeat-fasta id desc n_ sequence line-length)
  (let ((seqlen (bytes-length sequence))
        (out (current-output-port)))
    (display (string-append +segmarker+ id " " desc "\n") out)
    (let: loop-o : Void
          ((n : Integer n_) (k : Integer 0))
      (unless (<= n 0) 
        (let ((m (min n line-length)))
          (let loop-i ((i 0) (k k))
            (if (>= i m) 
                (begin
                  (newline out)
                  (loop-o (- n line-length) k))
                (let ([k (if (= k seqlen) 0 k)])
                  (write-byte (bytes-ref sequence k) out)
                  (loop-i (add1 i) (add1 k))))))))))

;; -------------

(: random-fasta (String String Integer CumulativeTable Integer -> Void))
(define (random-fasta id desc n_ cumulative-table line-length)
  (let ((out (current-output-port)))
    (display (string-append +segmarker+ id " " desc "\n") out)
    (let: loop-o : Void ((n : Integer n_))
      (unless (<= n 0)
        (for ([i (in-range (min n line-length))])
          (write-byte (select-random cumulative-table) out))
        (newline out)
        (loop-o (- n line-length))))))

;; -------------------------------
  
(let ((n (command-line #:args (n) (assert (string->number (assert n string?)) exact-integer?))))
    
  (repeat-fasta "ONE" "Homo sapiens alu" (* n 2) +alu+ +line-size+)
  
  (random-fasta "TWO" "IUB ambiguity codes" (* n 3)
                (make-cumulative-table +iub+) +line-size+)
  
  (random-fasta "THREE" "Homo sapiens frequency" (* n 5)
                (make-cumulative-table +homosapien+) +line-size+))
