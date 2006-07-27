;; The Great Computer Language Shootout
;; http://shootout.alioth.debian.org/
;;
;; fasta - benchmark
;;
;; Derived from the Chicken variant, which was
;; Contributed by Anthony Borla

(module fasta mzscheme 

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
  
  (define (make-random seed)
    (let* ((ia 3877) (ic 29573) (im 139968) (last seed))
      (lambda (max)
	(set! last (modulo (+ ic (* last ia)) im))
	(/ (* max last) im) )))

  ;; -------------------------------

  (define (make-cumulative-table frequency-table)
    (let ((cumulative 0.0))
      (map
       (lambda (x) 
	 (set! cumulative (+ cumulative (cdr x))) 
	 (cons (char->integer (car x)) cumulative))
       frequency-table)))

  ;; -------------
  
  (define random-next (make-random 42))
  (define +segmarker+ ">")

  ;; -------------

  (define (select-random cumulative-table)
    (let ((rvalue (random-next 1.0)))
      (select-over-threshold rvalue cumulative-table)))
  
  (define (select-over-threshold rvalue table)
    (if (<= rvalue (cdar table))
	(caar table)
	(select-over-threshold rvalue (cdr table))))

  ;; -------------
  
  (define (repeat-fasta id desc n_ sequence line-length)
    (let ((seqlen (bytes-length sequence))
	  (out (current-output-port)))
      (display (string-append +segmarker+ id " " desc "\n") out)
      (let loop-o ((n n_) (k 0))
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

  (define (random-fasta id desc n_ cumulative-table line-length)
    (let ((out (current-output-port)))
      (display (string-append +segmarker+ id " " desc "\n") out)
      (let loop-o ((n n_))
	(unless (<= n 0)
	  (let ((m (min n line-length)))
	    (let loop-i ((i 0))
	      (unless (>= i m)
		(write-byte (select-random cumulative-table) out)
		(loop-i (add1 i))))
	    (newline out)
	    (loop-o (- n line-length)))))))
  
  ;; -------------------------------
  
  (define (main args)
    (let ((n (string->number (vector-ref args 0))))
      
      (repeat-fasta "ONE" "Homo sapiens alu" (* n 2) +alu+ +line-size+)
      
      (random-fasta "TWO" "IUB ambiguity codes" (* n 3)
		    (make-cumulative-table +iub+) +line-size+)
      
      (random-fasta "THREE" "Homo sapiens frequency" (* n 5)
		    (make-cumulative-table +homosapien+) +line-size+) ))
  
  ;; -------------------------------

  (main (current-command-line-arguments)))

