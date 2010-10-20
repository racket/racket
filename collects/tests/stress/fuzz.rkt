#lang racket

(require racket/runtime-path)

(define (flip n bit)
  (define new-bit (expt 2 bit))
  (bitwise-xor new-bit n))

(define (flip-file bs idx)
  (define-values (byte bit) (quotient/remainder idx 8))  
  (define val (bytes-ref bs byte))
  (bytes-set! bs byte (flip val bit)))

(define (run-file bs)
  (eval (parameterize ([read-accept-compiled #t])
          (with-input-from-bytes bs read))))


(define sd #f)
(define fl #f)
(define dir #f)

(define (run fname [seed (or sd (+ 1 (random (expt 2 30))))])
  (printf "DrDr Ignore! random-seed ~s\n" seed)
  (random-seed seed)
  (define bs (file->bytes fname))
  (define len (* 8 (bytes-length bs)))
  (printf "name: ~a\n" fname)
  (for ([i (in-range (quotient len 10000))])
    (flip-file bs (random len)))
  (with-handlers ([void void])
    (run-file bs)))


(define collects-dir 
  (find-executable-path (find-system-path 'exec-file)
			(find-system-path 'collects-dir)))

(command-line 
 #:once-each
 ["-s" seed "random seed" (set! sd (string->number seed))]
 #:once-any
 ["-f" file "filename to run"
  (set! fl file)]
 ["-d" dir* "dir to run" (set! dir dir*)]
 ["-c" "run over all collections"
 (set! dir collects-dir)]
  #:args () (void))

(cond [fl (run fl)]
      [dir (for ([p (in-directory dir)]
		 #:when (regexp-match "\\.zo" p))
	      (run p))])
