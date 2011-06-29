#lang racket

(require setup/dirs)

(define (flip-bit bs idx)
  (define-values [byte bit] (quotient/remainder idx 8))
  (define val (bytes-ref bs byte))
  (bytes-set! bs byte (bitwise-xor (expt 2 bit) val)))

(define (run-file bs)
  (eval (parameterize ([read-accept-compiled #t])
          (with-input-from-bytes bs read))))

(define (run fname seed0)
  (define seed (or seed0 (+ 1 (random (expt 2 30)))))
  (printf "seed: ~s\nname: ~a\n" seed fname)
  (flush-output)
  (random-seed seed)
  (define bs  (file->bytes fname))
  (define len (* 8 (bytes-length bs)))
  (for ([i (in-range (quotient len 10000))]) (flip-bit bs (random len)))
  (with-handlers ([void void]) (run-file bs)))

(let ([seed #f] [file #f] [dir #f])
  (command-line
   #:once-each
   ["-s" seed "random seed" (set! seed (string->number seed))]
   #:once-any
   ["-f" file "filename to run"     (set! file file)]
   ["-d" dir* "dir to run"          (set! dir dir*)]
   ["-c" "run over all collections" (set! dir (find-collects-dir))]
   #:args () (void))
  (cond [file (run file seed)]
        [dir (for ([p (in-directory dir)]
                   #:when (regexp-match #rx"\\.zo" p))
               (run p seed))]
        [else (printf "Nothing to do.\n")]))
