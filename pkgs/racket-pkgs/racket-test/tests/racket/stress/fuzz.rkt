#lang racket

(require setup/dirs)

(define (flip-bit bs idx)
  (define-values [byte bit] (quotient/remainder idx 8))
  (define val (bytes-ref bs byte))
  (bytes-set! bs byte (bitwise-xor (expt 2 bit) val)))

(define (run-file bs)
  (sync
   (parameterize ([current-custodian (make-custodian)])
     (thread
      (lambda ()
        (custodian-limit-memory (current-custodian)
                                (* 512 (expt 2 20)))
        (with-handlers ([void void])
          (eval (parameterize ([read-accept-compiled #t])
                  (with-input-from-bytes bs read)))))))))

(define (run fname seed0)
  (define seed (or seed0 (+ 1 (random (expt 2 30)))))
  (printf "seed: ~s\nname: ~a\n" seed fname)
  (flush-output)
  (random-seed seed)
  (define bs  (file->bytes fname))
  (define len (* 8 (bytes-length bs)))
  (for ([i (in-range (quotient len 10000))]) (flip-bit bs (random len)))
  (with-handlers ([void void]) (run-file bs)))

(let ([seed0 #f] [file #f] [dir #f] [forever? #f] [global-seed #f])
  (command-line
   #:once-each
   ["--oo" "forever" (set! forever? #t)]
   #:once-any
   ["-g" global-seed* "gloabl random seed"  (set! global-seed (string->number global-seed*))]
   ["-s" seed "random seed" (set! seed0 (string->number seed))]
   #:once-any
   ["-f" file* "filename to run"     (set! file file*)]
   ["-d" dir* "dir to run"          (set! dir dir*)]
   ["-c" "run over all collections" (set! dir (find-collects-dir))]
   #:args () (void))
  (unless global-seed
    (set! global-seed (+ 1 (random (expt 2 30)))))
  (printf "Global seed: ~a\n" global-seed)
  (random-seed global-seed)
  (let loop ()
    (cond [file (run file seed0)]
          [dir 
	   (define files (sort (for/list ([f (in-directory dir)]
					  #:when (regexp-match #rx"\\.zo" f))
			         f)
			       #:key path->string
			       string<?))
	   (for ([p files]) (run p seed0))]
          [else (printf "Nothing to do.\n")])
    (when forever? (loop))))

(module test racket/base
  (require syntax/location)
  (parameterize ([current-command-line-arguments (vector "-c")])
    (dynamic-require (quote-module-path "..") #f))
  (module config info
    (define random? #t)))
