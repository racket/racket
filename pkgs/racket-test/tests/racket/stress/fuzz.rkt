#lang racket

(require setup/dirs)

(define (flip-bit bs idx)
  (define-values [byte bit] (quotient/remainder idx 8))
  (define val (bytes-ref bs byte))
  (bytes-set! bs byte (bitwise-xor (expt 2 bit) val)))

(define (read-bytes bs)
  (sync
   (parameterize ([current-custodian (make-custodian)])
     (thread
      (lambda ()
        (custodian-limit-memory (current-custodian)
                                (* 512 (expt 2 20)))
        (with-handlers ([void void])
          (eval (parameterize ([read-accept-compiled #t]
                               [current-code-inspector (make-inspector)])
                  (with-input-from-bytes bs read)))))))))

(define (run-file fname seed0 #:write? [out-fname? #f])
  (define seed (or seed0 (+ 1 (random (expt 2 30)))))
  (printf "seed: ~s\nname: ~a\n" seed fname)
  (flush-output)
  (random-seed seed)
  (define bs  (file->bytes fname))
  (define len (* 8 (bytes-length bs)))
  (for ([i (in-range (quotient len 10000))]) (flip-bit bs (random len)))
  (with-handlers ([void void]) 
    (if out-fname?
        (begin
          (displayln (build-path (current-directory) out-fname?))
          (call-with-output-file (build-path (current-directory) out-fname?)
            (lambda (o)
              (write-bytes bs o))))
        (read-bytes bs))))

(define (go)
  (let ([seed0 #f] [file #f] [dir #f] [forever? #f] [global-seed #f] [write? #f])
    (command-line
     #:once-each
     ["--oo" "forever" (set! forever? #t)]
     #:once-any
     ["-g" global-seed* "global random seed"  (set! global-seed (string->number global-seed*))]
     ["-s" seed "random seed" (set! seed0 (string->number seed))]
     #:once-any
     ["-f" file* "filename to run"     (set! file file*)]
     ["-d" dir* "dir to run"          (set! dir dir*)]
     ["-c" "run over all collections" (set! dir (find-collects-dir))]
     #:once-any
     ["--write" filename "write mutated file" (begin (unless file
                                                       (error "--write requires -f"))
                                                     (set! write? filename))]
     #:args () (void))
    (cond [global-seed]
          [(getenv "RACKET_FUZZ_GLOBAL_SEED") => (lambda (v) (set! global-seed (string->number v)))]
          [else (set! global-seed (+ 1 (random (expt 2 30))))])
    (run seed0 file dir forever? global-seed write?)))

(define (run seed0 file dir forever? global-seed write?)
  (printf "Global seed: ~a\n" global-seed)
  (random-seed global-seed)
  (let loop ()
    (cond [file (run-file file seed0 #:write? write?)]
          [dir 
           (define files (sort (for/list ([f (in-directory dir)]
                                          #:when (regexp-match #rx"\\.zo" f))
                                 f)
                               #:key path->string
                               string<?))
           (for ([p files]) (run-file p seed0))]
          [else (printf "Nothing to do.\n")])
    (when forever? (loop))))

(module+ main
  (let ([seed0 #f] [file #f] [dir #f] [forever? #f] [global-seed #f] [write? #f])
    (command-line
     #:once-each
     ["--oo" "forever" (set! forever? #t)]
     #:once-any
     ["-g" global-seed* "global random seed"  (set! global-seed (string->number global-seed*))]
     ["-s" seed "random seed" (set! seed0 (string->number seed))]
     #:once-any
     ["-f" file* "filename to run"     (set! file file*)]
     ["-d" dir* "dir to run"          (set! dir dir*)]
     ["-c" "run over all collections" (set! dir (find-collects-dir))]
     #:once-any
     ["--write" filename "write mutated file" (begin (unless file
                                                       (error "--write requires -f"))
                                                     (set! write? filename))]
     #:args () (void))
    (cond [global-seed]
          [(getenv "RACKET_FUZZ_GLOBAL_SEED") => (lambda (v) (set! global-seed (string->number v)))]
          [else (set! global-seed (+ 1 (random (expt 2 30))))])
    (run seed0 file dir forever? global-seed write?)))

(module+ test
  (require racket/vector syntax/location)
  (parameterize ([current-command-line-arguments (vector-append #("-c") (current-command-line-arguments))])
    (dynamic-require (quote-module-path ".." main) #f))
  (module config info
    (define random? #t)))
