(module run mzscheme
  (require (only scheme/runtime-path define-runtime-path)
           racket/port)
  (define input-map
    `(
      ("ackermann" "11")
      ("ary" "300000")
      ("binarytrees" "16")
      ("chameneos" "100000")
      ("cheapconcurrency" "2000")
      ("echo" "200000")
      ("except" "2000000")
      ("fannkuch" "10")
      ("fasta" "1000000")
      ("fibo" "40")
      ("hash" "2000000")
      ("hash2" "750")
      ("heapsort" "1500000")
      ("lists" "18")
      ("mandelbrot" "3000")
      ("matrix" "10000")
      ("moments" #f ,(lambda () (mk-moments-input)))
      ("nbody" "2000000")
      ("nestedloop" "30")
      ("nothing" "")
      ("nsieve" "12")
      ("nsievebits" "12")
      ("partialsums" "2500000")
      ("pidigits" "4000")
      ("pidigits1" "4000")
      ("random" "40000000")
      ("recursive" "11")
      ("regexmatch")
      ("regexpdna" #f ,(lambda () (mk-regexpdna-input)))
      ("reversecomplement" #f ,(lambda () (mk-revcomp-input)))
      ("k-nucleotide" #f ,(lambda () (mk-knuc-input)))
      ("reversefile" #f ,(lambda () (mk-reversefile-input)))
      ("sieve" "25000")
      ("spellcheck")
      ("spectralnorm" "5500")
      ("strcat" "50000000")
      ("sumcol" #f ,(lambda () (mk-sumcol-input)))
      ("wc" #f ,(lambda () (mk-wc-input)))
      ("wordfreq" #f ,(lambda () (mk-wordfreq-input)))
      ))

  (define-runtime-path here ".")

  (define (dynreq f)
    (parameterize ([current-load-relative-directory here]
                   [current-output-port (open-output-nowhere)])
      (dynamic-require f #f)))

  (define (mk-fasta n suffix)
    (let ([f (build-path (find-system-path 'temp-dir) (string-append "fasta-" suffix))])
      (unless (file-exists? f)
        (printf "Building FASTA ~a output for input: ~a\n" n f)
        (with-output-to-file f
          (lambda ()
            (parameterize ([current-command-line-arguments (vector n)]
                           [current-load-relative-directory here])
              (dynamic-require "fasta.rkt" #f)))))
      f))

  (define (mk-revcomp-input)
    (mk-fasta "5000000" "5m"))

  (define (mk-knuc-input)
    (mk-fasta "1000000" "1m"))

  (define (mk-regexpdna-input)
    (mk-fasta "5000000" "5m"))

  (define (mk-sumcol n suffix)
    (let ([f (build-path (find-system-path 'temp-dir) (string-append "sumcol-" suffix))])
      (unless (file-exists? f)
        (printf "Building sumcol ~a input: ~a\n" n f)
        (let ([c (with-input-from-file (build-path (collection-path "tests")
                                                   "racket"
                                                   "benchmarks"
                                                   "shootout"
                                                   "sumcol-input.txt")
                   (lambda ()
                     (read-bytes 10000)))])
          (with-output-to-file f
            (lambda ()
              (let loop ([n n])
                (unless (zero? n)
                  (printf "~a" c)
                  (loop (sub1 n))))))))
      f))

  (define (mk-moments-input)
    (mk-sumcol 2000 "2k"))

  (define (mk-reversefile-input)
    (mk-sumcol 3000 "3k"))

  (define (mk-sumcol-input)
    (mk-sumcol 10000 "10k"))

  (define (mk-wc-input)
    (mk-sumcol 20000 "20k"))

  (define (mk-wordfreq-input)
    (mk-sumcol 10000 "10k"))

  (define iters
    (let ([len (vector-length (current-command-line-arguments))])
      (unless (<= 1 len 3)
        (printf "provide ~athe name of a benchmark on the command line, an optional version of the benchmark to run, and an optional iteration count\n"
                (if (zero? len) "" "ONLY "))
        (exit))
      (if (= len 3)
          (string->number (vector-ref (current-command-line-arguments) 2))
          1)))
      
  (let* ([version (if (< (vector-length (current-command-line-arguments)) 2)
                      "racket"
                      (vector-ref (current-command-line-arguments) 1))]
         [bench   (vector-ref (current-command-line-arguments) 0)]
         [prog    (cond
                    ((string=? version "racket")                  (format "~a.rkt" bench))
                    ((string=? version "typed-scheme")            (format "typed/~a-non-optimizing.rkt" bench))
                    ((string=? version "typed-scheme-optimizing") (format "typed/~a-optimizing.rkt" bench))
                    (else (error 'run "unknown version ~a" version)))])
    (let ([m (assoc bench input-map)])
      (unless m
        (error 'run "cannot find input for ~a" bench))
      (when (null? (cdr m))
        (error 'run "don't know input for ~a" bench))
      (let loop ([n iters])
        (parameterize ([current-command-line-arguments 
                        (if (cadr m)
                            (vector (cadr m))
                            (vector))]
                       [current-input-port
                        (if (null? (cddr m))
                            (current-input-port)
                            (open-input-file ((caddr m))))])
          (parameterize ([current-namespace (make-namespace)])
            (collect-garbage)
            (collect-garbage)
            (time (dynreq prog))))
        (unless (= n 1)
          (loop (sub1 n)))))))
