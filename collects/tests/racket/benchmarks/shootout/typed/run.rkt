(module run mzscheme
  (require (only scheme/runtime-path define-runtime-path)
           racket/port)
  (define input-map
    `(
      ("ackermann" "11")
      ("ary" "9000")
      ("binarytrees" "16")
      ("chameneos" "1000000")
      ("cheapconcurrency" "15000")
      ("echo" "150000")
      ("except" "2500000")
      ("fannkuch" "10")
      ("fasta" "25000000")
      ("fibo" "32")
      ("hash" "100000")
      ("hash2" "200")
      ("heapsort" "100000")
      ("lists" "18")
      ("mandelbrot" "3000")
      ("matrix" "600")
      ("moments" #f ,(lambda () (mk-sumcol-input)))
      ("nbody" "20000000")
      ("nestedloop" "18")
      ("nsieve" "9")
      ("nsievebits" "11")
      ("partialsums" "2500000")
      ("pidigits" "2500")
      ("pidigits1" "2500")
      ("random" "900000")
      ("recursive" "11")
      ("regexmatch")
      ("regexpdna" #f ,(lambda () (mk-regexpdna-input)))
      ("reversecomplement" #f ,(lambda () (mk-revcomp-input)))
      ("k-nucleotide" #f ,(lambda () (mk-knuc-input)))
      ("reversefile" #f ,(lambda () (mk-sumcol-input)))
      ("sieve" "1200")
      ("spellcheck")
      ("spectralnorm" "5500")
      ("spectralnorm-unsafe" "5500")
      ("strcat" "40000")
      ("sumcol" #f ,(lambda () (mk-sumcol-input)))
      ("wc" #f ,(lambda () (mk-sumcol-input)))
      ("wordfreq" #f ,(lambda () (mk-sumcol-input)))
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
            (parameterize ([current-command-line-arguments (vector n)])
              (dynreq "../fasta.rkt"))))) ; we can use the untyped version to generate inputs
      f))

  (define (mk-revcomp-input)
    (mk-fasta "2500000" "2m5"))

  (define (mk-knuc-input)
    (mk-fasta "1000000" "1m"))

  (define (mk-regexpdna-input)
    (mk-fasta "5000000" "5m"))

  (define (mk-sumcol-input)
    (let ([f (build-path (find-system-path 'temp-dir) "sumcol-21k")])
      (unless (file-exists? f)
        (printf "Building sumcol 21000 input: ~a\n" f)
        (let ([c (with-input-from-file (build-path (collection-path "tests")
                                                   "racket"
                                                   "benchmarks"
                                                   "shootout"
                                                   "sumcol-input.txt")
                   (lambda ()
                     (read-bytes 10000)))])
          (with-output-to-file f
            (lambda ()
              (let loop ([n 21000])
                (unless (zero? n)
                  (printf "~a" c)
                  (loop (sub1 n))))))))
      f))

  (define iters
    (let ([len (vector-length (current-command-line-arguments))])
      (unless (<= 2 len 3)
        (printf "provide ~athe name of a benchmark on the command line, which version of the benchmark to run, and an optional iteration count\n"
                (if (<= len 1) "" "ONLY "))
        (exit))
      (if (= len 3)
          (string->number (vector-ref (current-command-line-arguments) 2))
          1)))
      
  (let* ([version (vector-ref (current-command-line-arguments) 1)] ; racket, typed-scheme, typed-scheme-optimizing
         [bench   (vector-ref (current-command-line-arguments) 0)]
         [prog    (cond
                    ((string=? version "racket")                  (format "../~a.rkt" bench))
                    ((string=? version "typed-scheme")            (format "~a-non-optimizing.rkt" bench))
                    ((string=? version "typed-scheme-optimizing") (format "~a-optimizing.rkt" bench))
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
