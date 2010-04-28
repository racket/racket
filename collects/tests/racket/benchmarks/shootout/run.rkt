(module run mzscheme
  (require (only scheme/runtime-path define-runtime-path))
  (define input-map
    `(
      ("ackermann.ss" "11")
      ("ary.ss" "9000")
      ("binarytrees.ss" "16")
      ("chameneos.ss" "1000000")
      ("cheapconcurrency.ss" "15000")
      ("echo.ss" "150000")
      ("except.ss" "2500000")
      ("fannkuch.ss" "10")
      ("fasta.ss" "25000000")
      ("fibo.ss" "32")
      ("hash.ss" "100000")
      ("hash2.ss" "200")
      ("heapsort.ss" "100000")
      ("lists.ss" "18")
      ("mandelbrot.ss" "3000")
      ("matrix.ss" "600")
      ("moments.ss") ; 200 somethings...
      ("nbody.ss" "20000000")
      ("nestedloop.ss" "18")
      ("nsieve.ss" "9")
      ("nsievebits.ss" "11")
      ("partialsums.ss" "2500000")
      ("pidigits.ss" "2500")
      ("pidigits1.ss")
      ("random.ss" "900000")
      ("recursive.ss" "11")
      ("regexmatch.ss")
      ("regexpdna.ss" #f ,(lambda () (mk-regexpdna-input)))
      ("reversecomplement.ss" #f ,(lambda () (mk-revcomp-input)))
      ("k-nucleotide.ss" #f ,(lambda () (mk-knuc-input)))
      ("reversefile.ss")
      ("sieve.ss" "1200")
      ("spellcheck.ss")
      ("spectralnorm.ss" "5500")
      ("spectralnorm-unsafe.ss" "5500")
      ("strcat.ss" "40000")
      ("sumcol.ss" #f ,(lambda () (mk-sumcol-input)))
      ("wc.ss")
      ("wordfreq.ss")
      ))

  (define-runtime-path here ".")

  (define (dynreq f)
    (parameterize ([current-load-relative-directory here])
      (dynamic-require f #f)))

  (define (mk-fasta n suffix)
    (let ([f (build-path (find-system-path 'temp-dir) (string-append "fasta-" suffix))])
      (unless (file-exists? f)
        (printf "Building FASTA ~a output for input: ~a\n" n f)
        (with-output-to-file f
          (lambda ()
            (parameterize ([current-command-line-arguments (vector n)])
              (dynreq "fasta.ss")))))
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
                                                   "mzscheme"
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
      (unless (<= 1 len 2)
        (printf "provide ~athe name of a benchmark on the command line and an optional iteration count\n"
                (if (zero? len) "" "ONLY "))
        (exit))
      (if (= len 2)
          (string->number (vector-ref (current-command-line-arguments) 1))
          1)))
      
  (let ([prog (vector-ref (current-command-line-arguments) 0)])
    (let ([m (assoc prog input-map)])
      (unless m
        (error 'run "cannot find input for ~a" prog))
      (when (null? (cdr m))
        (error 'run "don't know input for ~a" prog))
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
            (time (dynreq prog))))
        (unless (= n 1)
          (loop (sub1 n)))))))
