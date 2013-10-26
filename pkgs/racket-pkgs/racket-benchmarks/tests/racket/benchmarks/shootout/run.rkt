(module run mzscheme
  (require (only scheme/runtime-path define-runtime-path)
           racket/port
           mzlib/kw)
  (define input-map
    `(
      ("ackermann" "12")
      ("ary" "600000")
      ("binarytrees" "17")
      ("chameneos" "120000")
      ("cheapconcurrency" "2000")
      ("echo" "200000")
      ("except" "2000000")
      ("fannkuch" "10")
      ("fannkuch-redux" "10")
      ("fasta" "100000000")
      ("fibo" "42")
      ("hash" "2000000")
      ("hash2" "1000")
      ("heapsort" "2500000")
      ("hello" "")
      ("lists" "2000")
      ("mandelbrot" "3000")
      ("mandelbrot-generic" "3000")
      ("matrix" "12000")
      ("meteor" "200000") ; increasing this won't change the running time
      ("moments" #f ,(lambda () (mk-moments-input 2000)))
      ("nbody" "3000000")
      ("nbody-generic" "3000000")
      ("nbody-vec" "3000000")
      ("nbody-vec-generic" "3000000")
      ("nestedloop" "33")
      ("nothing" "")
      ("nsieve" "12")
      ("nsievebits" "12")
      ("partialsums" "3000000")
      ("pidigits" "4000")
      ("pidigits1" "4000")
      ("random" "40000000")
      ("recursive" "12")
      ("regexmatch" #f ,(lambda () (mk-regexmatch-input 1000000)))
      ("regexpdna" #f ,(lambda () (mk-fasta-input 1000000)))
      ("reversecomplement" #f ,(lambda () (mk-fasta-input 30000000)))
      ("k-nucleotide" #f ,(lambda () (mk-fasta-input 500000)))
      ("reversefile" #f ,(lambda () (mk-sumcol-input 3000)))
      ("sieve" "25000")
      ("spellcheck")
      ("spectralnorm" "2000")
      ("spectralnorm-generic" "2000")
      ("strcat" "50000000")
      ("sumcol" #f ,(lambda () (mk-sumcol-input 10000)))
      ("thread-ring" "1000000") ; calls exit, so won't display running time
      ("wc" #f ,(lambda () (mk-sumcol-input 20000)))
      ("wordfreq" #f ,(lambda () (mk-sumcol-input 10000)))
      ))

  (define-runtime-path here ".")

  (define (dynreq f)
    (parameterize ([current-load-relative-directory here]
                   [current-output-port (open-output-nowhere)])
      (dynamic-require f #f)))

  (define (mk-regexmatch-input n)
    (let ([f (build-path (find-system-path 'temp-dir) (string-append "regexmatch-" (number->string n)))])
      (unless (file-exists? f)
        (printf "Building regexmatch ~a output for input: ~a\n" n f)
        (with-output-to-file f
          (lambda ()
            ;; taken from heapsort.rkt
            (define IM   139968)
            (define IA     3877)
            (define IC    29573)
            (define LAST 42)
            (define (gen_random max)
              (set! LAST (modulo (+ (* LAST IA) IC) IM))
              (/ (* max LAST) IM))
            (define (random-int max) (inexact->exact (round (gen_random max))))
            ;; this can generate malformed phone numbers (with a 1 or 2 number area
            ;; code, for instance) but that's fine, the regex just won't match
            (let loop ((n n))
              (unless (zero? n)
                (printf (format "(~a) ~a-~a\n" (random-int 1000) (random-int 1000) (random-int 10000)))
                (loop (sub1 n)))))))
      f))
  
  (define (mk-fasta-input n)
    (let ([f (build-path (find-system-path 'temp-dir) (string-append "fasta-" (number->string n)))])
      (unless (file-exists? f)
        (printf "Building FASTA ~a output for input: ~a\n" n f)
        (with-output-to-file f
          (lambda ()
            (parameterize ([current-command-line-arguments (vector (number->string n))]
                           [current-load-relative-directory here])
              (dynamic-require "fasta.rkt" #f)))))
      f))

  (define-runtime-path sumcol-input "sumcol-input.txt")

  (define/kw (mk-sumcol-input n #:optional moments?)
    (let ([f (build-path (find-system-path 'temp-dir) (string-append (if moments? "moments-" "sumcol-")
                                                                     (number->string n)))])
      (unless (file-exists? f)
        (printf "Building ~a ~a input: ~a\n" (if moments? "moments" "sumcol") n f)
        (let ([c (with-input-from-file sumcol-input
                   (lambda ()
                     (if moments?
                         (apply string-append ; like sumcol, but with floats
                                (map (lambda (x) (string-append (number->string (exact->inexact x)) "\n"))
                                     (port->list)))
                         (read-bytes 10000))))])
          (with-output-to-file f
            (lambda ()
              (let loop ([n n])
                (unless (zero? n)
                  (printf "~a" c)
                  (loop (sub1 n))))))))
      f))

  (define (mk-moments-input n)
    (mk-sumcol-input n #t))

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
                    ((string=? version "racket")                      "current-bm.rkt")
                    ((string=? version "typed-racket-non-optimizing") "typed/current-bm.rkt")
                    ((string=? version "typed-racket")                "typed/current-bm.rkt")
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
