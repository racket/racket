#!/bin/sh
#|
exec mzscheme -qu "$0" ${1+"$@"}
|#

(module auto mzscheme
  (require (lib "process.ss")
           (lib "port.ss")
           (lib "list.ss")
           (lib "date.ss")
           (lib "cmdline.ss"))

  (define (test-mzscheme input rx iterations)
    (let ([rx (byte-pregexp rx)])
      (let ([start (current-inexact-milliseconds)])
        (let loop ([n iterations])
          (unless (zero? n)
            (regexp-match-positions rx input)
            (loop (sub1 n))))
        (- (current-inexact-milliseconds) start))))

  (define (test-perl input rx iterations)
    (with-output-to-file "test.pl"
      (lambda ()
        (with-input-from-file "perl_prefix.pl"
          (lambda ()
            (copy-port (current-input-port)
                       (current-output-port))))
        (printf "test \"~a\", /~a/, \"/~a/\", ~a;\n"
                input rx rx iterations))
      'truncate)
    (let ([s (open-output-bytes)])
      (parameterize ([current-output-port s])
        (system "perl test.pl"))
      (parameterize ([current-input-port (open-input-string (get-output-string s))])
        (read-line)
        (* 1000 (read)))))

  (define (test-pcre input rx iterations)
    (let ([s (open-output-bytes)])
      (parameterize (; [current-output-port s]
                     [current-input-port (open-input-bytes
                                          (bytes-append
                                           (string->bytes/latin-1 (format "/~a/S\n" rx))
                                           input
                                           #"\n"))])
        (system (format "pcretest -t -Q -n ~a" iterations)))
      (let ([m (regexp-match #rx"Execute time ([0-9.]*)" (get-output-string s))])
        (if m
            (* (string->number (cadr m)) iterations)
            (begin
              (printf "~a\n" (get-output-string s))
              #f)))))

  (define (random-letters n)
    (parameterize ([current-pseudo-random-generator (make-pseudo-random-generator)])
      (random-seed 43)
      (let loop ([n n][accum null])
        (if (zero? n)
            (list->bytes accum)
            (loop (sub1 n) (cons (+ (char->integer #\a)
                                    (random 26))
                                 accum))))))

  (define (add-index l)
    (let loop ([i 1][l l])
      (if (null? l)
          l
          (cons (cons (let ([n (format "0~a" i)])
                        (substring n (- (string-length n) 2)))
                      (car l)) 
                (loop (add1 i) (cdr l))))))

  (define inputs
    (add-index
     (list
      (list (make-bytes 100 (char->integer #\x))  #"(?s:.*)" 100000)
      (list (make-bytes 1000 (char->integer #\x))  #"(?s:.*)" 100000)
      (list (make-bytes 10000 (char->integer #\x))  #"(?s:.*)" 100000)
      (list (make-bytes 100000 (char->integer #\x))  #"(?s:.*)" 100000)
      (list (make-bytes 100 (char->integer #\x))  #"(?m:.*)" 100000)
      (list (make-bytes 1000 (char->integer #\x))  #"(?m:.*)" 100000)
      (list (make-bytes 10000 (char->integer #\x))  #"(?m:.*)" 100000)
      (list (make-bytes 100000 (char->integer #\x))  #"(?m:.*)" 10000)
      (list (make-bytes 100 (char->integer #\x))  #"(?s:(.)*)" 100000)
      (list (make-bytes 1000 (char->integer #\x))  #"(?s:(.)*)" 100000)
      (list (make-bytes 10000 (char->integer #\x))  #"(?s:(.)*)" 10000)
      (list (make-bytes 100000 (char->integer #\x))  #"(?s:(.)*)" 1000)
      (list (make-bytes 100 (char->integer #\x))  #"x*" 100000)
      (list (make-bytes 1000 (char->integer #\x))  #"x*" 100000)
      (list (make-bytes 10000 (char->integer #\x))  #"x*" 10000)
      (list (make-bytes 100000 (char->integer #\x))  #"x*" 1000)
      (list (make-bytes 100 (char->integer #\x))  #"[xy]*" 100000)
      (list (make-bytes 1000 (char->integer #\x))  #"[xy]*" 10000)
      (list (make-bytes 10000 (char->integer #\x))  #"[xy]*" 1000)
      (list (make-bytes 100000 (char->integer #\x))  #"[xy]*" 100)
      (list (make-bytes 100 (char->integer #\x))  #"(?m:(.)*)" 100000)
      (list (make-bytes 1000 (char->integer #\x))  #"(?m:(.)*)" 100000)
      (list (make-bytes 10000 (char->integer #\x))  #"(?m:(.)*)" 100000)
      (list (make-bytes 100000 (char->integer #\x))  #"(?m:(.)*)" 10000)
      (list (make-bytes 100 (char->integer #\x))  #"(x)*" 100000)
      (list (make-bytes 1000 (char->integer #\x))  #"(x)*" 100000)
      (list (make-bytes 10000 (char->integer #\x))  #"(x)*" 10000)
      (list (make-bytes 100000 (char->integer #\x))  #"(x)*" 1000)
      (list (make-bytes 100 (char->integer #\x))  #"(y|x)*" 10000)
      (list (make-bytes 1000 (char->integer #\x))  #"(y|x)*" 1000)
      (list (make-bytes 10000 (char->integer #\x))  #"(y|x)*" 100)
      (list (make-bytes 100000 (char->integer #\x))  #"(y|x)*" 10)
      (list (make-bytes 100 (char->integer #\x))  #"([yz]|x)*" 10000)
      (list (make-bytes 1000 (char->integer #\x))  #"([yz]|x)*" 1000)
      (list (make-bytes 10000 (char->integer #\x))  #"([yz]|x)*" 100)
      (list (make-bytes 100000 (char->integer #\x))  #"([yz]|x)*" 10)
      (list (make-bytes 100 (char->integer #\x))  #"([xy])*" 100000)
      (list (make-bytes 1000 (char->integer #\x))  #"([xy])*" 10000)
      (list (make-bytes 10000 (char->integer #\x))  #"([xy])*" 1000)
      (list (make-bytes 100000 (char->integer #\x))  #"([xy])*" 100)
      (list (make-bytes 100 (char->integer #\x))  #"((x){2})*" 10000)
      (list (make-bytes 1000 (char->integer #\x))  #"((x){2})*" 10000)
      (list (make-bytes 10000 (char->integer #\x))  #"((x){2})*" 100)
      (list (make-bytes 100000 (char->integer #\x))  #"((x){2})*" 100)
      (list (bytes-append (random-letters 100) #"FOOBARBAZ")  #"[a-z]*FOOBARBAZ" 100000)
      (list (bytes-append (random-letters 1000) #"FOOBARBAZ")  #"[a-z]*FOOBARBAZ" 10000)
      (list (bytes-append (random-letters 10000) #"FOOBARBAZ")  #"[a-z]*FOOBARBAZ" 1000)
      (list (bytes-append (random-letters 100) #"NOPE")  #"[a-z]*FOOBARBAZ" 1000000)
      (list (bytes-append (random-letters 1000) #"NOPE")  #"[a-z]*FOOBARBAZ" 100000)
      (list (bytes-append (random-letters 10000) #"NOPE")  #"[a-z]*FOOBARBAZ" 10000)
      (list (bytes-append (random-letters 100) #"FOOBARBAZ")  #"([a-z])*FOOBARBAZ" 100000)
      (list (bytes-append (random-letters 1000) #"FOOBARBAZ")  #"([a-z])*FOOBARBAZ" 10000)
      (list (bytes-append (random-letters 10000) #"FOOBARBAZ")  #"([a-z])*FOOBARBAZ" 1000)
      (list (bytes-append (random-letters 100) #"NOPE")  #"([a-z])*FOOBARBAZ" 1000000)
      (list (bytes-append (random-letters 1000) #"NOPE")  #"([a-z])*FOOBARBAZ" 100000)
      (list (bytes-append (random-letters 10000) #"NOPE")  #"([a-z])*FOOBARBAZ" 10000)
      (list (bytes-append (random-letters 100) #"FOOBARBAZ")  #"([a-z]|ab)*FOOBARBAZ" 10000)
      (list (bytes-append (random-letters 1000) #"FOOBARBAZ")  #"([a-z]|ab)*FOOBARBAZ" 1000)
      (list (bytes-append (random-letters 10000) #"FOOBARBAZ")  #"([a-z]|ab)*FOOBARBAZ" 10)
      (list (bytes-append (random-letters 100) #"NOPE")  #"([a-z]|ab)*FOOBARBAZ" 1000000)
      (list (bytes-append (random-letters 1000) #"NOPE")  #"([a-z]|ab)*FOOBARBAZ" 100000)
      (list (bytes-append (random-letters 10000) #"NOPE")  #"([a-z]|ab)*FOOBARBAZ" 10000)
      (list (bytes-append (random-letters 100) #"NOPE")  #"(?i:[a-z]*FOOBARBAZ)" 1000)
      (list (bytes-append (random-letters 1000) #"NOPE")  #"(?i:[a-z]*FOOBARBAZ)" 10)
      (list (bytes-append (random-letters 10000) #"NOPE")  #"(?i:[a-z]*FOOBARBAZ)" 10))))
  
  (define benchmark-names (map (lambda (t)
                                 (string->symbol (car t)))
                               inputs))
  
  (define testers
    (list (list 'mzscheme test-mzscheme)
          (list 'perl test-perl)
          (list 'pcre test-pcre)))

  (define impls (map car testers))

  (define no-benchmarks (map (lambda (s)
                               (cons (string->symbol (format "no-~a" s))
                                     s))
                             benchmark-names))
  (define no-implementations (map (lambda (s)
                                    (cons (string->symbol (format "no-~a" s))
                                          s))
                                  impls))
  
  (define run-benchmarks #f)
  (define run-implementations #f)

  (define default-benchmarks benchmark-names)

  (define default-implementations impls)
  (define current-output-file (make-parameter #f))

  ;; Extract command-line arguments --------------------

  (define args
    (command-line
     "auto"
     (current-command-line-arguments)
     (once-each
      [("--show") "show implementations and benchmarks"
       (printf "Implementations:\n")
       (for-each (lambda (impl)
                   (printf " ~a\n" impl))
                 default-implementations)
       (printf "Benchmarks: 1 - ~a\n"
               (length inputs))]
      [("-o" "--out") filename "append output to <filename>"
       (current-output-file filename)])
     (args impl-or-benchmark impl-or-benchmark)))

  ;; Process arguments ------------------------------

  (for-each (lambda (arg)
              (let ([s (string->symbol arg)])
                (cond
                 [(memq s impls)
                  (set! run-implementations
                        (append (or run-implementations null)
                                (list s)))]
                 [(assq s no-implementations)
                  => (lambda (a)
                       (set! run-implementations
                             (remq (cdr a)
                                   (or run-implementations default-implementations))))]
                 [(memq s benchmark-names)
                  (set! run-benchmarks
                        (append (or run-benchmarks null)
                                (list s)))]
                 [(assq s no-benchmarks)
                  => (lambda (a)
                       (set! run-benchmarks
                             (remq (cdr a)
                                   (or run-benchmarks default-benchmarks))))]
                 [else
                  (error 'auto "mysterious argument: ~a" arg)])))
            args)

  (define actual-benchmarks-to-run 
    (or run-benchmarks
        default-benchmarks))

  (define actual-implementations-to-run 
    (or run-implementations
        default-implementations))
  
  ;; Run benchmarks -------------------------------

  (define (rprintf . args)
    (apply printf args)
    (when (current-output-file)
      (with-output-to-file (current-output-file)
        (lambda ()
          (apply printf args))
        'append)))

  (define (run who which)
    (let ([t (assoc (symbol->string which) inputs)])
      (let-values ([(index input rx iterations) (apply values t)])
        #;
        (printf "Testing ~a: ~s on ~a iterations of a ~a-byte input\n" 
        who
        rx
        iterations
        (bytes-length input))
        (let ([ms ((cadr (assoc who testers)) input rx iterations)])
          (rprintf "[~a ~s (~a #f #f) #f]\n"
                   who
                   (string->symbol (format "~a.~a/~a/~a" index rx (bytes-length input) iterations))
                   (and ms (inexact->exact (round ms))))))))

  (rprintf "; ~a\n" (date->string (seconds->date (current-seconds)) #t))

  (for-each (lambda (i)
              (for-each (lambda (bm)
                          (run i bm))
                        actual-benchmarks-to-run))
            actual-implementations-to-run))
