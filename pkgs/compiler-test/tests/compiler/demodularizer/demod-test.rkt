#lang racket
(require tests/eli-tester
         racket/runtime-path
         compiler/find-exe
         racket/cmdline)

(define fast? #f)
(command-line
 #:once-each
 [("--fast") "Skip slower tests"
             (set! fast? #t)]
 #:args ()
 (void))

(define-runtime-path tests "tests")

(define (slow-test? i)
  (case (path->string i)
    [("racket-5.rkt") #t]
    [else #f]))

(define (non-base-test? i)
  (case (path->string i)
    [("kernel-5.rkt") #t]
    [else #f]))

(define (get-pruned-expected i)
  (case (path->string i)
    [("base-effect-defn.rkt")
     "\"result\"\n"]
    [("base-assign.rkt")
     "used!\n\"stayed\"\n"]
    [else
     ;; #f means "same as non-pruned"
     #f]))

(define (capture-output command . args)
  (define o (open-output-string))
  (define e (open-output-string))
  (parameterize ([current-input-port (open-input-string "")]
                 [current-output-port o]
                 [current-error-port e])
    (apply system* command args))
  (values (get-output-string o) (get-output-string e)))

(define (test-on-program filename
                         #:flags [flags null]
                         #:excludes [exceptions null]
                         #:expected-output [expected-output #f])
  (define desc (string-join(append flags
                                   exceptions
                                   (list filename))))
  (printf "Checking ~a\n" desc)

  ;; run modular program, capture output
  (define-values (modular-output modular-error)
    (capture-output (find-exe) filename))
  
  (define demod-filename 
    (let-values ([(base filename dir?) (split-path filename)])
      (path->string
       (build-path
        (find-system-path 'temp-dir)
        (path-add-suffix filename #"_merged.zo")))))
  
  ;; demodularize
  (parameterize ([current-input-port (open-input-string "")])
    (apply system* (find-exe) "-l-" "raco" "demod" "-o" demod-filename
           "--work" (build-path tests "compiled" "demod")
           (append flags
                   exceptions
                   (list filename))))
  
  ;; run whole program
  (define-values (whole-output whole-error)
    (capture-output (find-exe) demod-filename))
  
  ;; compare output 
  (test
   #:failure-prefix (format "~a stdout" desc)
   whole-output => (or expected-output
                       modular-output)
   #:failure-prefix (format "~a stderr" desc)
   whole-error => modular-error)

  (when (null? exceptions)
    ;; try creating an executable
    (define exe-filename (build-path
                          (find-system-path 'temp-dir)
                          (if (eq? (system-type) 'windows)
                              "demod-exe.exe"
                              "demod-exe")))
    (system* (find-exe) "-l-" "raco" "exe" "-o" exe-filename demod-filename)
    (define-values (whole-exe-output whole-exe-error)
      (capture-output exe-filename))
    (test
     #:failure-prefix (format "~a exe stdout" desc)
     whole-exe-output => (or expected-output
                             modular-output)
     #:failure-prefix (format "~a exe stderr" desc)
     whole-exe-error => modular-error)))

(define (modular-program? filename)
  (and (not (regexp-match #rx"merged" filename))
       (regexp-match #rx"rkt$" filename)))

(test
 (for ([i (in-list (directory-list tests))]
       #:when (and (regexp-match? #rx"[.]rkt$" i)
                   (or (not fast?)
                       (not (slow-test? i)))))
   (define ip (build-path tests i))
   (define keep-syntax? (regexp-match? #rx"-lib" i))
   (define syntax-flags (if keep-syntax? '("-s") '()))
   (when (modular-program? ip)
     (test-on-program (path->string ip)
                      #:flags syntax-flags))
     (test-on-program (path->string ip)
                      #:flags (append syntax-flags '("-g"))
                      #:expected-output (get-pruned-expected i))
     (unless (non-base-test? i)
       (test-on-program (path->string ip)
                        #:flags syntax-flags
                        #:excludes
                        (list "-e"
                              (path->string
                               (collection-file-path "pre-base.rkt" "racket/private")))))))

(module+ test
  (module config info
    (define timeout 600)))
