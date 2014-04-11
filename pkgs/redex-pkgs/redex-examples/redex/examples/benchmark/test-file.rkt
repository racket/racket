#lang racket/base

(require "logging.rkt"
         redex/private/search
         racket/cmdline
         racket/list
         racket/set
         racket/match
         racket/path
         racket/system
         (only-in redex/reduction-semantics caching-enabled?)
         math/statistics)

(define minutes 1)
(define verbose #f)
(define output-file #f)
(define n-procs #f)
(define first-only #f)

(define all-types '(search grammar search-gen search-gen-ref 
                           search-gen-enum search-gen-enum-ref
                           enum ordered fixed))
(define types '())

(define timeout-time (* 5 60 1000)) ;; 5 mins in ms

(define (set-type! arg)
  (define t (string->symbol arg))
  (unless (list? (member t all-types))
    (error "Expected a generation type, got" t))
  (set! types (cons t types)))
(define filename
  (command-line
   #:once-each
   [("-m" "--minutes") mins "Minutes to run each instance for"
                       (set! minutes (string->number mins))]
   [("-v" "--verbose") "Report test information during the run"
                       (set! verbose #t)]
   [("-o" "--output") out-file "Output file name"
                      (set! output-file out-file)]
   [("-n" "--num-processes") nps "Number of parallel processes to use"
                                  (set! n-procs (string->number nps))]
   [("-f" "--first-only") "Find the first counterexample only"
                          (set! first-only #t)]
   #:multi
   [("-t" "--type") t "Generation type to run, one of: search, grammar, search-gen, search-gen-ref, search-gen-enum, search-gen-enum-ref, enum, ordered, fixed"
                    (set-type! t)]
   #:args filenames
   (match filenames
     [`()
      (exit)]
     [(list name)
      name]
     [else
      (error "Expected a single file as an argument")])))

(when (empty? types)
  (set! types all-types))

(define results (for/hash ([t (in-list types)])
                  (values t '())))
(define stats (for/hash ([t (in-list types)])
                (values t empty-statistics)))

(unless output-file
  (set! output-file (string-append 
                     (first 
                      (regexp-split #rx"\\."
                                    (last (regexp-split #rx"/" filename))))
                     "-results.rktd")))

(define (log-file-name gen-type)
  (string-append 
   (first 
    (regexp-split #rx"\\."
                  (last (regexp-split #rx"/" filename))))
   "-" (symbol->string gen-type)
   ".log"))

(define (print-and-log str)
  (display str)
  (log-info str))

(define (with-timeout time thunk fail-thunk [on-exn raise])
  (define res-chan (make-channel))
  (define exn-chan (make-channel))
  (define thd (thread (λ () 
                        (with-handlers ([exn:fail? (λ (exn) (channel-put exn-chan exn))])
                          (channel-put res-chan (thunk))))))
  (sync
   (handle-evt (alarm-evt (+ (current-inexact-milliseconds) time))
               (λ (_) 
                 (break-thread thd)
                 (fail-thunk)))
   (handle-evt exn-chan
               (λ (exn) (on-exn exn)))
   (handle-evt res-chan
               (λ (result-of-thunk) result-of-thunk))))

(define (with-heartbeat name type thunk)
  (define thd (thread (λ () (thunk))))
  (define heartbeat-thd
    (thread (λ () 
              (let loop () 
                (log-heartbeat name type) 
                (sleep 10)
                (loop)))))
  (sync
   (handle-evt thd
               (λ (_) (kill-thread heartbeat-thd)))
   (handle-evt heartbeat-thd
               (λ (_) (error 'with-hearbeat "heartbeat thread ended")))))

(define (run/spawn-generations fname verbose? no-errs? get-gen check seconds type)
  (if n-procs
      (spawn-parallel fname verbose? no-errs? get-gen check seconds type)
      (run-generations fname verbose? no-errs? get-gen check seconds type)))

(define (spawn-parallel fname verbose? no-errs? get-gen check seconds type)
  (define (make-cmd n)
    (define oname (string-append
                   (first 
                      (regexp-split #rx"\\."
                                    (last (regexp-split #rx"/" (path->string fname)))))
                   "-"
                   (symbol->string type)
                   "-results-" (number->string n) ".rktd"))
    (format "racket test-file.rkt -m ~s -t ~s -o ~s ~a"
            (/ seconds 60)
            type
            oname
            fname))
  (map thread-wait
       (for/list ([n n-procs])
         (thread 
          (λ () (system (make-cmd n)))))))

(struct timeout ())
(struct reached-limit (tries))

(define (run-generations fname verbose? no-errs? get-gen check seconds type)
  (log-start fname type)
  (with-heartbeat fname type
   (λ () (run-gens fname verbose? no-errs? get-gen check seconds type))))

(define (run-gens fname verbose? no-errs? get-gen check seconds type)
  (collect-garbage)
  (define s-time (current-process-milliseconds))
  (define time-limit (+ s-time (* 1000 seconds)))
  (define terms 0)
  (define counterexamples 0)
  (define (tot-time) (- (current-process-milliseconds) s-time))
  (let trials-loop ([t 0]
                    [g (get-gen)])
    (define trial-start-time (current-process-milliseconds))
    (define (me-time) (- (current-process-milliseconds) trial-start-time))
    (match (one-counterexample trial-start-time time-limit 
                               g check fname type)
      [(timeout)
       (trials-loop t g)]
      [(reached-limit tries)
       (exit-message fname type (+ tries terms) (tot-time) counterexamples)]
      [(list tries term)
       (define continue? (update-results (me-time) fname type verbose?))
       (set! counterexamples (add1 counterexamples))
       (log-counterexample fname type term tries (me-time))
       (when no-errs?
         (printf "!---------------------------------------------------!\n")
         (error 'run-generations "~a: unexpected error on ~s" 
                fname term))
       (if (and (not first-only)
                (or continue?
                    (t . < . 5)))
           (begin
             (set! terms (+ tries terms))
             (trials-loop (add1 t) (get-gen)))
           (exit-message fname type (+ tries terms) (tot-time) counterexamples))])))

(define (one-counterexample s-time time-limit generator check fname type)
  (let/ec break
    (let loop ([tries 0])
      (when ((current-process-milliseconds) . > . time-limit)
        (break (reached-limit tries))) 
      (define term (with-timeout timeout-time generator
                                 (λ () 
                                   (log-gen-timeout fname type)
                                   (break (timeout)))))
      (define ok? (with-timeout timeout-time (λ () (check term))
                                (λ () 
                                  (log-check-timeout fname type term)
                                  (break (timeout)))
                                (λ (exn)
                                  (printf "\nException when calling check with:\n~s\n" term)
                                  (raise exn))))
      (cond
        [(not ok?)
         (list tries term)]
        [else
         (loop (add1 tries))]))))

(define (exit-message file type terms time countxmps)
  (log-finished file type time terms countxmps)
  (printf "-----------------\n~a, ~s\n" file type)
  (printf "Quitting after ~s iterations and ~s milliseconds\n ~s terms/sec\n"
          terms time (exact->inexact (/ terms (/ time 1000))))
  (printf "~s counterexamples, ~s tries... ratio: ~s\n"
          countxmps terms (if (zero? countxmps)
                              'N/A
                              (exact->inexact (/ terms countxmps))))
  (printf "-----------------\n"))


(define (update-results time fname type verbose?)
  (set! results (hash-set results type (cons time (hash-ref results type))))
  (define new-stats (update-statistics (hash-ref stats type) time))
  (set! stats (hash-set stats type new-stats))
  (define avg (statistics-mean new-stats))
  (define dev (/ (statistics-stddev new-stats #:bias #t) (sqrt (length (hash-ref results type)))))
  (log-new-avg fname type (exact->inexact avg) dev)
  (or (= dev 0)
      ((/ dev avg) . > . 0.1)))

(define (test-file fname verbose? no-errs? gen-type seconds)
  (define maybe-fpath (string->path fname))
  (define fpath (if (relative-path? maybe-fpath)
                    maybe-fpath
                    (find-relative-path (current-directory) maybe-fpath)))
  (define tc (dynamic-require fpath 'type-check))
  (define check (dynamic-require fpath 'check))
  (define gen-term (dynamic-require fpath 'generate-M-term))
  (define gen-typed-term (dynamic-require fpath 'generate-typed-term))
  (define typed-generator (dynamic-require fpath 'typed-generator))
  (define gen-enum (dynamic-require fpath 'generate-enum-term))
  (define ordered-generator (dynamic-require fpath 'ordered-enum-generator))
  (define err (dynamic-require fpath 'the-error))
  (printf "\n-------------------------------------------------------------------\n")
  (printf "~a has the error: ~a\n\n" fpath err)
  (printf "Running ~a....\n" fpath)
  (printf "Using generator: ~s\n" gen-type)
  (cond
    [(equal? gen-type 'fixed)
     (define small-counter-example
       (dynamic-require 
        fpath 'small-counter-example 
        (λ ()
          (error 'fixed "contains no small counter example"))))
     (unless (tc small-counter-example)
       (error 'fixed "The counter example doesn't type-check: ~e"
              small-counter-example))
     (define ok? (check small-counter-example))
     (when ok?
       (error 'fixed "Expected ~e to fail on check, but it didn't"
              small-counter-example))]
    [(equal? gen-type 'grammar)
     (run/spawn-generations fpath verbose? no-errs? (λ () gen-term)
                      check seconds gen-type)]
    [(equal? gen-type 'enum)
     (run/spawn-generations fpath verbose? no-errs? (λ () gen-enum)
                      check seconds gen-type)]
    [(equal? gen-type 'ordered)
     (run/spawn-generations fpath verbose? no-errs? (λ () (ordered-generator))
                      check seconds gen-type)]
    [(equal? gen-type 'search)
     (run/spawn-generations fpath verbose? no-errs? (λ () gen-typed-term)
                      check seconds gen-type)]
    [(equal? gen-type 'search-gen)
     (run/spawn-generations fpath verbose? no-errs? typed-generator
                      check seconds gen-type)]
    [(equal? gen-type 'search-gen-ref)
     (define t (current-process-milliseconds))
     (define g (typed-generator))
     (define (gen)
       (when ((- (current-process-milliseconds) t) . > . (* 30 1000))
         (set! t (current-process-milliseconds))
         (set! g (typed-generator)))
       (g))
     (run/spawn-generations fpath verbose? no-errs? (λ () gen)
                      check seconds gen-type)]
    [(equal? gen-type 'search-gen-enum)
     (parameterize ([gen-state (set-remove (gen-state) 'shuffle-clauses)])
       (run/spawn-generations fpath verbose? no-errs? typed-generator
                        check seconds gen-type))]
    [(equal? gen-type 'search-gen-enum-ref)
     (parameterize ([gen-state (set-remove (gen-state) 'shuffle-clauses)])
       (define t (current-process-milliseconds))
       (define g (typed-generator))
       (define (gen)
         (when ((- (current-process-milliseconds) t) . > . (* 30 1000))
           (set! t (current-process-milliseconds))
           (set! g (typed-generator)))
         (g))
       (run/spawn-generations fpath verbose? no-errs? (λ () gen)
                        check seconds gen-type))])) 

(for ([gen-type (in-list types)])
  (with-logging-to (log-file-name gen-type)
                   (λ () (test-file filename verbose #f gen-type (* minutes 60)))))

(unless (member 'fixed types)
  (call-with-output-file output-file
    (λ (out)
      (write
       (apply append
              (for/list ([(type times) (in-hash results)])
                (apply append
                       (for/list ([t times])
                         (list filename type t)))))
     out))
    #:exists 'replace))
