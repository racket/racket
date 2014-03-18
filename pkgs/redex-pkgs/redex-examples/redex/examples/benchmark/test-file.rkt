#lang racket/base

(require redex/private/search
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

(define (with-timeout time thunk fail-thunk)
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
               (λ (exn) (raise exn)))
   (handle-evt res-chan
               (λ (result-of-thunk) result-of-thunk))))

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

(define (run-generations fname verbose? no-errs? get-gen check seconds type)
  (collect-garbage)
  (define s-time (current-process-milliseconds))
  (define terms 0)
  (let trials-loop ([t 0])
    (define t-time (current-process-milliseconds))
    (define g (get-gen))
    (let loop ([i 0])
      (define tot-time (- (current-process-milliseconds) s-time))
      (cond
        [((/ tot-time 1000) . > . seconds)
         (when verbose?
           (printf "Quitting after ~s iterations and ~s milliseconds\n ~s terms/sec\n"
                   (+ i terms) tot-time (exact->inexact (/ (+ i terms) (/ tot-time 1000)))))
         (void)]
        [else
         (define term (with-timeout (* 5 60 1000) g 
                                    (λ () (printf "\nTimed out generating a test term in: ~a, ~a\n"
                                                  fname type)
                                      (trials-loop t))))
         (define me-time (- (current-process-milliseconds) t-time))
         (define ok? (with-timeout (* 5 60 1000) (λ () (check term))
                                   (λ () (printf "\nIn ~a, ~a, timed out checking the term:~a\n"
                                                 fname type term)
                                     (trials-loop t))))
         (cond
           [(not ok?)
            (when verbose?
              (printf "~a: counterexample: ~s\n  ~s iterations and ~s milliseconds\n"
                      fname term i me-time))
            (when no-errs?
              (printf "!---------------------------------------------------!\n")
              (error 'run-generations "~a: unexpected error on ~s" 
                     fname term))
            (define continue? (update-results me-time fname type verbose?))
            (if (and (not first-only)
                     (or continue?
                         (t . < . 5)))
                (begin
                  (set! terms (+ i terms))
                  (trials-loop (add1 t)))
                (void))]
           [else
            (loop (add1 i))])]))))

(define (update-results time fname type verbose?)
  (set! results (hash-set results type (cons time (hash-ref results type))))
  (define new-stats (update-statistics (hash-ref stats type) time))
  (set! stats (hash-set stats type new-stats))
  (define avg (statistics-mean new-stats))
  (define dev (/ (statistics-stddev new-stats #:bias #t) (sqrt (length (hash-ref results type)))))
  (when verbose?
    (printf "new average for ~s, ~s: ~s +/- ~s\n" fname type (exact->inexact avg) dev))
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
  (define fixed (dynamic-require fpath 'fixed))
  (define err (dynamic-require fpath 'the-error))
  (printf "\n-------------------------------------------------------------------\n")
  (printf "~a has the error: ~a\n\n" fpath err)
  (printf "Running ~a....\n" fpath)
  (printf "Using generator: ~s\n" gen-type)
  (define (gen-and-type gen)
    (λ ()
      (define t (gen))
      (and (tc t)
           t)))
  (cond
    [(equal? gen-type 'fixed)
     (define some-failed?
       (for/or ([t (in-list fixed)])
         (define ok? (check t))
         (not ok?)))
     (unless some-failed?
       (error 'fixed "Expected some term to fail, but didn't find one in ~a" fixed))]
    [(equal? gen-type 'grammar)
     (run/spawn-generations fpath verbose? no-errs? (λ () (gen-and-type gen-term))
                      check seconds gen-type)]
    [(equal? gen-type 'enum)
     (run/spawn-generations fpath verbose? no-errs? (λ () (gen-and-type gen-enum))
                      check seconds gen-type)]
    [(equal? gen-type 'ordered)
     (run/spawn-generations fpath verbose? no-errs? (λ ()
                                                      (define g (ordered-generator))
                                                      (gen-and-type g))
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

(parameterize ([caching-enabled? #f])
  (for ([gen-type (in-list types)])
    (test-file filename verbose #f gen-type (* minutes 60))))

(call-with-output-file output-file
  (λ (out)
    (write
     (apply append
            (for/list ([(type times) (in-hash results)])
              (apply append
                     (for/list ([t times])
                       (list filename type t)))))
     out))
  #:exists 'replace)
