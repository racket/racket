#lang racket/base

(require redex/private/search
         racket/cmdline
         racket/list
         racket/set
         racket/match
         math/statistics)

(define minutes 1)
(define verbose #f)
(define output-file #f)

(define all-types '(search grammar search-gen search-gen-ref search-gen-enum search-gen-enum-ref))
(define types '())

(define filename
  (command-line
   #:once-each
   [("-m" "--minutes") mins "Minutes to run each instance for"
                       (set! minutes (string->number mins))]
   [("-v" "--verbose") "Report test information during the run"
                       (set! verbose #t)]
   [("-o" "--output") out-file "Output file name"
                      (set! output-file out-file)]
   #:multi
   [("-t" "--type") t "Generation type to run, one of: search, grammar, search-gen, search-gen-ref, search-gen-enum, search-gen-enum-ref"
                    (set! types (cons (string->symbol t) types))]
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
         (define term (g))
         (define me-time (- (current-process-milliseconds) t-time))
         (define ok? (check term))
         (cond
           [(not ok?)
            (when verbose?
              (printf "~s: counterexample: ~s\n  ~s iterations and ~s milliseconds\n"
                      fname term i me-time))
            (when no-errs?
              (printf "!---------------------------------------------------!\n")
              (error 'run-generations "~s: unexpected error on ~s" 
                     fname term))
            (define continue? (update-results me-time fname type verbose?))
            (if (or continue?
                    (t . < . 5))
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
  (define fpath (string->path fname))
  (define tc (dynamic-require fpath 'type-check))
  (define check (dynamic-require fpath 'check))
  (define gen-term (dynamic-require fpath 'generate-M-term))
  (define gen-typed-term (dynamic-require fpath 'generate-typed-term))
  (define typed-generator (dynamic-require fpath 'typed-generator))
  (define err (dynamic-require fpath 'the-error))
  (printf "\n-------------------------------------------------------------------\n")
  (printf "~s has the error: ~a\n\n" fname err)
  (printf "Running ~s....\n" fname)
  (printf "Using generator: ~s\n" gen-type)
  (cond
    [(equal? gen-type 'grammar)
     (define (gen-and-type)
       (define t (gen-term))
       (and (tc t)
            t))
     (run-generations fname verbose? no-errs? (λ () gen-and-type) 
                      check seconds gen-type)]
    [(equal? gen-type 'search)
     (run-generations fname verbose? no-errs? (λ () gen-typed-term)
                      check seconds gen-type)]
    [(equal? gen-type 'search-gen)
     (run-generations fname verbose? no-errs? typed-generator
                      check seconds gen-type)]
    [(equal? gen-type 'search-gen-ref)
     (define t (current-process-milliseconds))
     (define g (typed-generator))
     (define (gen)
       (when ((- (current-process-milliseconds) t) . > . (* 30 1000))
         (set! t (current-process-milliseconds))
         (set! g (typed-generator)))
       (g))
     (run-generations fname verbose? no-errs? (λ () gen)
                      check seconds gen-type)]
    [(equal? gen-type 'search-gen-enum)
     (parameterize ([gen-state (set-remove (gen-state) 'shuffle-clauses)])
       (run-generations fname verbose? no-errs? typed-generator
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
       (run-generations fname verbose? no-errs? (λ () gen)
                        check seconds gen-type))])) 

(for ([gen-type (in-list types)])
  (test-file filename verbose #f gen-type (* minutes 60)))

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
