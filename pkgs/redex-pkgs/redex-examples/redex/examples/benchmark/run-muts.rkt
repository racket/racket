#lang racket/base

(require racket/cmdline
         racket/function
         racket/list
         racket/place
         racket/match
         racket/system
         racket/runtime-path
         "apply-diffs.rkt")

(define names '())
(define verbose? #f)
(define gen-types '())
(define minutes 1)
(define files '())
(define num-procs 1)

(command-line
   #:once-each
   [("-v" "--verbose") "Also report counterexamples as they are found"
                       (set! verbose? #t)]
   [("-m" "--minutes") mins "Minutes to run each instance for"
                       (set! minutes (string->number mins))]
   [("-n" "--num-processes") n "Number of processes to run in parallel"
                               (set! num-procs (string->number n))]
   #:once-any
   [("-a" "--all") "Run all broken models and gather results"
                   (set! names directories)]
   [("-d" "--dir") dirname "Run tests for a single prefix"
                    (set! names (list dirname))]
   [("-f" "--file") fname "Run tests for a single file"
                    (set! files (list fname))]
   #:multi
   [("-t" "--type") t "Generation type to run, one of: search, grammar, search-gen, search-gen-ref, search-gen-enum, search-gen-enum-ref, enum, ordered, fixed"
                    (set! gen-types (cons (string->symbol t) gen-types))])

(define-runtime-path here ".")

(when (empty? files)
  (set! files
        (flatten
         (for/list ([dir (in-list names)])
           (map
            (λ (fn)
              (string-append dir "/" (path->string fn)))
            (filter
             (compose (curry regexp-match #px"^.*([\\d]+)\\.rkt$") path->string)
             (directory-list (get-directory dir))))))))

(struct work (file type))

(define worklist (for*/list ([f files] [t gen-types])
                   (work f t)))

(define work-sem (make-semaphore 1))

(define (do-next)
  (semaphore-wait work-sem)
  (cond
    [(empty? worklist)
     (semaphore-post work-sem)
     (void)]
    [else
     (match-define (work file type) (car worklist))
     (set! worklist (cdr worklist))
     (semaphore-post work-sem)
     (define path (simplify-path (build-path here file)))
     (define output-name 
       (string-append (first 
                       (regexp-split #rx"\\."
                                     (last (regexp-split #rx"/" file))))
                      "-"
                      (symbol->string type)
                      "-results.rktd"))
     (define args 
       (apply string-append 
              (add-between (list (if verbose? "-v" "")
                                 (string-append "-m " (number->string minutes))
                                 (string-append "-o " output-name)
                                 (string-append "-t "
                                                (symbol->string type))
                                 (if (equal? type 'ordered) "-f" ""))
                           " ")))
     (define command 
       (apply string-append 
              (add-between 
               (list "racket" (path->string (build-path here "test-file.rkt"))
                     args (path->string path)) " ")))
     (when verbose?
       (printf "running: ~s\n" command))
     (system command)
     (do-next)]))

(define (do-work)
  (printf "worklist:\n~a\n"
          (apply string-append
                 (add-between (for/list ([w (in-list worklist)])
                                (match-define (work f t) w)
                                (string-append f ": " (symbol->string t)))
                              ", ")))
  (for/list ([_ (in-range num-procs)])
    (thread do-next)))

(define threads (do-work))

(for ([t threads])
  (thread-wait t))

(printf "all tests finished\n")
  
