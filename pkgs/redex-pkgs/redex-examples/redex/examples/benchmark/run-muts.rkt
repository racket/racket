#lang racket/base

(require racket/cmdline
         racket/function
         racket/list
         racket/place
         racket/match
         racket/system
         "make-mutants.rkt"
         "test-place.rkt")

(define names '())
(define verbose? #f)
(define gen-types '())
(define minutes 1)
(define files '())
(define num-procs 1)

(command-line
   #:once-each
   [("-a" "--all") "Run all mutation tests and gather results"
                   (set! names
                         directories)]
   [("-v" "--verbose") "Also report counterexamples as they are found"
                       (set! verbose? #t)]
   [("-m" "--minutes") mins "Minutes to run each instance for"
                       (set! minutes (string->number mins))]
   [("-n" "--num-processes") n "Number of processes to run in parallel"
                               (set! num-procs (string->number n))]
   #:once-any
   [("-d" "--dir") dirname "Run tests for a single prefix"
                    (set! names (list dirname))]
   [("-f" "--file") fname "Run tests for a single file"
                    (set! files (list fname))]
   #:multi
   [("-t" "--type") t "Generation type to run, one of: search, grammar, search-gen, search-gen-ref, search-gen-enum, search-gen-enum-ref"
                    (set! gen-types (cons (string->symbol t) gen-types))])

(when (empty? files)
  (set! files
        (flatten
         (for/list ([name (in-list names)])
           (define path (build-path (current-directory) name))
           (filter
            (curry regexp-match #px"^.*([\\d]+|base)\\.rkt$")
            (map
             (λ (fp)
               (string-append name "/" (path->string fp)))
             (directory-list path)))))))

(define worklist files)

(define work-sem (make-semaphore 1))

(define (do-next)
  (semaphore-wait work-sem)
  (cond
    [(empty? worklist)
     (semaphore-post work-sem)
     (void)]
    [else
     (define fname (car worklist))
     (set! worklist (cdr worklist))
     (semaphore-post work-sem)
     (define fullpath (build-path (current-directory) fname))
     (define args (apply string-append 
                         (add-between (list* (if verbose? "-v" "")
                                             (string-append "-m " (number->string minutes))
                                             (map (λ (t)
                                                    (string-append "-t "
                                                                   (symbol->string t))) 
                                                  gen-types))
                                      " ")))
     
     (system (let ([ans (apply string-append (add-between (list "racket" "test-file.rkt" args (path->string fullpath)) " "))])
               (printf "~s\n" ans)
               ans))
     (do-next)]))

(define (do-work)
  (displayln worklist)
  (for/list ([_ (in-range num-procs)])
    (thread do-next)))

(define threads (do-work))

(for ([t threads])
  (thread-wait t))

(printf "all tests finished\n")
  