#lang racket/base

(require racket/cmdline
         racket/list
         racket/match
         racket/system
         racket/runtime-path
         racket/function
         compiler/find-exe
         racket/file)

(define-runtime-path here ".")
(define-runtime-path run-generators "run-generator.rkt")

(struct work (name gen-mod check-mod))

(define print-work-status? (make-parameter #f))

(define (do-work worklist time-seconds num-procs [log-directory (current-directory)])

  (define work-sem (make-semaphore 1))
  
  (when (print-work-status?)
    (printf "worklist:\n~a\n"
            (apply string-append
                   (add-between (for/list ([w (in-list worklist)])
                                  (match-define (work f g c) w)
                                  (define t (dynamic-require g 'type))
                                  (format "~a : ~s" f t))
                                ",\n"))))
  
  (define (do-next)
    (semaphore-wait work-sem)
    (cond
      [(empty? worklist)
       (semaphore-post work-sem)
       (void)]
      [else
       (match-define (work name gen-mod check-mod) (car worklist))
       (set! worklist (cdr worklist))
       (semaphore-post work-sem)
       (define args 
         (apply string-append 
                (add-between (list (string-append "-n " name)
                                   (string-append "-c " (format "'~s'" check-mod))
                                   (string-append "-g " (format "'~s'" gen-mod))
                                   (string-append "-s " (number->string time-seconds))
                                   (string-append "-d " (path->string log-directory))
                                   "-l"
                                   (if (print-work-status?)
                                       "-v" ""))
                             " ")))
       (define command 
         (apply string-append 
                (add-between 
                 (list (path->string (find-exe)) 
                       (path->string run-generators)
                       args) " ")))
       (system command)
       (do-next)]))
  
  (map thread-wait
       (for/list ([_ (in-range num-procs)])
         (thread do-next)))
  (void))

(define (run-benchmark all-mods seconds num-processes log-dir)
  (define worklist
    (map (λ (l) (apply work l))
         all-mods))
  (do-work worklist seconds num-processes log-dir))
  
(module+ main
  
  (define gen-types '())
  (define secs 0)
  (define num-procs 1)
  (define all? #f)
  
  (define info-mod-file
    (command-line
     #:once-each
     [("-m" "--minutes") minutes "Minutes to run each instance for"
                         (set! secs (* 60 (string->number minutes)))]
     [("-s" "--seconds") seconds "Seconds to run each instance for"
                         (set! secs (string->number seconds))]
     [("-n" "--num-processes") num-processes "Number of processes to run in parallel"
                               (set! num-procs (string->number num-processes))]
     [("-a" "--all") "Run all generation types"
                     (set! all? #t)]
     #:multi
     [("-t" "--type") t "Generation type to run, one of: grammar, ordered, enum"
                      (set! gen-types (cons (string->symbol t) gen-types))]
     #:args (info-mod-filename)
     info-mod-filename))
  
  (define log-dir (make-temporary-file "bmark-logs~a" 'directory))
  
  (define all-mods ((if all?
                        values
                        ((curry filter)
                         (match-lambda 
                           [(list _ gen-mod _)
                            (member (dynamic-require gen-mod 'type) gen-types)])))
                    ((dynamic-require info-mod-file 'all-mods))))
  
  (printf "Logging to: ~a\n" log-dir)
  
  (parameterize ([print-work-status? #t])
    (run-benchmark all-mods secs num-procs log-dir)))