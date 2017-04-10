#lang racket/base
(require "command-name.rkt"
         "all-tools.rkt"
         racket/string)

(module test racket/base)

(define (filter-by-prefix hash str)
  (for/hash ([key (in-hash-keys hash)]
             #:when (string-prefix? key str))
    (values key (hash-ref hash key))))

(define (filter-importance hash)
  (for/hash ([key (in-hash-keys hash)]
             #:when (cadddr (hash-ref hash key)))
    (values key (hash-ref hash key))))

; sort methods for show-tools
(define (alphabetic<? a b)
  (string<? (car a) (car b)))

(define (importance<? a b)
  (> (or (cadddr a) -inf.0)
     (or (cadddr b) -inf.0)))

(define (show-tools msg tools all-tools <?)
  (eprintf "\n~a commands:\n" msg)
  (define l (sort (hash-map tools (lambda (k v) v)) <?))
  (define largest (apply max 0 (hash-map all-tools
                                         (lambda (k v) (string-length (car v))))))
  (for ([i (in-list l)])
   (eprintf "  ~a~a~a\n"
            (car i)
            (make-string (- largest -3 (string-length (car i))) #\space)
            (caddr i))))

(define (done [result 0])
  ((executable-yield-handler) result) ; to enable GUI commands
  (exit result))

(let* ([cmdline (vector->list (current-command-line-arguments))]
       [cmdline (if (and (pair? cmdline)
                         (equal? "help" (car cmdline))
                         (pair? (cdr cmdline))
                         (not (regexp-match? #rx"^-" (cadr cmdline))))
                    (list* (cadr cmdline) "--help" (cddr cmdline))
                    cmdline)]
       [tools (all-tools)]
       [prefix-tools (if (pair? cmdline)
                         (filter-by-prefix tools (car cmdline))
                         #hasheq())]
       [tool (and (pair? cmdline)
                  (or (hash-ref tools (car cmdline) #f)
                      (and (= (hash-count prefix-tools) 1)
                           (car (hash-values prefix-tools)))))]
       [ambiguous? (> (hash-count prefix-tools) 1)]
       [show-all?
        (cond
         [(null? cmdline) #f]
         [(or (equal? (car cmdline) "--help")
              (equal? (car cmdline) "-h"))
          #t]
         [(regexp-match? #rx"^-" (car cmdline))
          (eprintf "~a: A flag must follow a command: ~a\n\n"
                   (find-system-path 'run-file)
                   (car cmdline))
          #f]
         [tool
          (parameterize ([current-command-line-arguments (list->vector (cdr cmdline))]
                         [current-command-name (car tool)])
             (dynamic-require (cadr tool) #f)
             (done))]
         [ambiguous?
          (eprintf "~a: Ambiguous command prefix: ~a\n\n"
                   (find-system-path 'run-file)
                   (car cmdline))
          #f]
         [(equal? (car cmdline) "help") #t]
         [else
          (eprintf "~a: Unrecognized command: ~a\n\n"
                   (find-system-path 'run-file)
                   (car cmdline))
          #f])])
  (eprintf "Usage: raco <command> <option> ... <arg> ...\n")
  (when ambiguous?
    (show-tools "Matching" prefix-tools tools importance<?))
  (show-tools "Frequently used" (filter-importance tools) tools importance<?)
  (when show-all?
    (show-tools "All available" tools tools alphabetic<?))
  (printf "\nA command can be specified by an unambiguous prefix.")
  (unless show-all?
    (printf "\nSee `raco help' for a complete list of commands."))
  (printf "\nSee `raco help <command>' for help on a command.")
  (newline)
  (done (if show-all? 0 1)))
