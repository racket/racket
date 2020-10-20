#lang racket/base
(require "command-name.rkt"
         "all-tools.rkt"
         racket/string
         racket/list)

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

(define (parse-cmd-output-option cmdline)
  (let* ([tout-idx (index-of cmdline "--cmd-output")]
         [cmd-is-not-help (not (and (pair? cmdline)
                                    (equal? "help" (car cmdline))))])
    (if (and tout-idx
             (< tout-idx (sub1 (length cmdline)))
             cmd-is-not-help)
        (string->path (list-ref cmdline (add1 tout-idx)))
        #f)))

(define (remove-cmd-output-option cmdline cmd-output-file)
  (if (not (path? cmd-output-file))
      cmdline
      (let ([tout-idx (index-of cmdline "--cmd-output")]
            [tout-file-idx (index-of cmdline (path->string cmd-output-file))])
        (append (take cmdline tout-idx)
                (drop cmdline (add1 tout-file-idx))))))

(define (open-cmd-output-port cmd-output-file)
  (if (path? cmd-output-file)
      (with-handlers ([exn:fail:filesystem:errno?
                       (lambda (e)
                         (displayln e)
                         (displayln "Failed to open output file, fallback to standard output")
                         (current-output-port))])
        (open-output-file cmd-output-file
                          #:exists 'truncate))
      (current-output-port)))

(let* ([cmdline (vector->list (current-command-line-arguments))]
       ; Convert: raco help xyz a b c -> raco xynz --help a b c
       [cmdline (if (and (pair? cmdline)
                         (equal? "help" (car cmdline))
                         (pair? (cdr cmdline))
                         (not (regexp-match? #rx"^-" (cadr cmdline))))
                    (list* (cadr cmdline) "--help" (cddr cmdline))
                    cmdline)]
       ; Output to file, --cmd-output <file-name>
       [cmd-output-file (parse-cmd-output-option cmdline)]
       [cmd-output-port (open-cmd-output-port cmd-output-file)]
       [cmdline (remove-cmd-output-option cmdline cmd-output-file)]

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
                         [current-command-name (car tool)]
                         [current-output-port cmd-output-port])
            (dynamic-require (cadr tool) #f)
            (when (file-stream-port? cmd-output-port)
              (close-output-port cmd-output-port))
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
