#lang racket/base
(require "command-name.rkt"
         "all-tools.rkt")

(module test racket/base)

(define (find-by-prefix hash str)
  (let ([trie (make-hash)])
    (for ([key (in-hash-keys hash)])
      (for/fold ([trie trie]) ([c (string->list key)])
        (let ([next (hash-ref trie c (lambda () (make-hash)))])
          (if (hash-ref next #f #f)
              (hash-set! next #f null)
              (hash-set! next #f key))
          (hash-set! trie c next)
          next)))
    (let ([t (for/fold ([trie trie]) ([c (string->list str)])
               (and trie
                    (hash-ref trie c #f)))])
      (and t
           (let ([s (hash-ref t #f #f)])
             (if (string? s)
                 (hash-ref hash s)
                 'ambiguous))))))

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
         [(or (hash-ref tools (car cmdline) #f)
              (find-by-prefix tools (car cmdline)))
          => (lambda (tool)
               (if (eq? 'ambiguous tool)
                   (begin
                     (eprintf "~a: Ambiguous command prefix: ~a\n\n"
                              (find-system-path 'run-file)
                              (car cmdline))
                     #f)
                   (parameterize ([current-command-line-arguments
                                   (list->vector (cdr cmdline))]
                                  [current-command-name (car tool)])
                     (dynamic-require (cadr tool) #f)
                     (done))))]
         [(equal? (car cmdline) "help") #t]
         [else
          (eprintf "~a: Unrecognized command: ~a\n\n"
                   (find-system-path 'run-file)
                   (car cmdline))
          #f])])
  (eprintf "Usage: raco <command> <option> ... <arg> ...\n")
  (for-each
   (lambda (show-all?)
     (eprintf "\n~a commands:\n"
              (if show-all? "All available" "Frequently used"))
     (let ([l (sort (hash-map tools (lambda (k v) v))
                    (if show-all?
                        (lambda (a b) (string<? (car a) (car b)))
                        (lambda (a b) (> (or (list-ref a 3) -inf.0) (or (list-ref b 3) -inf.0)))))])
       (let ([largest (apply max 0 (map (lambda (v) (string-length (car v))) l))])
         (for ([i (in-list l)])
           (when (or show-all? (cadddr i))
             (eprintf "  ~a~a~a\n"
                      (car i)
                      (make-string (- largest -3 (string-length (car i))) #\space)
                      (caddr i)))))))
   (if show-all?
       (list #f #t)
       (list #f)))
  (printf "\nA command can be specified by an unambiguous prefix.")
  (unless show-all?
    (printf "\nSee `raco help' for a complete list of commands."))
  (printf "\nSee `raco help <command>' for help on a command.")
  (newline)
  (done (if show-all? 0 1)))
