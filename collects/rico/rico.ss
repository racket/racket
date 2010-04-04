#lang scheme/base
(require setup/getinfo
         "command-name.ss")

(define cmdline (vector->list (current-command-line-arguments)))

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

(let* ([dirs (find-relevant-directories '(rico))]
       [infos (map get-info/full dirs)]
       [tools (make-hash)])
  (for-each (lambda (i d)
              (for-each (lambda (entry)
                          (cond
                           [(and (list? entry)
                                 (= (length entry) 4)
                                 (string? (car entry))
                                 (module-path? (cadr entry))
                                 (string? (caddr entry))
                                 (or (not (list-ref entry 3))
                                     (real? (list-ref entry 3))))
                            (let ([p (hash-ref tools (car entry) #f)])
                              (when p
                                (fprintf
                                 (current-error-port)
                                 "warning: tool ~s registered twice: ~e and ~e"
                                 (car entry)
                                 (car p)
                                 d)))
                            (hash-set! tools (car entry) entry)]
                           [else
                            (fprintf
                             (current-error-port)
                             "warning: ~s provided bad `rico' spec: ~e"
                             d
                             entry)]))
                        (let ([l (i 'rico (lambda () null))])
                          (if (list? l)
                              l
                              (list l)))))
            infos
            dirs)
  (let ([show-all?
         (cond
          [(null? cmdline) #f]
          [(or (equal? (car cmdline) "--help")
               (equal? (car cmdline) "-h"))
           #t]
          [(regexp-match? #rx"^-" (car cmdline))
           (fprintf (current-error-port) "~a: A flag must follow a command: ~a\n\n"
                    (find-system-path 'run-file)
                    (car cmdline))
           #f]
          [(or (hash-ref tools (car cmdline) #f)
               (find-by-prefix tools (car cmdline)))
           => (lambda (tool)
                (if (eq? 'ambiguous tool)
                    (begin
                      (fprintf (current-error-port) "~a: Ambiguous command prefix: ~a\n\n"
                               (find-system-path 'run-file)
                               (car cmdline))
                      #f)
                    (parameterize ([current-command-line-arguments
                                    (list->vector (cdr cmdline))]
                                   [current-command-name (car tool)])
                      (dynamic-require (cadr tool) #f)
                      (exit))))]
          [else
           (fprintf (current-error-port) "~a: Unrecognized command: ~a\n\n"
                    (find-system-path 'run-file)
                    (car cmdline))
           #f])])
    (fprintf (current-error-port) "Usage: rico <command> <option> ... <arg> ...\n\n")
    (fprintf (current-error-port) "~a commands:\n" (if show-all? 
                                                       "Available"
                                                       "Frequently used"))
    (let ([l (sort (hash-map tools (lambda (k v) v))
                   (if show-all?
                       (lambda (a b) (string<? (car a) (car b)))
                       (lambda (a b) (> (or (list-ref a 3) -inf.0) (or (list-ref b 3) -inf.0)))))])
      (let ([largest (apply max 0 (map (lambda (v) (string-length (car v))) l))])
        (for ([i (in-list l)])
          (when (or show-all? (cadddr i))
            (fprintf (current-error-port)
                     "  ~a~a~a\n"
                     (car i)
                     (make-string (- largest -3 (string-length (car i))) #\space)
                     (caddr i))))))
    (printf "\nA command can be specified by an unambigous prefix.")
    (unless show-all?
      (printf "\nSee `rico --help' for a complete list of commands."))
    (printf "\nSee `rico <command> --help' for help on a command."))
  (newline)
  (exit 1))
