(module sandbox mzscheme
  (require (lib "string.ss") (lib "list.ss"))

  (provide mred?
           coverage-enabled
           namespace-specs
           sandbox-reader
           sandbox-security-guard
           get-uncovered-expressions
           make-evaluator)

  (define mred?
    (with-handlers ([void (lambda (_) #f)])
      (dynamic-require '#%mred-kernel #f)
      #t))
  (define-syntax mz/mr ; use a value for mzscheme, or pull a mred binding
    (syntax-rules ()
      [(mz/mr mzval mrsym)
       (if mred? (dynamic-require '(lib "mred.ss" "mred") 'mrsym) mzval)]))

  ;; Configuration ------------------------------------------------------------

  (define coverage-enabled (make-parameter #f))

  (define namespace-specs
    (make-parameter
     (let ([mods '((lib "posn.ss" "lang"))]
           [mred-mods '((lib "cache-image-snip.ss" "mrlib"))])
       `(,(mz/mr make-namespace make-namespace-with-mred)
         ,@mods ,@(if mred? mred-mods '())))))

  (define (default-sandbox-reader)
    (parameterize ([read-case-sensitive #t] [read-decimal-as-inexact #f])
      (let loop ([l '()])
        (let ([expr (read-syntax 'program)])
          (if (eof-object? expr)
            (reverse! l)
            (loop (cons expr l)))))))

  (define sandbox-reader (make-parameter default-sandbox-reader))

  (define ok-path-re
    (byte-regexp
     (bytes-append
      #"^(?:"
      (apply bytes-append
             (cdr (apply append
                         (map (lambda (p)
                                (list #"|" (regexp-quote (path->bytes p))))
                              (current-library-collection-paths)))))
      #")(?:/|$)")))

  (define sandbox-security-guard
    (make-parameter
     (make-security-guard
      (current-security-guard)
      (lambda (what path modes)
        (when (or (memq 'write modes)
                  (memq 'execute modes)
                  (memq 'delete modes)
                  (and path
                       (not (regexp-match? ok-path-re (path->bytes path)))))
          (error what "file access denied (~a)" path)))
      (lambda (what host port mode) (error what "network access denied")))))

  (define null-input (open-input-string ""))
  (define (safe-eval expr)
    (parameterize ([current-security-guard (sandbox-security-guard)]
                   [current-input-port null-input]
                   ;; breaks: [current-code-inspector (make-inspector)]
                   )
      (eval expr)))

  ;; Execution ----------------------------------------------------------------

  (define (make-evaluation-namespace)
    (let* ([specs   (namespace-specs)]
           [new-ns  ((car specs))]
           [orig-ns (current-namespace)]
           [mods    (cdr specs)]
           [resolve (current-module-name-resolver)])
      (for-each (lambda (mod) (dynamic-require mod #f)) mods)
      (let ([modsyms (map (lambda (mod) (resolve mod #f #f)) mods)])
        (parameterize ([current-namespace new-ns])
          (for-each (lambda (ms) (namespace-attach-module orig-ns ms))
                    modsyms)))
      new-ns))

  (define (read-code inp)
    (parameterize ([current-input-port
                    (cond [(input-port? inp) inp]
                          [(string? inp) (open-input-string inp)]
                          [(bytes?  inp) (open-input-bytes inp)]
                          [(path?   inp) (open-input-file inp)]
                          [else (error 'read-code "bad input: ~e" inp)])])
      (port-count-lines! (current-input-port))
      ((sandbox-reader))))

  (define (evaluate-program language teachpacks input-program uncovered!)
    (let* ([body (read-code input-program)]
           [body (append (if (and (pair? teachpacks)
                                  (eq? 'begin (car teachpacks)))
                           (cdr teachpacks)
                           (map (lambda (tp)
                                  `(,#'require ,(if (pair? tp) tp `(file ,tp))))
                                teachpacks))
                         body)]
           [body (cond [(and (symbol? language)
                             (memq language '(beginner
                                              beginner-abbr
                                              intermediate
                                              intermediate-lambda
                                              advanced)))
                        `(module m
                             (lib ,(case language
                                     [(beginner) "htdp-beginner.ss"]
                                     [(beginner-abbr) "htdp-beginner-abbr.ss"]
                                     [(intermediate) "htdp-intermediate.ss"]
                                     [(intermediate-lambda)
                                      "htdp-intermediate-lambda.ss"]
                                     [(advanced) "htdp-advanced.ss"])
                                  "lang")
                           ,@body)]
                       [(or (and (pair? language) (eq? 'lib (car language)))
                            (symbol? language))
                        `(module m ,language ,@body)]
                       [(and (pair? language)
                             (eq? 'begin (car language)))
                        `(begin ,language ,@body)]
                       [else (error 'make-evaluator
                                    "Bad language specification: ~e"
                                    language)])]
           [ns (current-namespace)])
      (when uncovered!
        (safe-eval '(require (lib "coverage.ss" "handin-server" "private"))))
      (safe-eval body)
      (when (and (pair? body) (eq? 'module (car body))
                 (pair? (cdr body)) (symbol? (cadr body)))
        (let ([mod (cadr body)])
          (safe-eval `(require ,mod))
          (current-namespace (module->namespace mod))))
      (when uncovered!
        (uncovered! (filter (lambda (x) (eq? 'program (syntax-source x)))
                            (parameterize ([current-namespace ns])
                              (safe-eval '(get-uncovered-expressions))))))))

  (define current-eventspace (mz/mr (make-parameter #f) current-eventspace))
  (define make-eventspace    (mz/mr void make-eventspace))
  (define run-in-bg          (mz/mr thread queue-callback))

  (define (get-uncovered-expressions eval) (eval get-uncovered-expressions))

  (define (make-evaluator language teachpacks input-program)
    (let ([coverage-enabled (coverage-enabled)]
          [uncovered-expressions #f]
          [ns (make-evaluation-namespace)]
          [input-ch  (make-channel)]
          [result-ch (make-channel)])
      (parameterize ([current-namespace ns]
                     [current-inspector (make-inspector)]
                     ;; bogus parameter and value if we're in mzscheme
                     [current-eventspace (make-eventspace)])
        (run-in-bg
          (lambda ()
            ;; First read program and evaluate it as a module:
            (with-handlers ([void (lambda (exn) (channel-put result-ch exn))])
              (evaluate-program
               language teachpacks input-program
               (and coverage-enabled
                    (lambda (exprs) (set! uncovered-expressions exprs))))
              (channel-put result-ch 'ok))
            ;; Now wait for interaction expressions:
            (let loop ()
              (let ([expr (channel-get input-ch)])
                (unless (eof-object? expr)
                  (with-handlers ([void (lambda (exn)
                                          (channel-put result-ch
                                                       (cons 'exn exn)))])
                    (channel-put result-ch
                                 (cons 'vals (call-with-values
                                                 (lambda () (safe-eval expr))
                                                 list))))
                  (loop))))
            (let loop ()
              (channel-put result-ch '(exn . no-more-to-evaluate))
              (loop))))
        (let ([r (channel-get result-ch)])
          (if (eq? r 'ok)
            ;; Initial program executed ok, so return an evaluator:
            (lambda (expr)
              (if (eq? expr get-uncovered-expressions)
                uncovered-expressions
                (begin (channel-put input-ch expr)
                       (let ([r (channel-get result-ch)])
                         (if (eq? (car r) 'exn)
                           (raise (cdr r))
                           (apply values (cdr r)))))))
            ;; Program didn't execute:
            (raise r))))))

  ;; Resources ----------------------------------------------------------------

  (define (call-with-limits sec mb thunk)
    (let ([cust (make-custodian)]
          [ch   (make-channel)])
      (when mb (custodian-limit-memory cust (* mb 1024 1024) cust))
      (let* ([work (parameterize ([current-custodian cust])
                     (thread (lambda ()
                               (channel-put ch
                                 (with-handlers ([void (lambda (e)
                                                         (list raise e))])
                                   (call-with-values thunk
                                       (lambda vs (cons values vs))))))))]
             [watch (thread (lambda ()
                              (channel-put ch
                                (if (sync/timeout sec work) 'memory 'time))))]
             [r (channel-get ch)])
        (custodian-shutdown-all cust)
        (kill-thread watch)
        (if (list? r)
          (apply (car r) (cdr r))
          (error 'with-limit "out of ~a" r)))))

  (define-syntax with-limits
    (syntax-rules ()
      [(with-limits sec mb body ...)
       (call-with-limits sec mb (lambda () body ...))]))

  )
