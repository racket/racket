(module sandbox mzscheme
  (require (lib "string.ss") (lib "list.ss") (lib "port.ss")
           (lib "moddep.ss" "syntax"))

  (provide mred?
           coverage-enabled
           namespace-specs
           sandbox-reader
           sandbox-security-guard
           sandbox-read-ok-paths
           sandbox-write-ok-paths
           sandbox-input
           sandbox-output
           get-output
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

  (define sandbox-input  (make-parameter #f))
  (define sandbox-output (make-parameter #f))
  (define null-input (open-input-bytes #""))

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

  (define (simplify-path* path)
    (simplify-path
     (expand-path
      (path->complete-path (if (bytes? path) (bytes->path path) path)))))
  (define (paths->path-bytes paths)
    (map (lambda (p)
           (if (or (path? p) (string? p) (bytes? p))
             (path->bytes (simplify-path* p))
             (error 'paths->path-bytes "bad path value: ~e" p)))
         paths))
  (define sandbox-read-ok-paths
    (make-parameter (paths->path-bytes (current-library-collection-paths))
                    paths->path-bytes))
  (define sandbox-write-ok-paths
    (make-parameter '() paths->path-bytes))

  (define sep (bytes-ref (path->bytes (simplify-path "/")) 0)) ; '\' on windows
  (define (path-ok? path paths)
    (let* ([path (path->bytes (simplify-path* path))]
           [len (bytes-length path)])
      (ormap (lambda (ok)
               (let ([ok-len (bytes-length ok)])
                 (or (and (= ok-len len) (bytes=? path ok))
                     (and (< ok-len len) (= sep (bytes-ref path ok-len))
                          (bytes=? ok (subbytes path 0 ok-len))))))
             paths)))

  (define default-sandbox-guard
    (make-security-guard
     (current-security-guard)
     (lambda (what path modes)
       (unless (cond
                 ;; Hack: allow all exists accesses -- this is required for
                 ;; `expand-path' which is itself used by the security guard.
                 [(equal? modes '(exists)) #t]
                 [(memq 'execute modes) #f] ; execution is never allowed
                 [(or (memq 'write modes) (memq 'delete modes))
                  ;; write+delete requires being in the write-paths
                  (path-ok? path (sandbox-write-ok-paths))]
                 [(or (memq 'read modes) (memq 'exists modes))
                  ;; read+exists requires being in the read-paths
                  (path-ok? path (sandbox-read-ok-paths))]
                 [else (error what "unknown access modes: ~e" modes)])
         (error what "file access denied ~a" (cons path modes))))
     (lambda (what host port mode) (error what "network access denied"))))

  (define sandbox-security-guard (make-parameter default-sandbox-guard))

  (define (safe-eval expr)
    (parameterize ([current-security-guard (sandbox-security-guard)]
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

  (define (input->port inp)
    (cond [(input-port? inp) inp]
          [(string? inp) (open-input-string inp)]
          [(bytes?  inp) (open-input-bytes inp)]
          [(path?   inp) (open-input-file inp)]
          [else (error 'input->port "bad input: ~e" inp)]))

  (define (read-code inp)
    (parameterize ([current-input-port (input->port inp)])
      (port-count-lines! (current-input-port))
      ((sandbox-reader))))

  ;; takes a module-spec and returns all module paths that are needed
  ;; ==> ignores (lib ...) modules
  (define (module-spec->non-lib-paths mod)
    (define (lib? x)
      (if (module-path-index? x)
        (let-values ([(m base) (module-path-index-split x)]) (lib? m))
        (and (pair? x) (eq? 'lib (car x)))))
    (define (path->compiled-dir path)
      (let-values ([(base name dir?) (split-path path)])
        (build-path base "compiled")))
    (let loop ([todo (if (lib? mod)
                       '()
                       (list (simplify-path* (resolve-module-path mod #f))))]
               [r '()])
      (cond
        [(null? todo) r]
        [(member (car todo) r) (loop (cdr todo) r)]
        [else
         (let ([path (car todo)])
           (loop (map (lambda (i)
                        (simplify-path* (resolve-module-path-index i path)))
                      (filter (lambda (i)
                                (and (module-path-index? i) (not (lib? i))))
                              (apply append
                                     (call-with-values
                                         (lambda ()
                                           (module-compiled-imports
                                            (get-module-code (car todo))))
                                         list))))
                 (list* path (path->compiled-dir path) r)))])))
  ;; (define (module-spec->paths mod)
  ;;   (let loop ([todo (list (simplify-path* (resolve-module-path mod #f)))]
  ;;              [r '()])
  ;;     (cond
  ;;       [(null? todo) r]
  ;;       [(member (car todo) r) (loop (cdr todo) r)]
  ;;       [else
  ;;        (let ([path (car todo)])
  ;;          (loop (map (lambda (i)
  ;;                       (simplify-path* (resolve-module-path-index i path)))
  ;;                     (filter module-path-index?
  ;;                             (apply append
  ;;                                    (call-with-values
  ;;                                        (lambda ()
  ;;                                          (module-compiled-imports
  ;;                                           (get-module-code (car todo))))
  ;;                                        list))))
  ;;                (cons path r)))])))

  (define (evaluate-program language teachpacks input-program uncovered!)
    (let* ([body (read-code input-program)]
           [required-paths
            (if (and (pair? teachpacks) (eq? 'begin (car teachpacks)))
              (apply append
                     (map cdr
                          (filter
                           (lambda (x)
                             (let ([fst (and (pair? x) (car x))])
                               (eq? 'require
                                    (if (syntax? fst) (syntax-e fst) fst))))
                           (cdr teachpacks))))
              teachpacks)]
           [required-paths (apply append (map module-spec->non-lib-paths
                                              required-paths))]
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
                       [(or (and (pair? language)
                                 (memq (car language) '(file planet)))
                            (string? language))
                        (set! required-paths
                              (append (module-spec->non-lib-paths language)
                                      required-paths))
                        `(module m ,language ,@body)]
                       [(and (pair? language)
                             (eq? 'begin (car language)))
                        `(begin ,language ,@body)]
                       [else (error 'make-evaluator
                                    "Bad language specification: ~e"
                                    language)])]
           [ns (current-namespace)])
      (unless (null? required-paths)
        (sandbox-read-ok-paths (append required-paths (sandbox-read-ok-paths))))
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
  (define (get-output eval) (eval get-output))

  (define (make-evaluator language teachpacks input-program)
    (let ([coverage-enabled (coverage-enabled)]
          [uncovered-expressions #f]
          [input-ch  (make-channel)]
          [result-ch (make-channel)]
          [output    #f])
      (parameterize
          ([current-namespace (make-evaluation-namespace)]
           [current-inspector (make-inspector)]
           [current-input-port
            (let ([inp (sandbox-input)]) (if inp (input->port inp) null-input))]
           [current-output-port
            (let ([out (sandbox-output)])
              (cond [(not out) (open-output-nowhere)]
                    [(output-port? out) (set! output out) out]
                    [(eq? out 'pipe)
                     (let-values ([(i o) (make-pipe)]) (set! output i) o)]
                    [(memq out '(bytes string))
                     (let-values
                         ([(open get)
                           (if (eq? out 'bytes)
                             (values open-output-bytes  get-output-bytes)
                             (values open-output-string get-output-string))])
                       (let ([o (open)])
                         (set! output (lambda ()
                                        (let ([o1 o])
                                          (set! o (open))
                                          (current-output-port o)
                                          (get-output-bytes o1))))
                         o))]
                    [else (error 'make-evaluator "bad output: ~e" out)]))])
        ;; Note the above definition of `current-eventspace': in MzScheme, it
        ;; is a parameter that is not used at all.  Also note that creating an
        ;; eventspace starts a thread that will eventually run the callback
        ;; code (which evaluates the program in `run-in-bg') -- so this
        ;; parameterization must be nested in the above, or it will not use the
        ;; new namespace.
        (parameterize ([current-eventspace (make-eventspace)])
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
            (define (eval-in-user-context expr)
              (channel-put input-ch expr)
              (let ([r (channel-get result-ch)])
                (if (eq? (car r) 'exn) (raise (cdr r)) (apply values (cdr r)))))
            (if (eq? r 'ok)
              ;; Initial program executed ok, so return an evaluator:
              (lambda (expr)
                (cond [(eq? expr get-uncovered-expressions)
                       uncovered-expressions]
                      [(eq? expr get-output)
                       (if (procedure? output)
                         (eval-in-user-context `(,output))
                         output)]
                      [else (eval-in-user-context expr)]))
              ;; Program didn't execute:
              (raise r)))))))

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
