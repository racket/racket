#lang scheme/base

(require scheme/port
         syntax/moddep
         scheme/gui/dynamic)

(provide gui?
         sandbox-init-hook
         sandbox-reader
         sandbox-input
         sandbox-output
         sandbox-error-output
         sandbox-propagate-breaks
         sandbox-coverage-enabled
         sandbox-namespace-specs
         sandbox-override-collection-paths
         sandbox-security-guard
         sandbox-path-permissions
         sandbox-network-guard
         sandbox-make-inspector
         sandbox-make-logger
         sandbox-eval-limits
         kill-evaluator
         break-evaluator
         set-eval-limits
         put-input
         get-output
         get-error-output
         get-uncovered-expressions
         make-evaluator
         make-module-evaluator
         call-with-limits
         with-limits
         exn:fail:resource?
         exn:fail:resource-resource)

(define gui? (gui-available?))

(define-syntax mz/mr ; use a value for mzscheme, or pull a mred binding
  (syntax-rules ()
    [(mz/mr mzval mrsym)
     (if gui? (gui-dynamic-require 'mrsym) mzval)]))

;; Configuration ------------------------------------------------------------

(define sandbox-init-hook    (make-parameter void))
(define sandbox-input        (make-parameter #f))
(define sandbox-output       (make-parameter #f))
(define sandbox-error-output
  (make-parameter (lambda () (dup-output-port (current-error-port)))))
(define sandbox-eval-limits  (make-parameter '(30 20))) ; 30sec, 20mb
(define sandbox-propagate-breaks (make-parameter #t))
(define sandbox-coverage-enabled (make-parameter #f))

(define sandbox-namespace-specs
  (make-parameter `(,(mz/mr make-base-namespace make-gui-namespace)
                    #| no modules here by default |#)))

(define (default-sandbox-reader source)
  (let loop ([l '()])
    (let ([expr (read-syntax source)])
      (if (eof-object? expr)
          (reverse l)
          (loop (cons expr l))))))

(define sandbox-reader (make-parameter default-sandbox-reader))

(define sandbox-override-collection-paths (make-parameter '()))

(define teaching-langs
  '(beginner beginner-abbr intermediate intermediate-lambda advanced))

;; Security Guard -----------------------------------------------------------

(define sep (bytes-ref (path->bytes (simplify-path "/")) 0)) ; '\' on windows

(define (simplify-path* path)
  (if (symbol? path)
      #f
      (simplify-path (cleanse-path (path->complete-path
                                    (cond [(bytes? path) (bytes->path path)]
                                          [(string? path) (string->path path)]
                                          [else path]))))))

(define permission-order '(execute write delete read exists))
(define (perm<=? p1 p2)
  (memq p1 (memq p2 permission-order)))

;; gets a path (can be bytes/string), returns a regexp for that path that
;; matches also subdirs (if it's a directory)
(define path->bregexp
  (let* ([sep-re    (regexp-quote (bytes sep))]
         [last-sep  (byte-regexp (bytes-append sep-re #"?$"))]
         [suffix-re (bytes-append #"(?:$|" sep-re #")")])
    (lambda (path)
      (if (byte-regexp? path)
          path
          (let* ([path (path->bytes (simplify-path* path))]
                 [path (regexp-quote (regexp-replace last-sep path #""))])
            (byte-regexp (bytes-append #"^" path suffix-re)))))))

(define sandbox-path-permissions
  (make-parameter '()
                  (lambda (new)
                    (map (lambda (perm) (cons (car perm) (map path->bregexp (cdr perm))))
                         new))))

(define sandbox-network-guard
  (make-parameter (lambda (what . xs)
                    (error what "network access denied: ~e" xs))))

(define default-sandbox-guard
  (let ([orig-security (current-security-guard)])
    (make-security-guard
     orig-security
     (lambda (what path modes)
       (when path
         (let ([needed (let loop ([order permission-order])
                         (cond [(null? order)
                                (error 'default-sandbox-guard
                                       "unknown access modes: ~e" modes)]
                               [(memq (car order) modes) (car order)]
                               [else (loop (cdr order))]))]
               [bpath (parameterize ([current-security-guard orig-security])
                        (path->bytes (simplify-path* path)))])
           (unless (ormap (lambda (perm)
                            (and (perm<=? needed (car perm))
                                 (regexp-match (cadr perm) bpath)))
                          (sandbox-path-permissions))
             (error what "file access denied ~a" (cons path modes))))))
     (lambda args (apply (sandbox-network-guard) args)))))

(define sandbox-security-guard (make-parameter default-sandbox-guard))

(define sandbox-make-inspector (make-parameter make-inspector))

(define sandbox-make-logger (make-parameter current-logger))

;; computes permissions that are needed for require specs (`read' for all
;; files and "compiled" subdirs, `exists' for the base-dir)
(define (module-specs->path-permissions mods)
  (define paths (module-specs->non-lib-paths mods))
  (define bases
    (let loop ([paths paths] [bases '()])
      (if (null? paths)
          (reverse bases)
          (let-values ([(base name dir?) (split-path (car paths))])
            (let ([base (simplify-path* base)])
              (loop (cdr paths)
                    (if (member base bases) bases (cons base bases))))))))
  (append (map (lambda (p) `(read ,(path->bytes p))) paths)
          (map (lambda (b) `(read ,(build-path b "compiled"))) bases)
          (map (lambda (b) `(exists ,b)) bases)))

;; takes a module-spec list and returns all module paths that are needed
;; ==> ignores (lib ...) modules
(define (module-specs->non-lib-paths mods)
  (define (lib? x)
    (if (module-path-index? x)
        (let-values ([(m base) (module-path-index-split x)]) (lib? m))
        (and (pair? x) (eq? 'lib (car x)))))
  ;; turns a module spec to a simple one (except for lib specs)
  (define (simple-modspec mod)
    (cond [(and (pair? mod) (eq? 'lib (car mod))) #f]
          [(module-path? mod)
           (simplify-path* (resolve-module-path mod #f))]
          [(not (and (pair? mod) (pair? (cdr mod))))
           ;; don't know what this is, leave as is
           #f]
          [(eq? 'only (car mod))
           (simple-modspec (cadr mod))]
          [(eq? 'rename (car mod))
           (simple-modspec (cadr mod))]
          [(and (eq? 'prefix (car mod)) (pair? (cddr mod)))
           (simple-modspec (caddr mod))]
          [else #f]))
  (let loop ([todo (filter values (map simple-modspec mods))]
             [r '()])
    (cond
     [(null? todo) r]
     [(member (car todo) r) (loop (cdr todo) r)]
     [else
      (let ([path (car todo)])
        (loop (filter values
                      (map (lambda (i)
                             (simplify-path* (resolve-module-path-index i path)))
                           (filter (lambda (i)
                                     (and (module-path-index? i) (not (lib? i))))
                                   (apply append
                                          (call-with-values
                                              (lambda ()
                                                (module-compiled-imports
                                                 (get-module-code (car todo))))
                                            list)))))
              (cons path r)))])))

;; Resources ----------------------------------------------------------------

(define-struct (exn:fail:resource exn:fail) (resource))

(define memory-accounting? (custodian-memory-accounting-available?))

(define (call-with-limits sec mb thunk)
  (let ([r #f]
        [c (make-custodian)]
        ;; used to copy parameter changes from the nested thread
        [p current-preserved-thread-cell-values])
    (when (and mb memory-accounting?)
      (custodian-limit-memory c (* mb 1024 1024) c))
    (parameterize ([current-custodian c])
      ;; The nested-thread can die on a time-out or memory-limit,
      ;; and never throws an exception, so we never throw an error,
      ;; just assume the a death means the custodian was shut down
      ;; due to memory limit.  Note: cannot copy the
      ;; parameterization in this case.
      (with-handlers ([exn:fail? (lambda (e)
                                   (unless r (set! r (cons #f 'memory))))])
        (call-in-nested-thread
         (lambda ()
           (define this (current-thread))
           (define timer
             (and sec
                  (thread (lambda ()
                            (sleep sec)
                            ;; even in this case there are no parameters
                            ;; to copy, since it is on a different thread
                            (set! r (cons #f 'time))
                            (kill-thread this)))))
           (set! r
                 (with-handlers ([void (lambda (e) (list (p) raise e))])
                   (call-with-values thunk (lambda vs (list* (p) values vs)))))
           (when timer (kill-thread timer)))))
      (custodian-shutdown-all c)
      (unless r (error 'call-with-limits "internal error"))
      ;; apply parameter changes first
      (when (car r) (p (car r)))
      (if (pair? (cdr r))
          (apply (cadr r) (cddr r))
          (raise (make-exn:fail:resource (format "with-limit: out of ~a" (cdr r))
                                         (current-continuation-marks)
                                         (cdr r)))))))

(define-syntax with-limits
  (syntax-rules ()
    [(with-limits sec mb body ...)
     (call-with-limits sec mb (lambda () body ...))]))

;; Execution ----------------------------------------------------------------

(define (literal-identifier=? x y)
  (or (free-identifier=? x y)
      (eq? (syntax-e x) (syntax-e y))))

(define-namespace-anchor anchor)

(define (make-evaluation-namespace)
  (let* ([specs   (sandbox-namespace-specs)]
         [new-ns  ((car specs))]
         [orig-ns (namespace-anchor->empty-namespace anchor)]
         [mods    (cdr specs)])
    (parameterize ([current-namespace orig-ns])
      (for-each (lambda (mod) (dynamic-require mod #f)) mods))
    (parameterize ([current-namespace new-ns])
      (for-each (lambda (ms) (namespace-attach-module orig-ns ms))
                mods))
    new-ns))

(define (extract-required language requires)
  (let* ([requires (cond [(string? language) (cons language requires)]
                         [(not (pair? language)) requires]
                         [(memq (car language) '(lib file planet quote))
                          (cons language requires)]
                         [(eq? (car language) 'begin) requires]
                         [else (error 'extract-required
                                      "bad language spec: ~e" language)])])
    requires))

(define (input->port inp)
  ;; returns #f when it can't create a port
  (cond [(input-port? inp) inp]
        [(string? inp) (open-input-string inp)]
        [(bytes?  inp) (open-input-bytes inp)]
        [(path?   inp) (open-input-file inp)]
        [else #f]))

;; Gets an input spec returns a list of syntaxes.  The input can be a list of
;; sexprs/syntaxes, or a list with a single input port spec
;; (path/string/bytes) value.
(define (input->code inps source n)
  (if (null? inps)
      '()
      (let ([p (input->port (car inps))])
        (cond [(and p (null? (cdr inps)))
               (port-count-lines! p)
               (parameterize ([current-input-port p])
                 ((sandbox-reader) source))]
              [p (error 'input->code "ambiguous inputs: ~e" inps)]
              [else (let loop ([inps inps] [n n] [r '()])
                      (if (null? inps)
                          (reverse r)
                          (loop (cdr inps) (and n (add1 n))
                                ;; 1st at line#1, pos#1, 2nd at line#2, pos#2 etc
                                ;; (starting from the `n' argument)
                                (cons (datum->syntax
                                       #f (car inps)
                                       (list source n (and n 0) n (and n 1)))
                                      r))))]))))

(define ((init-for-language language))
  (cond [(or (not (pair? language))
             (not (eq? 'special (car language))))
         (void)]
        [(eq? (cadr language) 'r5rs)
         (read-case-sensitive #f)
         (read-square-bracket-as-paren #f)
         (read-curly-brace-as-paren #f)
         (read-accept-infix-dot #f)]
        [(memq (cadr language) teaching-langs)
         (read-case-sensitive #t)
         (read-decimal-as-inexact #f)]))

;; Returns a single (module ...) or (begin ...) expression (a `begin' list
;; will be evaluated one by one -- the language might not have a `begin').
;;
;; FIXME: inserting `#%require's here is bad if the language has a
;; `#%module-begin' that processes top-level forms specially.
;; A more general solution would be to create anew module that exports
;; the given language plus all of the given extra requires.
;;
;; We use `#%requre' because, unlike the `require' of scheme/base,
;; it comes from `#%kernel', so it's always present through
;; transitive requires.
(define (build-program language requires input-program)
  (let* ([body (append (if (and (pair? requires) (eq? 'begin (car requires)))
                         (cdr requires)
                         (map (lambda (r) (list #'#%require r)) requires))
                       (input->code input-program 'program 1))]
         [use-lang (lambda (lang) `(module program ,lang . ,body))])
    (cond [(decode-language language) => use-lang]
          [(module-path? language) (use-lang language)]
          [(and (list? language) (eq? 'begin (car language)))
           (append language body)]
          [else (error 'make-evaluator "bad language spec: ~e" language)])))

(define (decode-language language)
  (cond [(and (list? language)
              (= 2 (length language))
              (eq? (car language) 'special)
              (memq (cadr language) teaching-langs))
         `(lib ,(format "htdp-~a.ss" (cadr language)) "lang")]
        [(equal? language '(special r5rs))
         `(lib "lang.ss" "r5rs")]
        [else #f]))

;; Like a toplevel (eval `(begin ,@exprs)), but the language that is used may
;; not have a begin.
(define (eval* exprs)
  (call-with-continuation-prompt
   (lambda ()
     (if (null? exprs)
         (void)
         (let ([deftag (default-continuation-prompt-tag)])
           (let loop ([expr (car exprs)] [exprs (cdr exprs)])
             (if (null? exprs)
                 (eval expr)
                 (begin
                   (call-with-continuation-prompt
                    (lambda () (eval expr))
                    deftag
                    (lambda (x) (abort-current-continuation deftag x)))
                   (loop (car exprs) (cdr exprs))))))))))

(define (evaluate-program program limits uncovered!)
  (when uncovered!
    (eval `(,#'#%require scheme/private/sandbox-coverage)))
  ;; the actual evaluation happens under specified limits, if given
  (let ([run (if (and (pair? program) (eq? 'begin (car program)))
                 (lambda () (eval* (cdr program)))
                 (lambda () (eval program)))]
        [sec (and limits (car limits))]
        [mb  (and limits (cadr limits))])
    (if (or sec mb) (call-with-limits sec mb run) (run)))
  (let ([ns (syntax-case* program (module) literal-identifier=?
              [(module mod . body)
               (identifier? #'mod)
               (let ([mod #'mod])
                 (eval `(,#'require (quote ,mod)))
                 (module->namespace `(quote ,(syntax-e mod))))]
              [_else #f])])
    (when uncovered!
      (let ([get (let ([ns (current-namespace)])
                   (lambda () (eval '(get-uncovered-expressions) ns)))])
        (uncovered! (list (get) get))))
    (when ns (current-namespace ns))))

(define current-eventspace (mz/mr (make-parameter #f) current-eventspace))
(define make-eventspace    (mz/mr void make-eventspace))
(define run-in-bg          (mz/mr thread queue-callback))
(define null-input         (open-input-bytes #""))
(define bg-run->thread
  (if gui?
    (lambda (ignored)
      ((mz/mr void eventspace-handler-thread) (current-eventspace)))
    values))

;; special message values for the evaluator procedure, also inside the user
;; context they're used for function applications.
(define-struct evaluator-message (msg args))
(define-syntax define-evaluator-messenger
  (syntax-rules ()
    ;; with extra args
    [(define-evaluator-messenger (name arg ...) msg)
     (define (name evaluator arg ...)
       (evaluator (make-evaluator-message msg (list arg ...))))]
    [(define-evaluator-messenger (name . args) msg)
     (define (name evaluator . args)
       (evaluator (make-evaluator-message msg (list* args))))]
    ;; without
    [(define-evaluator-messenger name msg)
     (define name
       (let ([evmsg (make-evaluator-message msg '())])
         (lambda (evaluator) (evaluator evmsg))))]))

(define-evaluator-messenger kill-evaluator 'kill)
(define-evaluator-messenger break-evaluator 'break)
(define-evaluator-messenger (set-eval-limits . xs) 'limits)
(define-evaluator-messenger (put-input . xs) 'input)
(define-evaluator-messenger get-output 'output)
(define-evaluator-messenger get-error-output 'error-output)
(define-evaluator-messenger (get-uncovered-expressions . xs) 'uncovered)

(define (make-evaluator* init-hook require-perms program-or-maker)
  (define cust          (make-custodian))
  (define coverage?     (sandbox-coverage-enabled))
  (define uncovered     #f)
  (define input-ch      (make-channel))
  (define result-ch     (make-channel))
  (define input         #f)
  (define output        #f)
  (define error-output  #f)
  (define limits        (sandbox-eval-limits))
  (define user-thread   #t) ; set later to the thread
  (define orig-cust (current-custodian))
  (define (user-kill)
    (when user-thread
      (let ([t user-thread])
        (set! user-thread #f)
        (custodian-shutdown-all cust)
        (kill-thread t))) ; just in case
    (void))
  (define (user-break)
    (when user-thread (break-thread user-thread)))
  (define (user-process)
    (with-handlers ([void (lambda (exn) (channel-put result-ch exn))])
      ;; first set up the environment
      (init-hook)
      ((sandbox-init-hook))
      ;; now read and evaluate the input program
      (evaluate-program
       (if (procedure? program-or-maker) (program-or-maker) program-or-maker)
       limits
       (and coverage? (lambda (es+get) (set! uncovered es+get))))
      (channel-put result-ch 'ok))
    ;; finally wait for interaction expressions
    (let loop ([n 1])
      (let ([expr (channel-get input-ch)])
        (when (eof-object? expr) (channel-put result-ch expr) (user-kill))
        (with-handlers ([void (lambda (exn)
                                (channel-put result-ch (cons 'exn exn)))])
          (let* ([run (if (evaluator-message? expr)
                        (lambda ()
                          (apply (evaluator-message-msg expr)
                                 (evaluator-message-args expr)))
                        (lambda ()
                          (eval* (input->code (list expr) 'eval n))))]
                 [sec (and limits (car limits))]
                 [mb  (and limits (cadr limits))]
                 [run (if (or sec mb)
                        (lambda () (with-limits sec mb (run)))
                        run)])
            (channel-put result-ch
                         (cons 'vals (call-with-values run list)))))
        (loop (add1 n)))))
  (define (user-eval expr)
    (let ([r (if user-thread
               (begin (channel-put input-ch expr)
                      (let loop ()
                        (with-handlers ([(lambda (e)
                                           (and (sandbox-propagate-breaks)
                                                (exn:break? e)))
                                         (lambda (e)
                                           (user-break)
                                           (loop))])
                          (channel-get result-ch))))
               eof)])
      (cond [(eof-object? r) (error 'evaluator "terminated")]
            [(eq? (car r) 'exn) (raise (cdr r))]
            [else (apply values (cdr r))])))
  (define get-uncovered
    (case-lambda
     [() (get-uncovered #t)]
     [(prog?) (get-uncovered prog? 'program)]
     [(prog? src)
      (unless uncovered
        (error 'get-uncovered-expressions "no coverage information"))
      (let ([uncovered (if prog? (car uncovered) ((cadr uncovered)))])
        (if src
            (filter (lambda (x) (equal? src (syntax-source x))) uncovered)
            uncovered))]))
  (define (output-getter p)
    (if (procedure? p) (user-eval (make-evaluator-message p '())) p))
  (define input-putter
    (case-lambda
     [() (input-putter input)]
     [(arg) (cond [(not input)
                   (error 'put-input "evaluator input is not 'pipe")]
                  [(or (string? arg) (bytes? arg))
                   (display arg input) (flush-output input)]
                  [(eof-object? arg) (close-output-port input)]
                  [else (error 'put-input "bad argument: ~e" arg)])]))
  (define (evaluator expr)
    (if (evaluator-message? expr)
      (let ([msg (evaluator-message-msg expr)])
        (case msg
          [(kill)   (user-kill)]
          [(break)  (user-break)]
          [(limits) (set! limits (evaluator-message-args expr))]
          [(input)  (apply input-putter (evaluator-message-args expr))]
          [(output) (output-getter output)]
          [(error-output) (output-getter error-output)]
          [(uncovered) (apply get-uncovered (evaluator-message-args expr))]
          [else (error 'evaluator "internal error, bad message: ~e" msg)]))
      (user-eval expr)))
  (define linked-outputs? #f)
  (define (make-output what out set-out! allow-link?)
    (cond [(not out) (open-output-nowhere)]
          [(and (procedure? out) (procedure-arity-includes? out 0)) (out)]
          [(output-port? out) out]
          [(eq? out 'pipe) (let-values ([(i o) (make-pipe)]) (set-out! i) o)]
          [(memq out '(bytes string))
           (let* ([bytes? (eq? 'bytes out)]
                  ;; the following doesn't really matter: they're the same
                  [out ((if bytes? open-output-bytes open-output-string))])
             (set-out!
              (lambda ()
                (parameterize ([current-custodian orig-cust])
                  (let ([buf (get-output-bytes out #t)])
                    (if bytes? buf (bytes->string/utf-8 buf #\?))))))
             out)]
          [else (error 'make-evaluator "bad sandox-~a spec: ~e" what out)]))
  (parameterize* ; the order in these matters
   (;; create a sandbox context first
    [current-custodian cust]
    [current-thread-group (make-thread-group)]
    [current-namespace (make-evaluation-namespace)]
    ;; set up the IO context
    [current-input-port
     (let ([inp (sandbox-input)])
       (cond
        [(not inp) null-input]
        [(input->port inp) => values]
        [(and (procedure? inp) (procedure-arity-includes? inp 0)) (inp)]
        [(eq? 'pipe inp)
         (let-values ([(i o) (make-pipe)]) (set! input o) i)]
        [else (error 'make-evaluator "bad sandbox-input: ~e" inp)]))]
    [current-output-port (make-output 'output (sandbox-output)
                                      (lambda (o) (set! output o))
                                      #f)]
    [current-error-port (make-output 'error-output (sandbox-error-output)
                                     (lambda (o) (set! error-output o))
                                     #t)]
    ;; paths
    [current-library-collection-paths
     (filter directory-exists?
             (append (sandbox-override-collection-paths)
                     (current-library-collection-paths)))]
    [sandbox-path-permissions
     (append (map (lambda (p) `(read ,p))
                  (current-library-collection-paths))
             (module-specs->path-permissions require-perms)
             (sandbox-path-permissions))]
    ;; general info
    [current-command-line-arguments '#()]
    ;; restrict the sandbox context from this point
    [current-security-guard (sandbox-security-guard)]
    [exit-handler (lambda x (error 'exit "user code cannot exit"))]
    [current-inspector ((sandbox-make-inspector))]
    [current-logger ((sandbox-make-logger))]
    ;; This breaks because we need to load some libraries that are trusted
    ;; [current-code-inspector (make-inspector)]
    ;; Note the above definition of `current-eventspace': in MzScheme, it
    ;; is an unused parameter.  Also note that creating an eventspace
    ;; starts a thread that will eventually run the callback code (which
    ;; evaluates the program in `run-in-bg') -- so this parameterization
    ;; must be nested in the above (which is what paramaterize* does), or
    ;; it will not use the new namespace.
    [current-eventspace (make-eventspace)])
   (set! user-thread (bg-run->thread (run-in-bg user-process)))
   (let ([r (channel-get result-ch)])
     (if (eq? r 'ok)
         ;; initial program executed ok, so return an evaluator
         evaluator
         ;; program didn't execute
         (raise r)))))

(define (make-evaluator language
                        #:requires [requires null] #:allow-read [allow null]
                        . input-program)
  ;; `input-program' is either a single argument specifying a file/string, or
  ;; multiple arguments for a sequence of expressions
  (let (;; make it possible to provide #f for no language and no requires
        [lang language]
        ;; make it possible to use simple paths to files to require
        [reqs (if (not (list? requires))
                (error 'make-evaluator "bad requires: ~e" requires)
                (map (lambda (r)
                       (if (or (pair? r) (symbol? r))
                         r
                         `(file ,(path->string (simplify-path* r)))))
                     requires))])
    (make-evaluator* (init-for-language lang)
                     (append (extract-required (or (decode-language lang)
                                                   lang)
                                               reqs)
                             allow)
                     (lambda () (build-program lang reqs input-program)))))

(define (make-module-evaluator input-program #:allow-read [allow null])
  ;; this is for a complete module input program
  (let ([prog (input->code (list input-program) 'program #f)])
    (unless (= 1 (length prog))
      (error 'make-evaluator "expecting a single `module' program; ~a"
             (if (zero? (length prog))
               "no program expressions given"
               "got more than a single expression")))
    (syntax-case* (car prog) (module) literal-identifier=?
      [(module modname lang body ...)
       (make-evaluator* void allow (car prog))]
      [_else (error 'make-evaluator "expecting a `module' program; got ~e"
                    (syntax->datum (car prog)))])))

