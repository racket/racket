#lang racket/base

;; ----------------------------------------------------------------------------
;; customization

(define toplevel-prefix       (make-parameter "-")) ; when not in a module
(define saved-values-number   (make-parameter 5))
(define saved-values-patterns (make-parameter '("^" "$~a")))
;; TODO: when there's a few more of these, make them come from the prefs

;; ----------------------------------------------------------------------------

(require racket/list racket/match scribble/text/wrap)

;; ----------------------------------------------------------------------------
;; utilities

(define home-dir (find-system-path 'home-dir))

(define-namespace-anchor anchor)
(define (here-namespace) (namespace-anchor->namespace anchor))

;; autoloads: avoid loading a ton of stuff to minimize startup penalty
(define autoloaded-specs (make-hasheq))
(define (autoloaded? sym) (hash-ref autoloaded-specs sym #f))
(define-syntax-rule (defautoload libspec id ...)
  (begin (define (id . args)
           (set! id (parameterize ([current-namespace (here-namespace)])
                      (dynamic-require 'libspec 'id)))
           (hash-set! autoloaded-specs 'libspec #t)
           (hash-set! autoloaded-specs 'id #t)
           (apply id args))
         ...))

(defautoload racket/system          system system*)
(defautoload racket/file            file->string)
(defautoload setup/path-to-relative path->relative-string/setup)
(defautoload syntax/modcode         get-module-code)
(defautoload racket/path            find-relative-path)

;; similar, but just for identifiers
(define hidden-namespace (make-base-namespace))
(define initial-namespace (current-namespace))
;; when `racket/enter' initializes, it grabs the `current-namespace' to get
;; back to -- which means it should be instantiated in a top level namespace
;; rather than in (here-namespace); but if we use `initial-namespace' we
;; essentially rely on the user to not kill `enter!' (eg, (define enter! 4)).
;; the solution is to make a `hidden-namespace' where we store these bindings,
;; then instantiate needed modules in the initial namespace and immediately
;; attach the modules to the hidden one then use it, so changes to the binding
;; in `initial-namespace' doesn't affect us.
(define (make-lazy-identifier sym from)
  (define id #f)
  (λ () (or id (begin (parameterize ([current-namespace initial-namespace])
                        (namespace-require from))
                      (parameterize ([current-namespace hidden-namespace])
                        (namespace-attach-module initial-namespace from)
                        (namespace-require from)
                        (set! id (namespace-symbol->identifier sym))
                        id)))))

;; makes it easy to use meta-tools without user-namespace contamination
(define (eval-sexpr-for-user form)
  (eval (namespace-syntax-introduce (datum->syntax #f form))))

;; If `mod' is a known module, return it; if it's a symbol and 'mod is
;; known, return 'mod; otherwise return #f.  If `mode' is 'path, return
;; a path for modules that come from files and #f otherwise, and if it's
;; 'path/sym return a path for the same and a symbolic name for known
;; modules with that name.
(define (known-module mod [mode #f])
  (define (known-top mod)
    (and (not (eq? mode 'path))
         (with-handlers ([exn:fail? (λ (_) #f)])
           (module->imports mod)
           (if (eq? mode 'path/sym) (cadr mod) mod))))
  (match mod
    [(list 'quote (? symbol?)) (known-top mod)]
    [_ (or (with-handlers ([exn:fail? (λ (_) #f)])
             (define r
               (resolved-module-path-name
                ((current-module-name-resolver) mod #f #f #f)))
             (if (not mode)
               (and r mod)
               ;; sanity check that path results exists
               (and (or (and (path? r) (file-exists? r))
                        (and (eq? mode 'path/sym) (symbol? r)))
                    r)))
           ;; for symbols, try also 'mod
           (and (symbol? mod) (known-top `',mod)))]))
(define (module->path module)
  (resolved-module-path-name ((current-module-name-resolver) module #f #f #f)))

(define (mpi->name mpi)
  (resolved-module-path-name (module-path-index-resolve mpi)))
(define (->relname x)
  (cond [(path-string? x) (path->relative-string/setup x)]
        [x]))

(define (module-displayable-name mod)
  (define (choose-path x)
    ;; choose the shortest from an absolute path, a relative path, and a
    ;; "~/..." path.
    (if (not (complete-path? x)) ; shouldn't happen
      x
      (let* ([r (path->string (find-relative-path (current-directory) x))]
             [h (path->string (build-path (string->path-element "~")
                                          (find-relative-path home-dir x)))]
             [best (if (< (string-length r) (string-length h)) r h)]
             [best (if (< (string-length best) (string-length x)) best x)])
        best)))
  (define (get-prefix* path)
    (define x (if (string? path) path (path->string path)))
    (define y (->relname path))
    (if (equal? x y)
      (format "~s" (choose-path x))
      (regexp-replace #rx"[.]rkt$" y "")))
  (let loop ([mod mod])
    (match mod
      [(? symbol?)                 (symbol->string mod)]
      [(list 'quote (? symbol? s)) (format "'~a" (loop s))]
      [(list 'file (? string? s))  (loop (string->path s))]
      [(or (? path?) (? string?))  (get-prefix* mod)]
      [_ (error 'xrepl "internal error; ~v" mod)])))

(define (here-source) ; returns a path, a symbol, or #f (= not in a module)
  (variable-reference->module-source
   (eval (namespace-syntax-introduce
          (datum->syntax #f `(,#'#%variable-reference))))))

(define (phase->name phase [fmt #f])
  (define s
    (case phase
      [(0) #f] [(#f) "for-label"] [(1) "for-syntax"] [(-1) "for-template"]
      [else (format "for-meta:~a" phase)]))
  (cond [(not fmt) s] [s (format fmt s)] [else ""]))

;; true if (quote sym) is a known module name
(define (known-module-name? sym)
  (and (symbol? sym)
       (with-handlers ([exn? (λ (_) #f)]) (module->imports `',sym) #t)))

(define last-output-port #f)
(define last-error-port  #f)
(define (maybe-new-output-ports)
  (define-syntax-rule (maybe last cur)
    (unless (eq? last cur)
      (when last (flush-output last)) ; just in case
      (set! last cur)
      (flush-output last)
      (port-count-lines! last)))
  (maybe last-output-port (current-output-port))
  (maybe last-error-port (current-error-port)))
(define (fresh-line [stderr? #f])
  (maybe-new-output-ports)
  (define port (if stderr? last-error-port last-output-port))
  (flush-output port)
  (define-values [line col pos] (port-next-location port))
  (unless (eq? col 0) (newline)))
(define (zero-column!)
  ;; there's a problem whenever there's some printout followed by a read: the
  ;; cursor will at column zero, but the port counting will think that it's
  ;; still right after the printout; call this function in such cases to adjust
  ;; the column to 0.
  (maybe-new-output-ports)
  (define-values [line col pos] (port-next-location last-output-port))
  (set-port-next-location! last-output-port line 0 pos))

;; wrapped output
(define-syntax-rule (with-wrapped-output body ...)
  (do-xrepl-wrapped-output (λ () body ...)))
(define (do-xrepl-wrapped-output thunk)
  (do-wrapped-output thunk #:indent-first -2 #:line-prefix #rx"^;+ *"))
;; maybe move this into scribble/text/wrap
(define (do-wrapped-output thunk
                           #:wrap-width   [width (wrap-width)]
                           #:line-prefix  [prefix-rx #f] ; not including spaces
                           #:indent-first [fst-indent 0] ; can be negative
                           #:split-word   [split-word #f])
  (define-values [ip op] (make-pipe))
  (define widths
    (cond [(fst-indent . > . 0) (cons (- width fst-indent) width)]
          [(fst-indent . < . 0) (cons width (+ width fst-indent))]
          [else (cons width width)]))
  (define indents
    (let ([spaces (make-bytes (abs fst-indent) (char->integer #\space))])
      (cond [(fst-indent . > . 0) (cons spaces #"")]
            [(fst-indent . < . 0) (cons #"" spaces)]
            [else (cons #"" #"")])))
  (define out (current-output-port))
  (define (wrapper)
    (define m (cond [(regexp-match #rx#"^(?:\n|[^\n]+)" ip) => car] [else #f]))
    (when m ; #f => we're at the end
      (if (equal? #"\n" m)
        (newline out)
        (let* ([i (cdar (regexp-match-positions #rx#"^ *" m))]
               [p (regexp-match-positions prefix-rx m i)]
               [i (if (and p (= (caar p) i)) (cdar p) i)]
               [j (caar (regexp-match-positions #rx" *$" m))]
               [widths (cons (- (car widths) i) (- (cdr widths) i))]
               [lines  (wrap-line (bytes->string/utf-8 (subbytes m i j))
                                  widths split-word)])
          (write-bytes m out 0 i)
          (write-bytes (car indents) out)
          (write-string (car lines) out)
          (for ([l (in-list (cdr lines))])
            (newline out)
            (write-bytes m out 0 i)
            (write-bytes (cdr indents) out)
            (write-string l out))))
      (wrapper)))
  (define th (thread wrapper))
  (parameterize ([current-output-port op]) (thunk))
  (close-output-port op)
  (thread-wait th))

;; ----------------------------------------------------------------------------
;; toplevel "," commands management

(struct command (names argline blurb desc handler))
(define commands (make-hasheq))
(define commands-list '()) ; for help displays, in definition order
(define current-command (make-parameter #f))
(define (register-command! names blurb argline desc handler)
  (let* ([names (if (list? names) names (list names))]
         [cmd (command names blurb argline desc handler)])
    (for ([n (in-list names)])
      (if (hash-ref commands n #f)
        (error 'defcommand "duplicate command name: ~s" n)
        (hash-set! commands n cmd)))
    (set! commands-list (cons cmd commands-list))))
(define-syntax-rule (defcommand cmd+aliases argline blurb [desc ...]
                      body0 body ...)
  (register-command! `cmd+aliases `argline `blurb `(desc ...)
                     (λ () body0 body ...)))

(define (cmderror fmt #:default-who [dwho #f] . args)
  (let ([cmd (current-command)])
    (raise-user-error (or (and cmd (string->symbol (format ",~a" cmd)))
                          dwho '???)
                      (apply format fmt args))))

;; returns first peeked non-space/tab char (#\return is considered space too)
(define string->list*
  (let ([t (make-weak-hasheq)]) ; good for string literals
    (λ (s) (hash-ref! t s (λ () (string->list s))))))
(define (skip-spaces/peek [skip " \t\r"])
  (let ([skip (string->list* skip)])
    (let loop ()
      (let ([ch (peek-char)])
        (if (memq ch skip) (begin (read-char) (loop)) ch)))))

(define (here-path [no-path eof])
  (let ([x (here-source)]) (if (path? x) x no-path)))
(define (here-mod-or-eof)
  (let ([x (here-source)])
    (if (not x)
      eof
      (datum->syntax #f
        (cond [(symbol? x) `',x]
              [(path? x)   (let ([s (path->string x)])
                             (if (absolute-path? x) `(file ,s) s))]
              [else (error 'here-mod-or-eof "internal error: ~s" x)])))))

(define (getarg kind [flag 'req] #:default [dflt #f])
  (unless (memq flag '(req opt list list+))
    (error 'getarg "unknown flag: ~e" flag))
  (define (argerror fmt . args)
    (apply cmderror #:default-who 'getarg fmt args))
  (define (missing) (argerror "missing ~a argument" kind))
  (define (get read)
    (define (get-one)
      (cond [(eq? read read-line-arg) (read)]
            [(eq? #\newline (skip-spaces/peek)) eof]
            [else (read)]))
    (define (get-list)
      (let ([x (get-one)]) (if (eof-object? x) '() (cons x (get-list)))))
    (define 0th  (get-one))
    (define 0th? (not (eof-object? 0th)))
    (define 1st  (if (and (not 0th?) dflt) (dflt) 0th))
    (define 1st? (not (eof-object? 1st)))
    (cond [1st? (if (memq flag '(list list+))
                  (cons 1st (if 0th? (get-list) '()))
                  1st)]
          [(eq? flag 'opt) #f]
          [(eq? flag 'list) '()]
          [else (missing)]))
  (define (read-string-arg)
    (define ch (skip-spaces/peek " \t\r\n"))
    (let* ([i (current-input-port)]
           [m (if (eq? ch #\")
                (let ([m (regexp-match #px#"((?:\\\\.|[^\"\\\\]+)+)\"" i)])
                  (and m (regexp-replace* #rx#"\\\\(.)" (cadr m) #"\\1")))
                (cond [(regexp-match #px#"\\S+" i) => car] [else #f]))])
      (if m (bytes->string/locale m) eof)))
  (define (read-line-arg)
    (regexp-replace* #px"^\\s+|\\s+$" (read-line) ""))
  (define (symbolic-shorthand x)
    ;; convenience: symbolic requires that name a file turn to a `file'
    ;; require, and those that name a known module turn to a (quote sym)
    (define dtm (if (syntax? x) (syntax->datum x) x))
    (if (not (symbol? dtm))
      x
      (let* (;; try a file
             [f (expand-user-path (symbol->string dtm))]
             [f (and (file-exists? f) (path->string f))]
             [f (and f (if (absolute-path? f) `(file ,f) f))]
             ;; try a quoted one if the above failed
             [m (or f (and (known-module-name? dtm) `',dtm))]
             [m (and m (if (syntax? x) (datum->syntax x m x) m))])
        (or m x))))
  (define (process-require req)
    ;; no verification of requires -- let the usual error happen if needed
    (symbolic-shorthand req))
  (define (process-module mod)
    (or (known-module (symbolic-shorthand mod))
        (cmderror "unknown module: ~s" mod)))
  (define (translate arg convert)
    (and arg (if (memq flag '(list list+)) (map convert arg) (convert arg))))
  (let loop ([kind kind])
    (case kind
      [(line)    (get read-line-arg)]
      [(string)  (get read-string-arg)]
      [(path)    (translate (loop 'string) expand-user-path)]
      [(sexpr)   (get read)]
      [(syntax)  (translate (get read-syntax) namespace-syntax-introduce)]
      [(require) (translate (loop 'syntax) process-require)]
      [(module)  (translate (loop 'sexpr) process-module)]
      [else (error 'getarg "unknown arg kind: ~e" kind)])))

(define (run-command cmd)
  (parameterize ([current-command cmd])
    (with-handlers ([void (λ (e)
                            (if (exn? e)
                              (eprintf "~a\n" (exn-message e))
                              (eprintf "~s\n" e)))])
      ((command-handler (or (hash-ref commands cmd #f)
                            (error "Unknown command:" cmd)))))))

(defcommand (help h ?) "[<command-name>]"
  "display available commands"
  ["Lists known commands and their help; use with a command name to get"
   "additional information for that command."]
  (define arg (match (getarg 'sexpr 'opt) [(list 'unquote x) x] [x x]))
  (define cmd
    (and arg (hash-ref commands arg
                       (λ () (printf "*** Unknown command: `~s'\n" arg) #f))))
  (define (show-cmd cmd indent)
    (define names (command-names cmd))
    (printf "~a~s" indent (car names))
    (when (pair? (cdr names)) (printf " ~s" (cdr names)))
    (printf ": ~a\n" (command-blurb cmd)))
  (with-wrapped-output
    (if cmd
      (begin (show-cmd cmd "; ")
             (printf ";   usage: ,~a" arg)
             (let ([a (command-argline cmd)]) (when a (printf " ~a" a)))
             (printf "\n")
             (for ([d (in-list (command-desc cmd))])
               (printf "; ~a\n" d)))
      (begin (printf "; Available commands:\n")
             (for-each (λ (c) (show-cmd c ";   ")) (reverse commands-list))))))

;; ----------------------------------------------------------------------------
;; generic commands

(defcommand (exit quit ex) "[<exit-code>]"
  "exit racket"
  ["Optional argument specifies exit code."]
  (cond [(getarg 'sexpr 'opt) => exit] [else (exit)]))

(define last-2dirs
  (make-parameter (let ([d (current-directory)]) (cons d d))))
(define (report-directory-change [mode #f])
  (define curdir (current-directory))
  (define (report) ; remove last "/" and say where we are
    (define-values [base name dir?] (split-path curdir))
    (printf "; now in ~a\n" (if base (build-path base name) curdir)))
  (cond [(not (equal? (car (last-2dirs)) curdir))
         (last-2dirs (cons curdir (car (last-2dirs))))
         (report)]
        [else (case mode
                [(pwd) (report)]
                [(cd)  (printf "; still in the same directory\n")])]))

(defcommand cd "[<path>]"
  "change the current directory"
  ["Sets `current-directory'; expands user paths.  With no arguments, goes"
   "to your home directory.  An argument of `-' indicates the previous"
   "directory."]
  (let* ([arg (or (getarg 'path 'opt) home-dir)]
         [arg (if (equal? arg (string->path "-")) (cdr (last-2dirs)) arg)])
    (if (directory-exists? arg)
      (begin (current-directory arg) (report-directory-change 'cd))
      (eprintf "; cd: no such directory: ~a\n" arg))))

(defcommand pwd #f
  "display the current directory"
  ["Displays the value of `current-directory'."]
  (report-directory-change 'pwd))

(defcommand (shell sh ls cp mv rm md rd git svn) "<shell-command>"
  "run a shell command"
  ["`sh' runs a shell command (via `system'), the aliases run a few useful"
   "unix commands.  (Note: `ls' has some default arguments set.)"
   "If the REPL is inside some module's namespace, the command can use $F"
   "which is set to the full path to this module's source file."]
  (let* ([arg (getarg 'line)]
         [arg (if (equal? "" arg) #f arg)]
         [cmd (current-command)])
    (case cmd
      [(ls) (set! cmd "ls -F")]
      [(shell) (set! cmd 'sh)])
    (let ([cmd (cond [(eq? 'sh cmd) #f]
                     [(symbol? cmd) (symbol->string cmd)]
                     [else cmd])]
          [here (here-path #f)])
      (putenv "F" (if here (path->string here) ""))
      (unless (system (cond [(and (not cmd) (not arg)) (getenv "SHELL")]
                            [(not cmd) arg]
                            [(not arg) cmd]
                            [else (string-append cmd " " arg)]))
        (eprintf "; (exit with an error status)\n"))
      (when here (putenv "F" ""))
      (void))))

(defcommand (edit e) "<file> ..."
  "edit files in your $EDITOR"
  ["Runs your $EDITOR with the specified file/s.  If no files are given, and"
   "the REPL is currently inside a module, the file for that module is used."
   "If $EDITOR is not set, the ,drracket will be used instead."]
  (define env (let ([e (getenv "EDITOR")]) (and (not (equal? "" e)) e)))
  (define exe (and env (find-executable-path env)))
  (cond [(not env)
         (printf "~a, using the ,drracket command.\n"
                 (if env
                   (string-append "$EDITOR ("env") not found in your path")
                   "no $EDITOR variable"))
         (run-command 'drracket)]
        [(not (apply system* exe (getarg 'path 'list #:default here-path)))
         (eprintf "; (exit with an error status)\n")]
        [else (void)]))

(define ->running-dr #f)
(define (->dr . xs) (unless ->running-dr (start-dr)) (->running-dr xs))
(define (start-dr)
  (printf "; starting DrRacket...\n")
  (define c (make-custodian))
  (define ns ((dynamic-require 'racket/gui 'make-gui-namespace)))
  (parameterize ([current-custodian c]
                 [current-namespace ns]
                 [exit-handler (λ (x)
                                 (eprintf "; DrRacket shutdown.\n")
                                 (set! ->running-dr #f)
                                 (custodian-shutdown-all c))])
    ;; construct a kind of a fake sandbox to run drracket in
    (define es
      (eval '(begin (require racket/class racket/gui framework racket/file)
                    (define es (make-eventspace))
                    es)))
    (define (E expr)
      (parameterize ([current-custodian c]
                     [current-namespace ns]
                     [(eval 'current-eventspace ns) es])
        (eval expr ns)))
    (E '(begin
          (define c (current-custodian))
          (define-syntax-rule (Q expr ...)
            (parameterize ([current-eventspace es])
              (queue-callback
               (λ () (parameterize ([current-custodian c]) expr ...)))))
          ;; problem: right after we read commands, readline will save a new
          ;; history in the prefs file which frequently collides with drr; so
          ;; make it use a writeback thing, with silent failures.  (actually,
          ;; this is more likely a result of previously starting drr wrongly,
          ;; but keep this anyway.)
          (let ([t (make-hasheq)] [dirty '()])
            (preferences:low-level-get-preference
             (λ (sym [dflt (λ () #f)])
               (hash-ref t sym
                 (λ () (let ([r (get-preference sym dflt)])
                         (hash-set! t sym r)
                         r)))))
            (preferences:low-level-put-preferences
             (λ (prefs vals)
               (Q (set! dirty (append prefs dirty))
                  (for ([pref (in-list prefs)] [val (in-list vals)])
                    (hash-set! t pref val)))))
            (define (flush-prefs)
              (set! dirty (remove-duplicates dirty))
              (with-handlers ([void void])
                (put-preferences dirty (map (λ (p) (hash-ref t p)) dirty))
                (set! dirty '())))
            (exit:insert-on-callback flush-prefs)
            (define (write-loop)
              (sleep (random 4))
              (when (pair? dirty) (Q (flush-prefs)))
              (write-loop))
            (define th (thread write-loop))
            (exit:insert-on-callback (λ () (Q (kill-thread th)))))
          ;; start it
          (Q (dynamic-require 'drracket #f))
          ;; hide the first untitled window, so drr runs in "server mode"
          (Q (dynamic-require 'drracket/tool-lib #f))
          (define top-window
            (let ([ch (make-channel)])
              (Q (let ([r (get-top-level-windows)])
                   (channel-put ch (and (pair? r) (car r)))))
              (channel-get ch)))
          (Q (when top-window (send top-window show #f))
             ;; and avoid trying to open new windows in there
             (send (group:get-the-frame-group) clear))
          ;; avoid being able to quit so the server stays running,
          ;; also hack: divert quitting into closing all group frames
          (define should-exit? #f)
          (exit:insert-can?-callback
           (λ () (or should-exit?
                     (let ([g (group:get-the-frame-group)])
                       (when (send g can-close-all?) (send g on-close-all))
                       #f))))
          (require drracket/tool-lib))) ; used as usual below
    (define (new)
      (E '(Q (drracket:unit:open-drscheme-window #f))))
    (define open
      (case-lambda
        [() (E '(Q (handler:open-file)))]
        [paths
         (let ([paths (map path->string paths)])
           (E `(Q (let ([f (drracket:unit:open-drscheme-window ,(car paths))])
                    (send f show #t)
                    ,@(for/list ([p (in-list (cdr paths))])
                        `(begin (send f open-in-new-tab ,p)
                                (send f show #t)))))))]))
    (define (quit)
      (E `(Q (set! should-exit? #t) (exit:exit))))
    (define (loop)
      (define m (thread-receive))
      (if (pair? m)
        (let ([proc (case (car m) [(new) new] [(open) open] [(quit) quit]
                          [else (cmderror "unknown flag: -~a" (car m))])])
          (if (procedure-arity-includes? proc (length (cdr m)))
            (apply proc (cdr m))
            (cmderror "bad number of arguments for the -~a flag" (car m))))
        (error '->dr "internal error"))
      (loop))
    (define th (thread loop))
    (set! ->running-dr (λ (xs) (thread-send th xs)))))
(defcommand (drracket dr drr) "[-flag] <file> ..."
  "edit files in DrRacket"
  ["Runs DrRacket with the specified file/s.  If no files are given, and"
   "the REPL is currently inside a module, the file for that module is used."
   "DrRacket is launched directly, without starting a new subprocess, and it"
   "is kept running in a hidden window so further invocations are immediate."
   "In addition to file arguments, the arguments can have a flag that"
   "specifies one of a few operations for the running DrRacket:"
   "* -new: opens a new editing window.  This is the default when no files are"
   "  given and the REPL is not inside a module,"
   "* -open: opens the specified file/s (or the current module's file).  This"
   "  is the default when files are given or when inside a module."
   "* -quit: exits the running instance.  Quitting the application as usual"
   "  will only close the visible window, but it will still run in a hidden"
   "  window.  This command should not be needed under normal circumstances."]
  (define args (getarg 'path 'list #:default here-path))
  (if (null? args)
    (->dr 'new)
    (let* ([cmd (let ([s (path->string (car args))])
                  (and (regexp-match? #rx"^-" s)
                       (string->symbol (substring s 1))))]
           [args (if cmd (cdr args) args)])
      (apply ->dr (or cmd 'open) args))))

;; ----------------------------------------------------------------------------
;; binding related commands

(defcommand (apropos ap) "<search-for> ..."
  "look for a binding"
  ["Additional arguments restrict the shown matches.  The search specs can"
   "have symbols (which specify what to look for in bound names), and regexps"
   "(for more complicated matches)."]
  (let* ([look (map (λ (s) (cond [(symbol? s)
                                  (regexp (regexp-quote (symbol->string s)))]
                                 [(regexp? s) s]
                                 [else (cmderror "bad search spec: ~e" s)]))
                    (getarg 'sexpr 'list))]
         [look (and (pair? look)
                    (λ (str) (andmap (λ (rx) (regexp-match? rx str)) look)))]
         [syms (map (λ (sym) (cons sym (symbol->string sym)))
                    (namespace-mapped-symbols))]
         [syms (if look (filter (λ (s) (look (cdr s))) syms) syms)]
         [syms (sort syms string<? #:key cdr)]
         [syms (map car syms)])
    (with-wrapped-output
      (if (null? syms)
        (printf "; No matches found")
        (begin (printf "; Matches: ~s" (car syms))
               (for ([s (in-list (cdr syms))]) (printf ", ~s" s))))
      (printf ".\n"))))

(defcommand (describe desc id) "[<phase-number>] <identifier-or-module> ..."
  "describe a (bound) identifier"
  ["For a bound identifier, describe where is it coming from; for a known"
   "module, describe its imports and exports.  You can use this command with"
   "several identifiers.  An optional numeric argument specifies phase for"
   "identifier lookup."]
  (define-values [try-mods? level ids/mods]
    (let ([xs (getarg 'syntax 'list)])
      (if (and (pair? xs) (number? (syntax-e (car xs))))
        (values #f (syntax-e (car xs)) (cdr xs))
        (values #t 0 xs))))
  (with-wrapped-output
    (for ([id/mod (in-list ids/mods)])
      (define dtm (syntax->datum id/mod))
      (define mod (and try-mods? (known-module dtm 'path/sym)))
      (define bind
        (cond [(identifier? id/mod) (identifier-binding id/mod level)]
              [mod #f]
              [else (cmderror "not an identifier or a known module: ~s" dtm)]))
      (define bind? (or bind (not mod)))
      (when bind? (describe-binding dtm bind level))
      (when mod
        (parameterize (;; without this the reported paths are wrong
                       [current-load-relative-directory
                        (and (path? mod)
                             (let-values ([(base name dir?) (split-path mod)])
                               (and (path? base) base)))])
          (describe-module dtm mod bind?))))))
(define (describe-binding sym b level)
  (define at-phase (phase->name level " (~a)"))
  (cond
    [(not b)
     (printf "; `~s' is a toplevel (or unbound) identifier~a\n" sym at-phase)]
    [(eq? b 'lexical)
     (printf "; `~s' is a lexical identifier~a\n" sym at-phase)]
    [(or (not (list? b)) (not (= 7 (length b))))
     (cmderror "*** internal error, racket changed ***")]
    [else
     (define-values [src-mod src-id nominal-src-mod nominal-src-id
                     src-phase import-phase nominal-export-phase]
       (apply values b))
     (set! src-mod         (->relname (mpi->name src-mod)))
     (set! nominal-src-mod (->relname (mpi->name nominal-src-mod)))
     (printf "; `~s' is a bound identifier~a,\n" sym at-phase)
     (printf ";   defined~a in ~a~a\n" (phase->name src-phase "-~a") src-mod
             (if (not (eq? sym src-id)) (format " as `~s'" src-id) ""))
     (printf ";   required~a ~a\n" (phase->name import-phase "-~a")
             (if (equal? src-mod nominal-src-mod)
               "directly"
               (format "through \"~a\"~a"
                       nominal-src-mod
                       (if (not (eq? sym nominal-src-id))
                         (format " where it is defined as `~s'" nominal-src-id)
                         ""))))
     (printf "~a" (phase->name nominal-export-phase ";   (exported-~a)\n"))]))
(define (describe-module sexpr mod-path/sym also?)
  (define get
    (if (symbol? mod-path/sym)
      (let ([spec `',mod-path/sym])
        (λ (imp?) ((if imp? module->imports module->exports) spec)))
      (let ([code (get-module-code mod-path/sym)])
        (λ (imp?)
          ((if imp? module-compiled-imports module-compiled-exports) code)))))
  (define (phase<? p1 p2)
    (cond [(eq? p1 p2) #f]
          [(or (eq? p1 0) (not p2)) #t]
          [(or (eq? p2 0) (not p1)) #f]
          [(and (> p1 0) (> p2 0)) (< p1 p2)]
          [(and (< p1 0) (< p2 0)) (> p1 p2)]
          [else (> p1 0)]))
  (define (modname<? x y)
    (cond [(and (string? x) (string? y)) (string<? x y)]
          [(and (symbol? x) (symbol? y))
           (string<? (symbol->string x) (symbol->string y))]
          [(and (symbol? x) (string? y)) #t]
          [(and (string? x) (symbol? y)) #f]
          [else (error 'describe-module "internal error: ~s, ~s" x y)]))
  (define imports
    (filter-map
     (λ (x)
       (and (pair? (cdr x))
            (cons (car x) (sort (map (λ (m) (->relname (mpi->name m))) (cdr x))
                                modname<?))))
     (sort (get #t) phase<? #:key car)))
  (define-values [val-exports stx-exports]
    (let-values ([(vals stxs) (get #f)])
      (define (get-directs l)
        (filter-map
         (λ (x)
           (let ([directs (filter-map (λ (b) (and (null? (cadr b)) (car b)))
                                      (cdr x))])
             (and (pair? directs) (cons (car x) directs))))
         (sort l phase<? #:key car)))
      (values (get-directs vals) (get-directs stxs))))
  (printf "; `~a' is~a a module,\n" sexpr (if also? " also" ""))
  (let ([relname (->relname mod-path/sym)])
    (printf ";   ~a~a\n"
            (if (symbol? relname) "defined directly as '" "located at ")
            relname))
  (if (null? imports)
    (printf ";   no imports.\n")
    (for ([imps (in-list imports)])
      (let ([phase (car imps)] [imps (cdr imps)])
        (printf ";   imports~a: ~a" (phase->name phase "-~a") (car imps))
        (for ([imp (in-list (cdr imps))]) (printf ", ~a" imp))
        (printf ".\n"))))
  (define (show-exports exports kind)
    (for ([exps (in-list exports)])
      (let ([phase (car exps)]
            [exps (sort (cdr exps) string<? #:key symbol->string)])
        (printf ";   direct ~a exports~a: ~a"
                kind (phase->name phase "-~a") (car exps))
        (for ([exp (in-list (cdr exps))]) (printf ", ~a" exp))
        (printf ".\n"))))
  (if (and (null? val-exports) (null? stx-exports))
    (printf ";   no direct exports.\n")
    (begin (show-exports val-exports "value")
           (show-exports stx-exports "syntax"))))

(define help-id (make-lazy-identifier 'help 'racket/help))
(defcommand doc "<any> ..."
  "browse the racket documentation"
  ["Uses Racket's `help' to browse the documentation.  (Note that this can be"
   "used even in languages that don't have the `help' binding.)"]
  (eval-sexpr-for-user `(,(help-id) ,@(getarg 'syntax 'list))))

;; ----------------------------------------------------------------------------
;; require/load commands

(defcommand (require req r) "<require-spec> ...+"
  "require a module"
  ["The arguments are usually passed to `require', unless an argument"
   "specifies an existing filename -- in that case, it's like using a"
   "\"string\" or a (file \"...\") in `require'.  (Note: this does not"
   "work in subforms.)"]
  (more-inputs #`(require #,@(getarg 'require 'list+)))) ; use *our* `require'

(define rr-modules (make-hash)) ; hash to remember reloadable modules

(define last-rr-modules '())

(defcommand (require-reloadable reqr rr) "<module> ..."
  "require a module, make it reloadable"
  ["Same as ,require but the module is required in a way that makes it"
   "possible to reload later.  If it was already loaded then it is reloaded."
   "Note that this is done by setting `compile-enforce-module-constants' to"
   "#f, which prohibits some optimizations."]
  (let ([ms (getarg 'module 'list)])
    (when (pair? ms) (set! last-rr-modules ms)))
  (when (null? last-rr-modules) (cmderror "missing module argument(s)"))
  (parameterize ([compile-enforce-module-constants
                  (compile-enforce-module-constants)])
    (compile-enforce-module-constants #f)
    (for ([mod (in-list last-rr-modules)])
      (define resolved ((current-module-name-resolver) mod #f #f #f))
      (define path     (resolved-module-path-name resolved))
      (define disp     (module-displayable-name mod))
      (if (hash-ref rr-modules resolved #f)
        ;; reload
        (begin (printf "; reloading ~a\n" disp)
               (parameterize ([current-module-declare-name resolved])
                 (load/use-compiled path)))
        ;; require
        (begin (hash-set! rr-modules resolved #t)
               (printf "; requiring ~a\n" disp)
               ;; (namespace-require mod)
               (eval #`(require #,mod)))))))

(define enter!-id (make-lazy-identifier 'enter! 'racket/enter))

(defcommand (enter en) "[<module>] [noisy?]"
  "require a module and go into its namespace"
  ["Uses `enter!' to go into the module's namespace.  A module name can"
   "specify an existing file as with the ,require command.  If no module is"
   "given, and the REPL is already in some module's namespace, then `enter!'"
   "is used with that module, causing it to reload if needed.  (Note that this"
   "can be used even in languages that don't have the `enter!' binding.)"]
  (eval-sexpr-for-user `(,(enter!-id)
                         ,(getarg 'module #:default here-mod-or-eof)
                         ,@(getarg 'syntax 'list)
                         #:dont-re-require-enter)))

(defcommand (toplevel top) #f
  "go back to the toplevel"
  ["Go back to the toplevel, same as ,enter with no arguments."]
  (eval-sexpr-for-user `(,(enter!-id) #f)))

(defcommand (load ld) "<filename> ..."
  "load a file"
  ["Uses `load' to load the specified file(s)."]
  (more-inputs* (map (λ (name) #`(load #,name)) (getarg 'path 'list))))

;; ----------------------------------------------------------------------------
;; debugging commands

;; not useful: catches only escape continuations
;; (define last-break-exn (make-parameter #f))
;; (defcommand (continue cont) #f
;;   "continue from a break"
;;   ["Continue running from the last break."]
;;   (if (last-break-exn)
;;     ((exn:break-continuation (last-break-exn)))
;;     (cmderror 'continue "no break exception to continue from")))

(define last-backtrace #f)
(defcommand (backtrace bt) #f
  "see a backtrace of the last exception"
  ["Display the last exception with its backtrace."]
  (printf "; ~a\n"
          (regexp-replace* #rx"\n+" (or last-backtrace "(no backtrace)")
                           "\n; ")))

(define time-id
  (make-lazy-identifier 'time* 'unstable/time))
(defcommand time "[<count>] <expr> ..."
  "time an expression"
  ["Times execution of an expression, similar to `time' but prints a"
   "little easier to read information.  You can provide an initial number"
   "that specifies how many times to run the expression -- in this case,"
   "the expression will be executed that many times, extreme results are"
   "removed (top and bottom 2/7ths), and the remaining results will be"
   "averaged.  Two garbage collections are triggered before each run; the"
   "resulting value(s) are from the last run."]
  (more-inputs #`(#,(time-id) #,@(getarg 'syntax 'list))))

(define trace-id (make-lazy-identifier 'trace 'racket/trace))
(defcommand (trace tr) "<function> ..."
  "trace a function"
  ["Traces a function (or functions), using the `racket/trace' library."]
  (eval-sexpr-for-user `(,(trace-id) ,@(getarg 'syntax 'list))))

(define untrace-id (make-lazy-identifier 'untrace 'racket/trace))
(defcommand (untrace untr) "<function> ..."
  "untrace a function"
  ["Untraces functions that were traced with ,trace."]
  (eval-sexpr-for-user `(,(untrace-id) ,@(getarg 'syntax 'list))))

(defautoload errortrace
  profiling-enabled instrumenting-enabled clear-profile-results
  output-profile-results execute-counts-enabled annotate-executed-file)

(defcommand (errortrace errt inst) "[<flag>]"
  "errortrace instrumentation control"
  ["An argument is used to perform a specific operation:"
   "  + : turn errortrace instrumentation on (effective only for code that is"
   "      evaluated from now on)"
   "  - : turn it off (also only for future evaluations)"
   "  ? : show status without changing it"
   "With no arguments, toggles instrumentation."]
  (case (getarg 'sexpr 'opt)
    [(#f) (if (autoloaded? 'errortrace)
            (instrumenting-enabled (not (instrumenting-enabled)))
            (instrumenting-enabled #t))]
    [(-)  (when (autoloaded? 'errortrace) (instrumenting-enabled #f))]
    [(+)  (instrumenting-enabled #t)]
    [(?)  (void)]
    [else (cmderror "unknown subcommand")])
  (if (autoloaded? 'errortrace)
    (printf "; errortrace instrumentation is ~a\n"
            (if (instrumenting-enabled) "on" "off"))
    (printf "; errortrace not loaded\n")))

(define profile-id
  (make-lazy-identifier 'profile 'profile))
(define (statistical-profiler)
  (more-inputs #`(#,(profile-id) #,(getarg 'syntax))))
(define (errortrace-profiler)
  (instrumenting-enabled #t)
  (define flags (regexp-replace* #rx"[ \t]+" (getarg 'line) ""))
  (for ([cmd (in-string (if (equal? "" flags)
                          (if (profiling-enabled) "*!" "+")
                          flags))])
    (case cmd
      [(#\+) (profiling-enabled #t) (printf "; profiling is on\n")]
      [(#\-) (profiling-enabled #f) (printf "; profiling is off\n")]
      [(#\*) (output-profile-results #f #t)]
      [(#\#) (output-profile-results #f #f)]
      [(#\!) (clear-profile-results) (printf "; profiling data cleared\n")]
      [else (cmderror "unknown subcommand")])))
(defcommand (profile prof) "[<expr> | <flag> ...]"
  "profiler control"
  ["Runs either the exact errortrace-based profiler, or the statistical one."
   "* If a parenthesized expression is given, run the statistical profiler"
   "  while running it.  This profiler requires no special setup and adds"
   "  almost no overhead, it samples stack traces as execution goes on."
   "* Otherwise the errortrace profiler is used.  This profiler produces"
   "  precise results, but like other errortrace uses, it must be enabled"
   "  before loading the code and it adds noticeable overhead.  In this case,"
   "  an argument is used to determine a specific operation:"
   "  + : turn the profiler on (effective only for code that is evaluated"
   "      from now on)"
   "  - : turn the profiler off (also only for future evaluations)"
   "  * : show profiling results by time"
   "  # : show profiling results by counts"
   "  ! : clear profiling results"
   "  Multiple flags can be combined, for example \",prof *!-\" will show"
   "  profiler results, clear them, and turn it off."
   "* With no arguments, turns the errortrace profiler on if it's off, and if"
   "  it's on it shows the collected results and clears them."
   "Note: using no arguments or *any* of the flags turns errortrace"
   "  instrumentation on, even a \",prof -\".  Use the ,errortrace command if"
   "  you want to turn instrumentation off."]
  (if (memq (skip-spaces/peek) '(#\( #\[ #\{))
    (statistical-profiler)
    (errortrace-profiler)))

(defcommand execution-counts "<file> ..."
  "execution counts"
  ["Enable errortrace instrumentation for coverage, require the file(s),"
   "display the results, disables coverage, and disables instrumentation if"
   "it wasn't previously turned on."]
  (let ([files (getarg 'path 'list)]
        [inst? (and (autoloaded? 'errortrace) (instrumenting-enabled))])
    (more-inputs
     (λ ()
       (instrumenting-enabled #t)
       (execute-counts-enabled #t))
     #`(require #,@(map (λ (file) `(file ,(path->string file))) files))
     (λ ()
       (for ([file (in-list files)])
         (annotate-executed-file file " 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
     (λ ()
       (execute-counts-enabled #f)
       (unless inst? (instrumenting-enabled #f))))))

(defautoload racket/sandbox
  make-module-evaluator kill-evaluator call-with-trusted-sandbox-configuration
  sandbox-coverage-enabled get-uncovered-expressions)

(defcommand (coverage cover) "<file>"
  "coverage information via a sandbox"
  ["Runs the given file in a (trusted) sandbox, and annotates it with"
   "uncovered expression information."]
  (let ([file (getarg 'path)])
    (sandbox-coverage-enabled) ; autoload it
    (parameterize ([sandbox-coverage-enabled #t])
      (define e
        (call-with-trusted-sandbox-configuration
         (λ () (make-module-evaluator file))))
      (define uncovered
        (map (λ (x) (let ([p (sub1 (syntax-position x))])
                      (cons p (+ p (syntax-span x)))))
             (get-uncovered-expressions e #t)))
      (kill-evaluator e)
      (call-with-input-file file
        (λ (inp)
          ;; this is a naive and inefficient solution, could be made efficient
          ;; using `mzlib/integer-set'
          (let loop ()
            (let* ([start  (file-position inp)]
                   [line   (read-line inp)]
                   [len    (and (string? line) (string-length line))]
                   [end    (and len (+ len start))]
                   [indent (and len (regexp-match-positions #px"\\S" line))]
                   [indent (and indent (caar indent))])
              (when len
                (displayln line)
                (when indent
                  (string-fill! line #\space)
                  (for ([u (in-list uncovered)])
                    (when (and ((car u) . < . end)
                               ((cdr u) . > . indent))
                      (for ([i (in-range (max (- (car u) start) indent)
                                         (min (- (cdr u) start) len))])
                        (string-set! line i #\^))))
                  (displayln (regexp-replace #rx" +$" line "")))
                (loop)))))))))

;; ----------------------------------------------------------------------------
;; namespace switching

(define default-namespace-name '*)
(define current-namespace-name (make-parameter default-namespace-name))
(define namespaces
  (let* ([r (namespace-symbol->identifier '#%top-interaction)]
         [r (identifier-binding r)]
         [r (and r (mpi->name (caddr r)))]
         [t (make-hasheq)])
    (hash-set! t (current-namespace-name) (cons (current-namespace) r))
    t))
(defcommand (switch-namespace switch) "[<name>] [? | - | ! [<init>]]"
  "switch to a different repl namespace"
  ["Switch to the <name> namespace, creating it if needed.  The <name> of a"
   "namespace is a symbol or an integer; `*' indicates the initial namespace."
   "These names are used only by this command, they're not bindings.  A new"
   "namespace is initialized using the name of the namespace if it names a"
   "module, or using the same initial module that was used for the current"
   "namespace."
   "If `! <init>' is used, the new namespace will be created even if it"
   "exists, using `<init>' as the initial module.  If `!' is used without an"
   "<init> to reset an existing namespace its initial module is used again,"
   "and if it is used to create a new namespace, the initial module in current"
   "namespace used."
   "You can also use `-' and a name to drop the corresponding namespace"
   "(allowing it to be garbage-collected), and `?' to list all known"
   "namespaces."
   "A few examples:"
   "  ,switch !             reset the current namespace"
   "  ,switch ! racket      reset it using the `racket' language"
   "  ,switch r5rs          switch to a new `r5rs' namespace, initializing it"
   "                        with `r5rs'"
   "  ,switch foo           switch to `foo', creating it if it doesn't exist"
   "  ,switch foo ! racket  switch to newly made `foo', even if it exists"
   "  ,switch foo !         same, but using the same <init> as it was created"
   "                        with, or as the current namespace if `foo' is new"
   "  ,switch ?             list known namespaces and their initial modules"
   "  ,switch - r5rs        drop the `r5rs' namespace"
   "(Note that you can use `^' etc to communicate values between namespaces.)"]
  (define (list-namespaces)
    (printf "; namespaces and their languages:\n")
    (define nss (sort (map (λ (x) (cons (format "~s" (car x)) (cddr x)))
                           (hash-map namespaces cons))
                      string<? #:key car))
    (define maxlen (apply max (map (λ (x) (string-length (car x))) nss)))
    (for ([x (in-list nss)])
      (printf "; ~a:~a ~s\n"
              (car x)
              (make-string (- maxlen (string-length (car x))) #\space)
              (cdr x))))
  (define (delete name)
    (when (eq? name default-namespace-name)
      (cmderror "cannot drop the default namespace"))
    (when (eq? name (current-namespace-name))
      (cmderror "cannot drop the current namespace"))
    (unless (hash-ref namespaces name #f)
      (cmderror "unknown namespace name: ~s" name))
    (hash-remove! namespaces name)
    (printf "; namespace dropped: ~s\n" name))
  (define (switch name force-reset? init)
    (unless (or (not name) (symbol? name) (fixnum? name))
      (cmderror "bad namespace name, must be symbol or fixnum"))
    (define old-namespace (current-namespace))
    ;; if there's an <init>, then it must be forced
    (let* ([name (or name (current-namespace-name))]
           [init
            (cond [init]
                  [(or force-reset? (not (hash-ref namespaces name #f)))
                   (when (eq? name default-namespace-name)
                     ;; no deep reason for this, but might be usful to keep it
                     ;; possible to ,en xrepl/xrepl to change options etc
                     (cmderror "cannot reset the default namespace"))
                   (cdr (or (hash-ref namespaces name #f)
                            (let ([k (known-module name)]) (and k (cons #f k)))
                            (hash-ref namespaces (current-namespace-name) #f)
                            ;; just in case
                            (hash-ref namespaces default-namespace-name #f)))]
                  [else #f])])
      (when init
        (printf "; *** ~a `~s' namespace with ~a ***\n"
                (if (hash-ref namespaces name #f)
                  "Resetting the" "Initializing a new")
                name
                (module-displayable-name init))
        (current-namespace (make-base-empty-namespace))
        (unless (known-module init)
          (parameterize ([current-namespace old-namespace])
            (dynamic-require init #f)) ; instantiate it if needed
          (namespace-attach-module old-namespace init))
        (namespace-require init)
        (hash-set! namespaces name (cons (current-namespace) init))))
    (when (and name (not (eq? name (current-namespace-name))))
      (printf "; *** Switching to the `~s' namespace ***\n" name)
      (let ([x (hash-ref namespaces (current-namespace-name))])
        (unless (eq? (car x) old-namespace)
          (printf "; (note: saving current namespace for `~s')\n"
                  (current-namespace-name))
          (hash-set! namespaces (current-namespace-name)
                     (cons old-namespace (cdr x)))))
      (current-namespace-name name)
      (current-namespace (car (hash-ref namespaces name)))))
  (define (syntax-error)
    (cmderror "syntax error, see \",help ~s\"" (current-command)))
  (match (getarg 'sexpr 'list)
    [(list) (cmderror "what do you want to do?")]
    [(list '?)           (list-namespaces)]
    [(list '? _ ...)     (syntax-error)]
    [(list '- name)      (delete name)]
    [(list '- _ ...)     (syntax-error)]
    [(list '!)           (switch #f   #t #f  )]
    [(list '! init)      (switch #f   #t init)]
    [(list name)         (switch name #f #f  )]
    [(list name '!)      (switch name #t #f  )]
    [(list name '! init) (switch name #t init)]
    [_      (syntax-error)]))

;; ----------------------------------------------------------------------------
;; syntax commands

(define current-syntax (make-parameter #f))
(defautoload racket/pretty pretty-write)
(defautoload macro-debugger/stepper-text expand/step-text)
(define not-in-base
  (λ () (let ([base-stxs #f])
          (unless base-stxs
            (set! base-stxs ; all ids that are bound to a syntax in racket/base
                  (parameterize ([current-namespace hidden-namespace])
                    (let-values ([(vals stxs) (module->exports 'racket/base)])
                      (map (λ (s) (namespace-symbol->identifier (car s)))
                           (cdr (assq 0 stxs)))))))
          (λ (id) (not (ormap (λ (s) (free-identifier=? id s)) base-stxs))))))
(define (macro-stepper . args)
  (define-values [i o] (make-pipe))
  (parameterize ([current-output-port o])
    (thread (λ () (apply expand/step-text args) (close-output-port o))))
  (let loop ()
    (define l (read-line i))
    (unless (eof-object? l)
      ;; hack: beautify the stepper's output -- remove empty line, indent code
      (unless (equal? "" l)
        (printf (if (regexp-match? #px"^[A-Z][a-z]+\\b" l)
                  "; ---- ~a ----\n" "; ~a\n")
                l))
      (loop))))
(defcommand (syntax stx st) "[<expr>] [<flag> ...]"
  "set syntax object to inspect, and control it"
  ["With no arguments, will show the previously set (or expanded) syntax"
   "additional arguments serve as an operation to perform:"
   "- `^' sets the syntax from the last entered expression"
   "- other sexprs set the current syntax explicitly"
   "- `+' will `expand-once' the syntax and show the result (can be used again"
   "      for additional `expand-once' steps)"
   "- `!' will `expand' the syntax and show the result"
   "- `*' will use the syntax stepper to show expansion steps, leaving macros"
   "      from racket/base intact (does not change the currently set syntax)"
   "- `**' similar to `*', but expanding everything"
   "Note that you can specify several syntaxes and operations in a single"
   "invocation."]
  (define args (getarg 'syntax 'list))
  (for ([stx (in-list (if (null? args) '(#f) args))])
    (define (show/set label stx)
      (printf "; ~a\n" label)
      (current-syntax stx)
      (display "; ") (pretty-write (syntax->datum stx)))
    (define (cur) (or (current-syntax) (cmderror "no syntax set yet")))
    (case (and stx (if (identifier? stx) (syntax-e stx) '--none--))
      [(#f) (show/set "Current syntax:" (cur))]
      [(^)  (if (last-input-syntax)
              (show/set "Using last expression:" (last-input-syntax))
              (cmderror "no expression entered yet"))]
      [(+)  (show/set "expand-once ->" (expand-once (cur)))]
      [(!)  (show/set "expand ->" (expand (cur)))]
      [(*)  (printf "; Stepper:\n") (macro-stepper (cur) (not-in-base))]
      [(**) (printf "; Stepper:\n") (macro-stepper (cur))]
      [else
       (if (syntax? stx)
         (begin (printf "; Syntax set\n") (current-syntax stx))
         (cmderror "internal error: ~s ~s" stx (syntax? stx)))])))

(defautoload macro-debugger/analysis/check-requires show-requires)
(defcommand (check-requires ckreq) "[<module>]"
  "check the `require's of a module"
  ["Uses `macro-debugger/analysis/check-requires', see the docs for more"
   "information."]
  (define mod (getarg 'module #:default here-mod-or-eof))
  (define rs (show-requires mod))
  (with-wrapped-output
    (for ([decision (in-list '(keep bypass drop))])
      (define all (filter (λ (x) (eq? decision (car x))) rs))
      (unless (null? all)
        (define names (map cadr all))
        ;; doesn't print the phase number (third element of all members)
        (printf "; ~a: ~a"
                (string-titlecase (symbol->string decision)) (car names))
        (for ([n (in-list (cdr names))]) (printf ", ~a" n))
        (printf ".\n")))))

;; ----------------------------------------------------------------------------
;; dynamic log output control

;; defautoload doesn't support keyword args
(require racket/lazy-require)
(lazy-require [racket/format (~a)])

(define current-log-receiver-thread (make-parameter #f))
(define global-logger (current-logger))
(define other-level 'fatal) ;for "all other" loggers

;; Default some specific loggers one notch above their "noisy"
;; level. That way, if someone sets "all other" loggers to e.g. debug,
;; these won't get noisy. They need to be cranked up explicitly.
(define logger-levels (make-hasheq '([cm-accomplice . warning]
                                     [gc . info])))

(defcommand log "<opt> ..."
  "control log output"
  ["  ,log                  ;show the levels currently in effect."
   "  ,log <logger> <level> ;set logger to show level"
   "  ,log <logger> default ;set logger to use the default, 'all other' level."
   "  ,log <level>          ;set the default level, for 'all other' loggers."
   "  Valid levels: none, fatal, error, warning, info, debug."]
  (define (update)
    (show-logger-levels) ;handy to show new values
    (cond [(current-log-receiver-thread) => kill-thread])
    (let* ([args (append (list global-logger)
                         (flatten (for/list ([(k v) logger-levels])
                                    (list v k)))
                         (list other-level))]
           [r (apply make-log-receiver args)])
      (current-log-receiver-thread
       (thread
        (λ ()
           (let loop ()
             (match (sync r)
               [(vector l m v _)
                (display (format "; [~a] ~a~a\n"
                                 l m
                                 ;; Print v unless opaque/useless
                                 ;; "#<continuation-mark-set>"
                                 (if (and v (not (continuation-mark-set? v)))
                                     (format " ~.s" v) "")))
                (flush-output)])
             (loop)))))))
  (define (show-logger-levels)
    (define wid 20)
    (define (pr k v)
      (printf "; ~a ~a\n"
              (~a k
                  #:min-width wid
                  #:max-width wid
                  #:limit-marker "...")
              v))
    (pr "Logger" "Level")
    (pr (make-string wid #\-) "-------")
    (for ([(k v) logger-levels])
      (pr k v))
    (pr "[all other]" other-level))
  (match (getarg 'sexpr 'list)
    [(list) (show-logger-levels)]
    [(list (and level (or 'none 'fatal 'error 'warning 'info 'debug)))
     (set! other-level level)
     (update)]
    [(list logger 'default)
     (hash-remove! logger-levels logger)
     (update)]
    [(list logger (and level (or 'none 'fatal 'error 'warning 'info 'debug)))
     (hash-set! logger-levels logger level)
     (update)]
    [_ (cmderror "Bad argument. Enter \",help log\" for usage.")]))

;; ----------------------------------------------------------------------------
;; meta evaluation hook

;; questionable value, (and need to display the resulting values etc)
#;
(defcommand meta "<expr>"
  "meta evaluation"
  ["Evaluate the given expression where bindings are taken from the xrepl"
   "module.  This is convenient when you're in a namespace that does not have"
   "a specific binding -- for example, you might be using a language that"
   "doesn't have `current-namespace', so to get it, you can use"
   "`,eval (current-namespace)'.  The evaluation happens in the repl namespace"
   "as usual, only the bindings are taken from the xrepl module -- so you can"
   "use `^' to refer to the result of such an evaluation."]
  (eval (datum->syntax #'here `(#%top-interaction . ,(getarg 'sexpr))))
  (void))

;; ----------------------------------------------------------------------------
;; setup xrepl in the user's racketrc file

(define init-file (find-system-path 'init-file))
(defcommand install! #f
  "install xrepl in your Racket init file"
  ["Installs xrepl in your Racket REPL initialization file.  This is done"
   "carefully: I will tell you about the change, and ask for permission."
   "You can then edit the file if you want to; in your system, you can find it"
   ,(format "at \"~a\"." init-file)]
  (define comment "The following line loads `xrepl' support")
  (define expr  "(require xrepl)")
  (define dexpr "(dynamic-require 'xrepl #f)")
  (define contents (if (file-exists? init-file) (file->string init-file) ""))
  ;; discard the newline for further input
  (let loop () (when (byte-ready?) (read-byte)))
  (define (look-for comment-rx expr)
    (let ([m (regexp-match-positions
              (format "(?<=\r?\n|^) *;+ *~a *\r?\n *~a *(?=\r?\n|$)"
                      comment-rx (regexp-quote expr))
              contents)])
      (and m (car m))))
  (define existing? (look-for (regexp-quote comment) expr))
  (define existing-readline?
    (look-for "load readline support[^\r\n]*" "(require readline/rep)"))
  (define (yes? question)
    (define qtext (string->bytes/utf-8 (format "; ~a? " question)))
    (define inp
      (case (object-name (current-input-port))
        [(readline-input)
         (parameterize ([(dynamic-require 'readline/pread 'readline-prompt)
                         qtext])
           (read-line))]
        [else (write-bytes qtext) (flush-output) (read-line)]))
    (and (string? inp) (regexp-match? #px"^[[:space:]]*[yY]" inp)))
  (cond
    [existing?
     (printf "; already installed, nothing to do\n")
     (when existing-readline?
       (printf "; (better to remove the readline loading, xrepl does that)"))]
    [(let ([m (regexp-match
               (string-append (regexp-quote expr) "|" (regexp-quote dexpr))
               contents)])
       (and m (begin (printf "; found \"~a\", ~a\n"
                             (car m) "looks like xrepl is already installed")
                     (not (yes? "should I continue anyway")))))]
    [else
     (when existing-readline?
       (printf "; found a `readline' loading line\n")
       (if (yes? "xrepl will already do that, ok to remove")
         (set! contents (string-append
                         (substring contents 0 (car existing-readline?))
                         (substring contents (cdr existing-readline?))))
         (printf "; it will be kept ~a\n"
                 "(you can edit the file and removing it later)")))
     (printf "; writing new contents, with an added \"~a\"\n" expr)
     (printf "; (if you want to load it conditionally, edit the file and\n")
     (printf ";  use \"~a\" instead, which is a plain expression)\n" dexpr)
     (if (yes? "OK to continue")
       (begin
         (call-with-output-file* init-file #:exists 'truncate
           (λ (o) (define new (regexp-replace #rx"(?:\r?\n)+$" contents ""))
                  (write-string new o)
                  (unless (equal? "" new) (write-string "\n\n" o))
                  (fprintf o ";; ~a\n~a\n" comment expr)))
         (printf "; new contents written to ~a\n" init-file))
       (printf "; ~a was not updated\n" init-file))])
  (void))

;; ----------------------------------------------------------------------------
;; eval hook that keep track of recent evaluation results

;; saved interaction values (can be #f to disable saving)
(define saved-values (make-parameter '()))
(define (save-values! xs)
  (let* ([xs (filter (λ (x) (not (void? x))) xs)] ; don't save void values
         [xs (map (λ (x) (and x (make-weak-box x))) xs)]) ; save weakly
    (unless (null? xs)
      ;; the order is last, 2nd-to-last, ..., same from prev interactions
      ;; the idea is that `^', `^^', etc refer to the values as displayed
      (saved-values (append (reverse xs) (saved-values)))
      (let ([n (saved-values-number)] [l (saved-values)])
        (when (< n (length l)) (saved-values (take l n)))))))

(define last-saved-names+state (make-parameter '(#f #f #f)))
(define (get-saved-names)
  (define last       (last-saved-names+state))
  (define last-num   (cadr last))
  (define last-ptrns (caddr last))
  (define cur-num    (saved-values-number))
  (define cur-ptrns  (saved-values-patterns))
  (if (and (equal? last-num cur-num) (equal? last-ptrns cur-ptrns))
    (car last)
    (let ([new
           (for*/list ([i (in-range 1 (add1 (saved-values-number)))]
                       [p (in-list cur-ptrns)])
             (string->symbol
              (cond
                [(= 1 (string-length p)) (make-string i (string-ref p 0))]
                [(regexp-match? #rx"^[^~]*~a[^~]*$" p) (format p i)]
                [else (error 'saved-names "bad name pattern: ~e" p)])))])
      (last-saved-names+state (list new cur-num cur-ptrns))
      new)))

;; see comment at the top of this module for the below hair
(require xrepl/saved-values)

;; make saved values available through bindings, but avoid names that
;; already exist in the namespace (possibly from a previous initialization)
(define (initialize-namespace)
  ;; We might run into circularity problems, give up silently in that case
  (when (with-handlers ([exn? (λ (_) #f)])
          (namespace-attach-module (here-namespace) 'xrepl/saved-values)
          (dynamic-require 'xrepl/saved-values (void))
          #t)
    ;; Hack: wire in our parameter for expansions (see comment in saved-values)
    (eval-sexpr-for-user `(,#'set-saved-values-param! ,saved-values))
    (for ([sym (in-list (get-saved-names))])
      (define id (namespace-symbol->identifier sym))
      (unless (identifier-binding id)
        (eval-sexpr-for-user
         `(,#'require (,#'only-in ,#'xrepl/saved-values
                                  [,#'saved-value-ref ,id])))))))

(require (for-syntax racket/base))
(define ((make-xrepl-evaluator orig) expr)
  ;; not useful: catches only escape continuations
  ;;   (with-handlers ([exn:break? (λ (e) (last-break-exn e) (raise e))]) ...)
  (if (saved-values)
    (let ([results (call-with-values (λ () (orig expr)) list)])
      (save-values! results)
      (apply values results))
    (orig expr)))

;; ----------------------------------------------------------------------------
;; capture ",..." and run the commands, use readline/rep when possible

(define get-prefix ; to show before the "> " prompt
  (let ()
    (define (get-prefix)
      (let* ([x (here-source)]
             [x (and x (module-displayable-name (if (symbol? x) `',x x)))]
             [x (or x (toplevel-prefix))]
             [x (let ([ph (namespace-base-phase)])
                  (if (eq? 0 ph) x (format "~a[~a]" x ph)))])
        (if (eq? (current-namespace-name) default-namespace-name)
          x (format "~a::~a" (current-namespace-name) x))))
    (define last-directory #f)
    (define last-namespace #f)
    (define prefix #f)
    (λ ()
      (define curdir (current-directory))
      (unless (and (equal? (current-namespace) last-namespace)
                   (equal? curdir last-directory))
        (report-directory-change)
        (initialize-namespace)
        (set! prefix
              (with-handlers
                  ([exn? (λ (e)
                           (eprintf "; error during prompt calculation: ~a\n"
                                    (exn-message e))
                           "[internal-error]")])
                (get-prefix)))
        (set! last-namespace (current-namespace))
        (set! last-directory curdir))
      prefix)))

;; the last non-command expression read
(define last-input-syntax (make-parameter #f))

(struct more-inputs (list)
        #:constructor-name more-inputs* #:omit-define-syntaxes)
(define (more-inputs . inputs) (more-inputs* inputs))

(define (make-xrepl-reader orig)
  (define (plain-reader prefix) ; a plain reader, without readline
    (display prefix) (display "> ") (flush-output) (zero-column!)
    (let ([in ((current-get-interaction-input-port))])
      ((current-read-interaction) (object-name in) in)))
  (define RL ; no direct dependency on readline
    (with-handlers ([exn? (λ (_) #f)])
      (collection-file-path "pread.rkt" "readline")))
  (define (make-readline-reader)
    (let ([p (dynamic-require RL 'current-prompt)]
          [r (dynamic-require RL 'read-cmdline-syntax)])
      (λ (prefix) ; uses the readline prompt
        (parameterize ([p (bytes-append (string->bytes/locale prefix) (p))])
          (r)))))
  (define reader
    (case (object-name (current-input-port))
      [(stdin)
       (if (or (not (terminal-port? (current-input-port)))
               (eq? 'windows (system-type))
               (regexp-match? #rx"^dumb" (or (getenv "TERM") ""))
               (not RL))
         plain-reader
         (with-handlers ([exn?
                          (λ (e)
                            (eprintf "; Warning: no readline support (~a)\n"
                                     (exn-message e))
                            plain-reader)])
           (dynamic-require 'readline/rep-start #f)
           ;; requiring readline should have changed the reader
           (if (eq? (current-prompt-read)
                    (dynamic-require RL 'read-cmdline-syntax))
             (make-readline-reader)
             (begin (eprintf "; Warning: could not initialize readline\n")
                    plain-reader))))]
      [(readline-input)
       (eprintf "; Note: readline already loaded\n~a\n"
                ";   (better to let xrepl load it for you)")
       (make-readline-reader)]
      [else plain-reader]))
  ;; IO management
  (port-count-lines! (current-input-port))
  ;; wrap the reader to get the command functionality
  (define more-inputs '())
  (define (reader-loop)
    (parameterize ([saved-values #f])
      (define from-queue? (pair? more-inputs))
      (define input
        (if from-queue?
          (begin0 (car more-inputs) (set! more-inputs (cdr more-inputs)))
          (begin (fresh-line) (reader (get-prefix)))))
      (syntax-case input ()
        [(uq cmd) (eq? 'unquote (syntax-e #'uq))
         (let ([r (run-command (syntax->datum #'cmd))])
           (cond [(void? r) (reader-loop)]
                 [(more-inputs? r)
                  (set! more-inputs (append (more-inputs-list r) more-inputs))
                  (reader-loop)]
                 [else (eprintf "; Warning: internal weirdness: ~s\n" r) r]))]
        [_ (begin (unless from-queue? (last-input-syntax input)) input)])))
  reader-loop)

;; ----------------------------------------------------------------------------
;; a display handler that omits stacktraces (making them available later)

(define ((make-xrepl-display-handler orig) str exn)
  (define backtrace?
    (parameterize ([current-error-port (open-output-string)])
      (orig str exn)
      (let* ([s (get-output-string (current-error-port))]
             [s (regexp-replace* #rx"^\n+|\n+$" s "")]
             [s (regexp-replace* #rx"\n\n+" s "\n")])
        ;; temporary hack: this is always on since it shows all fields,
        ;; so ",bt" is now really a generic "more info"
        (and ; (not (equal? str s))
             (begin (set! last-backtrace s) #t)))))
  (define msg "[,bt for context]")
  (parameterize ([current-output-port (current-error-port)])
    (let* ([s (regexp-replace* #rx"^\n+|\n+$" str "")]
           [s (regexp-replace* #rx"\n\n+" s "\n")]
           [s (regexp-replace* #rx"\n  [^\n]+\\.\\.\\.:(?:[^\n]+|\n   )+" s "")]
           [s (regexp-replace* #rx"\n" s "\n; ")]
           [s (if backtrace?
                (string-append s (if (regexp-match? #rx"\n" s) "\n; " " ") msg)
                s)])
      (fresh-line #t)
      (with-wrapped-output (printf "; ~a\n" s)))))

;; ----------------------------------------------------------------------------
;; set up the xrepl environment

(provide setup-xrepl-environment)
(define (setup-xrepl-environment)
  (define (tweak param maker) (param (maker (param))))
  (tweak error-display-handler make-xrepl-display-handler)
  (tweak current-eval make-xrepl-evaluator)
  (tweak current-prompt-read make-xrepl-reader))
