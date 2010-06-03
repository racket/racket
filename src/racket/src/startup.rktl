;; This file is converted to [c]startup.inc and evaluated by
;; Racket's scheme_basic_env().

;; It implements, in a non-bootstrapping way, some functions
;; needed to start up Racket --- especially to install the
;; default module-name resolver.

;; Racket runs ((dynamic-require ''#%boot boot)) on startup. Then,
;; after configuring all startup parameters, Racket may run
;; ((dynamic-require ''#%boot seal)), and it may seal multiple
;; times. So, replace the content of this file to get a different set
;; of initial module definitions and parameter values.

;; When using makefiles, `make startup' in [the build directory for]
;; plt/src/mzscheme creates plt/src/mzscheme/src/cstartup.inc. Note
;; that `make startup' requires a working Racket executable; see
;; schminc.h for information about avoiding cstartup.inc, and using
;; startup.inc (requires perl), instead. In fact, the recommend
;; build strategy for cstartup.inc is
;;   * Run configure in <builddir> with --enable-perl
;;   * Set USE_COMPILED_STARTUP in schminc.h to 0
;;   * Modify startup.rkt to taste
;;   * Run make in <builddir>/mzscheme
;;   * Run make startup in <builddir>/mzscheme
;;   * Set USE_COMPILED_STARTUP in schminc.h to 1
;;   * Run make in <builddir>/mzscheme

;; Do not use block comments (with #| and |#) in this file. The
;; pre-processing script to build startup.inc can't handle them.

;; ------------------------------------------------------
;; Minimal syntax (no error checks!) needed for the rest

(module #%min-stx '#%kernel
  (#%require '#%paramz
             (for-syntax '#%kernel))

  (#%provide unless when
             and or
             cond
             let let* letrec
             parameterize
             define)

  (define-values-for-syntax (here-stx)
    (quote-syntax here))

  (define-syntaxes (unless)
    (lambda (stx)
      (let-values ([(s) (syntax->list stx)])
        (datum->syntax here-stx
                       (list 'if (cadr s)
                             (void)
                             (cons 'begin (cddr s)))))))

  (define-syntaxes (when)
    (lambda (stx)
      (let-values ([(s) (syntax->list stx)])
        (datum->syntax here-stx
                       (list 'if (cadr s)
                             (cons 'begin (cddr s))
                             (void))))))

  (define-syntaxes (and)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (if (null? s)
            (quote-syntax #t)
            (if (null? (cdr s))
                (car s)
                (datum->syntax here-stx
                               (list 'if (car s) (cons 'and (cdr s)) #f)))))))

  (define-syntaxes (or)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (if (null? s)
            (quote-syntax #f)
            (if (null? (cdr s))
                (car s)
                (datum->syntax here-stx
                               (list 'let-values (list (list (list 'x)
                                                             (car s)))
                                     (list 'if 'x 'x (cons 'or (cdr s))))))))))

  (define-syntaxes (let)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (datum->syntax 
         here-stx
         (if (symbol? (syntax-e (car s)))
             (let-values ([(clauses)
                           (map (lambda (c)
                                  (syntax->list c))
                                (syntax->list (cadr s)))])
               (list 'letrec-values (list (list (list (car s))
                                                (list* 'lambda
                                                       (map car clauses)
                                                       (cddr s))))
                     (cons (car s) (map cadr clauses))))
             (list* 'let-values (map (lambda (c)
                                       (let-values ([(c) (syntax->list c)])
                                         (cons (list (car c))
                                               (cdr c))))
                                     (syntax->list (car s)))
                    (cdr s)))))))

  (define-syntaxes (letrec)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (datum->syntax 
         here-stx
         (list* 'letrec-values (map (lambda (c)
                                      (let-values ([(c) (syntax->list c)])
                                        (cons (list (car c))
                                              (cdr c))))
                                    (syntax->list (car s)))
                (cdr s))))))

  (define-syntaxes (let*)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (let-values ([(fst) (syntax->list (car s))])
          (datum->syntax 
           here-stx
           (if (null? fst)
               (list* 'let-values () (cdr s))
               (list 'let (list (car fst))
                     (list* 'let* (cdr fst) (cdr s)))))))))

  (define-syntaxes (parameterize)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (let-values ([(bindings) (apply append
                                        (map syntax->list (syntax->list (car s))))])
          (datum->syntax 
           here-stx
           (list 'with-continuation-mark
                 'parameterization-key
                 (list* 'extend-parameterization
                        '(continuation-mark-set-first #f parameterization-key)
                        bindings)
                 (list* 'let-values ()
                        (cdr s))))))))

  (define-syntaxes (cond)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (if (null? s)
            (quote-syntax (void))
            (datum->syntax 
             here-stx
             (let-values ([(a) (syntax->list (car s))])
               (if (eq? '=> (syntax-e (cadr a)))
                   (list 'let-values (list (list '(v) (car a)))
                         (list* 'cond
                                (list 'v (list (caddr a) 'v))
                                (cdr s)))
                   (list 'if (if (eq? (syntax-e (car a)) 'else)
                                 #t
                                 (car a))
                         (list* 'let-values '() (cdr a))
                         (cons 'cond (cdr s))))))))))
  
  (define-syntaxes (define)
    (lambda (stx)
      (let-values ([(s) (cdr (syntax->list stx))])
        (datum->syntax 
         here-stx
         (if (symbol? (syntax-e (car s)))
             (list 'define-values (list (car s)) (cadr s))
             (let-values ([(a) (syntax-e (car s))])
               (list 'define-values (list (car a))
                     (list* 'lambda (cdr a)
                            (cdr s))))))))))

;; ----------------------------------------
;; Utilities that are eventually exported to the rest of the world
;; (along with much of '#%kernel)

(module #%utils '#%kernel
  (#%require '#%min-stx)

  (#%provide path-string?
             normal-case-path
             path-replace-suffix
             path-add-suffix
             -find-col
             collection-path
             find-library-collection-paths
             path-list-string->path-list
             find-executable-path
             load/use-compiled
             embedded-load)

  (define-values (path-string?)
    (lambda (s)
      (or (path? s) 
          (and (string? s)
               (or (relative-path? s)
                   (absolute-path? s))))))

  (define-values (bsbs) (string #\u5C #\u5C))

  (define-values (normal-case-path)
    (lambda (s)
      (unless (or (path-for-some-system? s)
                  (path-string? s))
        (raise-type-error 'normal-path-case "path (for any system) or valid-path string" s))
      (cond
       [(if (path-for-some-system? s)
            (eq? (path-convention-type s) 'windows)
            (eq? (system-type) 'windows))
        (let ([str (if (string? s) s (bytes->string/locale (path->bytes s)))])
          (if (regexp-match? #rx"^[\u5C][\u5C][?][\u5C]" str)
              (if (string? s)
                  (string->path s)
                  s)
              (let ([s (string-locale-downcase str)])
                (bytes->path 
                 (string->bytes/locale
                  (regexp-replace* #rx"/" 
                                   (if (regexp-match? #rx"[/\u5C][. ]+[/\u5C]*$" s)
                                       ;; Just "." or ".." in last path element - don't remove
                                       s
                                       (regexp-replace* #rx"\u5B .\u5D+([/\u5C]*)$" s "\u005C1"))
                                   bsbs))
                 'windows))))]
       [(string? s) (string->path s)]
       [else s])))

  ;; ------------------------------ Collections ------------------------------

  (define-values (-check-relpath)
    (lambda (who s)
      (unless (path-string? s)
        (raise-type-error who "path or valid-path string" s))
      (unless (relative-path? s)
        (raise (exn:fail:contract
                (string->immutable-string
                 (format "~a: invalid relative path: ~s" who s))
                (current-continuation-marks))))))

  (define-values (-check-collection)
    (lambda (who collection collection-path)
      (-check-relpath who collection) 
      (for-each (lambda (p) (-check-relpath who p)) collection-path)))

  (define-values (collection-path)
    (lambda (collection . collection-path) 
      (-check-collection 'collection-path collection collection-path)
      (-find-col 'collection-path (lambda (s)
                                    (raise
                                     (exn:fail:filesystem s (current-continuation-marks))))
                 collection collection-path)))

  (define-values (-find-col)
    (lambda (who fail collection collection-path)
      (let ([all-paths (current-library-collection-paths)])
        (let cloop ([paths all-paths])
          (if (null? paths)
              (fail
               (format "~a: collection not found: ~s in any of: ~s" 
                       who (if (null? collection-path)
                               collection
                               (apply build-path collection collection-path))
                       all-paths))
              (let ([dir (build-path (car paths) collection)])
                (if (directory-exists? dir)
                    (let ([cpath (apply build-path dir collection-path)])
                      (if (directory-exists? cpath)
                          cpath
                          ;; sub-collection not here; try next instance
                          ;; of the top-level collection
                          (cloop (cdr paths))))
                    (cloop (cdr paths)))))))))

  (define-values (check-suffix-call)
    (lambda (s sfx who)
      (unless (or (path-for-some-system? s)
                  (path-string? s))
        (raise-type-error who "path (for any system) or valid-path string" 0 s sfx))
      (unless (or (string? sfx) (bytes? sfx))
        (raise-type-error who "string or byte string" 1 s sfx))
      (let-values ([(base name dir?) (split-path s)])
        (when (not base)
          (raise-mismatch-error who "cannot add a suffix to a root path: " s))
        (values base name))))
        

  (define-values (path-replace-suffix)
    (lambda (s sfx)
      (let-values ([(base name) (check-suffix-call s sfx 'path-replace-suffix)])
        (let ([new-name (bytes->path-element
                         (regexp-replace #rx#"(?:[.][^.]*|)$"
                                         (path-element->bytes name)
                                         (if (string? sfx)
                                             (string->bytes/locale sfx (char->integer #\?))
                                             sfx))
                         (if (path-for-some-system? s)
                             (path-convention-type s)
                             (system-path-convention-type)))])
          (if (path? base)
              (build-path base new-name)
              new-name)))))

  (define-values (path-add-suffix)
    (lambda (s sfx)
      (let-values ([(base name) (check-suffix-call s sfx 'path-add-suffix)])
        (let ([new-name (bytes->path-element
                         (bytes-append
                          (regexp-replace* #rx#"[.]"
                                           (path-element->bytes name)
                                           "_")
                          (if (string? sfx)
                              (string->bytes/locale sfx (char->integer #\?))
                              sfx))
                         (if (path-for-some-system? s)
                             (path-convention-type s)
                             (system-path-convention-type)))])
          (if (path? base)
              (build-path base new-name)
              new-name)))))

  (define-values (load/use-compiled)
    (lambda (f) ((current-load/use-compiled) f #f)))
    
  (define-values (find-library-collection-paths)
    (case-lambda
     [() (find-library-collection-paths null null)]
     [(extra-collects-dirs) (find-library-collection-paths extra-collects-dirs null)]
     [(extra-collects-dirs post-collects-dirs)
      (let ([user-too? (use-user-specific-search-paths)]
	    [cons-if (lambda (f r) (if f (cons f r) r))])
	(path-list-string->path-list
	 (if user-too?
	     (or (getenv "PLTCOLLECTS") "")
	     "")
	 (cons-if
	  (and user-too?
	       (build-path (find-system-path 'addon-dir)
			   (version)
			   "collects"))
	  (let loop ([l (append
			 extra-collects-dirs
			 (list (find-system-path 'collects-dir))
                         post-collects-dirs)])
	    (if (null? l)
		null
		(let* ([collects-path (car l)]
		       [v
			(cond
			 [(complete-path? collects-path) collects-path]
			 [(absolute-path? collects-path)
			  (path->complete-path collects-path
					       (find-executable-path (find-system-path 'exec-file) #f #t))]
			 [else
			  (find-executable-path (find-system-path 'exec-file) collects-path #t)])])
		  (if v
		      (cons (simplify-path (path->complete-path v (current-directory)))
			    (loop (cdr l)))
		      (loop (cdr l)))))))))]))
  
  (define-values (path-list-string->path-list)
    (let ((r (byte-regexp (string->bytes/utf-8
			   (let ((sep (if (eq? (system-type) 'windows)
                                          ";"
                                          ":")))
			     (format "([^~a]*)~a(.*)" sep sep)))))
	  (cons-path (lambda (default s l) 
		       (if (bytes=? s #"")
			   (append default l)
			     (cons (bytes->path s) l)))))
      (lambda (s default)
	(unless (or (bytes? s)
		    (string? s))
	  (raise-type-error 'path-list-string->path-list "byte string or string" s))
	(unless (and (list? default)
		     (andmap path? default))
	  (raise-type-error 'path-list-string->path-list "list of paths" default))
	(let loop ([s (if (string? s)
			  (string->bytes/utf-8 s)
			  s)])
	  (let ([m (regexp-match r s)])
	    (if m
		(cons-path default (cadr m) (loop (caddr m)))
		(cons-path default s null)))))))

  (define-values (find-executable-path)
    (case-lambda 
     [(program libpath reverse?)
      (unless (path-string? program) 
	(raise-type-error 'find-executable-path "path or string (sans nul)" program))
      (unless (or (not libpath) (and (path-string? libpath) 
				     (relative-path? libpath)))
	(raise-type-error 'find-executable-path "#f or relative path or string" libpath))
      (letrec ([found-exec
		(lambda (exec-name)
                  (if libpath
		      (let-values ([(base name isdir?) (split-path exec-name)])
			(let ([next
			       (lambda ()
				 (let ([resolved (resolve-path exec-name)])
				   (cond
				    [(equal? resolved exec-name) #f]
				    [(relative-path? resolved)
				     (found-exec (build-path base resolved))]
				    [else (found-exec resolved)])))])
			  (or (and reverse? (next))
			      (if (path? base)
				  (let ([lib (build-path base libpath)])
				    (and (or (directory-exists? lib) 
					     (file-exists? lib))
					 lib))
				  #f)
			      (and (not reverse?) (next)))))
		      exec-name))])
	(if (and (relative-path? program)
		 (let-values ([(base name dir?) (split-path program)])
		   (eq? base 'relative)))
	    (let ([paths-str (getenv "PATH")]
		  [win-add (lambda (s) (if (eq? (system-type) 'windows) 
					   (cons (bytes->path #".") s) 
					   s))])
	      (let loop ([paths (if paths-str 
				    (win-add (path-list-string->path-list paths-str null))
				    null)])
		(if (null? paths)
		    #f
		    (let* ([base (path->complete-path (car paths))]
			   [name (build-path base program)])
		      (if (file-exists? name)
			  (found-exec name)
			  (loop (cdr paths)))))))
	    (let ([p (path->complete-path program)])
	      (and (file-exists? p) (found-exec p)))))]
     [(program libpath) (find-executable-path program libpath #f)]
     [(program) (find-executable-path program #f #f)]))

  ;; used for the -k command-line argument:
  (define (embedded-load start end str)
    (let* ([s (if str
                  str
                  (let* ([sp (find-system-path 'exec-file)] 
                         [exe (find-executable-path sp #f)]
                         [start (or (string->number start) 0)]
                         [end (or (string->number end) 0)])
                    (with-input-from-file exe 
                      (lambda ()
                        (file-position (current-input-port) start)
                        (read-bytes (max 0 (- end start)))))))]
           [p (open-input-bytes s)])
      (let loop ()
        (let ([e (parameterize ([read-accept-compiled #t])
                   (read p))])
          (unless (eof-object? e)
            (eval e)
            (loop)))))))

;; ----------------------------------------
;; A module that collects all the built-in modules,
;;  so that it's easier to keep them attached in new
;;  namespaces.

(module #%builtin '#%kernel
  (#%require '#%expobs
             (only '#%foreign)  ; so it's attached, but doesn't depend on any exports
             (only '#%unsafe)   ; ditto
             (only '#%flfxnum)  ; ditto
             '#%paramz
             '#%network
             '#%utils
             (only '#%place)
             (only '#%futures)))

;; ----------------------------------------
;; Handlers to install on startup

(module #%boot '#%kernel
  (#%require '#%min-stx '#%utils '#%paramz)

  (#%provide boot seal)

  (define-values (dll-suffix)
    (system-type 'so-suffix))

  (define-values (default-load/use-compiled)
    (let* ([resolve (lambda (s)
                      (if (complete-path? s)
                          s
                          (let ([d (current-load-relative-directory)])
                            (if d (path->complete-path s d) s))))]
           [date-of-1 (lambda (a)
                        (let ([v (file-or-directory-modify-seconds a #f (lambda () #f))])
                          (and v (cons a v))))]
           [date-of (lambda (a modes)
                      (ormap
                       (lambda (compiled-dir)
                         (let ([a (a compiled-dir)])
                           (date-of-1 a)))
                       modes))]
           [date>=?
            (lambda (modes a bm)
              (and a
                   (let ([am (date-of a modes)])
                     (or (and (not bm) am) 
                         (and am bm (>= (cdr am) (cdr bm)) am)))))])
      (lambda (path expect-module)
        (unless (path-string? path)
          (raise-type-error 'load/use-compiled "path or valid-path string" path))
        (let*-values ([(orig-path) (resolve path)]
                      [(base orig-file dir?) (split-path path)]
                      [(file alt-file) (if expect-module
                                           (let* ([b (path->bytes orig-file)]
                                                  [len (bytes-length b)])
                                             (cond
                                              [(and (len . >= . 4)
                                                    (bytes=? #".rkt" (subbytes b (- len 4))))
                                               ;; .rkt => try .rkt then .ss
                                               (values orig-file
                                                       (bytes->path (bytes-append (subbytes b 0 (- len 4)) #".ss")))]
                                              [else
                                               ;; No search path
                                               (values orig-file #f)]))
                                           (values orig-file #f))]
                      [(path) (if (eq? file orig-file)
                                  orig-path
                                  (build-path base file))]
                      [(alt-path) (and alt-file
                                       (if (eq? alt-file orig-file)
                                           orig-path
                                           (build-path base alt-file)))]
                      [(base) (if (eq? base 'relative) 'same base)]
                      [(modes) (use-compiled-file-paths)])
          (let* ([main-path-d (date-of-1 path)]
                 [alt-path-d (and alt-path 
                                  (not main-path-d)
                                  (date-of-1 alt-path))]
                 [path-d (or main-path-d alt-path-d)]
                 [get-so (lambda (file rep-sfx?)
                           (lambda (compiled-dir)
                             (build-path base
                                         compiled-dir
                                         "native"
                                         (system-library-subpath)
                                         (if rep-sfx?
                                             (path-add-suffix
                                              file
                                              dll-suffix)
                                             file))))]
                 [zo (lambda (compiled-dir)
                       (build-path base
                                   compiled-dir
                                   (path-add-suffix file #".zo")))]
                 [alt-zo (lambda (compiled-dir)
                           (build-path base
                                       compiled-dir
                                       (path-add-suffix alt-file #".zo")))]
                 [so (get-so file #t)]
                 [alt-so (get-so alt-file #t)]
                 [with-dir (lambda (t) 
                             (parameterize ([current-load-relative-directory 
                                             (if (path? base) 
                                                 base 
                                                 (current-directory))])
                               (t)))]
                 [try-main? (or main-path-d (not alt-path-d))]
                 [try-alt? (and alt-file (or alt-path-d (not main-path-d)))])
            (cond
             [(and try-main?
                   (date>=? modes so path-d))
              => (lambda (so-d)
                   (parameterize ([current-module-declare-source #f])
                     (with-dir (lambda () ((current-load-extension) (car so-d) expect-module)))))]
             [(and try-alt?
                   (date>=? modes alt-so alt-path-d))
              => (lambda (so-d)
                   (parameterize ([current-module-declare-source alt-path])
                     (with-dir (lambda () ((current-load-extension) (car so-d) expect-module)))))]
             [(and try-main?
                   (date>=? modes zo path-d))
              => (lambda (zo-d)
                   (parameterize ([current-module-declare-source #f])
                     (with-dir (lambda () ((current-load) (car zo-d) expect-module)))))]
             [(and try-alt?
                   (date>=? modes alt-zo path-d))
              => (lambda (zo-d)
                   (parameterize ([current-module-declare-source alt-path])
                     (with-dir (lambda () ((current-load) (car zo-d) expect-module)))))]
             [else
              (let ([p (if try-main? path alt-path)])
                (parameterize ([current-module-declare-source (and expect-module 
                                                                   (not try-main?)
                                                                   p)])
                  (with-dir (lambda () ((current-load) p expect-module)))))]))))))

  (define-values (default-reader-guard)
    (lambda (path) path))

  (define-values (-module-hash-table-table) (make-weak-hasheq)) ; weak map from namespace to module ht
  (define-values (-path-cache) (make-weak-hash)) ; weak map from `lib' path + corrent-library-paths to symbols
  
  (define-values (-loading-filename) (gensym))
  (define-values (-loading-prompt-tag) (make-continuation-prompt-tag 'module-loading))
  (define-values (-prev-relto) #f)
  (define-values (-prev-relto-dir) #f)

  (define (split-relative-string s coll-mode?)
    (let ([l (let loop ([s s])
               (cond
                [(regexp-match #rx"^(.*?)/(.*)$" s)
                 => (lambda (m)
                      (cons (cadr m)
                            (loop (caddr m))))]
                [else (list s)]))])
      (if coll-mode?
          l
          (let loop ([l l])
            (if (null? (cdr l))
                (values null (car l))
                (let-values ([(c f) (loop (cdr l))])
                  (values (cons (car l) c) f)))))))

  (define-values (orig-paramz) #f)

  (define-values (standard-module-name-resolver)
    (let-values ()
      (define-values (planet-resolver) #f)
      (define-values (standard-module-name-resolver)
        (case-lambda 
         [(s) 
          (unless (resolved-module-path? s)
            (raise-type-error 'standard-module-name-resolver
                              "resolved-module-path"
                              s))
          ;; Just register s as loaded
          (when planet-resolver
            ;; Let planet resolver register, too:
            (planet-resolver s))
          (let ([ht (or (hash-ref -module-hash-table-table
                                  (namespace-module-registry (current-namespace))
                                  #f)
                        (let ([ht (make-hasheq)])
                          (hash-set! -module-hash-table-table
                                     (namespace-module-registry (current-namespace))
                                     ht)
                          ht))])
            (hash-set! ht s 'attach))]
         [(s relto stx) (standard-module-name-resolver s relto stx #t)]
         [(s relto stx load?)
          ;; If stx is not #f, raise syntax error for ill-formed paths
          ;; If s is #f, call to resolver is a notification from namespace-attach-module
          (unless (or (path? s)
                      (module-path? s))
            (if stx
                (raise-syntax-error #f
                                    "bad module path"
                                    stx)
                (raise-type-error 'standard-module-name-resolver
                                  "module-path or path"
                                  s)))
          (cond
           [(and (pair? s) (eq? (car s) 'quote))
            (make-resolved-module-path (cadr s))]
           [(and (pair? s) (eq? (car s) 'planet))
            (unless planet-resolver
              (with-continuation-mark
                  parameterization-key
                  orig-paramz
                (set! planet-resolver (dynamic-require '(lib "planet/resolver.rkt") 'planet-module-name-resolver))))
            (planet-resolver s relto stx load? orig-paramz)]
           [else
            (let ([get-dir (lambda ()
                             (or (and relto
                                      (if (eq? relto -prev-relto)
                                          -prev-relto-dir
                                          (let ([p (resolved-module-path-name relto)])
                                            (and (path? p)
                                                 (let-values ([(base n d?) (split-path p)])
                                                   (set! -prev-relto relto)
                                                   (set! -prev-relto-dir base)
                                                   base)))))
                                 (current-load-relative-directory)
                                 (current-directory)))]
                  [show-collection-err (lambda (s)
                                         (if stx
                                             (raise-syntax-error
                                              #f
                                              s
                                              stx)
                                             (error s)))]
                  [ss->rkt (lambda (s)
                             (let ([len (string-length s)])
                               (if (and (len . >= . 3)
                                        (string=? ".ss" (substring s (- len 3))))
                                   (string-append (substring s 0 (- len 3)) ".rkt")
                                   s)))]
                  [path-ss->rkt (lambda (p)
                                  (let-values ([(base name dir?) (split-path p)])
                                    (if (regexp-match #rx"[.]ss$" (path->bytes name))
                                        (path-replace-suffix p #".rkt")
                                        p)))])
              (let ([s-parsed
                     ;; Non-string result represents an error
                     (cond
                      [(symbol? s)
                       (or (hash-ref -path-cache
                                     (cons s (current-library-collection-paths))
                                     #f)
                           (let-values ([(cols file) (split-relative-string (symbol->string s) #f)])
                             (let ([p (-find-col 'standard-module-name-resolver
                                                 show-collection-err
                                                 (if (null? cols) file (car cols))
                                                 (if (null? cols) null (cdr cols)))])
                               (build-path p (if (null? cols)
                                                 "main.rkt"
                                                 (string-append file ".rkt"))))))]
                      [(string? s)
                       (let* ([dir (get-dir)])
                         (or (hash-ref -path-cache (cons s dir) #f)
                             (let-values ([(cols file) (split-relative-string s #f)])
                               (apply build-path 
                                      dir
                                      (append
                                       (map (lambda (s)
                                              (cond
                                               [(string=? s ".") 'same]
                                               [(string=? s "..") 'up]
                                               [else s]))
                                            cols)
                                       (list (ss->rkt file)))))))]
                      [(path? s) 
                       (if (absolute-path? s)
                           ;; Use filesystem-sensitive `simplify-path' here:
                           (path-ss->rkt (simplify-path s))
                           (list " (a path must be absolute)"))]
                      [(eq? (car s) 'lib)
                       (or (hash-ref -path-cache
                                     (cons s (current-library-collection-paths))
                                     #f)
                           (let*-values ([(cols file) (split-relative-string (cadr s) #f)]
                                         [(old-style?) (if (null? (cddr s))
                                                           (and (null? cols)
                                                                (regexp-match? #rx"[.]" file))
                                                           #t)])
                             (let ([p (let-values ([(cols)
                                                    (if old-style?
                                                        (append (if (null? (cddr s))
                                                                    '("mzlib")
                                                                    (apply append
                                                                           (map (lambda (p)
                                                                                  (split-relative-string p #t))
                                                                                (cddr s))))
                                                                cols)
                                                        (if (null? cols)
                                                            (list file)
                                                            cols))])
                                        (-find-col 'standard-module-name-resolver
                                                   show-collection-err
                                                   (car cols)
                                                   (cdr cols)))])
                               (build-path p (if old-style?
                                                 (ss->rkt file)
                                                 (if (null? cols)
                                                     "main.rkt"
                                                     (if (regexp-match? #rx"[.]" file)
                                                         (ss->rkt file)
                                                         (string-append file ".rkt"))))))))]
                      [(eq? (car s) 'file)
                       ;; Use filesystem-sensitive `simplify-path' here:
                       (path-ss->rkt 
                        (simplify-path (path->complete-path (expand-user-path (cadr s)) (get-dir))))])])
                (unless (or (path? s-parsed)			  
                            (vector? s-parsed))
                  (if stx
                      (raise-syntax-error
                       'require
                       (format "bad module path~a" (if s-parsed
                                                       (car s-parsed)
                                                       ""))
                       stx)
                      (raise-type-error 
                       'standard-module-name-resolver
                       (format "module path~a" (if s-parsed
                                                   (car s-parsed)
                                                   ""))
                       s)))
                ;; At this point, s-parsed is a complete path (or a cached vector)
                (let* ([filename (if (vector? s-parsed)
                                     (vector-ref s-parsed 0)
                                     (simplify-path (cleanse-path s-parsed) #f))]
                       [normal-filename (if (vector? s-parsed)
                                            (vector-ref s-parsed 1)
                                            (normal-case-path filename))])
                  (let-values ([(base name dir?) (if (vector? s-parsed)
                                                     (values 'ignored (vector-ref s-parsed 2) 'ignored)
                                                     (split-path filename))])
                    (let* ([no-sfx (if (vector? s-parsed)
                                       (vector-ref s-parsed 3)
                                       (path-replace-suffix name #""))])
                      (let ([modname (if (vector? s-parsed)
                                         (vector-ref s-parsed 4)
                                         (make-resolved-module-path filename))]
                            [ht (or (hash-ref -module-hash-table-table
                                              (namespace-module-registry (current-namespace))
                                              #f)
                                    (let ([ht (make-hasheq)])
                                      (hash-set! -module-hash-table-table
                                                 (namespace-module-registry (current-namespace))
                                                 ht)
                                      ht))])
                        ;; Loaded already?
                        (when load?
                          (let ([got (hash-ref ht modname #f)])
                            (unless got
                              ;; Currently loading?
                              (let ([l (let ([tag (if (continuation-prompt-available? -loading-prompt-tag)
                                                      -loading-prompt-tag
                                                      (default-continuation-prompt-tag))])
                                         (continuation-mark-set->list
                                          (current-continuation-marks tag)
                                          -loading-filename
                                          tag))]
                                    [nsr (namespace-module-registry (current-namespace))])
                                (for-each
                                 (lambda (s)
                                   (when (and (equal? (cdr s) normal-filename)
                                              (eq? (car s) nsr))
                                     (error
                                      'standard-module-name-resolver
                                      "cycle in loading at ~e: ~e"
                                      filename
                                      (map cdr (reverse (cons s l))))))
                                 l))
                              ((if (continuation-prompt-available? -loading-prompt-tag)
                                   (lambda (f) (f))
                                   (lambda (f) (call-with-continuation-prompt f -loading-prompt-tag)))
                               (lambda ()
                                 (with-continuation-mark -loading-filename (cons 
                                                                            (namespace-module-registry (current-namespace))
                                                                            normal-filename)
                                   (parameterize ([current-module-declare-name modname])
                                     ((current-load/use-compiled) 
                                      filename 
                                      (string->symbol (path->string no-sfx)))))))
                              (hash-set! ht modname #t))))
                        ;; If a `lib' path, cache pathname manipulations
                        (when (and (not (vector? s-parsed))
                                   (or (string? s)
                                       (symbol? s)
                                       (and (pair? s)
                                            (eq? (car s) 'lib))))
                          (hash-set! -path-cache
                                     (if (string? s)
                                         (cons s (get-dir))
                                         (cons s (current-library-collection-paths)))
                                     (vector filename
                                             normal-filename
                                             name
                                             no-sfx
                                             modname)))
                        ;; Result is the module name:
                        modname))))))])]))
      standard-module-name-resolver))

  (define-values (boot)
    (lambda ()
      (seal)
      (current-module-name-resolver standard-module-name-resolver)
      (current-load/use-compiled default-load/use-compiled)
      (current-reader-guard default-reader-guard)))

  (define-values (seal)
    (lambda ()
      (set! orig-paramz
            (reparameterize 
             (continuation-mark-set-first #f parameterization-key))))))
