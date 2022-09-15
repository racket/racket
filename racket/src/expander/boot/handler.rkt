#lang racket/base
(require '#%paramz
         racket/private/place-local
         racket/private/choose-file-to-load
         "../eval/collection.rkt"
         "../syntax/api.rkt"
         "../syntax/error.rkt"
         "../syntax/srcloc.rkt"
         "../syntax/taint.rkt"
         "../namespace/namespace.rkt"
         "../eval/parameter.rkt"
         "../eval/main.rkt"
         "../eval/dynamic-require.rkt"
         "../namespace/api.rkt"
         "../common/module-path.rkt"
         "../eval/module-read.rkt"
         "../expand/missing-module.rkt"
         "../read/api.rkt"
         "../read/primitive-parameter.rkt"
         "load-handler.rkt"
         "../common/performance.rkt")

(provide boot
         seal
         orig-paramz

         boot-primitives)

(define default-load/use-compiled
  (let* ([with-dir* (lambda (base t)
                      (parameterize ([current-load-relative-directory 
                                      (if (path? base) 
                                          base 
                                          (current-directory))])
                        (t)))])
    (lambda (path expect-module)
      (unless (path-string? path)
        (raise-argument-error 'load/use-compiled "path-string?" path))
      (unless (or (not expect-module)
                  (symbol? expect-module)
                  (and (list? expect-module)
                       ((length expect-module) . > . 1)
                       (or (symbol? (car expect-module))
                           (not (car expect-module)))
                       (andmap symbol? (cdr expect-module))))
        (raise-argument-error 'load/use-compiled "(or/c #f symbol? (cons/c (or/c #f symbol?) (non-empty-listof symbol?)))" path))
      (define name (and expect-module (current-module-declare-name)))
      (define ns-hts (and name (registry-table-ref (namespace-module-registry (current-namespace)))))
      (define use-path/src (and ns-hts (hash-ref (cdr ns-hts) name #f)))
      (if use-path/src
          ;; Use previous decision of .zo vs. source:
          (parameterize ([current-module-declare-source (cadr use-path/src)])
            (with-dir* (caddr use-path/src)
              (lambda () ((current-load) (car use-path/src) expect-module))))
          (let-values ([(the-module-declare-source file-type the-path)
                        (choose-file-to-load path expect-module #t expect-module
                                             #f
                                             (current-compiled-file-roots)
                                             (use-compiled-file-paths))])
            (when the-path
              (define-values (_base orig-file dir?) (split-path path))
              (define base (if (eq? _base 'relative) 'same _base))
              (when (equal? file-type 'zo)
                (register-zo-path name ns-hts the-path the-module-declare-source base))
              (parameterize ([current-module-declare-source the-module-declare-source])
                (with-dir* base
                  (lambda () ((if (equal? file-type 'so)
                                  (current-load-extension)
                                  (current-load))
                              the-path
                              expect-module))))))))))

(define (register-zo-path name ns-hts path src-path base)
  (when ns-hts
    (hash-set! (cdr ns-hts) name (list path src-path base))))

(define (default-reader-guard path)
  path)

;; weak map from namespace to pair of module-name hts
(define-place-local -module-hash-table-table
  (make-weak-hasheq))

(define (registry-table-ref reg)
  (define e (hash-ref -module-hash-table-table
                      reg
                      #f))
  (and e (ephemeron-value e)))

(define (registry-table-set! reg v)
  (hash-set! -module-hash-table-table
             reg
             (make-ephemeron reg v)))

;; Weak map from a module registries to a cache that maps module
;; references to resolved-module information. The idea behind mapping
;; from a registry is that changes made to the collection mapping
;; (e.g., by installing a package) reliably take effect when changing
;; namespaces, so using the same namespace may not see the change.
;; Also, we only cache on successful loads, so changing the mapping
;; for that namespace probably doesn't make sense, anyway, for
;; anything that was successfully loaded.
(define-place-local path-caches (make-weak-hasheq))

(define (path-cache-get p reg)
  (define cache (hash-ref path-caches reg #hash()))
  (hash-ref cache p #f))
  
(define (path-cache-set! p reg v)
  (define current-cache (hash-ref path-caches reg #hash()))
  ;; Limit cache memory use by flushing the whole thing when it
  ;; reaches a maximum size:
  (define cache (if (= (hash-count current-cache) 1024)
                    #hash()
                    current-cache))
  (hash-set! path-caches reg (hash-set cache p v)))

(define -loading-filename (gensym))
(define -loading-prompt-tag (make-continuation-prompt-tag 'module-loading))
(define-place-local -prev-relto #f)
(define-place-local -prev-relto-dir #f)

(define (split-relative-string s coll-mode?)
  (let ([l (let loop ([s s])
             (let ([len (string-length s)])
               (let iloop ([i 0])
                 (cond
                  [(= i len) (list s)]
                  [(char=? #\/ (string-ref s i))
                   (cons (substring s 0 i)
                         (loop (substring s (add1 i))))]
                  [else (iloop (add1 i))]))))])
    (if coll-mode?
        l
        (let loop ([l l])
          (if (null? (cdr l))
              (values null (car l))
              (let-values ([(c f) (loop (cdr l))])
                (values (cons (car l) c) f)))))))

(define (format-source-location stx)
  (srcloc->string (srcloc (syntax-source stx)
                          (syntax-line stx)
                          (syntax-column stx)
                          (syntax-position stx)
                          (syntax-span stx))))

(define-place-local orig-paramz #f)
(define-place-local planet-resolver #f)

(define (prep-planet-resolver!)
  (unless planet-resolver
    (with-continuation-mark
     parameterization-key
     orig-paramz
     (set! planet-resolver (dynamic-require '(lib "planet/resolver.rkt") 'planet-module-name-resolver)))))

(define standard-module-name-resolver
  (case-lambda 
    [(s from-namespace) 
     (unless (resolved-module-path? s)
       (raise-argument-error 'standard-module-name-resolver
                             "resolved-module-path?"
                             s))
     (unless (or (not from-namespace) (namespace? from-namespace))
       (raise-argument-error 'standard-module-name-resolver
                             "(or/c #f namespace?)"
                             from-namespace))
     (when planet-resolver
       ;; Let planet resolver register, too:
       (planet-resolver s))
     ;; Register s as loaded:
     (let ([hts (or (registry-table-ref (namespace-module-registry (current-namespace)))
                    (let ([hts (cons (make-hasheq) (make-hasheq))])
                      (registry-table-set! (namespace-module-registry (current-namespace))
                                           hts)
                      hts))])
       (hash-set! (car hts) s 'declared)
       ;; If attach from another namespace, copy over source-file path, if any:
       (when from-namespace
         (let ([root-name (if (pair? (resolved-module-path-name s))
                              (make-resolved-module-path (car (resolved-module-path-name s)))
                              s)]
               [from-hts (registry-table-ref (namespace-module-registry from-namespace))])
           (when from-hts
             (let ([use-path/src (hash-ref (cdr from-hts) root-name #f)])
               (when use-path/src
                 (hash-set! (cdr hts) root-name use-path/src)))))))]
    [(s relto stx) ; for backward-compatibility
     (log-message (current-logger) 'error
                  "default module name resolver called with three arguments (deprecated)"
                  #f)
     (standard-module-name-resolver s relto stx #t)]          
    [(s relto stx load?)
     ;; If stx is not #f, raise syntax error for ill-formed paths
     (unless (module-path? s)
       (if (syntax? stx)
           (raise-syntax-error #f
                               "bad module path"
                               stx)
           (raise-argument-error 'standard-module-name-resolver
                                 "module-path?"
                                 s)))
     (unless (or (not relto) (resolved-module-path? relto))
       (raise-argument-error 'standard-module-name-resolver
                             "(or/c #f resolved-module-path?)"
                             relto))
     (unless (or (not stx) (syntax? stx))
       (raise-argument-error 'standard-module-name-resolver
                             "(or/c #f syntax?)"
                             stx))
     (define (flatten-sub-path base orig-l)
       (let loop ([a null] [l orig-l])
         (cond
           [(null? l) (if (null? a)
                          base
                          (cons base (reverse a)))]
           [(equal? (car l) "..")
            (if (null? a)
                (error
                 'standard-module-name-resolver
                 "too many \"..\"s in submodule path: ~.s"
                 (list* 'submod
                        (if (equal? base ".") 
                            base 
                            (if (path? base)
                                base
                                (list (if (symbol? base) 'quote 'file) base)))
                        orig-l))
                (loop (cdr a) (cdr l)))]
           [else (loop (cons (car l) a) (cdr l))])))
     (cond
       [(and (pair? s) (eq? (car s) 'quote))
        (make-resolved-module-path (cadr s))]
       [(and (pair? s) (eq? (car s) 'submod)
             (pair? (cadr s)) (eq? (caadr s) 'quote))
        (make-resolved-module-path (flatten-sub-path (cadadr s) (cddr s)))]
       [(and (pair? s) (eq? (car s) 'submod)
             (or (equal? (cadr s) ".")
                 (equal? (cadr s) ".."))
             (and relto
                  (let ([p (resolved-module-path-name relto)])
                    (or (symbol? p)
                        (and (pair? p) (symbol? (car p)))))))
        (define rp (resolved-module-path-name relto))
        (make-resolved-module-path (flatten-sub-path (if (pair? rp) (car rp) rp)
                                                     (let ([r (if (equal? (cadr s) "..")
                                                                  (cdr s)
                                                                  (cddr s))])
                                                       (if (pair? rp)
                                                           (append (cdr rp) r)
                                                           r))))]
       [(and (pair? s) (eq? (car s) 'planet))
        (prep-planet-resolver!)
        (planet-resolver s relto stx load? #f orig-paramz)]
       [(and (pair? s)
             (eq? (car s) 'submod)
             (pair? (cadr s))
             (eq? (caadr s) 'planet))
        (prep-planet-resolver!)
        (planet-resolver (cadr s) relto stx load? (cddr s) orig-paramz)]
       [else
        (let ([get-dir (lambda ()
                         (or (and relto
                                  (if (eq? relto -prev-relto)
                                      -prev-relto-dir
                                      (let ([p (resolved-module-path-name relto)])
                                        (let ([p (if (pair? p) (car p) p)])
                                          (and (path? p)
                                               (let-values ([(base n d?) (split-path p)])
                                                 (set! -prev-relto relto)
                                                 (set! -prev-relto-dir base)
                                                 base))))))
                             (current-load-relative-directory)
                             (current-directory)))]
              [get-reg (lambda ()
                         (namespace-module-registry (current-namespace)))]
              [show-collection-err (lambda (msg)
                                     (let ([msg (string-append
                                                 (or (and stx
                                                          (error-print-source-location)
                                                          (format-source-location stx))
                                                     "standard-module-name-resolver")
                                                 ": "
                                                 (regexp-replace #rx"\n" 
                                                                 msg
                                                                 (format "\n  for module path: ~s\n"
                                                                         s)))])
                                       (raise
                                        (if stx
                                            (exn:fail:syntax:missing-module
                                             msg
                                             (current-continuation-marks)
                                             (list (syntax-taint stx))
                                             s)
                                            (exn:fail:filesystem:missing-module
                                             msg
                                             (current-continuation-marks)
                                             s)))))]
              [invent-collection-dir (lambda (f-file col col-path fail)
                                       (lambda (msg)
                                         ;; No such module => make a module-name symbol that
                                         ;; certainly isn't declared
                                         (string->uninterned-symbol
                                          (path->string
                                           (build-path (apply build-path col col-path) f-file)))))]
              [ss->rkt (lambda (s)
                         (let ([len (string-length s)])
                           (if (and (len . >= . 3)
                                    ;; ".ss"
                                    (equal? #\. (string-ref s (- len 3)))
                                    (equal? #\s (string-ref s (- len 2)))
                                    (equal? #\s (string-ref s (- len 1))))
                               (string-append (substring s 0 (- len 3)) ".rkt")
                               s)))]
              [path-ss->rkt (lambda (p)
                              (let-values ([(base name dir?) (split-path p)])
                                (if (regexp-match #rx"[.]ss$" (path->bytes name))
                                    (path-replace-extension p #".rkt")
                                    p)))]
              [s (if (and (pair? s) (eq? 'submod (car s)))
                     (let ([v (cadr s)])
                       (if (or (equal? v ".")
                               (equal? v ".."))
                           (if relto
                               ;; must have a path inside, or we wouldn't get here
                               (let ([p (resolved-module-path-name relto)])
                                 (if (pair? p)
                                     (car p)
                                     p))
                               (error 'standard-module-name-resolver
                                      "no base path for relative submodule path: ~.s"
                                      s))
                           v))
                     s)]
              [subm-path (if (and (pair? s) (eq? 'submod (car s)))
                             (let ([p (if (and (or (equal? (cadr s) ".")
                                                   (equal? (cadr s) ".."))
                                               relto)
                                          (let ([p (resolved-module-path-name relto)]
                                                [r (if (equal? (cadr s) "..")
                                                       (cdr s)
                                                       (cddr s))])
                                            (if (pair? p)
                                                (flatten-sub-path (car p) (append (cdr p) r))
                                                (flatten-sub-path p r)))
                                          (flatten-sub-path "." 
                                                            (if (equal? (cadr s) "..")
                                                                (cdr s)
                                                                (cddr s))))])
                               ;; flattening may erase the submodule path:
                               (if (pair? p)
                                   (cdr p)
                                   #f))
                             #f)])
          (let ([s-parsed
                 ;; Non-string, non-vector result represents an error, but
                 ;; a symbol result is a special kind of error for the purposes
                 ;; of dealing with a submodule path when there's no such
                 ;; collection
                 (cond
                   [(symbol? s)
                    (or (path-cache-get s (get-reg))
                        (let-values ([(cols file) (split-relative-string (symbol->string s) #f)])
                          (let* ([f-file (if (null? cols)
                                             "main.rkt"
                                             (string-append file ".rkt"))]
                                 [col (if (null? cols) file (car cols))]
                                 [col-path (if (null? cols) null (cdr cols))])
                            (find-col-file (if (not subm-path)
                                               show-collection-err
                                               ;; Invent a fictional collection directory, if necessary,
                                               ;; so that we don't raise an exception:
                                               (invent-collection-dir f-file col col-path
                                                                      show-collection-err))
                                           col
                                           col-path
                                           f-file
                                           #t))))]
                   [(string? s)
                    (let* ([dir (get-dir)])
                      (or (path-cache-get (cons s dir) #f)
                          (let-values ([(cols file) (split-relative-string s #f)])
                            (if (null? cols)
                                (build-path dir (ss->rkt file))
                                (apply build-path 
                                       dir
                                       (append
                                        (map (lambda (s)
                                               (cond
                                                 [(string=? s ".") 'same]
                                                 [(string=? s "..") 'up]
                                                 [else s]))
                                             cols)
                                        (list (ss->rkt file))))))))]
                   [(path? s) 
                    ;; Use filesystem-sensitive `simplify-path' here:
                    (path-ss->rkt (simplify-path (if (complete-path? s)
                                                     s
                                                     (path->complete-path s (get-dir)))))]
                   [(eq? (car s) 'lib)
                    (or (path-cache-get s (get-reg))
                        (let*-values ([(cols file) (split-relative-string (cadr s) #f)]
                                      [(old-style?) (if (null? (cddr s))
                                                        (and (null? cols)
                                                             (regexp-match? #rx"[.]" file))
                                                        #t)])
                          (let* ([f-file (if old-style?
                                             (ss->rkt file)
                                             (if (null? cols)
                                                 "main.rkt"
                                                 (if (regexp-match? #rx"[.]" file)
                                                     (ss->rkt file)
                                                     (string-append file ".rkt"))))])
                            (let-values ([(cols)
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
                              (find-col-file show-collection-err
                                             (car cols)
                                             (cdr cols)
                                             f-file
                                             #t)))))]
                   [(eq? (car s) 'file)
                    ;; Use filesystem-sensitive `simplify-path' here:
                    (path-ss->rkt 
                     (simplify-path (path->complete-path (expand-user-path (cadr s)) (get-dir))))])])
            (cond
              [(symbol? s-parsed)
               ;; Return a genenerated symnol
               (make-resolved-module-path
                (cons s-parsed subm-path))]
              [(not (or (path? s-parsed)
                        (vector? s-parsed)))
               (if stx
                   (raise-syntax-error
                    'require
                    (format "bad module path~a" (if s-parsed
                                                    (car s-parsed)
                                                    ""))
                    stx)
                   (raise-argument-error 
                    'standard-module-name-resolver
                    "module-path?"
                    s))]
              [else
               ;; At this point, s-parsed is a complete path (or a cached vector)
               (define filename (if (vector? s-parsed)
                                    (vector-ref s-parsed 0)
                                    (simplify-path (cleanse-path s-parsed) #f)))
               (define normal-filename (if (vector? s-parsed)
                                           (vector-ref s-parsed 1)
                                           (normal-case-path filename)))
               (define-values (base name dir?) (if (vector? s-parsed)
                                                   (values 'ignored (vector-ref s-parsed 2) 'ignored)
                                                   (split-path filename)))
               (define no-sfx (if (vector? s-parsed)
                                  (vector-ref s-parsed 3)
                                  (path-replace-extension name #"")))
               (define root-modname (if (vector? s-parsed)
                                        (vector-ref s-parsed 4)
                                        (make-resolved-module-path filename)))
               (define hts (or (registry-table-ref (get-reg))
                               (let ([hts (cons (make-hasheq) (make-hasheq))])
                                 (registry-table-set! (get-reg)
                                                      hts)
                                 hts)))
               (define modname (if subm-path
                                   (make-resolved-module-path 
                                    (cons (resolved-module-path-name root-modname)
                                          subm-path))
                                   root-modname))
               ;; Loaded already?
               (when load?
                 (let ([got (hash-ref (car hts) modname #f)])
                   (unless got
                     ;; Currently loading?
                     (let ([loading
                            (let ([tag (if (continuation-prompt-available? -loading-prompt-tag)
                                           -loading-prompt-tag
                                           (default-continuation-prompt-tag))])
                              (continuation-mark-set-first
                               #f
                               -loading-filename
                               null
                               tag))]
                           [nsr (get-reg)])
                       (for-each
                        (lambda (s)
                          (when (and (equal? (cdr s) normal-filename)
                                     (eq? (car s) nsr))
                            (error
                             'standard-module-name-resolver
                             "cycle in loading\n  at path: ~a\n  paths:~a"
                             filename
                             (apply string-append
                                    (let loop ([l (reverse loading)])
                                      (if (null? l)
                                          '()
                                          (list* "\n   " (path->string (cdar l)) (loop (cdr l)))))))))
                        loading)
                       ((if (continuation-prompt-available? -loading-prompt-tag)
                            (lambda (f) (f))
                            (lambda (f) (call-with-continuation-prompt f -loading-prompt-tag)))
                        (lambda ()
                          (with-continuation-mark
                           -loading-filename (cons (cons nsr normal-filename)
                                                   loading)
                           (parameterize ([current-module-declare-name root-modname]
                                          [current-module-path-for-load
                                           ;; If `s' is an absolute module path, then
                                           ;; keep it as-is, the better to let a tool
                                           ;; recommend how to get an unavailable module;
                                           ;; also, propagate the source location.
                                           ((if stx
                                                (lambda (p) (datum->syntax #f p stx))
                                                values)
                                            (cond
                                              [(symbol? s) s]
                                              [(and (pair? s) (eq? (car s) 'lib)) s]
                                              [else (if (resolved-module-path? root-modname)
                                                        (let ([src (resolved-module-path-name root-modname)])
                                                          (if (symbol? src)
                                                              (list 'quote src)
                                                              src))
                                                        root-modname)]))])
                             ((current-load/use-compiled) 
                              filename 
                              (let ([sym (string->symbol (path->string no-sfx))])
                                (if subm-path
                                    (if (hash-ref (car hts) root-modname #f)
                                        ;; Root is already loaded, so only use .zo
                                        (cons #f subm-path)
                                        ;; Root isn't loaded, so it's ok to load form source:
                                        (cons sym subm-path))
                                    sym)))))))))))
               ;; If a `lib' path, cache pathname manipulations
               (when (and (not (vector? s-parsed))
                          load?
                          (or (string? s)
                              (symbol? s)
                              (and (pair? s)
                                   (eq? (car s) 'lib))))
                 (path-cache-set! (if (string? s)
                                      (cons s (get-dir))
                                      s)
                                  (if (string? s)
                                      #f
                                      (get-reg))
                                  (vector filename
                                          normal-filename
                                          name
                                          no-sfx
                                          root-modname)))
               ;; Result is the module name:
               modname])))])]))

(define default-eval-handler
  (lambda (s)
    (eval s
          (current-namespace)
          (let ([c (current-compile)])
            (lambda (e ns)
              ;; `ns` is `(current-namespace)`, but possibly
              ;; phase-shifted
              (if (eq? ns (current-namespace))
                  (c e #t)
                  (parameterize ([current-namespace ns])
                    (c e #t))))))))

(define default-compile-handler
  ;; Constrained to two arguments:
  (lambda (s immediate-eval?) (compile s
                                       (current-namespace)
                                       (not immediate-eval?))))

(define (default-read-interaction src in)
  (unless (input-port? in)
    (raise-argument-error 'default-read-interaction "input-port?" in))
  (parameterize ([read-accept-reader #t]
                 [read-accept-lang #f])
    (read-syntax src in)))

(define (boot)
  (set! -module-hash-table-table (make-weak-hasheq))
  (set! path-caches (make-weak-hasheq))
  (seal)
  (current-module-name-resolver standard-module-name-resolver)
  (current-load/use-compiled default-load/use-compiled)
  (current-reader-guard default-reader-guard)
  (current-eval default-eval-handler)
  (current-compile default-compile-handler)
  (current-load default-load-handler)
  (current-read-interaction default-read-interaction))

(define (seal)
  (set! orig-paramz
        (reparameterize 
         (continuation-mark-set-first #f parameterization-key))))

(define (get-original-parameterization)
  orig-paramz)

(define (default-current-read-interaction?)
  (eq? (current-read-interaction) default-read-interaction))

;; ----------------------------------------
;; Mostly for historical uses of '#%boot

(define boot-primitives
  (hash 'boot boot
        'seal seal
        ;; Historically, exported a `orig-paramz` after place
        ;; initialization, but we now need an indirection
        'get-original-parameterization get-original-parameterization
        ;; Used to enable alternate reader interaction:
        'default-current-read-interaction? default-current-read-interaction?))
