#lang racket/base

(require syntax/modcode racket/list (for-syntax racket/base))

(provide enter!)

(define-syntax (enter! stx)
  (syntax-protect
   (syntax-case stx ()
     [(enter! mod flag ...) (andmap keyword? (syntax->datum #'(flag ...)))
      #'(do-enter! 'mod '(flag ...))]
     [_ (raise-syntax-error
         #f "bad syntax; should be `(enter! <module-path-or-#f> [flag...])'"
         stx)])))

(define orig-namespace (current-namespace))

(define-logger enter!)

(define (check-flags flags)
  ;; check that all flags are known, that at most one of the noise flags is
  ;; present, and add #:verbose-reload if none are (could be done at the macro
  ;; level, but this is intended for interactive use anyway)
  (let loop ([flags (remove-duplicates flags eq?)] [noise #f])
    (cond [(null? flags)
           (if noise '() '(#:verbose-reload))]
          [(eq? (car flags) '#:dont-re-require-enter)
           (cons (car flags) (loop (cdr flags) noise))]
          [(not (memq (car flags) '(#:verbose #:quiet #:verbose-reload)))
           (error 'enter! "unknown flag: ~e" (car flags))]
          [noise (error 'enter! "contradicting noise flags: ~e and ~e"
                        noise (car flags))]
          [else (cons (car flags) (loop (cdr flags) (car flags)))])))

(define (do-enter! mod flags)
  (let ([flags (check-flags flags)])
    (if mod
      (let* ([none (gensym)]
             [exn (with-handlers ([void (lambda (exn) exn)])
                    (if (module-path? mod)
                        (enter-require mod flags)
                        (raise-argument-error 'enter! "module-path?" mod))
                    none)])
        ;; Try to switch to the module namespace,
        ;; even if there was an exception, because the
        ;; idea is to allow debugging from inside the
        ;; module. If any excepiton happens in trying to
        ;; switch to the declared module, log that as
        ;; an internal exception.
        (with-handlers ([void (lambda (exn)
                                (if (exn? exn)
                                    (log-enter!-error (exn-message exn))
                                    (log-enter!-error "~s" exn)))])
          (when (and (module-path? mod)
                     (module-declared? mod #f))
            (let ([ns (module->namespace mod)])
              (current-namespace ns)
              (unless (memq '#:dont-re-require-enter flags)
                (namespace-require 'racket/enter)))))
        (unless (eq? none exn) (raise exn)))
      (current-namespace orig-namespace))))

(struct mod (name timestamp depends))

(define loaded (make-hash))

(define (enter-require mod flags)
  ;; Collect dependencies while loading:
  (parameterize ([current-load/use-compiled
                  (enter-load/use-compiled (current-load/use-compiled)
                                           #f flags)])
    (dynamic-require mod #f))
  ;; Reload anything that's not up to date:
  (check-latest mod flags))

(define (enter-load/use-compiled orig re? flags)
  (define notify
    (if (or (memq '#:verbose flags) (and re? (memq '#:verbose-reload flags)))
      (lambda (path)
        (eprintf "  [~aloading ~a]\n" (if re? "re-" "") path))
      void))
  (lambda (path name)
    (if (and name
             (not (and (pair? name)
                       (not (car name)))))
        ;; Module load:
        (with-handlers ([(lambda (exn)
                           (and (pair? name)
                                (exn:get-module-code? exn)))
                         (lambda (exn) 
                           ;; Load-handler protocol: quiet failure when a
                           ;; submodule is not found
                           (void))])
          (let* ([code (get-module-code
                        path "compiled"
                        (lambda (e)
                          (parameterize ([compile-enforce-module-constants #f])
                            (compile e)))
                        (lambda (ext loader?) (load-extension ext) #f)
                        #:notify notify)]
                 [dir  (or (current-load-relative-directory) (current-directory))]
                 [path (path->complete-path path dir)]
                 [path (normal-case-path (simplify-path path))])
            ;; Record module timestamp and dependencies:
            (define-values (ts actual-path) (get-timestamp path))
            (let ([a-mod (mod name
                              ts
                              (if code
                                  (append-map cdr (module-compiled-imports code))
                                  null))])
              (hash-set! loaded path a-mod))
            ;; Evaluate the module:
            (parameterize ([current-module-declare-source actual-path])
              (eval code))))
      ;; Not a module, or a submodule that we shouldn't load from source:
      (begin (notify path) (orig path name)))))

(define (get-timestamp path)
  (let ([ts (file-or-directory-modify-seconds path #f (lambda () #f))])
    (if ts
        (values ts path)
        (if (regexp-match? #rx#"[.]rkt$" (path->bytes path))
            (let* ([alt-path (path-replace-suffix path #".ss")]
                   [ts (file-or-directory-modify-seconds alt-path #f (lambda () #f))])
              (if ts
                  (values ts alt-path)
                  (values -inf.0 path)))
            (values -inf.0 path)))))

(define (check-latest mod flags)
  (define mpi (module-path-index-join mod #f))
  (define done (make-hash))
  (let loop ([mpi mpi])
    (define rpath (module-path-index-resolve mpi))
    (define path (let ([p (resolved-module-path-name rpath)])
                   (if (pair? p) (car p) p)))
    (when (path? path)
      (define npath (normal-case-path path))
      (unless (hash-ref done npath #f)
        (hash-set! done npath #t)
        (define mod (hash-ref loaded npath #f))
        (when mod
          (for-each loop (mod-depends mod))
          (define-values (ts actual-path) (get-timestamp npath))
          (when (ts . > . (mod-timestamp mod))
            (define orig (current-load/use-compiled))
            (parameterize ([current-load/use-compiled
                            (enter-load/use-compiled orig #f flags)]
                           [current-module-declare-name rpath]
                           [current-module-declare-source actual-path])
              ((enter-load/use-compiled orig #t flags)
               npath (mod-name mod)))))))))
