#lang racket/base

(require syntax/modcode racket/list (for-syntax racket/base))

(provide enter!)

(define-syntax (enter! stx)
  (define (do-enter mod noise)
    (unless (or (not (syntax-e mod)) (module-path? (syntax->datum mod)))
      (raise-syntax-error #f "not a valid module path, and not #f" stx mod))
    (unless (memq (syntax-e noise) '(#:verbose #:quiet #:verbose-reload))
      (raise-syntax-error #f "not a valid verbosity keyword" stx noise))
    #`(do-enter! '#,mod '#,noise))
  (syntax-case stx ()
    [(enter! mod)       (do-enter #'mod #'#:verbose-reload)]
    [(enter! mod noise) (do-enter #'mod #'noise)]
    [_ (raise-syntax-error
        #f "bad syntax; should be `(enter! <module-path-or-#f> [noise-flag])'"
        stx)]))

(define orig-namespace (current-namespace))

(define (do-enter! mod noise)
  (if mod
    (begin (enter-require mod noise)
           (let ([ns (module->namespace mod)])
             (current-namespace ns)
             (namespace-require 'racket/enter)))
    (current-namespace orig-namespace)))

(struct mod (name timestamp depends))

(define loaded (make-hash))

(define (enter-require mod noise)
  ;; Collect dependencies while loading:
  (parameterize ([current-load/use-compiled
                  (enter-load/use-compiled (current-load/use-compiled)
                                           #f noise)])
    (dynamic-require mod #f))
  ;; Reload anything that's not up to date:
  (check-latest mod noise))

(define ((enter-load/use-compiled orig re? noise) path name)
  (define notify
    (if (case noise [(#:verbose-reload) re?] [(#:verbose) #t] [(#:quiet) #f])
      (lambda (path)
        (fprintf (current-error-port)
                 "  [~aloading ~a]\n" (if re? "re-" "") path))
      void))
  (if name
    ;; Module load:
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
      (let ([a-mod (mod name
                        (get-timestamp path)
                        (if code
                          (append-map cdr (module-compiled-imports code))
                          null))])
        (hash-set! loaded path a-mod))
      ;; Evaluate the module:
      (eval code))
    ;; Not a module:
    (begin (notify path) (orig path name))))

(define (get-timestamp path)
  (file-or-directory-modify-seconds path #f
    (lambda ()
      (if (regexp-match? #rx#"[.]rkt$" (path->bytes path))
        (file-or-directory-modify-seconds
         (path-replace-suffix path #".ss") #f (lambda () -inf.0))
        -inf.0))))

(define (check-latest mod noise)
  (define mpi (module-path-index-join mod #f))
  (define done (make-hash))
  (let loop ([mpi mpi])
    (define rpath (module-path-index-resolve mpi))
    (define path (resolved-module-path-name rpath))
    (when (path? path)
      (define npath (normal-case-path path))
      (unless (hash-ref done npath #f)
        (hash-set! done npath #t)
        (define mod (hash-ref loaded npath #f))
        (when mod
          (for-each loop (mod-depends mod))
          (define ts (get-timestamp npath))
          (when (ts . > . (mod-timestamp mod))
            (define orig (current-load/use-compiled))
            (parameterize ([current-load/use-compiled
                            (enter-load/use-compiled orig #f noise)]
                           [current-module-declare-name rpath])
              ((enter-load/use-compiled orig #t noise)
               npath (mod-name mod)))))))))
