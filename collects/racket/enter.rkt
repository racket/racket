#lang racket/base

(require syntax/modcode
         (for-syntax racket/base))

(provide enter!)

(define-syntax (enter! stx)
  (syntax-case stx ()
    [(enter! mod)
     (if (or (not (syntax-e #'mod))
             (module-path? (syntax->datum #'mod)))
         #'(do-enter! 'mod)
         (raise-syntax-error
          #f
          "not a valid module path, and not #f"
          stx
          #'mod))]
    [_ (raise-syntax-error
        #f
        "bad syntax; should be `(enter! <module-path-or-#f>)'"
        stx)]))

(define orig-namespace (current-namespace))

(define (do-enter! mod)
  (if mod
      (begin
        (enter-require mod)
        (let ([ns (module->namespace mod)])
          (current-namespace ns)
          (namespace-require 'racket/enter)))
      (current-namespace orig-namespace)))

(struct mod (name timestamp depends))

(define loaded (make-hash))

(define (enter-require mod)
  ;; Collect dependencies while loading:
  (parameterize ([current-load/use-compiled
                  (enter-load/use-compiled (current-load/use-compiled) #f)])
    (dynamic-require mod #f))
  ;; Reload anything that's not up to date:
  (check-latest mod))

(define (notify re? path)
  (fprintf (current-error-port) " [~aloading ~a]\n" (if re? "re-" "") path))

(define ((enter-load/use-compiled orig re?) path name)
  (if name
      ;; Module load:
      (let ([code (get-module-code path
                                   "compiled"
                                   (lambda (e)
                                     (parameterize ([compile-enforce-module-constants #f])
                                       (compile e)))
                                   (lambda (ext loader?)
                                     (load-extension ext)
                                     #f)
                                   #:notify (lambda (chosen)
                                              (notify re? chosen)))]
            [path (normal-case-path
                   (simplify-path
                    (path->complete-path path
                                         (or (current-load-relative-directory)
                                             (current-directory)))))])
        ;; Record module timestamp and dependencies:
        (let ([a-mod (mod name
                          (get-timestamp path)
                          (if code
                              (apply append
                                     (map cdr (module-compiled-imports code)))
                              null))])
          (hash-set! loaded path a-mod))
        ;; Evaluate the module:
        (eval code))
      ;; Not a module:
      (begin
        (notify re? path)
        (orig path name))))

(define (get-timestamp path)
  (let ([v (file-or-directory-modify-seconds path #f (lambda () -inf.0))])
    (if (and (equal? v -inf.0)
             (regexp-match? #rx#"[.]rkt$" (path->bytes path)))
        (file-or-directory-modify-seconds (path-replace-suffix path #".ss")
                                          #f 
                                          (lambda () -inf.0))
        v)))

(define (check-latest mod)
  (let ([mpi (module-path-index-join mod #f)]
        [done (make-hash)])
    (let loop ([mpi mpi])
      (let* ([rpath (module-path-index-resolve mpi)]
             [path (resolved-module-path-name rpath)])
        (when (path? path)
	  (let ([path (normal-case-path path)])
            (unless (hash-ref done path #f)
              (hash-set! done path #t)
              (let ([mod (hash-ref loaded path #f)])
                (when mod
                  (for-each loop (mod-depends mod))
                  (let ([ts (get-timestamp path)])
                    (when (ts . > . (mod-timestamp mod))
                      (let ([orig (current-load/use-compiled)])
                        (parameterize ([current-load/use-compiled
                                        (enter-load/use-compiled orig #f)]
                                       [current-module-declare-name rpath])
                          ((enter-load/use-compiled orig #t) 
                           path
                           (mod-name mod)))))))))))))))
