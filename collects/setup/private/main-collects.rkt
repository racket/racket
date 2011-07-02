;; This file must not have any dependencies other than the built-in
;; #% modules, because it is used to check dependencies in the setup-plt
;; bootstrap phase.
(module main-collects '#%kernel
  (#%require '#%min-stx '#%utils)
  (#%provide system-path*
             find-main-collects)

  ;; Path normalization is not really necessary by any existing code,
  ;; but there might be applications that rely on these paths, so it's
  ;; best to do some minor normalization.  This is similar to what
  ;; "main-collects.rkt" does. Don't use this with 'exec-file, however,
  ;; because the treatment of an executable path can very depending
  ;; on whether it has a path prefix or not.
  (define (system-path* what)
    (simplify-path (cleanse-path (find-system-path what)) #f))

  (define-values (find-main-collects)
    (lambda ()
      (let ([d (system-path* 'collects-dir)])
        (cond
         [(complete-path? d) d]
         [(absolute-path? d)
          ;; This happens only under Windows; add a drive
          ;;  specification to make the path complete
          (let ([exec (path->complete-path
                       (find-executable-path (find-system-path 'exec-file))
                       (system-path* 'orig-dir))])
            (let-values ([(base name dir?) (split-path exec)])
              (path->complete-path d base)))]
         [else
          ;; Relative to executable...
          (parameterize ([current-directory (system-path* 'orig-dir)])
            (let ([p (or (find-executable-path (find-system-path 'exec-file) d #t)
                         ;; If we get here, then we can't find the directory
                         #f)])
              (and p (simplify-path p))))])))))

