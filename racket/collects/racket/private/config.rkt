(module config '#%kernel
  (#%require '#%paramz ; for cache-configuration
             "cond.rkt"
             "qq-and-or.rkt"
             "executable-path.rkt")

  (#%provide find-main-collects
             find-main-config
             
             exe-relative-path->complete-path)

  (define-values (find-main-collects)
    (lambda ()
      ;; Recorded once and for all (per place), which helps avoid
      ;; sandbox problems:
      (cache-configuration
       0
       (lambda ()
         (exe-relative-path->complete-path (find-system-path 'collects-dir))))))

  (define-values (find-main-config)
    (lambda ()
      ;; Also recorded once and for all (per place):
      (cache-configuration
       1
       (lambda ()
         (exe-relative-path->complete-path (find-system-path 'config-dir))))))

  (define-values (exe-relative-path->complete-path)
    (lambda (collects-path)
      (cond
       [(complete-path? collects-path) (simplify-path collects-path)]
       [(absolute-path? collects-path)
        ;; This happens only under Windows; add a drive
        ;;  specification to make the path complete
        (let ([exec (path->complete-path
                     (find-executable-path (find-system-path 'exec-file))
                     (find-system-path 'orig-dir))])
          (let-values ([(base name dir?) (split-path exec)])
            (simplify-path (path->complete-path collects-path base))))]
       [else
        (let ([p (find-executable-path (find-system-path 'exec-file) collects-path #t)])
          (and p (simplify-path p)))]))))
