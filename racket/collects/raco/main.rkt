;; Because `raco setup' is used to rebuild .zos, check for "setup"
;; directly.

;; Note that this file is listed in "info.rkt" so that it never gets a
;; .zo file. Do not `require' this module from anywhere, not even
;; `for-label', otherwise it could get a .zo anyway.

(module main '#%kernel
  (#%require '#%paramz
             ;; Need to make sure they're here:
             '#%builtin)

  (let-values ([(cmdline) (current-command-line-arguments)])
    (if (if (positive? (vector-length cmdline))
            (equal? "setup" (vector-ref cmdline 0))
            #f)
        (with-continuation-mark
          parameterization-key
          (extend-parameterization
           (continuation-mark-set-first #f parameterization-key)
           current-command-line-arguments
           (list->vector
            (cdr
             (vector->list cmdline))))
          (dynamic-require 'setup/main #f))
        (dynamic-require 'raco/raco #f))))
