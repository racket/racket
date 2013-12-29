;; Because `raco setup' is used to rebuild .zos, check for "setup"
;; directly.

;; Note that this file is listed in "info.rkt" so that it never gets a
;; .zo file. Do not `require' this module from anywhere, not even
;; `for-label', otherwise it could get a .zo anyway.

(module main '#%kernel
  (#%require '#%min-stx
             ;; Need to make sure they're here:
             '#%builtin)

  (module test '#%kernel)

  (let-values ([(cmdline) (current-command-line-arguments)])
    (if (and (positive? (vector-length cmdline))
             (equal? "setup" (vector-ref cmdline 0)))
        (parameterize ([current-command-line-arguments
                        (list->vector
                         (cdr
                          (vector->list cmdline)))])
          (dynamic-require 'setup/main #f))
        (dynamic-require 'raco/raco #f))))
