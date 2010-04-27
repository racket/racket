;; this module is shared between drscheme's and the user's namespace
;; the printer uses it, printing it as (void), so that ordinary
;; (void) results can still be ignored by the printer.
(module set-result mzscheme
  (provide set!-result)
  (define set!-result
    (let ()
      (define-struct set!-result ())
      (make-set!-result))))

