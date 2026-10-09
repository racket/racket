#lang racket/base
(when (collection-file-path "main.rkt" "xrepl"
                            #:fail (lambda _ #f))
  (dynamic-require 'xrepl #f)
  (define toplevel-prefix (dynamic-require 'xrepl/xrepl 'toplevel-prefix))
  (toplevel-prefix "")
  (displayln "Type ,? followed by the Enter/Return key for help")
  (flush-output)
  )

(let ([init-file (cleanse-path (find-system-path 'init-file))])
  (when (file-exists? init-file)
    (load init-file)))
