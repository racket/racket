(module moddep mzscheme
  (require "modread.rkt"
           "modcode.rkt"
           "modcollapse.rkt"
           "modresolve.rkt")

  (provide (all-from "modread.rkt")
           (all-from "modcode.rkt")
           (all-from "modcollapse.rkt")
           (all-from "modresolve.rkt")
           show-import-tree)

  (define (show-import-tree module-path)
    (let loop ([path (resolve-module-path module-path #f)][indent ""][fs ""])
      (printf "~a~a~a\n" indent path fs)
      (let ([code (get-module-code path)])
        (let ([imports (module-compiled-imports code)])
          (define ((mk-loop fs) i)
            (let ([p (resolve-module-path-index i path)])
              (unless (symbol? p)
                (loop p
                      (format " ~a" indent)
                      fs))))
          (for-each (lambda (i)
                      (for-each
                       (mk-loop (case (car i)
                                  [(0) ""]
                                  [(1) " [for-syntax]"]
                                  [(-1) " [for-syntax]"]
                                  [(#f) " [for-label]"]
                                  [else (format " [for-meta ~a]" (car i))]))
                       (cdr i)))
                    imports))))))
