
(module moddep mzscheme
  (require "modread.ss"
           "modcode.ss"
           "modcollapse.ss"
           "modresolve.ss")

  (provide (all-from "modread.ss")
           (all-from "modcode.ss")
           (all-from "modcollapse.ss")
           (all-from "modresolve.ss")
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
