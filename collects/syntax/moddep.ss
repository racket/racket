
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
        (let-values ([(imports fs-imports ft-imports fl-imports)
                      (module-compiled-imports code)])
          (define ((mk-loop fs) i)
            (unless (symbol? i)
              (loop (resolve-module-path-index i path)
                    (format " ~a" indent)
                    fs)))
          (for-each (mk-loop "") imports)
          (for-each (mk-loop " [for-syntax]") fs-imports)
          (for-each (mk-loop " [for-template]") ft-imports)
          (for-each (mk-loop " [for-label]") fl-imports))))))

