(module make-plt mzscheme
  
  (require (lib "pack.ss" "setup")
           (lib "util.ss" "planet"))
  
  (define (my-filter path)
    (and (std-filter path)
         (not (or (regexp-match #rx".svn$" path)
                  (regexp-match #rx".bak$" path)
                  (regexp-match #rx".1$" path)
                  (regexp-match #rx"make" path)))))
  
  
  (pack-collections "mztake.plt" "MzTake Debugger" '(("mztake")) #t '(("frtime") ("stepper")) my-filter #t))

; Now, check-out
;(make-planet-archive "mztake.plt"))