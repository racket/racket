(module make-plt mzscheme
  
  (require (lib "pack.ss" "setup")
           #;(lib "util.ss" "planet"))
  
  (define (my-filter path)
    (and (std-filter path)
         (not (or (regexp-match #rx".svn$" path)
                  (regexp-match #rx".bak$" path)
                  (regexp-match #rx".1$" path)
                  (regexp-match #rx"-uncommented.ss$" path)
                  (regexp-match #rx"make" path)))))
  

  ;without frtime bundled:
  (pack-collections "mztake-208.plt" "MzTake Debugger"
                    '(("mztake")) #t '(("frtime")("stepper")) my-filter #f)
  
  (pack-collections "mztake-frtime-pre-208.plt" "MzTake Debugger"
                    '(("mztake")("frtime")) #t '(("stepper")) my-filter #f))
