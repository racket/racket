#lang racket/base
(require racket/file planet/util racket/system)

(define files
  '(("info.rkt"
     #<<--
#lang setup/infotab

(define name "planet-docs-test")
(define scribblings '(("docs.scrbl")))

--
     )
    ("docs.scrbl"
     #<<--
#lang scribble/doc

@(require scribble/manual)

@title{AWESOMENESS!!1!!}
@author{Bob Trotsky}

--
     )))

(define planet-docs-test (make-temporary-file "planet-docs-test-~a" 'directory))
(for ([file (in-list files)])
  (call-with-output-file (build-path planet-docs-test (list-ref file 0))
    (λ (port)
      (display (list-ref file 1) port))))

(add-hard-link "planet" "docs-test.plt" 1 0 planet-docs-test)

(define raco-bin-path
  (simplify-path (build-path (collection-path "racket") 'up 'up "bin" "raco")))

(define sp (open-output-string))
(define res
  (parameterize ([current-output-port sp]
                 [current-error-port sp])
    (system* (path->string raco-bin-path)
             "setup" "-P" "planet" "docs-test.plt" "1" "0")))

(define failed?
  (or (not res)
      (regexp-match #rx"error" (get-output-string sp))))

(remove-hard-link "planet" "docs-test.plt" 1 0)

;; for drdr: print to stderr when the test fails, stdout otherwise
(cond
  [failed?
   (eprintf "FAILED; transcript:\n")
   (eprintf "~a" (get-output-string sp))]
  [else
   (printf "PASSED; transcript:\n")
   (printf "~a\n" (get-output-string sp))])
