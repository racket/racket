#lang racket/base
(require racket/file 
         racket/system
         racket/port
         planet/util)

(define files
  '(("info.rkt"
     #<<--
#lang info

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

;; send all of the output of 'raco setup' to stdout
;; and to 'sp' and then check to see if there is an 
;; error by grepping for 'error running' in order
;; to cooperate with drdr more effectively
(define-values (in out) (make-pipe))
(define sp (open-output-string))
(define thd (thread (λ () (copy-port in (current-output-port) sp))))

(define res
  (parameterize ([current-output-port out]
                 [current-error-port out])
    (system* (path->string raco-bin-path)
             "setup" "-xind" "-P" "planet" "docs-test.plt" "1" "0")))

(remove-hard-link "planet" "docs-test.plt" 1 0)

;; run raco setup a second time to clear out the
;; indices that were added by running docs-test.plt.
(define res2
  (parameterize ([current-output-port out]
                 [current-error-port out])
    (system* (path->string raco-bin-path)
             "setup" "-xin")))

;; make sure all of the output has appeared
(close-output-port out)
(thread-wait thd)

(cond
  [(or (not res)
       (not res2)
       (regexp-match #rx"error running" (get-output-string sp)))
   (eprintf "FAILED test (res ~s res2 ~s)\n" res res2)]
  [else
   (printf "PASSED\n")])
