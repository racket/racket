#lang racket/base
(require racket/cmdline
         raco/command-name
         racket/whereis)

;; Scriptability:
;; - Prints path(s) found to stdout, errors/warnings to stderr.
;; - TODO: Add option to set separator for multiple results?
;; - Exit code:
;;   - 1 if exn raised
;;   - 2 if any request returned #f (but not empty list)
;;   - 0 otherwise

;; The code below is written to allow list of requests, but
;; currently limited to one (or zero) by #:once-any.

(define todo null) ;; (Listof (-> (U Path (Listof Path))))
(define (push! proc [fail void])
  (set! todo (cons proc todo)))

(define (report v)
  (define (print-path p)
    (printf "~a\n" (path->string (simplify-path p))))
  (cond [(list? v) (for-each print-path v)]
        [(path? v) (print-path v)]))

(define (string->datum what s)
  (define in (open-input-string s))
  (begin0 (read in)
    (unless (eof-object? (peek-char in))
      (raise-user-error '|raco whereis|
                        "expected one S-expression for ~a, given: ~s"
                        what s))))

(command-line
 #:program (short-program+command-name)
 #:once-any

 [("-m" "--module")
  module-path
  "Print the path of the module file"
  (let ([modpath (string->datum "module path" module-path)])
    (push! (lambda () (whereis-module modpath))))]

 [("-p" "--pkg")
  package
  "Print the package's directory"
  (push! (lambda () (whereis-pkg package)))]

 [("-c" "--collect")
  collection
  "Print the collection's directories"
  (push! (lambda () (whereis-collection collection)))]

 [("-s" "--system")
  location
  "Print the location's path or paths"
  (let ([location (string->symbol location)])
    (push! (lambda () (whereis-system location))))]

 [("-r" "--raco")
  command
  "Print the path of the raco command's implementation"
  (push! (lambda () (whereis-raco command)))]

 [("-b" "--binding")
  providing-module name
  "Print the path where the given name was defined"
  (let ([providing-module (string->datum "module path" providing-module)]
        [name (string->symbol name)])
    (push! (lambda () (whereis-binding/symbol providing-module name))))]

 ;; [("--show-system-paths") "..." ;; doesn't include find-system-path
 ;;  (for ([sym (in-hash-keys whereis-system-procs)])
 ;;    (printf "-- ~s --\n" sym)
 ;;      (report (whereis-system sym))
 ;;    (newline))]

 #:args ()
 (let ([any-failed? #f])
   (for ([proc (in-list (reverse todo))])
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (set! any-failed? #t)
                        (define msg (exn-message e))
                        (eprintf "~a\n" (exn-message e)))])
       (report (proc))))
   (exit (if any-failed? 2 0))))
