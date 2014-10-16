#lang racket/base
(require "private/drracket-test-util.rkt"
         scribblings/tools/example-src
         racket/unit
         racket/gui/base
         racket/file)

(define new-collection-root 
  #;
  (string->path "C:\\tmp")
  (make-temporary-file "drracket-test-example-tool~a"
                       'directory))
(define coll (build-path new-collection-root "coll"))
(unless (directory-exists? coll) (make-directory coll))

(for ([f (in-list sexp-files)])
  (define fn (list-ref f 0))
  (define lang-line (format "#lang ~a" (list-ref f 1)))
  (define sexps (list-ref f 2))
  (call-with-output-file (build-path coll fn)
    (λ (port)
      (fprintf port "~a\n" lang-line)
      (for ([x (in-list sexps)])
        (fprintf port "~s\n" x)))
    #:exists 'truncate))

(parameterize ([current-namespace (make-gui-namespace)]
               [current-library-collection-paths
                (cons
                 new-collection-root
                 (current-library-collection-paths))])
  (namespace-require 'tests/drracket/private/run-example-tool)
  (eval '(go)))

(module+ test
  (module config info
    (define random? #t)))
