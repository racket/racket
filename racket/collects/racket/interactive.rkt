#lang racket/base
(when (collection-file-path "main.rkt" "xrepl"
                            #:fail (lambda _ #f))
  (dynamic-require 'xrepl #f))

(let ([init-file (cleanse-path (find-system-path 'init-file))])
  (when (file-exists? init-file)
    (load init-file)))
