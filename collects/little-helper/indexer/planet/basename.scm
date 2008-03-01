; from PLaneT dherman/io.plt/1/8/file.ss
#lang scheme

(provide basename)

;; basename : path -> relative-path
(define (basename p)
  (let-values ([(_ name __) (split-path p)])
    (if (symbol? name) 
        (build-path name)
        name)))
