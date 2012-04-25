#lang racket/base

(require racket/contract/base
         "private/modcollapse-noctc.rkt")

(define simple-rel-to-module-path-v/c
  (and/c module-path?
         (or/c
          path?
          (cons/c 'lib any/c)
          (cons/c 'file any/c)
          (cons/c 'planet any/c)
          (cons/c 'quote any/c)
          (cons/c 'submod 
                  (cons/c (or/c 
                           path?
                           (cons/c 'lib any/c)
                           (cons/c 'file any/c)
                           (cons/c 'planet any/c)
                           (cons/c 'quote any/c))
                          (cons/c symbol? (listof symbol?)))))))

(define rel-to-module-path-v/c
  (or/c simple-rel-to-module-path-v/c 
        path?
        symbol? ;; why do we allow symbols (which are non-normalized) as a `relto' value?
        (-> simple-rel-to-module-path-v/c)))

(provide/contract
 [collapse-module-path (module-path?
                        rel-to-module-path-v/c
                        . -> . simple-rel-to-module-path-v/c)]
 [collapse-module-path-index ((or/c symbol? module-path-index?)
                              rel-to-module-path-v/c
                              . -> . simple-rel-to-module-path-v/c)])
