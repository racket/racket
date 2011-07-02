#lang racket/base

(require racket/contract/base
         "private/modcollapse-noctc.rkt")

(define simple-rel-to-module-path-v/c
  (or/c (and/c module-path?
               (or/c
                symbol?
                (cons/c (symbols 'lib) any/c)
                (cons/c (symbols 'file) any/c)
                (cons/c (symbols 'planet) any/c)
                (cons/c (symbols 'quote) any/c)))
        path?))

(define rel-to-module-path-v/c
  (or/c simple-rel-to-module-path-v/c 
        path?
        (-> simple-rel-to-module-path-v/c)))

(provide/contract
 [collapse-module-path ((or/c module-path? path?) rel-to-module-path-v/c
                                                  . -> . simple-rel-to-module-path-v/c)]
 [collapse-module-path-index ((or/c symbol? module-path-index?)
                              rel-to-module-path-v/c
                              . -> . simple-rel-to-module-path-v/c)])
