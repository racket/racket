#lang racket/base
(require "../eval/reflect.rkt")

(provide extract-requested-submodule)

(define (extract-requested-submodule m expected-module)
  (define (drop-submodules m)
    (module-compiled-submodules (module-compiled-submodules m #f null)
                                #t
                                null))
  (cond
   [(symbol? expected-module)
    (drop-submodules m)]
   [else
    (let loop ([m m]
               [expected-module (cdr expected-module)]
               [pos 1])
      (cond
       [(null? expected-module)
        (drop-submodules m)]
       [else
        (define new-m
          (for/or ([m (in-list
                       (append
                        (module-compiled-submodules m #f)
                        (module-compiled-submodules m #t)))])
            (and (eq? (car expected-module)
                      (list-ref (module-compiled-name m) pos))
                 m)))
        (and new-m
             (loop new-m (cdr expected-module) (add1 pos)))]))]))
