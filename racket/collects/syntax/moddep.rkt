(module moddep racket/base
  (require "modread.rkt"
           "modcode.rkt"
           "modcollapse.rkt"
           "modresolve.rkt")

  (provide (all-from-out "modread.rkt")
           (all-from-out "modcode.rkt")
           (all-from-out "modcollapse.rkt")
           (all-from-out "modresolve.rkt")
           show-import-tree)

  (define (show-import-tree module-path
                            #:dag? [dag? #f]
                            #:path-to [given-path-to #f]
                            #:show [show (lambda (indent path require-mode phase)
                                           (printf "~a~a~a ~a\n" indent path require-mode phase))])
    (define path-to (and given-path-to (simplify-path (resolve-module-path given-path-to #f))))
    (define seen (and dag? (make-hash)))
    (let loop ([path (resolve-module-path module-path #f)] [indent ""] [fs ""] [phase 0] [accum '()])
      (when (equal? path-to path)
        (let ([accum (let loop ([accum (cons (list indent path fs phase) accum)])
                       (cond
                         [(null? accum) null]
                         [(hash-ref seen accum #f) null]
                         [else
                          (hash-set! seen accum #t)
                          (cons (car accum) (loop (cdr accum)))]))])
          (for ([i (in-list (reverse accum))])
            (apply show i))))
      (unless (and seen (hash-ref seen (cons path phase) #f))
        (unless path-to
          (show indent path fs phase))
        (when seen (hash-set! seen (cons path phase) #t))
        (define plain-path (if (pair? path) (cadr path) path))
        (let ([code (get-module-code plain-path
                                     #:submodule-path (if (pair? path) (cddr path) '()))])
          (let ([imports (module-compiled-imports code)]
                [accum (cons (list indent path fs phase) accum)])
            (define ((mk-loop phase-shift fs) i)
              (let ([p (resolve-module-path-index i plain-path)])
                (unless (symbol? p)
                  (loop (if (path? p)
                            (simplify-path p)
                            (list* 'submod (simplify-path (cadr p)) (cddr p)))
                        (format " ~a" indent)
                        fs
                        (and phase phase-shift (+ phase phase-shift))
                        accum))))
            (for-each (lambda (i)
                        (for-each
                         (mk-loop (car i)
                                  (case (car i)
                                    [(0) ""]
                                    [(1) " [for-syntax]"]
                                    [(-1) " [for-template]"]
                                    [(#f) " [for-label]"]
                                    [else (format " [for-meta ~a]" (car i))]))
                         (cdr i)))
                      imports)))))))
