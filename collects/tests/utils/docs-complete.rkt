#lang racket/base

(require setup/xref
         scribble/xref
         racket/pretty 
         racket/list
         racket/contract/base)

;; checks to make sure that all of the exports of
;; the 'what' library are documented
(provide/contract [check-docs (-> symbol? any)])
(define (check-docs what)
  (define pieces (regexp-split #rx"/" (symbol->string what)))
  (cond
    [(null? pieces) (error 'get-docs "bad arg ~s" what)]
    [(null? (cdr pieces))
     (set! pieces (list (car pieces) "main"))]
    [else (void)])
  (define lib0 (list-ref pieces 0))
  (define file (last pieces))
  (define lib1-n (reverse (cdr (reverse (cdr pieces)))))
  (define-values (val-info stx-info)
    (module-compiled-exports
     (parameterize ([read-accept-compiled #t])
       (call-with-input-file (apply build-path
                                    (append (list (collection-path lib0))
                                            lib1-n
                                            (list "compiled"
                                                  (format "~a_rkt.zo" file))))
         read))))
  
  (define (get n info)
    (define a (assoc n info))
    (if a (cdr a) '()))
  
  (define exports
    (map car
         (append (get 0 val-info)
                 (get 0 stx-info))))
  
  (define undocumented-exports
    (for/list ([ex (in-list exports)]
               #:when
               (not (xref-binding->definition-tag
                     xref
                     (list what ex)
                     #f)))
      ex))
  
  (unless (null? undocumented-exports)
    ;; show the undocumented exports from the racket/contract library
    (eprintf "~s has undocumented exports:\n" what)
    (parameterize ([pretty-print-print-line
                    (λ (line-num port old-len dest-cols)
                      (cond
                        [(not line-num)
                         (newline port)
                         0]
                        [(equal? line-num 0)
                         (fprintf port "  ")
                         2]
                        [else
                         (fprintf port "\n  ")
                         2]))])
      (pretty-write undocumented-exports))))

(define xref (load-collections-xref))
