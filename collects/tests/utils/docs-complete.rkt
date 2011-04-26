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
  (define (pick-one . args)
    (for/or ([fmt (in-list args)])
      (define pth
        (apply build-path
               (append (list (collection-path lib0))
                       lib1-n
                       (list "compiled"
                             (format fmt file)))))
      (and (file-exists? pth)
           pth)))
  (define-values (val-info stx-info)
    (let/ec k
      (module-compiled-exports
       (parameterize ([read-accept-compiled #t])
         (define file (pick-one "~a_rkt.zo"
                                "~a_ss.zo"
                                "~a_scm.zo"))
         (if file
             (call-with-input-file file read)
             (k '() '()))))))
  
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
                     #f))
               #:when
               (not (regexp-match #rx"^deserialize-info:" (symbol->string ex))))
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
      (pretty-write undocumented-exports (current-error-port)))))

(define xref (load-collections-xref))
