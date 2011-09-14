#lang racket/base

(require setup/xref
         scribble/xref
         racket/pretty 
         racket/list
         racket/contract/base)

;; checks to make sure that all of the exports of
;; the 'what' library are documented
(provide/contract [check-docs (->* (symbol?) 
                                   (#:skip (or/c regexp? 
                                                 symbol? 
                                                 #f
                                                 (-> symbol? any)
                                                 (listof (or/c regexp? symbol?)))) 
                                   any)])
(define (check-docs what #:skip [skip #f])
  (define (skip-proc f)
    (cond
      [(regexp? skip)
       (regexp-match skip (symbol->string f))]
      [(procedure? skip)
       (skip f)]
      [(not skip) #f]
      [(list? skip)
       (for/or ([x (in-list skip)])
         (cond
           [(regexp? x)
            (regexp-match skip (symbol->string f))]
           [(symbol? x)
            (eq? x f)]))]))
        
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
             (begin
               (eprintf "did not find compiled file for ~s\n" what)
               (k '() '())))))))
  
  (define (get n info)
    (define a (assoc n info))
    (if a (cdr a) '()))
  
  (define exports
    (map car
         (append (get 0 val-info)
                 (get 0 stx-info))))
  
  (define undocumented-exports
    (for/list ([ex (in-list exports)]
               #:unless (xref-binding->definition-tag xref (list what ex) #f)
               #:unless (skip-proc ex))
      ex))
  
  (unless (null? undocumented-exports)
    (cond
      [(= (length exports) (length undocumented-exports))
       (eprintf "~s has no documented exports\n" what)]
      [else
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
         (pretty-write (sort undocumented-exports
                             string<?
                             #:key symbol->string)
                       (current-error-port)))])))

(define xref (load-collections-xref))

