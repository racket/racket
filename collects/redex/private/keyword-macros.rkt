#lang racket/base

(require racket/match
         racket/contract
         setup/path-to-relative
         (for-template racket/base racket/contract))

(define (parse-kw-args formals actuals source form-name)
  (let loop ([current (for/hash ([arg formals]) (values (car arg) #f))]
             [rest actuals])
    (syntax-case rest ()
      [() (map (λ (arg) 
                 (match (hash-ref current (car arg))
                   [#f (cadr arg)]
                   [x (match (cdr (cdr arg))
                        ['() x]
                        [`((,ctc ,desc))
                         (apply-contract ctc x desc form-name)])]))
               formals)]
      [(kw . rest)
       (not (keyword? (syntax-e (syntax kw))))
       (raise-syntax-error #f "expected a keyword" source (syntax kw))]
      [(kw arg . rest)
       (keyword? (syntax-e (syntax arg)))
       (raise-syntax-error #f "expected an argument expression" source (syntax arg))]
      [(kw arg . rest)
       (let ([none (gensym)])
         (eq? none (hash-ref current (syntax-e (syntax kw)) none)))
       (raise-syntax-error #f "invalid keyword" source (syntax kw))]
      [(kw arg . rest)
       (hash-ref current (syntax-e (syntax kw)))
       (raise-syntax-error #f "repeated keyword" source (syntax kw))]
      [(kw)
       (raise-syntax-error #f "missing argument expression after keyword" source (syntax kw))]
      [(kw arg . rest)
       (loop (hash-set current (syntax-e (syntax kw)) (syntax arg))
             (syntax rest))]
      [else (raise-syntax-error #f "bad keyword argument syntax" source rest)])))

;; note: depents on current-directory (or current-load-relative-directory)
(define (client-name stx form)
  (define mpi/path/sym (syntax-source-module stx))
  (define pth/sym (if (module-path-index? mpi/path/sym)
                      (resolved-module-path-name 
                       (module-path-index-resolve mpi/path/sym))
                      mpi/path/sym))
  (if (path? pth/sym)
      (path->relative-string/library pth/sym)
      (format "~s" pth/sym)))

(define (src-loc-stx stx)
  #`#(#,(and (path? (syntax-source stx))
             (path->relative-string/library (syntax-source stx)))
      #,(syntax-line stx) 
      #,(syntax-column stx) 
      #,(syntax-position stx)
      #,(syntax-span stx)))

(define (apply-contract ctc expr desc form)
  #`(contract #,ctc #,expr
              #,(client-name expr form) '#,form
              #,desc #,(src-loc-stx expr)))

(provide src-loc-stx
         apply-contract
         client-name
         parse-kw-args)
