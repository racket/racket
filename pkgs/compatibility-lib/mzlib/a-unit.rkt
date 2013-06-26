(module a-unit mzscheme
  (require "unit.rkt")
  (require-for-syntax syntax/kerncase)
  
  (provide (rename module-begin #%module-begin)
           (all-from-except mzscheme #%module-begin)
           (all-from "unit.rkt"))
  
  (define-for-syntax (make-name s)
    (string->symbol
     (string-append (regexp-replace "-unit$" (symbol->string s) "")
                    "@")))

  ;; Look for `import' and `export', and start processing the body:
  (define-syntax (module-begin stx)
    (syntax-case stx ()
      [(_ elem ...)
       (with-syntax ([((elem ...) . (literal ...))
                      (let loop ([elems (syntax->list #'(elem ...))]
                                 [accum null])
                        (syntax-case elems (import export)
                          [((import . _1) (export . _2) . _3)
                           (cons (reverse accum) elems)]
                          [((import . _1) . _2)
                           (raise-syntax-error
                            #f
                            "expected an `export' clause after `import'"
                            stx)]
                          [()
                           (raise-syntax-error
                            #f
                            "missing an `import' clause"
                            stx)]
                          [_else
                           (loop (cdr elems) (cons (car elems) accum))]))])
         (with-syntax ((name (datum->syntax-object
                              stx
                              (make-name (syntax-property stx 'enclosing-module-name))
                              stx))
                       (orig-stx stx))
           (datum->syntax-object
            stx
            (syntax-e
             #'(#%module-begin (a-unit-module orig-stx finish-a-unit (import export) 
                                              "original import form"
                                              name (elem ...) (literal ...))))
            stx
            stx)))]))

  ;; Process one `require' form (and make sure it's a require form):
  (define-syntax (a-unit-module stx)
    (syntax-case stx ()
      [(_ orig-stx finish stops separator name (elem1 elem ...) (literal ...))
       (let ([e (local-expand #'elem1
                              'module
                              (append
                               (syntax->list #'stops)
                               (list*
                                #'require
                                #'require-for-syntax
                                #'require-for-template
                                (kernel-form-identifier-list))))])
         (syntax-case e (begin)
           [(req r ...)
            (or (module-identifier=? #'req #'require)
                (module-identifier=? #'req #'require-for-syntax)
                (module-identifier=? #'req #'require-for-template))
            #'(begin
                (req r ...)
                (a-unit-module orig-stx finish stops separator name (elem ...) (literal ...)))]
           [(begin b ...)
            #'(a-unit-module orig-stx finish stops separator name (b ... elem ...) (literal ...))]
           [_else
            (raise-syntax-error
             #f
             (format "non-require form before ~a" (syntax-e #'separator))
             #'orig-stx
             e)]))]
      [(_ orig-stx finish stops separator name () (literal ...))
       #'(finish orig-stx name literal ...)]))
    
  ;; All requires are done, so finish handling the unit:
  (define-syntax (finish-a-unit stx)
    (syntax-case stx (import export)
      [(_ orig-stx name imports exports elem ...)
       #'(begin
           (provide name)
           (define-unit name imports exports elem ...))])))

