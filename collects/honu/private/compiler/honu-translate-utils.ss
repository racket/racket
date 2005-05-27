(module honu-translate-utils mzscheme
  
  (require (lib "list.ss" "srfi" "1"))

  (require "../../ast.ss")
  (require "../../tenv.ss")
  
  (provide current-compile-context)
  (define current-compile-context (make-parameter #f))

;  (provide/contract [at      ((syntax/c any/c) any/c . -> . (syntax/c any/c))]
;                    [at-ctxt ((syntax/c any/c)       . -> . (syntax/c any/c))])
  (provide at at-ctxt)
  (define (at stx expr)
    (datum->syntax-object (current-compile-context) expr stx))
  (define (at-ctxt stx)
    (datum->syntax-object (current-compile-context) (syntax-e stx) stx))
  
  (provide honu-translate-class-name)
  (define (honu-translate-class-name sym)
    (at sym
        (string->symbol 
           (string-append "honu-" (symbol->string (printable-key sym)) "%"))))
        
  (provide honu-translate-type-name)
  (define (honu-translate-type-name typ)
    (if (honu-iface-top-type? typ) #f
        (at (honu-ast-src-stx typ)
            (string->symbol 
             (string-append "honu-" (symbol->string (printable-key (honu-iface-type-name typ))) "<%>")))))
  
  (provide honu-translate-mixin-name)
  (define (honu-translate-mixin-name mixin)
   (at mixin
       (string->symbol
        (string-append "honu-" (symbol->string (printable-key mixin)) "-mixin"))))

  (provide honu-translate-field-getter)
  (define (honu-translate-field-getter sym)
    (at sym
        (string->symbol (string-append "get-" (symbol->string (printable-key sym))))))

  (provide honu-translate-field-setter)
  (define (honu-translate-field-setter sym)
    (at sym
        (string->symbol (string-append "set-" (symbol->string (printable-key sym)) "!"))))

  (provide find-type-for-name)
  (define (find-type-for-name tenv name typ)
    (let ([type-defn (get-type-entry (honu-iface-type-name typ) tenv)])
      (if (ormap (lambda (d)
                   (cond
                     [(honu-field-decl? d) (tenv-key=? (honu-field-decl-name d) name)]
                     [(honu-method-decl? d) (tenv-key=? (honu-method-decl-name d) name)]))
                 (tenv-type-members type-defn))
          typ
          (find (lambda (t)
                  (find-type-for-name tenv name t))
                (tenv-type-supers type-defn)))))

  (provide honu-translate-dynamic-field-getter)
  (define (honu-translate-dynamic-field-getter tenv sym typ)
    (at sym
        (string->symbol (string-append "get-"
                                       (symbol->string (printable-key (honu-translate-type-name (find-type-for-name tenv sym typ))))
                                       "-"
                                       (symbol->string (printable-key sym))))))

  (provide honu-translate-dynamic-field-setter)
  (define (honu-translate-dynamic-field-setter tenv sym typ)
    (at sym
        (string->symbol (string-append "set-" 
                                       (symbol->string (printable-key (honu-translate-type-name (find-type-for-name tenv sym typ))))
                                       "-"
                                       (symbol->string (printable-key sym)) "!"))))

  (provide honu-translate-dynamic-method-name)
  (define (honu-translate-dynamic-method-name tenv name typ)
    (at name
        (string->symbol (string-append (symbol->string (printable-key (honu-translate-type-name (find-type-for-name tenv name typ))))
                                       "-"
                                       (symbol->string (printable-key name))))))
  )
