(module honu-translate-type-defn mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss"))
  
  (require "../../ast.ss")
  (require "../../tenv.ss")
  (require "../typechecker/honu-type-utils.ss")
  (require "honu-translate-utils.ss")
  
  (provide honu-translate-type-defn)
  (define (honu-translate-type-defn tenv defn)
    (match defn
      [(struct honu-type-defn (stx name supers decls))
       (let ([typ (make-honu-iface-type name name)]
             [method-names (filter-map (match-lambda
                                        [(struct honu-method-decl (_ name _ _)) name]
                                        [_ #f])
                                       decls)]
             [field-names (filter-map (match-lambda
                                        [(struct honu-field-decl (_ name _)) name]
                                        [_ #f])
                                      decls)])
         (at stx `(define ,(honu-translate-type-name typ)
                    (interface ,(filter-map honu-translate-type-name supers)
                      ,@(map (lambda (m)
                               (honu-translate-dynamic-method-name tenv m typ))
                             method-names)
                      ,@(map (lambda (f)
                               (honu-translate-dynamic-field-getter tenv f typ))
                             field-names)
                      ,@(map (lambda (f)
                               (honu-translate-dynamic-field-setter tenv f typ))
                             field-names)))))]))
  )
