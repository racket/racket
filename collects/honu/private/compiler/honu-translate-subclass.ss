(module honu-translate-subclass mzscheme

  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss"))
  
  (require "../../ast.ss")
  (require "../../utils.ss")
  (require "../../tenv.ss")
  (require "honu-translate-utils.ss")
  (require "honu-translate-class-utils.ss")
  (require "honu-translate-expression.ss")
  
  (define (get-prior-ifaces tenv cname)
    (match-let ([(struct tenv-class (stx sub-type impls _ _ super))
                 (get-class-entry cname tenv)])
      (if super
          (cons sub-type (append impls (get-prior-ifaces tenv super)))
          (cons sub-type impls))))
  
  (define (honu-translate-super-new tenv mxn sup-new)
    (match sup-new
      [(struct honu-super-new (stx arg-names arg-vals))
       (at stx `(super-new
                 ,@(map (lambda (name exp)
                          (list name (honu-translate-expression tenv mxn exp)))
                        arg-names
                        arg-vals)))]))
  
  (provide honu-translate-subclass)
  
  (define (honu-translate-subclass tenv mxn defn)
    ;; I would think the below is equivalent to:
    
;    (match-let ([(struct honu-subclass (stx subc-name mixin base)) defn]
;                [(struct honu-mixin (stx mxn-name _ _ _ init-names init-types impls
;                                     _ _ defns-before super-new defns-after exports)) mxn])
    
    ;; but it gave me errors, so I separated them out appropriately.  Check into this later.
    
    (match defn
      [(struct honu-subclass (stx subc-name mixin base))
       (match mxn
         [(struct honu-mixin (stx mxn-name _ _ _ init-names init-types impls
                                  _ _ defns-before super-new defns-after exports))
      (let ([prior-impls (get-prior-ifaces tenv base)])
         (at stx `(define ,(honu-translate-class-name subc-name)
                    (parameterize ([current-inspector (make-inspector (current-inspector))])
                      (define ,(honu-translate-class-name subc-name)
                        (class* ,(honu-translate-class-name base)
                          ,(filter-map honu-translate-type-name impls)
                          ,@(honu-translate-init-slots init-names)
                          ,@(honu-translate-slotdefns tenv mxn defns-before)
                          ,(honu-translate-super-new tenv mxn super-new)
                          ,@(honu-translate-slotdefns tenv mxn defns-after)
                          ,@(honu-translate-exports tenv mxn prior-impls exports)))
                      ,(honu-translate-class-name subc-name)))))
      ])]))
  
  )
