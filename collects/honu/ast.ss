(module ast mzscheme
  
  (provide (all-defined))
  
  (define-struct honu-program (defns))
  
  (define-struct honu-ast (src-stx))
   
  (define-struct (honu-defn honu-ast) ())

  (define-struct (honu-type honu-ast) ())

  (define-struct (honu-prim-type honu-type) (name))
  (define-struct (honu-func-type honu-type) (args return))
  (define-struct (honu-dispatch-type honu-type) (dispatches args return))
  (define-struct (honu-iface-type honu-type) (name))
  (define-struct (honu-iface-bottom-type honu-type) ())
  (define-struct (honu-iface-top-type honu-type) ())
  (define-struct (honu-bottom-type honu-type) ())
  (define-struct (honu-top-type honu-type) ())

  (define-struct (honu-function honu-defn) (name type arg-names arg-types body))
  (define-struct (honu-type-defn honu-defn)  (name supers decls))
  (define-struct (honu-class honu-defn) (name type final? init-names init-types impls defns exports))
  (define-struct (honu-mixin honu-defn) (name type arg-type final? init-names init-types impls with-names with-types
                                          defns-before super-new defns-after exports))
  (define-struct (honu-subclass honu-defn) (name mixin base))
  
  (define-struct (honu-field-decl honu-ast) (name type))
  (define-struct (honu-method-decl honu-ast) (name type arg-types))
   
  (define-struct (honu-init-field honu-ast) (name type value))
  (define-struct (honu-field honu-ast)      (name type value))
  (define-struct (honu-method honu-ast)     (name type arg-names arg-types body))
   
  (define-struct (honu-super-new honu-ast)  (arg-names arg-vals))
   
  (define-struct (honu-export honu-ast)     (type old-names new-names))
   
  (define-struct (honu-exp honu-ast)        ())

  (define-struct (honu-null   honu-exp) ())
  (define-struct (honu-int    honu-exp) (value))
  (define-struct (honu-float  honu-exp) (value))
  (define-struct (honu-bool   honu-exp) (value))
  (define-struct (honu-char   honu-exp) (value))
  (define-struct (honu-str    honu-exp) (value))
  (define-struct (honu-lambda honu-exp) (arg-names arg-types body))
  (define-struct (honu-prim   honu-exp) (op op-stx op-type left right))
  (define-struct (honu-uprim  honu-exp) (op op-stx op-type body))
  (define-struct (honu-facc   honu-exp) (obj elab field))
  (define-struct (honu-fassn  honu-exp) (obj elab field rhs))
  (define-struct (honu-mcall  honu-exp) (obj elab method args))
  (define-struct (honu-var    honu-exp) (name builtin?))
  (define-struct (honu-assn   honu-exp) (name rhs))
  (define-struct (honu-call   honu-exp) (name args builtin?))
  (define-struct (honu-this   honu-exp) ())
  (define-struct (honu-cast   honu-exp) (obj type))
  (define-struct (honu-isa    honu-exp) (obj type))
  (define-struct (honu-if     honu-exp) (cond true false))
  (define-struct (honu-new    honu-exp) (class type arg-names arg-vals))
  (define-struct (honu-while  honu-exp) (cond body))
  (define-struct (honu-block  honu-exp) (binds exps))
  (define-struct (honu-return honu-exp) (body))
   
  (define-struct (honu-binding honu-ast) (name type rhs))
 ) 
