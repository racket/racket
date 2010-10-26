
(module compat mzscheme
  (provide register-compatibility-mappings!)
  
  (define (register-compatibility-mappings! register-lib-mapping!)
    (register-lib-mapping!
     "(lib \"comment-snip.ss\" \"framework\")"
     '(lib "comment.ss" "wxme"))

    (register-lib-mapping!
     "drscheme:number"
     '(lib "number.ss" "wxme"))
    (register-lib-mapping!
     "(lib \"number-snip.ss\" \"drscheme\" \"private\")"
     '(lib "number.ss" "wxme"))

    (register-lib-mapping!
     "drscheme:xml-snip"
     '(lib "xml.ss" "wxme"))
    (register-lib-mapping!
     "(lib \"xml-snipclass.ss\" \"xml\")"
     '(lib "xml.ss" "wxme"))

    (register-lib-mapping!
     "drscheme:scheme-snip"
     '(lib "scheme.ss" "wxme"))
    (register-lib-mapping!
     "(lib \"scheme-snipclass.ss\" \"xml\")"
     '(lib "scheme.ss" "wxme"))

    (register-lib-mapping!
     "text-box%"
     '(lib "text.ss" "wxme"))
    (register-lib-mapping!
     "(lib \"text-snipclass.ss\" \"xml\")"
     '(lib "text.ss" "wxme"))

    (register-lib-mapping!
     "(lib \"cache-image-snip.ss\" \"mrlib\")"
     '(lib "cache-image.ss" "wxme"))
    
    (register-lib-mapping!
     "(lib \"image-core.ss\" \"mrlib\")"
     '(lib "image-core-wxme.rkt" "wxme"))

    (register-lib-mapping!
     "test-case-box%"
     '(lib "test-case.ss" "wxme"))))
