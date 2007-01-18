
(module wxmecompat mzscheme
  (require "wxmefile.ss")

  (register-lib-mapping!
   "(lib \"comment-snip.ss\" \"framework\")"
   "(lib \"comment.ss\" \"mred\" \"wxme\")")

  (register-lib-mapping!
   "drscheme:number"
   "(lib \"number.ss\" \"mred\" \"wxme\")")
  (register-lib-mapping!
   "(lib \"number-snip.ss\" \"drscheme\" \"private\")"
   "(lib \"number.ss\" \"mred\" \"wxme\")")

  (register-lib-mapping!
   "drscheme:xml-snip"
   "(lib \"xml.ss\" \"mred\" \"wxme\")")
  (register-lib-mapping!
   "(lib \"xml-snipclass.ss\" \"xml\")"
   "(lib \"xml.ss\" \"mred\" \"wxme\")")

  (register-lib-mapping!
   "drscheme:scheme-snip"
   "(lib \"scheme.ss\" \"mred\" \"wxme\")")
  (register-lib-mapping!
   "(lib \"scheme-snipclass.ss\" \"xml\")"
   "(lib \"scheme.ss\" \"mred\" \"wxme\")")

  (register-lib-mapping!
   "text-box%"
   "(lib \"text.ss\" \"mred\" \"wxme\")")
  (register-lib-mapping!
   "(lib \"text-snipclass.ss\" \"xml\")"
   "(lib \"text.ss\" \"mred\" \"wxme\")")

  (register-lib-mapping!
   "(lib \"cache-image-snip.ss\" \"mrlib\")"
   "(lib \"cache-image.ss\" \"mred\" \"wxme\")"))
