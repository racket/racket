(module highlight-placeholder mzscheme
  
  (provide highlight-placeholder highlight-placeholder-stx)
  
  ; highlight-placeholder : symbol
  ; highlight-placeholder-stx : syntax
  
  ; we rely upon the fact that the highlight-placeholder-stx is a syntax-object, so that
  ; syntax objects containing the highlight-placeholder-stx still fit the data definition
  ; for syntax objects 
  
  (define highlight-placeholder (gensym "highlight-placeholder"))
  (define highlight-placeholder-stx #`#,highlight-placeholder))
