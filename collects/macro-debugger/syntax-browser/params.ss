
(module params mzscheme
  (provide current-syntax-font-size
           current-default-columns)

  ;; current-syntax-font-size : parameter of number/#f
  ;; When non-false, overrides the default font size
  (define current-syntax-font-size (make-parameter #f))
  
  ;; current-default-columns : parameter of number
  (define current-default-columns (make-parameter 60))

  )
