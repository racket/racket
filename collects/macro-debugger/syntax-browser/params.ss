
(module params mzscheme
  (provide current-syntax-font-size
           current-default-columns
           current-colors
           current-suffix-option)

  ;; current-syntax-font-size : parameter of number/#f
  ;; When non-false, overrides the default font size
  (define current-syntax-font-size (make-parameter #f))
  
  ;; current-default-columns : parameter of number
  (define current-default-columns (make-parameter 60))
  
  ;; current-suffix-option : parameter of SuffixOption
  (define current-suffix-option (make-parameter 'over-limit))
  
  (define current-colors
    (make-parameter
     (list "black" "red" "blue"
           "mediumforestgreen" "darkgreen" 
           "darkred"
           "cornflowerblue" "royalblue" "steelblue" "darkslategray" "darkblue"
           "indigo" "purple" 
           "orange" "salmon" "darkgoldenrod" "olive")))

  )
