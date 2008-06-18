(module default-code-style mzscheme
  (provide color-default-code-styles
           bw-default-code-styles
           code-style-color
           code-style-slant?
           code-style-bold?
           code-style-underline?)
  
  (define-struct code-style (color slant? bold? underline?))
  ;; code-style = (make-code-style (union (list number number number) string) bolean boolean)
  
  ;; bw-default-code-styles : (listof (list symbol code-style
  (define bw-default-code-styles
    (list (list 'lexically-bound-variable (make-code-style "black" #f #f #t))
          (list 'lexically-bound-syntax (make-code-style "black" #f #f #t))
          (list 'imported-variable (make-code-style "black" #f #f #t))
          (list 'imported-syntax (make-code-style "black" #f #f #t))
          (list 'unbound-variable (make-code-style "black" #t #f #f))
          (list 'constant (make-code-style '(51 135 39) #f #f #f))))
  
  ;; color-default-code-styles : (listof (list symbol code-style))
  (define color-default-code-styles
    (list (list 'keyword (make-code-style '(40 25 15) #f #f #f))
          (list 'unbound-variable (make-code-style "red" #f #f #f))
          (list 'bound-variable (make-code-style "navy" #f #f #f))
          (list 'primitive (make-code-style "navy" #f #f #f))
          (list 'constant (make-code-style '(51 135 39) #f #f #f)))))
