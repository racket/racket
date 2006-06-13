(module bindings mzscheme
  (require (lib "list.ss"))
  (provide extract-binding/single
           extract-bindings
           exists-binding?)
                
  ; extract-binding/single : sym (listof (cons str str)) -> str
  (define (extract-binding/single name bindings)
    (define lst (extract-bindings name bindings))
    (cond
      [(empty? lst)
       (error 'extract-binding/single "~e not found in ~e" name bindings)]
      [(empty? (rest lst))
       (first lst)]
      [else 
       (error 'extract-binding/single "~e occurs multiple times in ~e" name bindings)]))
  
  ; extract-bindings : sym (listof (cons str str)) -> (listof str)
  (define (extract-bindings name bindings)
    (map cdr (filter (lambda (x) (equal? name (car x))) bindings)))
  
  ; exists-binding? : sym (listof (cons sym str)) -> bool
  ; for checkboxes
  (define (exists-binding? name bindings)
    (if (assq name bindings)
        #t
        #f)))