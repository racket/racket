(module list-signature mzscheme
  (provide provide-list
           provide-double-ended-list
           provide-random-access-list
           provide-catenable-double-ended-list
           )
  
  (define-syntax provide-list
    (syntax-rules ()
      [(_)
       (provide elements
                empty
                empty?
                insert
                insert*
                remove   
                first
                fold)]))

  (define-syntax provide-double-ended-list
    (syntax-rules ()
      [(_)
       (begin 
         (provide-list)
         (provide insert-first
                  insert-last
                  remove-first
                  snoc ; = insert-last
                  ))]))
  
  (define-syntax provide-random-access-list
    (syntax-rules ()
      [(_)
       (begin
         (provide-list)
         (provide ref ; (lookup in [Oka])   ; int list-of-alpha -> alpha
                  set))]))
  
  (define-syntax provide-catenable-list
    (syntax-rules ()
      [(_)
       (begin
         (provide-list)
         (provide append
                  insert-last
                  snoc))]))

  (define-syntax provide-catenable-double-ended-list
    (syntax-rules ()
      [(_)
       (begin
         (provide-double-ended-list)
         (provide append))]))

  
  )