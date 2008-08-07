(module errortrace-key '#%kernel
  (define-values (errortrace-key) (gensym 'key))
  
  (#%provide errortrace-key))

