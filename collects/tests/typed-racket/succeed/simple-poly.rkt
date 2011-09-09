(module simple-poly typed-scheme
  (: f (All (a) (a -> a)))
  (define (f x) x)
  (define: z : Any 2)
  (provide f))
