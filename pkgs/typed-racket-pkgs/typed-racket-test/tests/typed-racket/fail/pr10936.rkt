#;
(exn-pred 2)
#lang typed/racket
(define: my-rest : (All (elem) (elem elem * -> (Listof elem)))
  (lambda ( #{ arg : elem} . #{ tail : (Listof elem) }) 
    (rest tail)))
(define: my-rest2 : (All (elem) (elem elem * -> (Listof elem)))
  (lambda ( #{ arg : elem} . #{ tail : elem }) 
    (rest tail)))
