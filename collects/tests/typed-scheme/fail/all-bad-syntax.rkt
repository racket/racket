#;
(exn-pred 2)
#lang typed-scheme

(require scheme/list)

(define-type-alias (BT a) (U Boolean (node a)))
(define-struct: (a) node ([key : a] [l : (BT a)] [r : (BT a)]))

(: traverse-4 (All (a i) (BT a) (a ( -> i) ( -> i) -> i) i -> i))
(define (traverse-4 abt f i)
  (cond
    [(boolean? abt) i]
    [(node? abt) (f (node-key abt)
                    (lambda () (traverse-4 (node-l abt) f i))
                    (lambda () (traverse-4 (node-r abt) f i)))]))

(define: (a) (inorder-4 [abt : (BT a)]) : (Listof a)
  (traverse-4 abt
              (lambda: ([key : a] [lt : ( -> (Listof a))] [rt : ( -> (Listof a))])
                (append (lt)
                        (list key)
                        (rt)))
              #;empty))
(+ 'foo)
