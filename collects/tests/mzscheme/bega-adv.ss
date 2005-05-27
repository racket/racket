
(htdp-test 1 'quote '1)
(htdp-test (list 'quote 1) 'quote ''1)
(htdp-test "Hello" 'quote '"Hello")
(htdp-test (list 1 2) 'quote '(1 2))
(htdp-test (list 1 (list 2 "hi")) 'quote '(1 (2 "hi")))

(htdp-test 1 'qq `1)
(htdp-test '(1 2) 'qq `(1 2))
(htdp-test 7 'qq `,(+ 3 4))
(htdp-test '(1 3) 'qq `(1 ,(+ 1 2)))
(htdp-test '(99 88 77) 'qq `(,(* 11 9) ,(* 11 `8) ,`,(* 11 7)))
(htdp-test '(1 2 3 4) 'qq `(1 ,@(list 2 3) 4))
(htdp-test '(quasiquote 11) 'qq ``11)
(htdp-test '(quasiquote (unquote 11)) 'qq ``,11)
(htdp-test '(quasiquote (unquote 22)) 'qq ``,,(* 11 2))
(htdp-test '(quasiquote ((unquote-splicing (22)))) 'qq ``(,@(,@(list (* 11 2)))))

(htdp-syntax-test #'quasiquote)
(htdp-syntax-test #'`unquote)
(htdp-syntax-test #'`unquote-splicing)
(htdp-syntax-test #'`(unquote-splicing 10))

(htdp-syntax-test #'unquote)
(htdp-syntax-test #'(unquote))
(htdp-syntax-test #'(unquote 10))

(htdp-syntax-test #'unquote-splicing)
(htdp-syntax-test #'(unquote-splicing (list 10)))
(htdp-syntax-test #'((unquote-splicing (list 10))))

(htdp-err/rt-test `(,@4))
