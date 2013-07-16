
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

(htdp-syntax-test #'quasiquote "quasiquote: expected an open parenthesis before quasiquote, but found none")
(htdp-syntax-test #'`unquote "quasiquote: misuse of unquote within a quasiquoting backquote")
(htdp-syntax-test #'`unquote-splicing "quasiquote: misuse of ,@ or unquote-splicing within a quasiquoting backquote")
(htdp-syntax-test #'`(unquote-splicing 10) "quasiquote: misuse of ,@ or unquote-splicing within a quasiquoting backquote")

(htdp-syntax-test #'unquote "unquote: misuse of a comma or unquote, not under a quasiquoting backquote")
(htdp-syntax-test #'(unquote) "unquote: misuse of a comma or unquote, not under a quasiquoting backquote")
(htdp-syntax-test #'(unquote 10) "unquote: misuse of a comma or unquote, not under a quasiquoting backquote")

(htdp-syntax-test #'unquote-splicing "unquote-splicing: misuse of ,@ or unquote-splicing, not under a quasiquoting backquote")
(htdp-syntax-test #'(unquote-splicing (list 10)) "unquote-splicing: misuse of ,@ or unquote-splicing, not under a quasiquoting backquote")

(htdp-err/rt-test `(,@4) (exn-type-and-msg exn:fail:contract? "append: expects a list, given 4"))
