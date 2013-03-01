(#rx"define-language:.*expression context"
 ([illegal-def (define-language L)])
 (values illegal-def))

(#rx"define-language:.*unquote disallowed"
 ([illegal-unquote ,3])
 (let ()
   (define-language L
     (n illegal-unquote))
   (void)))

; error message shows correct form name
(#rx"define-extended-language:.*underscore"
 ([bad-underscore y_1])
 (let ()
   (define-language L)
   (define-extended-language M L
     (z () (1 bad-underscore)))
   (void)))

(#rx"expected an identifier" ([not-id (L)]) (define-language not-id))
(#rx"expected at least one production" ([separator ::=]) (define-language L (x separator)))
(#rx"expected at least one production" ([nt x]) (define-language L (nt)))
(#rx"expected at least one production" ([nt-pos (x)]) (define-language L (nt-pos)))
(#rx"expected preceding non-terminal names" ([separator ::=]) (define-language L (separator a b)))
(#rx"expected non-terminal name" ([not-nt (y)]) (define-language L (x not-nt ::= z)))
(#rx"expected production" ([not-prod ::=]) (define-language L (x ::= y not-prod z)))
(#rx"expected non-terminal definition" ([not-def q]) (define-language L not-def))
(#rx"expected non-terminal definition" ([not-def ()]) (define-language L not-def))
