#lang scheme

;; Not well tested:
;;  Non-character input data (i.e. specials)
;;  The R5RS part of numbers
;; 
;; Not supported
;;  Honu (#hx, #sx, #honu)
;;  #reader

(require syntax-color/scheme-lexer)

(define (char kind)
  (case kind
    ((white-space) #\space)
    ((symbol) #\i)
    ((constant) #\c)
    ((comment) #\;)
    ((sexp-comment) #\;)
    ((string) #\s)
    ((parenthesis) #\p)
    ((other) #\o)
    ((no-color) #\n)
    ((error) #\x)
    ((hash-colon-keyword) #\h)
    (else
     (error 'char "Given ~a" kind))))
  
(define (chunks x)
  (cond
    ((null? x) 0)
    ((null? (cdr x)) 1)
    ((char=? (car x) (cadr x))
     (chunks (cdr x)))
    (else (add1 (chunks (cdr x))))))
     
(define (lex f p)
  (define-values (lexeme kind paren? start end)
    (f p))
  (cond
    ((eq? 'eof kind) null)
    (else
     (cons (make-string (- end start) (char kind))
           (lex f p)))))
      
(define-syntax (test stx)
  (syntax-case stx ()
    [(_ args ...)
     (with-syntax ([line (syntax-line stx)])
       #'(test/proc line args ...))]))

(define (test/proc line input expected [e-n (chunks (string->list expected))])
  (define p (input->port input))
  (port-count-lines! p)
  (define l (lex scheme-lexer p))
  (define s (apply string-append l))
  (close-input-port p)
  (unless (string=? s expected)
    (eprintf "test on line ~a failed:\n" line)
    (eprintf "  input   : ~s\n" input)
    (eprintf "  output  : ~s\n" s)
    (eprintf "  expected: ~s\n\n" expected))
  (let ((a-n (length l)))
    (unless (= e-n a-n)
      (eprintf "test on line ~a failed:\n" line)
      (eprintf "  input   : ~a\n" input)
      (eprintf "  expected: ~a tokens\n" e-n)
      (eprintf "  got     : ~a tokens\n\n" a-n))))

(define (input->port input) 
  (let-values ([(in out) (make-pipe-with-specials)])
    (thread
     (λ ()
       (let loop ([input input])
         (cond
           [(list? input) 
            (for-each loop input)]
           [(string? input)
            (display input out)]
           [else
            (write-special input out)]))
       (close-output-port out)))
    in))
      
;; Delimiters
(test " " " ")
(test "\t" " ")
(test "\n" " ")
(test "\"" "x")
(test "," "o")
(test "'" "c")
(test "`" "c")
(test ";" ";")
(test "(" "p")
(test ")" "p")
(test "[" "p")
(test "]" "p")
(test "{" "p")
(test "}" "p")

;; #
(test "#fq" "xxx")
(test "#Fq" "xxx")
(test "#tq" "xxx")
(test "#Tq" "xxx")
(test "#true" "ccccc")
(test "#false" "cccccc")
(test "#f q" "cc i")
(test "#F q" "cc i")
(test "#t q" "cc i")
(test "#T q" "cc i")
(test "#true q" "ccccc i")
(test "#false q" "cccccc i")
(test "#f(q" "ccpi")
(test "#T(q" "ccpi")
(test "#true{q" "cccccpi")
(test "#012423(a" "ppppppppi")
(test "#1{a" "pppi")
(test "#1[a" "pppi")
(test "#(a" "ppi")
(test "#{a" "ppi")
(test "#[a" "ppi")
(test "#&a" "cci")
(test "#'a" "cci")
(test "#`a" "cci")
(test "#,a" "ooi")
(test "#,@a" "oooi")
(test "#CsA" "oooi")
(test "#cIA" "oooi")
(test "#hash(a" "ppppppi")
(test "#hasheq(a" "ppppppppi")
(test "#hash[a" "ppppppi")
(test "#hasheq[a" "ppppppppi")
(test "#hash{a" "ppppppi")
(test "#hasheq{a" "ppppppppi")
(test "#135#a" "oooooi")
(test "#453=a" "oooooi")
(test #<<string-end
#! a de \
ase
aa
string-end
      ";;;;;;;;;;;;; ii")
(test "#;z" ";;i")

;; Bad #
(test "#hashe(" "xxxxxxp")
(test "#HASH(" "xxxxxp")
(test "#HASHEQ(" "xxxxxxxp")
(test "#a#a" "xxxx")
(test "#a=a" "xxxx")

;; Identifier delimiters
(test "a" "i")
(test "a a" "i i")
(test "a\ta" "i i")
(test "a\na" "i i")
(test "a\"a" "ixx")
(test "a,a" "ioi")
(test "a'a" "ici")
(test "a`a" "ici")
(test "a;a" "i;;")
(test "a(a" "ipi")
(test "a)a" "ipi")
(test "a[a" "ipi")
(test "a]a" "ipi")
(test "a{a" "ipi")
(test "a}a" "ipi")
(test "a#a" "iii")
(test "a.a" "iii")
(test "a@a" "iii")
(test "a/a" "iii")
(test "a\"a\"a" "isssi")
(test "a1a" "iii")
(test "a%a" "iii")

;; Bad identidier delimiters
(test "#a\t#a" "xx xx")
(test "#a\n#a" "xx xx")
(test "#a\"#a" "xxxxx" 2)
(test "#a,#a" "xxoxx")
(test "#a'#a" "xxcxx")
(test "#a`#a" "xxcxx")
(test "#a;#a" "xx;;;")
(test "#a(#a" "xxpxx")
(test "#a)#a" "xxpxx")
(test "#a[#a" "xxpxx")
(test "#a]#a" "xxpxx")
(test "#a{#a" "xxpxx")
(test "#a}#a" "xxpxx")
(test "#a#a" "xxxx")
(test "#a.a" "xxxx")
(test "#a@a" "xxxx")
(test "#a/a" "xxxx")
(test "#a\"a\"#a" "xxsssxx")
(test "#a1a" "xxxx")
(test "#a%a" "xxxx")

;; Strange identifiers
(test "1a" "ii")
(test "#%a" "iii")
(test "%a" "ii")
(test "\\8" "ii")
(test "\\a" "ii")
(test "\\\\" "ii")
(test "a\\ a" "iiii")
(test "a\\\ta" "iiii")
(test "a\\\na" "iiii")
(test "a\\\"a" "iiii")
(test "a\\,a" "iiii")
(test "a\\'a" "iiii")
(test "a\\`a" "iiii")
(test "a\\;a" "iiii")
(test "a\\(a" "iiii")
(test "a\\)a" "iiii")
(test "a\\[a" "iiii")
(test "a\\]a" "iiii")
(test "a\\{a" "iiii")
(test "a\\}a" "iiii")
(test "\\|" "ii")
(test "a\\|a" "iiii")
(test #<<end-string
\ \8\ \a\\
end-string
      "iiiiiiiiii")
(test "||" "ii")
(test "|a|" "iii")
(test "|8|" "iii")
(test "|\\|" "iii")
(test "a| |a" "iiiii")
(test "a|\t|a" "iiiii")
(test "a|\n|a" "iiiii")
(test "a|\"|a" "iiiii")
(test "a|,|a" "iiiii")
(test "a|'|a" "iiiii")
(test "a|`|a" "iiiii")
(test "a|;|a" "iiiii")
(test "a|(|a" "iiiii")
(test "a|)|a" "iiiii")
(test "a|[|a" "iiiii")
(test "a|]|a" "iiiii")
(test "a|{|a" "iiiii")
(test "a|}|a" "iiiii")
(test "a|,|\\ |||\\|| se \\|\\\\|\\\\|" "iiiiiiiiiiiiiiiiiiiiiiii")
(test "\\ ,a" "iioi")
(test "| |a,a" "iiiioi")
(test "a#||#" "iiiii")
(test "a#;1 2" "ii;;;;")
(test "a#<<a" "iiiii")
(test "1#!1" "iiii")
(test "1+nan.0" "iiiiiii")
(test "-inf.0+1" "iiiiiiii")
(test "\\#:a" "iiii")
(test "#\\:a" "ccci")

;; Bad identifiers
(test "#a" "xx")
(test "#a1.1" "xxxxx")
(test "#a#\\a" "xxxxx")
(test "a|" "xx")
(test "a#|" "xxx")
(test "a||a|, a\n\"" "xxxxxxxxxx")

;; Characters
(test "#\\nul" "ccccc")
(test "#\\NUL" "ccccc")
(test "#\\NuL" "ccccc")
(test "#\\baCKspaCE" "ccccccccccc")
(test "#\\TaB" "ccccc")
(test "#\\NEwline" "ccccccccc")
(test "#\\liNEFEED" "cccccccccc")
(test "#\\vtAb" "cccccc")
(test "#\\PAgE" "cccccc")
(test "#\\reTURn" "cccccccc")
(test "#\\SPACe" "ccccccc")
(test "#\\rubouT" "cccccccc")
(test "#\\nul1a" "cccccii")
(test "#\\18a" "cccii")
(test "#\\411a" "ccciii")
(test "#\\377a" "ccccci")
(test "#\\1#\\a" "cccccc" 2)
(test "#\\uaF12a" "ccccccci")
(test "#\\uaF1g" "cccccci")
(test "#\\UaF12a" "cccccccc")
(test "#\\UaF12aaa" "ccccccccci")
(test "#\\UA" "cccc")

;; Bad Characters
(test "#\\abcsd1a" "xxxxxxxii")
(test "#\\nuls1a" "xxxxxxii")
(test "#\\17a" "xxxxi")
(test "#\\179" "xxxxc")

;; String
(test #<<end-string
""
end-string
      "ss")
(test #<<end-string
"\a\b\t\n\v\f\r\e\'\\\"x"
end-string
      "sssssssssssssssssssssssss")
(test #<<end-string
"\1\12\123"
end-string
      "sssssssssss")
(test #<<end-string
"\xA\xa\x11\xFF\xfff"
end-string
      "sssssssssssssssssssss")
(test #<<end-string
"\xA\xa\x11\xFF\xfff"
end-string
      "sssssssssssssssssssss")
(test #<<end-string
"
a
a \
"
end-string
      "sssssssss")
(test #<<end-string
"\ua\uaA\uAAA\uAAAAa"
end-string
      "sssssssssssssssssssss")
(test #<<end-string
"\Ua\UaA\UAAA\UAAAAa\UAAAAA\Uaaaaaa\Uaaaaaaaaaa"
end-string
      "ssssssssssssssssssssssssssssssssssssssssssssssss")
(test #<<end-string
#""
end-string
      "sss")
(test #<<end-string
#"\a\b\t\n\v\f\r\e\'\\\"x"
end-string
      "ssssssssssssssssssssssssss")
(test #<<end-string
#"\1\12\123"
end-string
      "ssssssssssss")
(test #<<end-string
#"\xA\xa\x11\xFF\xfff"
end-string
      "ssssssssssssssssssssss")
(test #<<end-string
#"\xA\xa\x11\xFF\xfff"
end-string
      "ssssssssssssssssssssss")
(test #<<end-string
#"
a
a \
"
end-string
      "ssssssssss")
(test #<<end-string
#rx"a"
end-string
      "ssssss")
(test #<<end-string
#rx#"a"
end-string
      "sssssss")

;; String Errors
(test #<<end-string
#"\u1"a
end-string
      "xxxxxxi")
(test #<<end-string
#"\U1"a
end-string
      "xxxxxxi")
(test #<<end-string
"\p"a
end-string
      "xxxxi")
(test #<<end-string
"\ug"a
end-string
      "xxxxxi")
(test #<<end-string
"\Ug"a
end-string
      "xxxxxi")
(test #<<end-string
"a
\
s
end-string
      "xxxxxx")
(test #<<end-string
"\p
a
a
end-string
      "xxxxxxx")
(test #<<end-string
#"\Ua
a
end-string
      "xxxxxxx")
(test #<<end-string
#rx"\Ua
a
end-string
      "xxxxxxxxx")
(test #<<end-string
#rx"a
a
end-string
      "xxxxxxx")

;; Here strings
(test #<<end-string
#<<x
x a
 x
x
a
end-string
      "sssssssssssss i")
(test #<<end-string
#<<a
as
 a
end-string
      "xxxxxxxxxx")
      
(test #<<end-string
#<<
a
end-string
      "xxx i")
      
      
;; Comments
(test ";ab" ";;;")
(test #<<end-string
1 a; asd\
1 ;a
end-string
    "c i;;;;;; c ;;")
(test '(";a" 1 "b") ";;;;" 1)  ;; a special comment
(test '(";a" 1 "b\n1" 1) ";;;; cn" 4)
(test ";αα" ";;;")

(test "#||#" ";;;;")
(test "#|#||#|#" ";;;;;;;;")
(test "#| #| \n|# |#" ";;;;;;;;;;;;")
(test "a #|a#|a|#a|#a" "i ;;;;;;;;;;;i")
(test "#|# |#" ";;;;;;")
(test "#|| |#" ";;;;;;")

;; Bad Comments
(test "#|#|#" "xxxxx")
(test "#|" "xx")
(test "a #| a" "i xxxx")
(test "a |# a|" "i iiiii")
(test "#|#| |# a\na" "xxxxxxxxxxx")
(test "#|\n#| |# a\na" "xxxxxxxxxxxx")

;; Numbers and extflonums
(test "1e0" "ccc")
(test "1f0" "ccc")
(test "1t0" "ccc")
(test "1.1e0" "ccccc")
(test "1.1f0" "ccccc")
(test "1.1t0" "ccccc")
(test "#b1.1" "ccccc")
(test "#o1.1" "ccccc")
(test "#d1.1" "ccccc")
(test "#xa.F" "ccccc")
(test "#b1e+1" "cccccc")
(test "#o1E+2" "cccccc")
(test "#d1E+2" "cccccc")
(test "#b1D+1" "cccccc")
(test "#o1d+2" "cccccc")
(test "#d1d+2" "cccccc")
(test "1/2e2" "ccccc")
(test "+iNf.0" "cccccc")
(test "+iNf.f" "cccccc")
(test "+iNf.t" "cccccc")
(test "-InF.0" "cccccc")
(test "-InF.f" "cccccc")
(test "-InF.t" "cccccc")
(test "+naN.0" "cccccc")
(test "-nAN.0" "cccccc")
(test "-nAN.f" "cccccc")
(test "-nAN.t" "cccccc")
(test "-inf.0+1i" "ccccccccc")
(test "1-inf.0I" "cccccccc")
(test "1#/2" "cccc")
(test "#e#x+e#s+e@-e#l-e" "ccccccccccccccccc")
(test "1/0" "ccc")
(test "#e1" "ccc")
(test "#i1" "ccc")
(test "#e1.0+2.3l5i" "cccccccccccc")

;; Bad numbers  
(test "#x1E+2" "xxxxxx")
(test "#x1d+2" "xxxxxx")
(test "#e1.0+2.3l5" "xxxxxxxxxxx")

;; Bad extflonums
(test "#e1t0" "xxxxx")
(test "#i1t0" "xxxxx")
(test "1t0+2t0i" "iiiiiiii")
(test "1t0@2t0" "iiiiiii")

;; Keywords
(test "#:" "hh")
(test "#:a#:a" "hhhhhh")
(test "#:a #:a" "hhh hhh")
(test "#:a\t#:a" "hhh hhh")
(test "#:a\n#:a" "hhh hhh")
(test "#:a\"#:a" "hhhxxxx")
(test "#:a,#:a" "hhhohhh")
(test "#:a'#:a" "hhhchhh" 3)
(test "#:a`#:a" "hhhchhh" 3)
(test "#:a;#:a" "hhh;;;;")
(test "#:a(#:a" "hhhphhh" 3)
(test "#:a)#:a" "hhhphhh" 3)
(test "#:a[#:a" "hhhphhh" 3)
(test "#:a]#:a" "hhhphhh" 3)
(test "#:a{#:a" "hhhphhh" 3)
(test "#:a}#:a" "hhhphhh" 3)
(test "#:a##:a" "hhhhhhh")
(test "#:a.#:a" "hhhhhhh")
(test "#:a@#:a" "hhhhhhh")
(test "#:a/#:a" "hhhhhhh")
(test "#:a\"#:a\"#:a" "hhhssssshhh")
(test "#:a1#:a" "hhhhhhh")
(test "#:a%#:a" "hhhhhhh")

(test "#:1a" "hhhh")
(test "#:\\8" "hhhh")
(test "#:\\a" "hhhh")
(test "#:\\\\" "hhhh")
(test "#:a\\ a" "hhhhhh")
(test "#:a\\\ta" "hhhhhh")
(test "#:a\\\na" "hhhhhh")
(test "#:a\\\"a" "hhhhhh")
(test "#:a\\,a" "hhhhhh")
(test "#:a\\'a" "hhhhhh")
(test "#:a\\`a" "hhhhhh")
(test "#:a\\;a" "hhhhhh")
(test "#:a\\(a" "hhhhhh")
(test "#:a\\)a" "hhhhhh")
(test "#:a\\[a" "hhhhhh")
(test "#:a\\]a" "hhhhhh")
(test "#:a\\{a" "hhhhhh")
(test "#:a\\}a" "hhhhhh")
(test "#:\\|" "hhhh")
(test "#:a\\|a" "hhhhhh")
(test #<<end-string
#:\ \8\ \a\\
end-string
      "hhhhhhhhhhhh")
(test "#:||" "hhhh")
(test "#:|a|" "hhhhh")
(test "#:|8|" "hhhhh")
(test "#:|\\|" "hhhhh")
(test "#:a| |a" "hhhhhhh")
(test "#:a|\t|a" "hhhhhhh")
(test "#:a|\n|a" "hhhhhhh")
(test "#:a|\"|a" "hhhhhhh")
(test "#:a|,|a" "hhhhhhh")
(test "#:a|'|a" "hhhhhhh")
(test "#:a|`|a" "hhhhhhh")
(test "#:a|;|a" "hhhhhhh")
(test "#:a|(|a" "hhhhhhh")
(test "#:a|)|a" "hhhhhhh")
(test "#:a|[|a" "hhhhhhh")
(test "#:a|]|a" "hhhhhhh")
(test "#:a|{|a" "hhhhhhh")
(test "#:a|}|a" "hhhhhhh")
(test "#:a|,|\\ |||\\|| se \\|\\\\|\\\\|" "hhhhhhhhhhhhhhhhhhhhhhhhhh")
(test "#:\\ ,#:a" "hhhhohhh")
(test "#:| |a,#:a" "hhhhhhohhh")
(test "#:a#||#" "hhhhhhh")
(test "#:a#;1 2" "hhhh;;;;")
(test "#:a#<<a" "hhhhhhh")
(test "#:1#!1" "hhhhhh")
(test "#:1+nan.0" "hhhhhhhhh")
(test "#:-inf.0+1" "hhhhhhhhhh")
(test "#:\\#:a" "hhhhhh")
(test "#:#\\:a" "hhhhhh")

(test "#:1.1" "hhhhh")
(test "#:a|" "xxxx")
(test "#:a#|" "xxxxx")
(test "#:a||a|, a\n\"" "xxxxxxxxxxxx")
