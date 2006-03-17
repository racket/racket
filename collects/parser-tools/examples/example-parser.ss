(module example-parser mzscheme
  (require (lib "lex.ss" "parser-tools")
           (lib "yacc.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools"))
           (lib "list.ss" "srfi" "1"))

  (provide malgol-lexer malgol-parser)
  
  
  (define-tokens regular (ID LITNUM STRING))
  (define-empty-tokens keywords (PROCEDURE SEMI BEGIN IN END IF ELSE EQUALS EOF))
  
  (display (equal? (token-ID "foo") (token-ID "foo"))) (newline)
  
  (define-lex-abbrevs
    (lower-letter (:/ "a" "z"))
    (upper-letter (:/ #\A #\Z))
    (id-start (:or lower-letter upper-letter "+" "-" "*" "_"))
    (digit (:/ "0" "9"))
    (id-cont (:or id-start digit)))
  
  (define example-lexer 
    (lexer ((eof) (token-EOF))
           (whitespace (malgol-lexer input-port))
           ("=" (token-EQUALS))
           (";" (token-SEMI))
           ("begin" (token-BEGIN))
           ("end" (token-end))
           ((:+ digit) (token-LITNUM lexeme))
           ((:: id-start (:* id-cont)) (token-ID lexeme))
           ((:: #\" (:* (:or (:~ #\") "\\\"")) #\") (token-STRING (substring lexeme 1 (- (string-length lexeme) 1))))))

  
  ;; keyword-filter : string -> token
  (define (keyword-filter str)
    (let ([maybe-kwd (assoc str keyword-list)])
      (if maybe-kwd 
          ((cadr maybe-kwd))
          (token-ID str))))
  
  (define keyword-list
    `(("--" ,token-ARROW)
      ("true" ,token-TRUE)
      ("false" ,token-FALSE)
      ("in" ,token-IN)
      ("procedure" ,token-PROCEDURE)
      ("if" ,token-IF)
      ("then" ,token-THEN)
      ("else" ,token-ELSE)
      ("begin" ,token-BEGIN)
      ("end" ,token-END)
      ("datatype" ,token-DATATYPE)
      ("cases" ,token-CASES)))
  
  (define p1 "procedure mapem <z,b,<c,d>>; <any -- any>  
begin in
  b <(+),if (3) then 4 else 9;,true,\"abc\\\"def\"> d
end

procedure dufus a; <<any,any,any> -- <any,any>> begin
int <a,b,z> = f<a,b>;
in
z b c (d e)
end

c

")

  (define p2 "procedure mapem <f,l>;
  <any -- any> f; any l;;
  begin in
  if (equal <l,null>) then null
    else
      begin 
        any <firstl,lastl> = l; 
      in
        <f firstl,mapem <f,lastl>>
      end
    ;
  end

3


")
  
    
  (print-struct #t)
  (define (make-lexer str)
    (let ([ip (open-input-string str)])
      (lambda () (malgol-lexer ip))))
  
  #;(define a (map (lambda (x) (malgol-lexer i)) (iota 51)))
  
  
  (define malgol-parser
    (parser (start program)
            #;(debug "/tmp/brox")
            (tokens regular keywords)
            (grammar (program ((datatype-defs proc-defs exps) (list $2 $3)))
                     (datatype-defs [(datatype-def datatype-defs) (cons $1 $2)]
                                    [() null])
                     (datatype-def [(DATATYPE ID BEGIN variant-defs END) (make-datatype $2 $4)])
                     (variant-defs [(variant-def variant-defs) (cons $1 $2)]
                                   [() null])
                     (variant-def [(ID typepattern SEMI) (make-variant $1 $2)])
                     (proc-defs ((proc-def proc-defs) (cons $1 $2))
                                (() null))
                     (proc-def ((PROCEDURE ID namepattern SEMI funtypepattern whereform) (make-fundef (string->symbol $2) $3 $6)))
                     (namepattern ((ID) (string->symbol $1))
                                  ((LBRACK namepattern COMMA namepattern namepattern2 RBRACK) (cons $2 (cons $4 $5))))
                     (namepattern2 ((COMMA namepattern namepattern2) (cons $2 $3))
                                   (() null))
                     (typepattern ((ID) (string->symbol $1))
                                  ((LBRACK typepattern COMMA typepattern typepattern2 RBRACK) (cons $2 (cons $4 $5)))
                                  ((funtypepattern) $1))
                     (typepattern2 ((COMMA typepattern typepattern2) (cons $2 $3))
                                   (() null))
                     (funtypepattern ((LBRACK typepattern ARROW typepattern RBRACK) (list 'arrow  $2 $4)))
                     (exps ((exps single-exp) (make-funcall $1 $2))
                           ((single-exp) $1))
                     (single-exp ((ID) (string->symbol $1))
                                 ((LBRACK exps COMMA exps tuplecont RBRACK) (make-tuple (cons $2 (cons $4 $5))))
                                 ((LITNUM) (string->number $1))
                                 ((TRUE) #t)
                                 ((FALSE) #f)
                                 ((STRING) $1)
                                 ((IF exps THEN exps ELSE exps SEMI) (make-if-s $2 $4 $6))
                                 ((LPAREN exps RPAREN) $2)
                                 ((whereform) $1)
                                 [(CASES ID exps SEMI BEGIN cases-clauses END) (make-cases $2 $3 $6)])
                     (cases-clauses [(cases-clause cases-clauses) (cons $1 $2)]
                                    [() null])
                     (cases-clause [(ID namepattern exps SEMI) (make-cases-clause $1 $2 $3)])
                     (tuplecont (() null)
                                ((COMMA exps tuplecont) (cons $2 $3)))
                     (whereform ((BEGIN defseq IN exps END) (unwind-where $2 $4)))
                     (defseq (() null)
                             ((proc-def SEMI defseq) (cons $1 $3))
                             ((typepattern namepattern EQUALS exps SEMI defseq) (cons (list $1 $2 $4) $6))))
            (end EOF)
            (error (lambda (a b c) (error 'malgol-parser "error occurred, ~v ~v ~v" a b c)))
            ))
  
  ;; unwind-where : (listof (or/c funcall? (list/c type pattern rhs)))
  (define (unwind-where defs-n-procs body)
    (fold-right (lambda (def-or-proc body)
                  (cond [(fundef? def-or-proc)
                         (make-where body (fundef-name def-or-proc) def-or-proc)]
                        [else (make-where body (cadr def-or-proc) (caddr def-or-proc))]))
                body
                defs-n-procs))
  
  (define (parse-test str result)
    (assert equal? (malgol-parser (make-lexer str))
            result))
  
  (define (tup . args)
    (make-tuple args))
  
  (define ts 
    (make-test-suite 
     "foo"
     (make-test-case "constants"
       (parse-test "23" '(() 23))
       (parse-test "true" '(() #t))
       (parse-test "false" '(() #f))
       (parse-test "\"abc\\\"def\"" '(() "abc\\\"def")))
     (make-test-case "simple exps"
       (parse-test "x" '(() x))
       (parse-test "x y" (list null (make-funcall 'x 'y)))
       (parse-test "<x y,x>" (list null (tup (make-funcall 'x 'y) 'x)))
       (parse-test "if true then true else false;" (list null (make-if-s #t #t #f)))
       (parse-test "(<2,<2,x (y)>>)" (list null (tup 2 (tup 2 (make-funcall 'x 'y))))))
     (make-test-case "wheres"
       (parse-test "begin any x = 3 ; in y end" (list null (make-where 'y 'x 3)))
       (parse-test "begin <a,a> q = <3,4> ; any <z,y> = 4 ; in x end" 
                   (list null
                         (make-where 
                          (make-where 'x
                                      '(z y)
                                      4)
                          'q
                          (tup 3 4))))
       (parse-test "begin any q = 4; procedure z <x,y> ; <<any,<any -- any>> -- any> begin in 3 end; any x = 5; in 13 end"
                   (list null
                         (make-where 
                          (make-where
                           (make-where 13 'x '5)
                           'z
                           (make-fundef 'z (list 'x 'y) 3))
                          'q
                          4))))
     (make-test-case "procs"
       (parse-test "procedure x y ; <int -- int> begin in y end 3" 
                   (list (list (make-fundef 'x 'y 'y)) 3))
       (parse-test "procedure id x ; <any -- any> begin in x end procedure g x ; <any -- any> begin in 14 end 3"
                   (list (list (make-fundef 'id 'x 'x) 
                               (make-fundef 'g 'x 14))
                         3)))
     (make-test-case "factorial"
       (parse-test #<<@
procedure factorial x ; <any -- any>
  begin in
    if equal <x,0> then
      1
    else
      * <x,factorial (- <x,1>)>
    ;
  end

factorial 6
@
                   (list (list (make-fundef 'factorial 'x
                                            (make-if-s (make-funcall 'equal
                                                                     (make-tuple (list 'x 0)))
                                                       1
                                                       (make-funcall '*
                                                                     (make-tuple
                                                                      (list 'x
                                                                            (make-funcall
                                                                             'factorial
                                                                             (make-funcall
                                                                              '-
                                                                              (make-tuple (list 'x 1))))))))))
                         (make-funcall 'factorial 6)))
       
       (parse-test #<<@
procedure fordax <y,z> ; <<any,<int,any>> -- <any,any>>
  begin
    <int,int> z = <3,4>;
    procedure inner z ; <<int,any> -- <any,any>> begin in + <z,3> end;
    any q = 14;
  in
    z q
  end

fordax <brug,zorb>
@
                   
                   (list 
                    (list 
                     (make-fundef
                      'fordax
                      '(y z)
                      (make-where
                        (make-where (make-where (make-funcall 'z 'q) 'q 14) 
                                    'inner 
                                    (make-fundef 'inner 'z (make-funcall '+ (make-tuple (list 'z 3)))))
                        'z
                        (make-tuple (list 3 4)))))
                    (make-funcall 'fordax (make-tuple (list 'brug 'zorb)))))
)
     ))
  
  (test/text-ui ts)
  
  (define p 
    (malgol-parser (make-lexer #<<@
procedure fordax <y,z> ; <<any,<int,any>> -- <any,any>>
  begin
    <int,int> z = <3,4>;
    procedure inner z ; <<int,any> -- <any,any>> begin in + <z,3> end;
    any q = 14;
  in
    z q
  end

fordax <brug,zorb>
@
                                )))
  #;(equal? (parse-string "procedure foo x ; any x;; begin any y = 3; in y end 3")
          (list (make-fundef )))
  (define result (malgol-parser (make-lexer p1)))
  #;(define result2 (malgol-parser (make-lexer p2)))
)


