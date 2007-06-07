(module parser-units (lib "lazy.ss" "lazy")
  
  (require (lib "unit.ss"))
  
  (require (lib "lex.ss" "parser-tools")
           (lib "combinator-unit.ss" "combinator-parser")
           "java-signatures.scm"
           (lib "string.ss"))

  (define-unit java-dictionary@
    (import)
    (export language-dictionary^ 
            (rename language-format-parameters^
                    (output-map input->output-name)))
    
    (define class-type "keyword")
    
    (define (output-map x)
      (!!! (when (position-token? x)
             (set! x (position-token-token x))))
      (!!! (case (token-name x)
             [(PIPE) "|"]
             [(OR) "||"]
             [(OREQUAL) "|="]
             [(EQUAL) "="]
             [(GT) ">"]
             [(LT) "<"]
             [(LTEQ) "<="]
             [(GTEQ) ">="]
             [(PLUS) "+"]
             [(MINUS) "-"]
             [(TIMES) "*"]
             [(DIVIDE) "/"]
             [(^T) "^"]
             [(O_PAREN) "("]
             [(C_PAREN) ")"]
             [(O_BRACE) "{"]
             [(C_BRACE) "}"]
             [(O_BRACKET) "["]
             [(C_BRACKET) "]"]
             [(SEMI_COLON) ";"]
             [(PERIOD) "."]
             [(COMMA) ","]
             [(NULL_LIT) "null"]
             [(TRUE_LIT) "true"]
             [(FALSE_LIT) "false"]
             [(EOF) "end of input"]
             [(caseT) "case"]
             [(doT) "do"]
             [(elseT) "else"]
             [(ifT) "if"]
             [(voidT) "void"]
             [(STRING_LIT) (format "\"~a\"" (token-value x))]
             [(CHAR_LIT) (format "'~a'" (token-value x))]
             [(INTEGER_LIT LONG_LIT FLOAT_LIT DOUBLE_LIT
                           HEX_LIT OCT_LIT HEXL_LIT OCTL_LIT) (token-value x)]
             [(IDENTIFIER) (format "identifier ~a" (token-value x))]
             [(STRING_ERROR) (format "misformatted string ~a" (token-value x))]
             [else (token-name x)])))
    
    (define (java-keyword? t)
      (memq  t `(? this super new instanceof while try throw synchronized switch return ifT goto for finally
                   elseT doT default continue catch case break voidT throws const interface implements extends
                   class import package EQUAL += -= *= /= &= ^= %= <<= >>= >>>=
                   boolean byte char double float int long short
                   abstract native private protected public static strictfp transient volatile)))
    
    (define (close-to-keyword? t arg)
      (printf "close-to-keyword ~a ~a~n" t arg)
      (and (string? t)
           (member t (select-words (string->symbol arg)))))
    
    (define (miscapitalized? t key)
      (and (string? t)
           (let ((s (string-copy t)))
             (string-lowercase! s)
             (equal? s key))))
    
    (define misspelled-list '((import "mport" "iport" "imort" "imprt" "impot" "impor" "improt" "impourt")
                              (class "lass" "cass" "clss" "clas" "calss")
                              (abstract 
                               "bstract" "astract" "abtract" "absract" "abstact" "abstrct" "abstrat" "abstract" "abstarct" "abstracts")
                              (extends "xtends" "etends" "exends" "extnds" "exteds" "extens" "extneds" "extend")
                              (new "nw" "ne" "nwe")
                              (this "his" "tis" "ths" "thi" "tihs" "thsi")
                              (instanceof "instancef" "instanceo" "intsanceof")
                              (if "fi")
                              (else "lse" "ese" "els" "eles")
                              (return "eturn" "rturn" "reurn" "retrn" "retun" "retur" "reutrn" "retrun" "returns")
                              (true "rue" "tue" "tre" "tru" "ture" "treu")
                              (false "flse" "fase" "fale" "fals" "flase" "fasle")
                              (interface
                                  "nterface" "iterface" "inerface" "intrface" "inteface" "interace" "interfce" "interfae" "intreface")
                              (implements 
                               "mplements" "iplements" "impements" "implments" "impleents" "implemnts" "implemets" "implemens"
                               "implement")
                              (void "oid" "vid" "voi" "viod")
                              (super "uper" "sper" "supr" "supe" "supper")
                              (public "ublic" "pblic" "pulic" "pubic" "publc" "publi" "pubilc")
                              (private "rivate" "pivate" "prvate" "priate" "privte" "privae" "privat" "pravite")
                              (package "ackage" "pckage" "pakage" "pacage" "packge" "packae" "packag")
                              (protected "rotected" "portected")
                              (final "inal" "fnal" "fial" "finl" "finale" "fianl")
                              (check "chek" "cehck" "chck" "chack")
                              (expect "expct" "expeet" "expec" "exect")
                              (within "with" "withi" "withen" "wihtin")
                              ))
    
    (define (select-words key)
      (safe-car (filter (lambda (x) (eq? (car x) key)) misspelled-list)))
    (define (safe-car f)
      (if (null? f) null (car f)))
    
    (define all-words (filter string? (apply append misspelled-list)))
    
    
    (define misspelled (lambda (id token-v) (if (close-to-keyword? token-v id) 1 0)))
    (define misscap (lambda (id token-v) (miscapitalized? token-v id)))
    (define missclass (lambda (id token-n) (and (eq? 'IDENTIFIER id) (java-keyword? token-n))))
    
    )
  
  (define-signature teaching-languages^ 
    (parse-beginner parse-beginner-interactions
     parse-intermediate parse-intermediate-interactions parse-intermediate+access 
     parse-advanced parse-advanced-interactions
     old-tokens->new))
  
  (define-signature id^ (id))
  
  ;Terminals unit  
  (define-unit java-terminals@
    (import combinator-parser^ id^)
    (export java-operators^ java-separators^ java-literals^ java-expression-keywords^
            java-statement-keywords^ java-definition-keywords^ 
            java-type-keywords^ java-reserved^ java-extras^ java-vals^ java-ids^ java-specials^)              
        
    (define-simple-terminals Operators
      ((PIPE "|") (OR "||") (OREQUAL "|=")
                  (EQUAL "=")	(GT ">") (LT "<") !	~	?	:
                  ==	(LTEQ "<=")	(GTEQ ">=")	!=	&&	++	--
                  (PLUS "+")	(MINUS "-")	
                  (TIMES "*")	(DIVIDE "/")	&	(^T "^")	%	<< >> >>>
                  +=	-=	*=	/=	&=	^=	%=	<<=	>>=	>>>=))
    
    (define-simple-terminals Separators
      ((O_PAREN "(") (C_PAREN ")") (O_BRACE "{") (C_BRACE "}")
                     (O_BRACKET "[") (C_BRACKET "]") (SEMI_COLON ";") (PERIOD ".") (COMMA ",")))
    
    (define-simple-terminals EmptyLiterals ((NULL_LIT "null") (TRUE_LIT "true") (FALSE_LIT "false") EOF))
    
    (define-simple-terminals Keywords 
      (abstract       default        (ifT "if")    private      this
                      boolean        (doT "do")    implements    protected    throw
                      break          double         import        public       throws
                      byte           (elseT "else") instanceof    return       transient
                      (caseT "case") extends        int           short        try
                      catch          final          interface     static       (voidT "void")
                      char           finally        long          strictfp     volatile
                      class          float          native        super        while
                      const          for            new           switch
                      continue       goto           package       synchronized))
    
    (define-simple-terminals ExtraKeywords (dynamic check expect within -> ->> ->>> test tests testcase))
    
    (define-terminals java-vals
      ((STRING_LIT "String literal" id) (CHAR_LIT "character" id) (INTEGER_LIT "integer" id)
                                        (LONG_LIT "long" id) (FLOAT_LIT "float" id) (DOUBLE_LIT "double" id)
                                        (IDENTIFIER "identifer" id)  (STRING_ERROR id) 
                                        (NUMBER_ERROR id) (HEX_LIT id) (OCT_LIT id) (HEXL_LIT id) (OCTL_LIT id)))
    
    (define-terminals special-toks ((EXAMPLE id) (TEST_SUITE id) (IMAGE_SPECIAL id) (OTHER_SPECIAL id)))
    
    )
  
    ;---------------------------------------------------------------------------------------------------
    ; Types, modifiers, operators
      
    

  (define-unit types@
    (import combinator-parser^ java-type-keywords^ java-ids^ java-separators^ id^)
    (export java-types^)
    
    (define integer-types
      (choose (byte int short long) "type"))
    (define inexact-types
      (choose (float double) "type"))
    (define numeric-type
      (choose (integer-types inexact-types) "numeric type"))
    (define prim-type
      (choose (boolean double byte int short char long float) "type"))
    
    (define (other-type-base types) (choice types "type"))
    
    (define (value+name-type base-type name)
      (choose (base-type name) "type"))
    
    (define (method-type base-t)
      (choice (list base-t voidT) "method return"))
    
    (define (array-type base-t)
      (choice (base-t (sequence (base-t O_BRACKET C_BRACKET (repeat (sequence (O_BRACKET C_BRACKET) id))) id
                                "array type")) "type"))
    )
    

  (define-unit mods@
    (import combinator-parser^ java-definition-keywords^)
    (export java-access^)
  
    (define access-mods
      (choose (public private protected) "access modifier"))
    
    (define (global-mods base-mods)
      (choice (list base-mods static) "modifier"))
    
    (define (method-mods base-mods)
      (choice (list base-mods abstract) "modifier"))
 
    )
    
  (define-unit operators@
    (import combinator-parser^ java-operators^ java-separators^)
    (export java-ops^)
  
    (define math-ops
      (choose (PLUS MINUS TIMES DIVIDE %) "binary operation"))
    
    (define shift-ops
      (choose (<< >> >>>) "shift operation"))
    
    (define compare-ops
      (choose (== GT LT LTEQ GTEQ !=) "binary operation"))
    
    (define bool-ops
      (choose (&& OR) "binary operation"))
    
    (define bit-ops 
      (choose (^T PIPE &) "binary operation"))
    
    (define assignment-ops
      (choose (EQUAL OREQUAL += -= *= /= &= ^= %= <<= >>= >>>=) "assignment"))
    
    (define (bin-ops ops)
      (choice ops "binary operation"))
    
    (define un-assignment
      (choose (++ --) "unary operation"))
    
    (define un-op
      (choose (~ + -) "unary operation"))
    
    )

  (define-unit general@
    (import combinator-parser^ java-separators^ java-operators^ java-ids^ id^)
    (export general-productions^)
  
    (define (comma-sep term name)
      (sequence (term (repeat (sequence (COMMA term) id))) id (string-append "list of " name)))
    
    (define (variable-declaration type expr share-type? name)
      (let* ([f (choose (IDENTIFIER (sequence ((^ IDENTIFIER) EQUAL expr) id)) (string-append name " declaration"))]
             [s&e (sequence (type (comma-sep f name) SEMI_COLON) id (string-append name " definition"))]
             [s (sequence (type (comma-sep IDENTIFIER name) SEMI_COLON) id (string-append name " definition"))]
             [e (sequence (type (^ IDENTIFIER) EQUAL expr SEMI_COLON) id (string-append name " definition"))]
             [base (sequence (type (^ IDENTIFIER) SEMI_COLON) id (string-append name " definition"))])
        (cond
          [(and expr share-type?) s&e]
          [share-type? s]
          [expr (choose (e base) (string-append name " definition"))]
          [else base])))
    
    (define name
      (sequence (IDENTIFIER (repeat (sequence (PERIOD IDENTIFIER) id))) id "name"))
    
    )
  
  (define-unit expressions@ 
    (import combinator-parser^ general-productions^ id^
            java-literals^ java-expression-keywords^ java-vals^ java-ids^ java-separators^
            java-operators^ java-extras^)
    (export expression-maker^ expr-lits^ expr-terms+^ expr-tails^)
            
    (define (simple-expression exprs)
      (choice exprs "expression"))
    
    (define boolean-lits
      (choose (TRUE_LIT FALSE_LIT) "boolean literal"))
    
    (define textual-lits
      (choose (STRING_LIT CHAR_LIT) "literal expression"))
    
    (define prim-numeric-lits
      (choose (INTEGER_LIT LONG_LIT) "literal expression"))
    
    (define numeric-lits
      (choose (HEX_LIT HEXL_LIT OCTL_LIT OCT_LIT) "literal expression"))
    
    (define double-lits
      (choose (FLOAT_LIT DOUBLE_LIT) "literal expression"))
    
    (define null-lit NULL_LIT)
    
    (define (literals lits)
      (choice lits "literal expression"))
    
    (define all-literals
      (choose (NULL_LIT boolean-lits textual-lits prim-numeric-lits double-lits numeric-lits)
              "literal expression"))
    
    (define (new-class class-name expr)
      (choose ((sequence (new class-name O_PAREN C_PAREN) id)
               (sequence (new class-name O_PAREN (comma-sep expr "arguments") C_PAREN) id))
              "class instantiation"))
    
    (define (new-array type-name expr)
      (sequence (new type-name O_BRACKET expr C_BRACKET (repeat (sequence (O_BRACKET expr C_BRACKET) id)))
                id "array instantiation"))
    
    (define field-access-end
      (sequence (PERIOD IDENTIFIER) id "field access"))
    
    (define (array-access-end expr)
      (sequence (O_BRACKET expr C_BRACKET) id "array access"))
    
    (define (array-init-maker contents)
      (sequence (O_BRACE (comma-sep contents "array elements") C_BRACE) id "array initializations"))
    
    (define (array-init type-name expr)
      (letrec ([base-init (array-init-maker expr)]
               [simple-init (array-init-maker (choose (expr base-init (eta init)) "array initializations"))]
               [init (array-init-maker (choose (expr simple-init) "array initializations"))])
        (sequence (new type-name init) "array initialization")))
    
    (define (binary-expression-end op expr)
      (sequence ((^ op) expr) id "binary expression"))
    
    (define (if-expr-end expr)
      (sequence (? expr : expr) id "conditional expression"))
    
    (define (simple-method-call expr)
      (choose
       ((sequence ((^ IDENTIFIER) O_PAREN C_PAREN) id "method invocation")
        (sequence ((^ IDENTIFIER) O_PAREN (comma-sep expr "argument list") C_PAREN) id "method invocation"))
       "method invocation"))
    
    (define (method-call-end expr)
      (choose
       ((sequence (PERIOD (^ IDENTIFIER) O_PAREN C_PAREN) id "method invocation")
        (sequence (PERIOD (^ IDENTIFIER) O_PAREN (comma-sep expr "argument list") C_PAREN) id "method invocation"))
       "method invocation"))
    
    (define (assignment name op expr)
      (sequence ((^ name) op expr) id "assignment"))
    
    (define (unary-assignment-front expr)
      (choose ((sequence (++ expr) id "unary modification")
               (sequence (-- expr) id "unary modification")) "unary modification"))
    
    (define (unary-assignment-back base)
      (choose ((sequence (base ++) id "unary modification")
               (sequence (base --) id "unary modification")) "unary modification"))
    
    (define (cast type expr)
      (sequence (O_PAREN type C_PAREN expr) "cast expression"))
    
    (define (instanceof-back name)
      (sequence (instanceof name) "instanceof expression"))
    
    (define (super-call expr)
      (choose ((sequence (super PERIOD IDENTIFIER O_PAREN C_PAREN) id "super method invocation")
               (sequence (super PERIOD IDENTIFIER O_PAREN (comma-sep expr "arguments") C_PAREN) id "super method invocation"))
              "super method invocation"))
    
    (define (checks expr)
      (choose
       ((sequence (check expr expect expr) id "check expression")
        (sequence (check expr expect expr within expr) id "check expression"))
       "check expression"))
    
    )
    
  (define-unit statements@
    (import combinator-parser^ general-productions^ id^
            java-statement-keywords^ java-separators^ java-ids^ java-operators^)
    (export statements^)

    (define (if-s expr statement else?)
      (cond
        [else?
         (choose ((sequence (ifT O_PAREN expr C_PAREN statement elseT statement) id)
                  (sequence (ifT O_PAREN expr C_PAREN statement) id)) "if")]
        [else (sequence (ifT O_PAREN expr C_PAREN statement elseT statement) id "if")]))
    
    (define (return-s expr opt?)
      (cond
        [opt? (choose ((sequence (return expr SEMI_COLON) id "return statement")
                       (sequence (return SEMI_COLON) id "return statement")) "return statement")]
        [else (sequence (return expr SEMI_COLON) id "return statement")]))
    
    (define (this-call expr)
      (choose ((sequence (this O_PAREN C_PAREN SEMI_COLON) id)
               (sequence (this O_PAREN (comma-sep expr "arguments") C_PAREN SEMI_COLON) id)) "this constructor call"))
    
    (define (super-ctor-call expr)
      (choose ((sequence (super O_PAREN C_PAREN SEMI_COLON) id)
               (sequence (super O_PAREN (comma-sep expr "arguments") C_PAREN SEMI_COLON) id)) "super constructor call"))
    
    (define (block statement)
      (sequence (O_BRACE statement C_BRACE) id "block statement"))
    
    (define (expression-stmt expr)
      (sequence (expr SEMI_COLON) id "statement"))
    
    
    (define (while-l expr statement)
      (sequence (while O_PAREN expr C_PAREN statement) id "while loop"))
    
    (define (do-while expr statement)
      (sequence (doT statement while O_PAREN expr C_PAREN SEMI_COLON) id "do loop"))
    
    (define (for-l init i-op? expr t-op? update up-op? statement)
      (let ([full (sequence (for O_PAREN init SEMI_COLON expr SEMI_COLON update C_PAREN statement) id "for loop")]
            [no-init (sequence (for O_PAREN SEMI_COLON expr SEMI_COLON update C_PAREN statement) id "for loop")]
            [no-tst (sequence (for O_PAREN init SEMI_COLON SEMI_COLON update C_PAREN statement) id "for loop")]
            [no-up (sequence (for O_PAREN init SEMI_COLON expr SEMI_COLON C_PAREN statement) id "for loop")]
            [no-it (sequence (for O_PAREN SEMI_COLON SEMI_COLON update C_PAREN statement) id "for loop")]
            [no-iu (sequence (for O_PAREN SEMI_COLON expr SEMI_COLON C_PAREN statement) id "for loop")]
            [no-tu (sequence (for O_PAREN init SEMI_COLON SEMI_COLON C_PAREN statement) id "for loop")]
            [none (sequence (for O_PAREN SEMI_COLON SEMI_COLON C_PAREN statement) id "for loop")])
        (cond
          [(and i-op? t-op? up-op?)
           (choice (list full no-init no-tst no-up no-it no-iu no-tu none) "for loop")]
          [(and t-op? up-op?)
           (choice (list full no-tst no-up no-tu) "for loop")]
          [(and i-op? t-op?)
           (choice (list full no-init no-tst no-it) "for loop")]
          [(and i-op? up-op?)
           (choice (list full no-init no-up no-iu) "for loop")]
          [i-op? (choice (list full no-init) "for loop")]
          [t-op? (choice (list full no-tst) "for loop")]
          [up-op? (choice (list full no-up) "for loop")]
          [else full])))
    
    (define (break-s label)
      (cond
        [label (choose ((sequence (break SEMI_COLON) id)
                        (sequence (break label SEMI_COLON) id)) "break statement")]
        [else (sequence (break SEMI_COLON) id "break statement")]))
    
    (define (cont-s label)
      (cond
        [label (choose ((sequence (continue SEMI_COLON) id)
                        (sequence (continue label SEMI_COLON) id)) "continue statement")]
        [else (sequence (continue SEMI_COLON) id "continue statement")]))
    
    (define init
      (sequence (this PERIOD IDENTIFIER EQUAL IDENTIFIER SEMI_COLON) id "field initialization"))
    
    (define (statement statements)
      (choice statements "statement"))

    )
    
  (define-unit members@
    (import combinator-parser^ general-productions^ id^ java-types^
            java-separators^ java-ids^ java-definition-keywords^)
    (export fields^ methods^ ctors^)

    (define (field mods type expr share-types?)
      (cond 
        [mods (sequence ((repeat mods) (variable-declaration type expr share-types? "field"))
                        id "field definition")]
        [else (variable-declaration type expr share-types? "field")]))  
    
    (define arg (sequence ((value+name-type prim-type IDENTIFIER) IDENTIFIER) id "argument"))
    
    (define args (comma-sep arg "parameter list"))
    
    ;method-signature: {U parser #f} [U parser #f] [U parser #f] bool bool parser -> parser
    (define (method-signature m ret a t? n)
      (let* ([method-parms (if a
                               (choose ((sequence (O_PAREN C_PAREN) id)
                                        (sequence (O_PAREN a C_PAREN) id)) "method parameter list")
                               (sequence (O_PAREN C_PAREN) id "method parameter list"))]
             [full (sequence ((repeat m) ret (^ IDENTIFIER) method-parms throws (comma-sep n "thrown type")) id "method signature")]
             [full-no-t (sequence ((repeat m) ret (^ IDENTIFIER) method-parms) id "method signature")]
             [no-mods-t (sequence (ret (^ IDENTIFIER) method-parms throws (comma-sep n "thrown type")) id "method signature")]
             [no-mods (sequence (ret (^ IDENTIFIER) method-parms) id "method signature")])
         (cond 
           [(and m t?) (choose (full full-no-t) "method signature")]
           [m full-no-t]
           [t? (choose (no-mods-t no-mods) "method signature")]
           [else no-mods])))
    
    (define (method-header method-sig)
      (sequence (method-sig SEMI_COLON) id "method declaration"))
        
    (define (method signature statement)
      (sequence ((^ signature) O_BRACE statement C_BRACE) id "method definition"))
    
    (define (constructor mod body)
      (let ([ctor (choose
                   ((sequence ((^ IDENTIFIER) O_PAREN C_PAREN O_BRACE body C_BRACE) id)
                    (sequence ((^ IDENTIFIER) O_PAREN args C_PAREN O_BRACE body C_BRACE) id))
                   "constructor definition")])
        (cond 
          [mod (sequence ((repeat mod) ctor) id "constructor definition")]
          [else ctor])))
    
    )            
            
  (define-unit interface@
    (import combinator-parser^ id^ java-definition-keywords^ java-ids^ java-separators^)
    (export interfaces^)
  
    (define (interface-body members)
      (repeat (choice members "interface member")))
    
    (define (interface-def modifier extends body)
      (let ([m&e (sequence ((repeat modifier) interface (^ IDENTIFIER) extends O_BRACE body C_BRACE)
                           id "interface definition")]
            [m (sequence ((repeat modifier) interface (^ IDENTIFIER) O_BRACE body C_BRACE) id "interface definition")]
            [e (sequence (interface (^ IDENTIFIER) extends O_BRACE body C_BRACE id) "interface definition")]
            [always (sequence (interface (^ IDENTIFIER) O_BRACE body C_BRACE) id "interface definition")])
        (choice (cond 
                  [(and modifier extends) (list m&e m e always)]
                  [modifier (list m always)]
                  [extends (list e always)]
                  [else (list always)])
                "interface definition")))
    
    )
    
  (define-unit class@
    (import combinator-parser^ id^ java-definition-keywords^ java-ids^ java-separators^)
    (export classes^)
  
    (define (class-body members)
      (choice members "class member"))
    
    (define (implements-dec name)
      (sequence (implements name) id "implementation declaration"))
    
    (define (extend-dec name)
      (sequence (extends name) id "extends declaration"))
    
    (define (class-def mods extends implements body)
      (let ([e&i (sequence (class (^ IDENTIFIER) extends implements O_BRACE body C_BRACE) id "class definition")]
            [e (sequence (class (^ IDENTIFIER) extends O_BRACE body C_BRACE) id "class definition")]
            [i (sequence (class (^ IDENTIFIER) implements O_BRACE body C_BRACE) id "class definition")]
            [base (sequence (class (^ IDENTIFIER) O_BRACE body C_BRACE) id "class definition")])
        (let ([base-choice
               (cond
                 [(and extends implements)
                  (choice (list e&i e i base) "class definition")]
                 [extends (choice (list e base) "class definition")]
                 [implements (choice (list i base) "class definition")]
                 [else base])])
          (cond 
            [mods (choose ((sequence (mods base-choice) id) base-choice) "class definition")]
            [else base-choice]))))
    
  ) 

  (define-unit top-forms@
    (import combinator-parser^ id^ java-definition-keywords^ java-separators^
            general-productions^)
    (export top-forms^)
  
    (define (top-member mems)
      (choice mems "program body"))
    
    (define import-dec
      (choose
       ((sequence (import name PERIOD TIMES SEMI_COLON) id)
        (sequence (import name SEMI_COLON) id)) "import declaration"))
    
    (define (program package import body)
      (let ([p&i (sequence (package import body) id "program")]
            [p (sequence (package body) id "program")]
            [i (sequence (import body) id "program")])
        (cond
          [(and package import)
           (choice (list p&i p i body) "program")]
          [package
           (choice (list p body) "program")]
          [import
           (choice (list i body) "program")]
          [else body])))
    
    ) 
  
  (define-signature language-forms^
    (beginner-program beginner-statement beginner-expression beginner-field
     intermediate-program intermediate+access-program intermediate-statement intermediate-expression
     advanced-program advanced-statement advanced-expression
     ))
  (define-signature token-proc^ (old-tokens->new))
  
  (define-signature parsers^ 
    (parse-beginner parse-intermediate parse-intermediate+access parse-advanced))
  
  (define-unit java-grammars@
    (import combinator-parser^ java-operators^ java-separators^ 
            java-statement-keywords^ java-definition-keywords^ 
            java-type-keywords^ java-ids^
            java-types^ java-access^ java-ops^ general-productions^
            expression-maker^ expr-lits^ expr-terms+^ expr-tails^ statements^
            fields^ methods^ ctors^ interfaces^ classes^ top-forms^ id^)
    (export language-forms^ token-proc^)
    
    ;Remembered Unsupported Features
    ;throws clause
    ;strictfp
    ;allowing static fields in interface
    
    ;Beginner definition
    
    (define beginner-unique-base
      (simple-expression
       (list (literals (list boolean-lits textual-lits prim-numeric-lits double-lits))
             this
             IDENTIFIER
             (new-class IDENTIFIER (eta beginner-expression))
             (simple-method-call (eta beginner-expression))
             (sequence (O_PAREN (eta beginner-expression) C_PAREN) id "expression")
             (sequence (! (eta beginner-expression)) id "conditional expression")
             (sequence (MINUS (eta beginner-expression)) id "negation expression")
             (checks (eta beginner-expression)))))
    
    (define beginner-unique-end
      (simple-expression
       (list field-access-end
             (method-call-end (eta beginner-expression))
             (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops))
                                    (eta beginner-expression)))))
    
    (define beginner-expression
      (sequence (beginner-unique-base (repeat beginner-unique-end)) id "expression"))
    
    (define beginner-statement
      (statement (list (if-s beginner-expression (eta beginner-statement) #f)
                       (return-s beginner-expression #f))))
    
    (define beginner-field (field #f (value+name-type prim-type IDENTIFIER) beginner-expression #f))
    
    (define beginner-method-sig
      (method-signature #f (value+name-type prim-type IDENTIFIER) args #f IDENTIFIER))
    
    (define beginner-method
      (method beginner-method-sig beginner-statement))
    
    (define beginner-constructor (constructor #f (repeat init)))
    
    (define beginner-interface
      (interface-def #f #f (repeat beginner-method-sig)))
    
    (define beginner-class
      (class-def #f #f (implements-dec IDENTIFIER)
                 (repeat (class-body (list beginner-field beginner-method beginner-constructor)))))
    
    (define beginner-program 
      (program #f (repeat import-dec) 
               (repeat (top-member (list beginner-class beginner-interface)))))
    
    
    ;
    ;Intermediate definition
    ;
    
    (define intermediate-unique-base
      (simple-expression 
       (list (literals (list null-lit boolean-lits textual-lits prim-numeric-lits double-lits))
             this
             IDENTIFIER
             (new-class IDENTIFIER (eta intermediate-expression))
             (simple-method-call (eta intermediate-expression))
             (sequence (O_PAREN (eta intermediate-expression) C_PAREN) id "expression")
             (sequence (! (eta intermediate-expression)) id "conditional expression")
             (sequence (MINUS (eta intermediate-expression)) id "negation expression")
             (cast (value+name-type prim-type IDENTIFIER) (eta intermediate-expression))
             (super-call (eta intermediate-expression))
             (checks (eta intermediate-expression)))))
    
    (define intermediate-unique-end
      (simple-expression
       (list field-access-end
             (method-call-end (eta intermediate-expression))
             (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops bit-ops))
                                    (eta intermediate-expression))
             (instanceof-back (value+name-type prim-type IDENTIFIER)))))
    
    (define intermediate-expression
      (sequence (intermediate-unique-base (repeat intermediate-unique-end))
                id "expression"))
    
    (define intermediate-stmt-expr
      (simple-expression (list (new-class IDENTIFIER intermediate-expression)
                               (super-call intermediate-expression)
                               (sequence (intermediate-expression 
                                          (method-call-end intermediate-expression))
                                         id "method call")
                               (assignment 
                                (choose (IDENTIFIER 
                                         (sequence (intermediate-unique-base field-access-end) id))
                                        "assignee")
                                EQUAL intermediate-expression))))
    
    (define intermediate-statement
      (statement (list (if-s intermediate-expression (eta intermediate-statement)  #f)
                       (return-s intermediate-expression #t)
                       (variable-declaration (value+name-type prim-type IDENTIFIER) intermediate-expression #f "local variable")
                       (block (repeat (eta intermediate-statement)))
                       (sequence (intermediate-stmt-expr SEMI_COLON) id "statement"))))
    
    (define intermediate-field (field #f (value+name-type prim-type IDENTIFIER) intermediate-expression #t))
    (define intermediate+access-field (field access-mods (value+name-type prim-type IDENTIFIER) intermediate-expression #t))
    
    (define intermediate-method-sig-no-abs
      (method-signature #f (method-type (value+name-type prim-type IDENTIFIER)) args #f IDENTIFIER))
    (define intermediate-method-sig-abs
      (method-signature abstract (method-type (value+name-type prim-type IDENTIFIER)) args #f IDENTIFIER))
    
    (define intermediate+access-method-sig-no-abs
      (method-signature access-mods (method-type (value+name-type prim-type IDENTIFIER)) args #f IDENTIFIER))
    (define intermediate+access-method-sig-abs
      (method-signature (method-mods access-mods) (method-type (value+name-type prim-type IDENTIFIER)) args #f IDENTIFIER))
    
    (define intermediate-method
      (choose ((method intermediate-method-sig-no-abs intermediate-statement)
               (method-header intermediate-method-sig-abs)) "method definition"))
    
    (define intermediate+access-method
      (choose ((method intermediate+access-method-sig-no-abs intermediate-statement)
               (method-header intermediate+access-method-sig-abs)) "method definition"))

    (define intermediate-constructor
      (constructor #f
                   (choose ((sequence ((super-call intermediate-expression) (repeat intermediate-statement)) id)
                            (sequence ((this-call intermediate-expression) (repeat intermediate-statement)) id)
                            (repeat intermediate-statement)) "constructor body")))
    
    (define intermediate+access-constructor
      (constructor access-mods
                   (choose ((sequence ((super-call intermediate-expression) (repeat intermediate-statement)) id)
                            (sequence ((this-call intermediate-expression) (repeat intermediate-statement)) id)
                            (repeat intermediate-statement)) "constructor body")))
    
    (define intermediate-interface 
      (interface-def
       #f
       (sequence (extends (comma-sep IDENTIFIER "interfaces")) id "extends")
       (repeat intermediate-method-sig-no-abs)))
    
    (define intermediate-class
      (class-def abstract (extend-dec IDENTIFIER) (implements-dec (comma-sep IDENTIFIER "interfaces"))
                 (repeat (class-body (list intermediate-field intermediate-method intermediate-constructor)))))

    (define intermediate+access-class
      (class-def abstract (extend-dec IDENTIFIER) (implements-dec (comma-sep IDENTIFIER "interfaces"))
                 (repeat (class-body (list intermediate+access-field 
                                           intermediate+access-method 
                                           intermediate+access-constructor)))))
    
    (define intermediate-program
      (program #f (repeat import-dec)
               (repeat (top-member (list intermediate-class intermediate-interface)))))
    
    (define intermediate+access-program
      (program #f (repeat import-dec)
               (repeat (top-member (list intermediate+access-class intermediate-interface)))))
    
    
    (define advanced-unique-base
      (simple-expression 
       (list (literals (list null-lit boolean-lits textual-lits prim-numeric-lits double-lits))
             this
             IDENTIFIER
             (new-class IDENTIFIER (eta advanced-expression))
             (simple-method-call (eta advanced-expression))
             (new-array (value+name-type prim-type IDENTIFIER) (eta advanced-expression))
             (sequence (O_PAREN (eta advanced-expression) C_PAREN) id "expression")
             (sequence (! (eta advanced-expression)) id "conditional expression")
             (sequence (MINUS (eta advanced-expression)) id "negation exxpression")
             (cast (value+name-type prim-type IDENTIFIER) (eta advanced-expression))
             (super-call (eta advanced-expression))
             (checks (eta advanced-expression)))))
    
    (define advanced-unique-end
      (simple-expression
       (list field-access-end
             (array-access-end (eta advanced-expression))
             (method-call-end (eta advanced-expression))
             (if-expr-end (eta advanced-expression))
             (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops bit-ops))
                                    (eta advanced-expression))
             (instanceof-back (value+name-type prim-type IDENTIFIER)))))

    
    (define advanced-expression
      (sequence (advanced-unique-base (repeat advanced-unique-end)) id "expression"))
    
    
    (define advanced-stmt-expr
      (simple-expression (list (new-class IDENTIFIER advanced-expression)
                               (super-call advanced-expression)
                               (sequence (advanced-expression
                                          (method-call-end advanced-expression)) id "method call")
                               (assignment 
                                (choose (IDENTIFIER
                                         (sequence (advanced-expression field-access-end) id)
                                         (sequence (advanced-expression array-access-end) id))
                                        "asignee")
                                assignment-ops advanced-expression)
                               (sequence (advanced-expression ++) id "unary mutation")
                               (sequence (advanced-expression --) id "unary mutation")
                               (sequence (++ advanced-expression) id "unary mutation")
                               (sequence (-- advanced-expression) id "unary mutation"))))
    
    (define advanced-statement
      (statement (list (if-s advanced-expression (eta advanced-statement) #t)
                       (return-s advanced-expression #t)
                       (variable-declaration (value+name-type prim-type IDENTIFIER) advanced-expression #t "local variable")
                       (block (repeat (eta advanced-statement)))
                       (sequence (advanced-stmt-expr SEMI_COLON) id "statement")
                       (for-l (choose ((variable-declaration (value+name-type prim-type IDENTIFIER) advanced-expression #t "for loop variable")
                                       (comma-sep advanced-stmt-expr "initializations")) "for loop initialization") 
                              #t
                              advanced-expression #t
                              (comma-sep advanced-stmt-expr "for loop increments") #t (eta advanced-statement))
                       (while-l advanced-expression (eta advanced-statement))
                       (do-while advanced-expression (eta advanced-statement))
                       (break-s #f)
                       (cont-s #f))))
    
    (define advanced-field (field (global-mods access-mods) (value+name-type prim-type IDENTIFIER) advanced-expression #t))
    
    (define advanced-method-sig-no-abs
      (method-signature (global-mods access-mods) 
                        (method-type (value+name-type prim-type IDENTIFIER)) args #f IDENTIFIER))
    (define advanced-method-sig-abs
      (method-signature (method-mods (global-mods access-mods)) 
                        (method-type (value+name-type prim-type IDENTIFIER)) args #f IDENTIFIER))
    
    (define advanced-method
      (choose ((method advanced-method-sig-no-abs advanced-statement)
               (method-header advanced-method-sig-abs)) "method definition"))
    
    (define advanced-constructor
      (constructor access-mods
                   (choose ((sequence ((super-call advanced-expression) (repeat advanced-statement)) id)
                            (sequence ((this-call advanced-expression) (repeat advanced-statement)) id)
                            (repeat advanced-statement)) "constructor body")))
    
    (define advanced-interface 
      (interface-def
       #f
       (sequence (extends (comma-sep IDENTIFIER "interfaces")) id "extends")
       (repeat (choose (advanced-method-sig-no-abs
                        (field (global-mods access-mods) (value+name-type prim-type IDENTIFIER) advanced-expression #t))
                       "interface member definition"))))
    
    (define advanced-class
      (class-def (choose (abstract public) "class modifier")
                 (extend-dec IDENTIFIER) (implements-dec (comma-sep IDENTIFIER "interfaces"))
                 (repeat (class-body (list advanced-field advanced-method advanced-constructor
                                           (method-header advanced-method-sig-abs))))))
    
    (define advanced-program
      (program (sequence (package name SEMI_COLON) id "package specification")
               (repeat import-dec)
               (repeat (top-member (list advanced-class advanced-interface)))))
    
    (define (old-tokens->new tok-list)
      (cond
        [(null? tok-list) null]
        [(eq? (token-name (position-token-token (car tok-list))) 'EOF) null]
        [else
         (cons
          (make-position-token
           (case (token-name (position-token-token (car tok-list)))
             [(=) (token-EQUAL)]
             ((<) (token-LT))
             ((>) (token-GT))
             ((<=) (token-LTEQ))
             ((>=) (token-GTEQ))
             ((+) (token-PLUS))
             ((-) (token-MINUS))
             ((*) (token-TIMES))
             ((/) (token-DIVIDE))
             ((^) (token-^T))
             ((if) (token-ifT))
             ((do) (token-doT))
             ((case) (token-caseT))
             ((else) (token-elseT))
             ((void) (token-voidT))
             (else (position-token-token (car tok-list))))
           (position-token-start-pos (car tok-list))
           (position-token-end-pos (car tok-list))) 
          (old-tokens->new (cdr tok-list)))]))  
    
    )
  
  
  (define-unit full-program-parsers@
    (import language-forms^ combinator-parser^)
    (export parsers^)
    
    (define parse-beginner (parser beginner-program))
    (define parse-intermediate (parser intermediate-program))
    (define parse-intermediate+access (parser intermediate+access-program))
    (define parse-advanced (parser advanced-program))
    
    )
  
  (define-unit interaction-parsers@
    (import language-forms^ combinator-parser^)
    (export parsers^)
    
    (define parse-beginner (parser (choose (beginner-expression beginner-statement beginner-field) 
                                           "interactions program")))
  
    (define parse-intermediate (parser (choose (intermediate-expression intermediate-statement) 
                                               "interactions program")))
    (define parse-intermediate+access parse-intermediate)
  
    (define parse-advanced
      (parser (choose (advanced-expression advanced-statement) "interactions program")))
    )

  (define-unit file-constants@
    (import)
    (export error-format-parameters^)
    (define src? #t)
    (define input-type "file")
    (define show-options #f)
    (define max-depth 2)
    (define max-choice-depth 3))
  
  (define-unit de-constants@
    (import)
    (export error-format-parameters^)
    (define src? #t)
    (define input-type "definitions window")
    (define show-options #f)
    (define max-depth 1)
    (define max-choice-depth 3))
  
  (define-unit interact-constants@
    (import)
    (export error-format-parameters^)
    (define src? #t)
    (define input-type "interactions-window")
    (define show-options #f)
    (define max-depth 0)
    (define max-choice-depth 3))
    
  (define-unit id@
    (import)
    (export id^)
    (define (id x . args) x)) 
  
  (define-compound-unit/infer java-file-parsers@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ file-constants@ id@
          java-terminals@ types@ mods@ operators@ general@
          expressions@ statements@ members@ interface@ class@ top-forms@
          java-grammars@ full-program-parsers@))
  
  (define-compound-unit/infer java-definitions-parsers@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ de-constants@ id@
          java-terminals@ types@ mods@ operators@ general@
          expressions@ statements@ members@ interface@ class@ top-forms@
          java-grammars@ full-program-parsers@))
  
  (define-compound-unit/infer java-interactions-parsers@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ interact-constants@ id@
          java-terminals@ types@ mods@ operators@ general@
          expressions@ statements@ members@ interface@ class@ top-forms@
          java-grammars@ interaction-parsers@))
  
  (provide java-definitions-parsers@ java-interactions-parsers@ parsers^ token-proc^)
  
  )

