(module parser-units (lib "lazy.ss" "lazy")
  
  (require (lib "unit.ss"))
  
  (require (lib "lex.ss" "parser-tools")
           (lib "combinator-unit.ss" "combinator-parser")
           "java-signatures.scm"
           (lib "string.ss"))

    
  (define-signature language-forms^ (program statement expression field interact)) ;value-type method-type))
  
  (define-signature token-proc^ (old-tokens->new))
  
  (define-unit java-dictionary@
    (import)
    (export language-dictionary^ 
            (rename language-format-parameters^
                    (output-map input->output-name)))
    
    (define class-type "keyword")
    
    (define (output-map x)
      #;(!!! (printf "in output-map ~a~n" x))
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
      ;(printf "close-to-keyword ~a ~a~n" t arg)
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
                              (else "lse" "ese" "els" "eles" "elseif")
                              (return "eturn" "rturn" "reurn" "retrn" "retun" "retur" "reutrn" "retrun" "returns" "raturn")
                              (true "rue" "tue" "tre" "tru" "ture" "treu")
                              (false "flse" "fase" "fale" "fals" "flase" "fasle")
                              (interface
                                  "nterface" "iterface" "inerface" "intrface" "inteface" "interace" "interfce" "interfae" "intreface")
                              (implements 
                               "mplements" "iplements" "impements" "implments" "impleents" "implemnts" "implemets" "implemens"
                               "implement")
                              (void "oid" "vid" "voi" "viod")
                              (for "fo" "fore" "fro")
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
    (import combinator-parser^ java-type-keywords^ java-variables^ java-separators^ id^)
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
    
    (define (value+name-type base-type)
      (choose (base-type name) "type"))
    
    (define (method-type base-t)
      (choice (list base-t voidT) "method return"))
    
    (define (array-type base-t)
      (sequence (base-t (repeat (sequence (O_BRACKET C_BRACKET) id "array type"))) id "type"))
    
    )    

  (define-unit mods@
    (import combinator-parser^ java-definition-keywords^)
    (export java-access^)
  
    (define access-mods
      (choose (public private protected) "access modifier"))
    
    (define (global-mods base-mods)
      (choose (base-mods static) "modifier"))
    
    (define (method-mods base-mods)
      (choose (base-mods abstract) "modifier"))
 
    )
    
  (define-unit operators@
    (import combinator-parser^ java-operators^ java-separators^)
    (export java-ops^)
  
    (define math-ops
      (choose (PLUS MINUS TIMES DIVIDE %) "binary operater"))
    
    (define shift-ops
      (choose (<< >> >>>) "shift operater"))
    
    (define compare-ops
      (choose (== GT LT LTEQ GTEQ !=) "binary operater"))
    
    (define bool-ops
      (choose (&& OR) "binary operater"))
    
    (define bit-ops 
      (choose (^T PIPE &) "binary operater"))
    
    (define assignment-ops
      (choose (EQUAL OREQUAL += -= *= /= &= ^= %= <<= >>= >>>=) "assignment"))
    
    (define (bin-ops ops)
      (choice ops "binary operater"))
    
    (define un-assignment
      (choose (++ --) "unary operater"))
    
    (define un-op
      (choose (~ PLUS MINUS) "unary operater"))
    
    )

  (define-unit general@
    (import combinator-parser^ java-separators^ java-operators^ java-ids^ id^)
    (export general-productions^)
  
    (define (comma-sep term name)
      (sequence (term (repeat (sequence (COMMA term) id))) id (string-append "a list of " name)))
    
    (define name
      (sequence (IDENTIFIER (repeat (sequence (PERIOD IDENTIFIER) id))) id "name"))
    
    )
  
  (define-unit unqualified-java-variables@
    (import combinator-parser^ general-productions^ java-separators^ java-operators^ java-ids^ id^)
    (export java-variables^)
    
    (define name IDENTIFIER)
    (define identifier IDENTIFIER)
    
    (define (variable-declaration type expr share-type? end? name)
      (let* ([var-name (string-append name " declaration")]
             [init (sequence ((^ identifier) EQUAL expr) id var-name)]
             [f (choose (identifier init) var-name)]
             [s&e (sequence (type (comma-sep f name)) id var-name)]
             [s (sequence (type (comma-sep identifier name)) id var-name)]
             [e (sequence (type init) id var-name)]
             [base (sequence (type (^ identifier)) id var-name)]
             [decl
              (cond
                [(and expr share-type?) (choose (s&e e base) var-name)]
                [share-type? s]
                [expr (choose (e base) var-name)]
                [else base])])
        (cond
          [end? (sequence (decl SEMI_COLON) id (string-append name " definition"))]
          [else decl])))
    )
  
  (define-unit expressions@ 
    (import combinator-parser^ general-productions^ id^
            java-literals^ java-expression-keywords^ java-vals^ java-ids^ 
            java-variables^ java-separators^
            java-operators^ java-extras^ language-forms^)
    (export expr-lits^ expr-terms+^ expr-tails^)
            
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
    
    (define new-class
      (choose ((sequence (new name O_PAREN C_PAREN) id)
               (sequence (new name O_PAREN (comma-sep expression "arguments") C_PAREN) id))
              "class instantiation"))
    
    (define (new-array type-name)
      (sequence (new type-name O_BRACKET expression C_BRACKET (repeat (sequence (O_BRACKET expression C_BRACKET) id)))
                id "array instantiation"))
    
    (define field-access-end
      (sequence (PERIOD identifier) id "field access"))
    
    (define array-access-end
      (sequence (O_BRACKET (eta expression) C_BRACKET) id "array access"))
    
    (define (array-init-maker contents)
      (sequence (O_BRACE (comma-sep contents "array elements") C_BRACE) id "array initializations"))
    
    (define array-init 
      (letrec ([base-init (array-init-maker (eta expression))]
               [simple-init (array-init-maker (choose (expression base-init (eta init)) "array initializations"))]
               [init (array-init-maker (choose (expression simple-init) "array initialization"))])
        init #;(sequence (new type-name init) "array initialization")))
    
    (define (binary-expression-end op)
      (sequence (op expression) id "binary expression"))
    
    (define if-expr-end 
      (sequence (? (eta expression) : (eta expression)) id "conditional expression"))
    
    (define simple-method-call
      (choose
       ((sequence ((^ identifier) O_PAREN C_PAREN) id)
        (sequence ((^ identifier) O_PAREN (comma-sep expression "arguments") C_PAREN) id))
       "method invocation"))
    
    (define method-call-end
      (choose
       ((sequence (PERIOD (^ identifier) O_PAREN C_PAREN) id)
        (sequence (PERIOD (^ identifier) O_PAREN (comma-sep expression "arguments") C_PAREN) id))
       "method invocation"))
    
    (define (assignment asignee op)
      (sequence ((^ asignee) op expression) id "assignment"))
    
    (define unary-assignment-front
      (choose ((sequence (++ expression) id)
               (sequence (-- expression) id)) "unary modification"))
    
    (define (unary-assignment-back base)
      (choose ((sequence (base ++) id)
               (sequence (base --) id)) "unary modification"))
    
    (define (cast type)
      (sequence (O_PAREN type C_PAREN expression) id "cast expression"))
    
    (define instanceof-back
      (sequence (instanceof name) id "instanceof expression"))
    
    (define super-ctor
      (choose ((sequence (super O_PAREN C_PAREN) id)
               (sequence (super O_PAREN (comma-sep expression "arguments") C_PAREN) id))
              "super constructor call"))
    
    (define super-call
      (choose ((sequence (super PERIOD identifier O_PAREN C_PAREN) id)
               (sequence (super PERIOD identifier O_PAREN (comma-sep expression "arguments") C_PAREN) id))
              "super method invocation"))
    
    (define checks
      (choose
       ((sequence (check (eta expression) expect (eta expression) within (eta expression)) id)
        (sequence (check (eta expression) expect (eta expression)) id))
       "check expression"))
    
    )
    
  (define-unit statements@
    (import combinator-parser^ general-productions^ id^ language-forms^
            java-statement-keywords^ java-separators^ java-ids^ java-operators^)
    (export statements^)

    (define (if-s stmt else?)
      (cond
        [else?
         (choose ((sequence (ifT O_PAREN expression C_PAREN stmt elseT stmt) id)
                  (sequence (ifT O_PAREN expression C_PAREN stmt) id)) "if statement")]
        [else (sequence (ifT O_PAREN expression C_PAREN stmt elseT stmt) id "if statement")]))
    
    (define (return-s opt?)
      (cond
        [opt? (choose ((sequence (return expression SEMI_COLON) id)
                       (sequence (return SEMI_COLON) id)) "return statement")]
        [else (sequence (return expression SEMI_COLON) id "return statement")]))
    
    (define this-call
      (choose ((sequence (this O_PAREN C_PAREN SEMI_COLON) id)
               (sequence (this O_PAREN (comma-sep expression "arguments") C_PAREN SEMI_COLON) id)) "this constructor call"))
    
    (define super-ctor-call
      (choose ((sequence (super O_PAREN C_PAREN SEMI_COLON) id)
               (sequence (super O_PAREN (comma-sep expression "arguments") C_PAREN SEMI_COLON) id)) "super constructor call"))
    
    (define (block repeat?)
      (let ([body (if repeat? (repeat-greedy statement) statement)])
        (sequence (O_BRACE body C_BRACE) id "block statement")))
    
    (define expression-stmt
      (sequence (expression SEMI_COLON) id "statement"))
    
    (define (while-l stmt)
      (sequence (while O_PAREN expression C_PAREN stmt) id "while loop"))
    
    (define (do-while stmt)
      (sequence (doT stmt while O_PAREN expression C_PAREN SEMI_COLON) id "do loop"))
    
    (define (for-l init i-op? t-op? update up-op? statement)
      (let ([full (sequence (for O_PAREN init SEMI_COLON expression SEMI_COLON update C_PAREN statement) id "for loop")]
            [no-init (sequence (for O_PAREN SEMI_COLON expression SEMI_COLON update C_PAREN statement) id "for loop")]
            [no-tst (sequence (for O_PAREN init SEMI_COLON SEMI_COLON update C_PAREN statement) id "for loop")]
            [no-up (sequence (for O_PAREN init SEMI_COLON expression SEMI_COLON C_PAREN statement) id "for loop")]
            [no-it (sequence (for O_PAREN SEMI_COLON SEMI_COLON update C_PAREN statement) id "for loop")]
            [no-iu (sequence (for O_PAREN SEMI_COLON expression SEMI_COLON C_PAREN statement) id "for loop")]
            [no-tu (sequence (for O_PAREN init SEMI_COLON SEMI_COLON C_PAREN statement) id "for loop")]
            [none (sequence (for O_PAREN SEMI_COLON SEMI_COLON C_PAREN statement) id "for loop")])
        (cond
          [(and i-op? t-op? up-op?)
           (choose (full no-init no-tst no-up no-it no-iu no-tu none) "for loop")]
          [(and t-op? up-op?)
           (choose (full no-tst no-up no-tu) "for loop")]
          [(and i-op? t-op?)
           (choose (full no-init no-tst no-it) "for loop")]
          [(and i-op? up-op?)
           (choose (full no-init no-up no-iu) "for loop")]
          [i-op? (choose (full no-init) "for loop")]
          [t-op? (choose (full no-tst) "for loop")]
          [up-op? (choose (full no-up) "for loop")]
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
    
    (define (make-statement statements)
      (choice statements "statement"))

    )
    
  (define-unit members@
    (import combinator-parser^ general-productions^ id^ java-types^
            java-separators^ java-ids^ java-definition-keywords^ java-variables^)
    (export fields^ methods^ ctors^)

    (define (make-field mods type expr share-types?)
      (cond 
        [mods (sequence ((repeat-greedy mods) (variable-declaration type expr share-types? #t "field"))
                        id "field definition")]
        [else (variable-declaration type expr share-types? #t "field")]))  
    
    (define (arg type)
      (sequence (type identifier) id "argument"))
    
    (define (args type) (comma-sep (arg type) "parameters"))
    
    ;method-signature: {U parser #f} [U parser #f] [U parser #f] bool bool parser -> parser
    (define (method-signature m ret a t? n)
      (let* ([method-parms (if a
                               (choose ((sequence (O_PAREN C_PAREN) id)
                                        (sequence (O_PAREN a C_PAREN) id)) "method parameter list")
                               (sequence (O_PAREN C_PAREN) id "method parameter list"))]
             [full (sequence ((repeat m) ret (^ identifier) method-parms throws (comma-sep n "thrown types")) id "method signature")]
             [full-no-t (sequence ((repeat m) ret (^ identifier) method-parms) id "method signature")]
             [no-mods-t (sequence (ret (^ identifier) method-parms throws (comma-sep n "thrown types")) id "method signature")]
             [no-mods (sequence (ret (^ identifier) method-parms) id "method signature")])
         (cond 
           [(and m t?) (choose (full full-no-t) "method signature")]
           [m full-no-t]
           [t? (choose (no-mods-t no-mods) "method signature")]
           [else no-mods])))
    
    (define (method-header method-sig)
      (sequence (method-sig SEMI_COLON) id "method declaration"))
        
    (define (make-method signature statement)
      (sequence ((^ signature) O_BRACE statement C_BRACE) id "method definition"))
    
    (define (make-constructor mod body type)
      (let ([ctor (choose
                   ((sequence ((^ identifier) O_PAREN C_PAREN O_BRACE body C_BRACE) id)
                    (sequence ((^ identifier) O_PAREN (args type) C_PAREN O_BRACE body C_BRACE) id))
                   "constructor definition")])
        (cond 
          [mod (sequence ((repeat mod) ctor) id "constructor definition")]
          [else ctor])))
    
    )
            
  (define-unit interface@
    (import combinator-parser^ id^ java-definition-keywords^ java-ids^ java-separators^)
    (export interfaces^)
  
    (define (interface-body members)
      (repeat-greedy (choice members "interface member")))
    
    (define (interface-def modifier extends body)
      (let ([m&e (sequence ((repeat modifier) interface (^ IDENTIFIER) extends O_BRACE body C_BRACE)
                           id "interface definition")]
            [m (sequence ((repeat modifier) interface (^ IDENTIFIER) O_BRACE body C_BRACE) id "interface definition")]
            [e (sequence (interface (^ IDENTIFIER) extends O_BRACE body C_BRACE) id "interface definition")]
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
            java-variables^ general-productions^)
    (export top-forms^)
  
    (define (top-member mems)
      (choice mems "class or interface"))
    
    ;Note -- should enfore name to be identifier.identifier instead of name
    (define import-dec
      (let ([name (sequence (identifier (repeat-greedy (sequence (PERIOD identifier) id "import name")))
                            id "import name")])
      (choose
       ((sequence (import name PERIOD TIMES SEMI_COLON) id)
        (sequence (import name SEMI_COLON) id)) "import declaration")))
    
    (define (make-program package import body)
      (let ([p&i (sequence (package import body) id "package program")]
            [p (sequence (package body) id "package program")]
            [i (sequence (import body) id "program")])
        (cond
          [(and package import)
           (choice (list p&i i ) "program")]
          [package
           (choice (list p body) "program")]
          [import
           (choice (list i body) "program")]
          [else body])))
    
    ) 
  
  ;Remembered Unsupported Features
  ;throws clause
  ;strictfp
  ;allowing static fields in interface
  
  (define-unit beginner-grammar@
    (import combinator-parser^ java-operators^ java-separators^ 
            java-statement-keywords^ java-type-keywords^ java-ids^
            java-types^ java-access^ java-ops^ general-productions^ java-variables^
            expr-lits^ expr-terms+^ expr-tails^ statements^
            fields^ methods^ ctors^ interfaces^ classes^ top-forms^ id^)
    (export language-forms^)
    
    (define unique-base
      (choose
       ((literals (list boolean-lits textual-lits prim-numeric-lits double-lits))
        this
        identifier
        new-class
        simple-method-call
        (sequence (O_PAREN (eta expression) C_PAREN) id "parened expression")
        (sequence (! (eta expression)) id "conditional expression")
        (sequence (MINUS (eta expression)) id "negation expression")
        checks) "expression"))
    
    (define unique-end 
      (choose (field-access-end
               method-call-end
               (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops))))
              "expression"))
    
    (define expression
      (sequence (unique-base (repeat-greedy unique-end)) id "expression"))
    
    (define statement
      (choose ((return-s #f) (if-s (block #f) #f)) "statement"))
    
    (define field (make-field #f (value+name-type prim-type) expression #f))
    
    (define method-sig
      (method-signature #f (value+name-type prim-type) (args (value+name-type prim-type)) #f identifier))
    
    (define method (make-method method-sig statement))
    
    (define constructor (make-constructor #f (repeat-greedy init) (value+name-type prim-type)))
    
    (define interface (interface-def #f #f 
                                     (repeat-greedy 
                                      (sequence (method-sig SEMI_COLON) id "method signature"))))
    
    (define class
      (class-def #f #f (implements-dec identifier)
                 (repeat-greedy (class-body (list field method constructor)))))
    
    (define program 
      (make-program #f (repeat-greedy import-dec) 
                    (repeat-greedy (top-member (list class interface)))))
    
    (define interact
      (choose (field statement expression) "interactive program"))
    )

  (define-unit intermediate-grammar@
    (import combinator-parser^ java-operators^ java-separators^ (prefix tok: java-definition-keywords^)
            java-statement-keywords^ java-type-keywords^ java-ids^
            java-types^ java-access^ java-ops^ general-productions^ java-variables^
            expr-lits^ expr-terms+^ expr-tails^ statements^
            fields^ methods^ ctors^ interfaces^ classes^ top-forms^ id^)
    (export language-forms^)
    
    (define unique-base
      (choose ((literals (list null-lit boolean-lits textual-lits prim-numeric-lits double-lits))
               this
               identifier
               new-class
               simple-method-call
               (sequence (O_PAREN (eta expression) C_PAREN) id "parened expression")
               (sequence (! (eta expression)) id "conditional expression")
               (sequence (MINUS (eta expression)) id "negation expression")
               (cast (value+name-type prim-type))
               super-call
               checks) "expression"))
    
    (define unique-end
      (choose (field-access-end
               method-call-end
               (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops bit-ops)))
               instanceof-back) "expression"))
    
    (define expression
      (sequence (unique-base (repeat-greedy unique-end)) id "expression"))
    
    (define stmt-expr
      (choose (#;new-class
               super-call
               simple-method-call
               (sequence (unique-base (repeat unique-end) method-call-end) id "method call")
               (assignment 
                (choose (identifier
                         (sequence (unique-base (repeat unique-end) field-access-end) id))
                        "assignee")
                EQUAL)) "expression"))
    
    (define (statement-c interact?)
      (if interact?
          (choose ((return-s #t)
                   (if-s (block #t) #f)
                   (assignment 
                    (choose (identifier
                             (sequence (unique-base (repeat unique-end) field-access-end) id))
                            "assignee") EQUAL)
                    (block #t)) "statement")
          (choose ((return-s #t)
                   (if-s (block #t) #f)
                   (block #t)
                   (variable-declaration (value+name-type prim-type) expression #f #t "local variable")                   
                   (sequence (stmt-expr SEMI_COLON) id)) "statement")))
    
    (define statement (statement-c #f))
    
    (define field (make-field #f (value+name-type prim-type) expression #t))
    
    (define method-sig-no-abs
      (method-signature #f (method-type (value+name-type prim-type)) 
                        (args (value+name-type prim-type)) #f identifier))
    (define method-sig-abs
      (method-signature tok:abstract (method-type (value+name-type prim-type)) 
                        (args (value+name-type prim-type)) #f identifier))
    
    (define method
      (choose ((make-method method-sig-no-abs (repeat-greedy statement))
               (method-header method-sig-abs)) "method definition"))
    
    (define constructor
      (make-constructor #f
                        (choose ((sequence (super-ctor-call (repeat-greedy statement)) id)
                                 (repeat-greedy statement)) "constructor body")
                        (value+name-type prim-type)))
    
    (define interface 
      (interface-def
       #f
       (sequence (tok:extends (comma-sep identifier "interfaces")) id "extends")
       (repeat-greedy (sequence (method-sig-no-abs SEMI_COLON) id "method signature"))))
    
    (define class
      (class-def tok:abstract (extend-dec identifier) 
                 (implements-dec (comma-sep identifier "interfaces"))
                 (repeat-greedy (class-body (list field method constructor)))))

    
    (define program
      (make-program #f (repeat-greedy import-dec) 
                    (repeat-greedy (choose (class interface) "class or interface"))))
    
    (define interact
      (choose (field 
               (return-s #t)
               (if-s (block #t) #f)
               (assignment 
                (choose (identifier
                         (sequence (unique-base (repeat unique-end) field-access-end) id))
                        "assignee") EQUAL)
               (block #t)
               expression) "interactive program"))
    
    )
    
  (define-unit intermediate+access-grammar@
    (import combinator-parser^ java-operators^ java-separators^ (prefix tok: java-definition-keywords^)
            java-statement-keywords^ java-type-keywords^ java-ids^
            java-types^ java-access^ java-ops^ general-productions^ java-variables^
            expr-lits^ expr-terms+^ expr-tails^ statements^
            fields^ methods^ ctors^ interfaces^ classes^ top-forms^ id^)
    (export language-forms^)
    
    (define unique-base
      (choose ((literals (list null-lit boolean-lits textual-lits prim-numeric-lits double-lits))
               this
               identifier
               new-class
               simple-method-call
               (sequence (O_PAREN (eta expression) C_PAREN) id)
               (sequence (! (eta expression)) id "conditional expression")
               (sequence (MINUS (eta expression)) id "negation expression")
               (cast (value+name-type prim-type))
               super-call
               checks) "expression"))
    
    (define unique-end
      (choose (field-access-end
               method-call-end
               (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops bit-ops)))
               instanceof-back) "expression"))
    
    (define expression
      (sequence (unique-base (repeat-greedy unique-end)) id "expression"))
    
    (define stmt-expr
      (choose (#;new-class
               super-call
               simple-method-call
               (sequence (unique-base (repeat unique-end) method-call-end) id "method call")
               (assignment 
                (choose (identifier
                         (sequence (unique-base (repeat unique-end) field-access-end) id))
                        "assignee")
                EQUAL)) "expression"))
    
    (define (statement-c interact?)
      (if (not interact?)
          (choose ((return-s #t)
                   (if-s statement #f)
                   (variable-declaration (value+name-type prim-type) expression #f #t "local variable")
                   (block #t)
                   (assignment 
                    (choose (identifier
                             (sequence (unique-base (repeat unique-end) field-access-end) id))
                            "assignee")
                    EQUAL)
                   (sequence (stmt-expr SEMI_COLON) id)) "statement")
          (choose ((return-s #t)
                   (if-s statement #f)
                   (block #t)) "statement")))
    
    (define statement (statement-c #f))
    
    (define field (make-field access-mods (value+name-type prim-type) expression #t))
        
    (define method-sig-no-abs
      (method-signature access-mods (method-type (value+name-type prim-type)) 
                        (args (value+name-type prim-type)) #f identifier))
    (define method-sig-abs
      (method-signature (method-mods access-mods) 
                        (method-type (value+name-type prim-type)) 
                        (args (value+name-type prim-type)) #f identifier))
        
    (define method
      (choose ((make-method method-sig-no-abs (repeat-greedy statement))
               (method-header method-sig-abs)) "method definition"))

    (define constructor
      (make-constructor access-mods
                        (choose ((sequence (super-ctor-call (repeat-greedy statement)) id)
                                 (sequence (this-call (repeat-greedy statement)) id)
                                 (repeat-greedy statement)) "constructor body")
                        (value+name-type prim-type)))
    
    (define interface 
      (interface-def
       #f
       (sequence (tok:extends (comma-sep identifier "interfaces")) id "extends")
       (repeat-greedy (sequence (method-sig-no-abs SEMI_COLON) id "method signature"))))
    
    (define class
      (class-def tok:abstract (extend-dec identifier) (implements-dec (comma-sep identifier "interfaces"))
                 (repeat-greedy (class-body (list field method constructor)))))
    
    (define program
      (make-program #f (repeat-greedy import-dec)
               (repeat-greedy (top-member (list class interface)))))
    
    (define interact (choose (field expression (statement-c #t)) "interactive program"))

    )
  
  (define-unit advanced-grammar@
    (import combinator-parser^ java-operators^ java-separators^ (prefix tok: java-definition-keywords^)
            java-statement-keywords^ java-type-keywords^ java-ids^
            java-types^ java-access^ java-ops^ general-productions^ java-variables^
            expr-lits^ expr-terms+^ expr-tails^ statements^
            fields^ methods^ ctors^ interfaces^ classes^ top-forms^ id^)
    (export language-forms^)

        
    (define unique-base
      (choose 
       ((literals (list null-lit boolean-lits textual-lits prim-numeric-lits double-lits))
        this
        IDENTIFIER
        new-class
        simple-method-call
        (new-array (value+name-type prim-type))
        (sequence (O_PAREN (eta expression) C_PAREN) id)
        (sequence (! (eta expression)) id "conditional expression")
        (sequence (MINUS (eta expression)) id "negation exxpression")
        (cast (value+name-type prim-type))
        super-call
        checks) "expression"))
    
    (define unique-end
      (choose (field-access-end
               array-access-end
               method-call-end
               if-expr-end
               (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops bit-ops)))
               instanceof-back)
              "expression"))
    
    (define expression
      (sequence (unique-base (repeat-greedy unique-end)) id "expression"))
    
    (define stmt-expr
      (choose (new-class
               super-call
               simple-method-call
               (sequence (unique-base (repeat unique-end) method-call-end) id "method call")
               (assignment 
                (choose (identifier
                         (sequence (unique-base (repeat unique-end) field-access-end) id)
                         (sequence (unique-base (repeat unique-end) array-access-end) id))
                        "asignee")
                assignment-ops)
               (sequence (expression ++) id "unary mutation")
               (sequence (expression --) id "unary mutation")
               (sequence (++ expression) id "unary mutation")
               (sequence (-- expression) id "unary mutation")) "expression"))
    
    (define (statement-c interact?)
      (if interact?
          (choose ((return-s #t)
                   (if-s statement #t)
                   (block #t)
                   (for-l (choose ((variable-declaration (array-type (value+name-type prim-type)) expression #t #f "for loop variable")
                                   (comma-sep stmt-expr "initializations")) "for loop initialization") 
                          #t #f
                          (comma-sep stmt-expr "for loop increments") #t (block #t))
                   (while-l (block #t))
                   (do-while (block #t))
                   (break-s #f)
                   (cont-s #f)
                   (assignment 
                    (choose (identifier
                             (sequence (unique-base (repeat unique-end) field-access-end) id)
                             (sequence (unique-base (repeat unique-end) array-access-end) id))
                            "asignee")
                    assignment-ops)
                   ) "statement")
          (choose ((return-s #t)
                   (if-s statement #t)
                   (variable-declaration (array-type (value+name-type prim-type)) 
                                         (choose (expression array-init) "variable initialization") #t #t "local variable")
                   (block #t)
                   (sequence (stmt-expr SEMI_COLON) id)
                   (for-l (choose ((variable-declaration (array-type (value+name-type prim-type)) expression #t #f "for loop variable")
                                   (comma-sep stmt-expr "initializations")) "for loop initialization") 
                          #t #f
                          (comma-sep stmt-expr "for loop increments") #t (block #t))
                   (while-l (block #t))
                   (do-while (block #t))
                   (break-s #f)
                   (cont-s #f)) "statement")))
    
    (define statement (statement-c #f))
    
    (define field (make-field (global-mods access-mods) 
                              (array-type (value+name-type prim-type)) 
                              (choose (expression array-init) "field initializer") #t))
    
    (define method-sig-no-abs
      (method-signature (global-mods access-mods) 
                        (method-type (array-type (value+name-type prim-type))) 
                        (args (array-type (value+name-type prim-type))) #f IDENTIFIER))
    (define method-sig-abs
      (method-signature (method-mods access-mods)
                        (method-type (array-type (value+name-type prim-type))) 
                        (args (array-type (value+name-type prim-type))) #f IDENTIFIER))
    
    (define method
      (choose ((make-method method-sig-no-abs (repeat-greedy statement))
               (method-header method-sig-abs)) "method definition"))
    
    (define constructor
      (make-constructor access-mods
                   (choose ((sequence (super-ctor-call (repeat-greedy statement)) id)
                            (sequence (this-call (repeat-greedy statement)) id)
                            (repeat-greedy statement)) "constructor body")
                   (array-type (value+name-type prim-type))))
    
    (define interface 
      (interface-def
       #f
       (sequence (tok:extends (comma-sep IDENTIFIER "interfaces")) id "extends")
       (repeat-greedy (choose ((sequence (method-sig-no-abs SEMI_COLON) id "method header")
                         (make-field (global-mods access-mods) 
                                     (array-type (value+name-type prim-type)) expression #t))
                       "interface member definition"))))
    
    (define class
      (class-def (choose (tok:abstract tok:public) "class modifier")
                 (extend-dec IDENTIFIER) (implements-dec (comma-sep IDENTIFIER "interfaces"))
                 (repeat-greedy (class-body (list field method constructor
                                           (method-header method-sig-abs))))))
    
    (define program
      (make-program (sequence (tok:package name SEMI_COLON) id "package specification")
                    (repeat-greedy import-dec)
                    (repeat-greedy (top-member (list class interface)))))
    
    (define interact 
      (choose (field expression (statement-c #t)) "interactive program"))
    
    )
  
  (define-unit token@
    (import java-operators^ java-separators^ java-definition-keywords^
            java-statement-keywords^ java-type-keywords^ java-ids^)
    (export token-proc^)
    
    (define (old-tokens->new tok-list)
      #;(!!! (printf "old-tokens->new ~a~n" (map position-token-token tok-list)))
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

  (define-signature parsers^ (parse-program))
  
  (define-unit definition-parsers@
    (import language-forms^ combinator-parser^)
    (export parsers^)
    (define parse-program (parser program)))
  
  (define-unit interactions-parsers@
    (import language-forms^ combinator-parser^)
    (export parsers^)
    (define parse-program (parser interact)))

  
;  (define-unit full-program-parsers@
;    (import language-forms^ combinator-parser^)
;    (export parsers^)
;    
;    (define parse-beginner (parser beginner-program))
;    (define parse-intermediate (parser intermediate-program))
;    (define parse-intermediate+access (parser intermediate+access-program))
;    (define parse-advanced (parser advanced-program))
;    
;    )
;  
;  (define-unit interaction-parsers@
;    (import language-forms^ combinator-parser^)
;    (export parsers^)
;    
;    (define parse-beginner (parser (choose (beginner-expression beginner-statement beginner-field) 
;                                           "interactions program")))
;  
;    (define parse-intermediate (parser (choose (intermediate-expression intermediate-statement) 
;                                               "interactions program")))
;    (define parse-intermediate+access parse-intermediate)
;  
;    (define parse-advanced
;      (parser (choose (advanced-expression advanced-statement) "interactions program")))
;    )

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
    (define input-type "Definitions")
    (define show-options #f)
    (define max-depth 1)
    (define max-choice-depth 3))
  
  (define-unit interact-constants@
    (import)
    (export error-format-parameters^)
    (define src? #t)
    (define input-type "Interactions")
    (define show-options #f)
    (define max-depth 0)
    (define max-choice-depth 3))
    
  (define-unit id@
    (import)
    (export id^)
    (define (id x . args) x)) 
  
  (define-compound-unit/infer beginner-file-parser@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ file-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          beginner-grammar@ token@ definition-parsers@))
  
  
  (define-compound-unit/infer beginner-definitions-parser@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ de-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          beginner-grammar@ token@ definition-parsers@))
  
  (define-compound-unit/infer beginner-interactions-parsers@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ interact-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          beginner-grammar@ token@ interactions-parsers@))
  
  (define-compound-unit/infer intermediate-file-parser@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ file-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          intermediate-grammar@ token@ definition-parsers@))
  
  
  (define-compound-unit/infer intermediate-definitions-parser@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ de-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          intermediate-grammar@ token@ definition-parsers@))
  
  (define-compound-unit/infer intermediate-interactions-parsers@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ interact-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          intermediate-grammar@ token@ interactions-parsers@))
  
  (define-compound-unit/infer intermediate+access-file-parser@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ file-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          intermediate+access-grammar@ token@ definition-parsers@))
  
  
  (define-compound-unit/infer intermediate+access-definitions-parser@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ de-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          intermediate+access-grammar@ token@ definition-parsers@))
  
  (define-compound-unit/infer intermediate+access-interactions-parsers@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ interact-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          intermediate+access-grammar@ token@ interactions-parsers@))

  (define-compound-unit/infer advanced-file-parser@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ file-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          advanced-grammar@ token@ definition-parsers@))
  
  
  (define-compound-unit/infer advanced-definitions-parser@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ de-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          advanced-grammar@ token@ definition-parsers@))
  
  (define-compound-unit/infer advanced-interactions-parsers@
    (import)
    (export parsers^ token-proc^ err^)
    (link java-dictionary@ combinator-parser-tools@ interact-constants@ id@
          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
          expressions@ statements@ members@ interface@ class@ top-forms@
          advanced-grammar@ token@ interactions-parsers@))
  
;  
  (provide advanced-file-parser@ advanced-definitions-parser@ advanced-interactions-parsers@
           intermediate+access-file-parser@ intermediate+access-definitions-parser@ intermediate+access-interactions-parsers@
           intermediate-file-parser@ intermediate-definitions-parser@ intermediate-interactions-parsers@
           beginner-file-parser@ beginner-definitions-parser@ beginner-interactions-parsers@
           parsers^ token-proc^)
           
;  (define-compound-unit/infer java-file-parsers@
;    (import)
;    (export parsers^ token-proc^ err^)
;    (link java-dictionary@ combinator-parser-tools@ file-constants@ id@
;          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
;          expressions@ statements@ members@ interface@ class@ top-forms@
;          java-grammars@ full-program-parsers@))
;  
;  (define-compound-unit/infer java-definitions-parsers@
;    (import)
;    (export parsers^ token-proc^ err^)
;    (link java-dictionary@ combinator-parser-tools@ de-constants@ id@
;          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
;          expressions@ statements@ members@ interface@ class@ top-forms@
;          java-grammars@ full-program-parsers@))
;  
;  (define-compound-unit/infer java-interactions-parsers@
;    (import)
;    (export parsers^ token-proc^ err^)
;    (link java-dictionary@ combinator-parser-tools@ interact-constants@ id@
;          java-terminals@ types@ mods@ operators@ general@ unqualified-java-variables@
;          expressions@ statements@ members@ interface@ class@ top-forms@
;          java-grammars@ interaction-parsers@))
;  
;  (provide java-definitions-parsers@ java-interactions-parsers@ parsers^ token-proc^)
  
  )

