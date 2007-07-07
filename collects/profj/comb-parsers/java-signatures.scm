(module java-signatures mzscheme
  
  (require (lib "unit.ss"))
  
  (require (lib "lex.ss" "parser-tools")
           (lib "combinator-unit.ss" "combinator-parser")
           (lib "string.ss"))
  
  (provide (all-defined))
  
  ;Terminal signatures
    
  (define-signature java-operators^
    ((terminals Operators
                (PIPE OR OREQUAL GT LT ! ~ ? : ==	LTEQ GTEQ != &&	++ -- PLUS 
                      MINUS DIVIDE & ^T % << >> >>> += -= *= /= &= ^= %= <<= >>= >>>=))))
  
  (define-signature java-separators^
    ((terminals Separators
                (O_PAREN C_PAREN O_BRACE C_BRACE O_BRACKET C_BRACKET SEMI_COLON 
                         PERIOD COMMA EQUAL TIMES))))
  
  (define-signature java-literals^
    ((terminals EmptyLiterals (NULL_LIT TRUE_LIT FALSE_LIT EOF))))
  
  (define-signature java-expression-keywords^
    ((terminals ExpressionKeywords (instanceof new))))
  
  (define-signature java-statement-keywords^
    ((terminals StatementKeywords (break caseT catch continue ifT doT elseT
                                         for return switch throw try while finally))))
  
  (define-signature java-definition-keywords^
    ((terminals DefinitionKeywords (abstract class extends final private implements import
                                             interface native package protected public static
                                             strictfp synchronized throws transient volatile))))  
  
  (define-signature java-type-keywords^
    ((terminals TypeKeywords (boolean byte char double float int long short voidT))))
  
  (define-signature java-reserved^
    ((terminals ReservedWords (default const goto))))
  
  (define-signature java-extras^
    ((terminals ExtraKeywords (dynamic check expect within -> ->> ->>> test tests testcase))))
    
  (define-signature java-ids^ ((terminals java-vals (IDENTIFIER this super))))
  
  (define-signature java-vals^
    ((terminals java-vals (STRING_LIT CHAR_LIT INTEGER_LIT  LONG_LIT FLOAT_LIT DOUBLE_LIT
                                     STRING_ERROR NUMBER_ERROR HEX_LIT OCT_LIT 
                                     HEXL_LIT  OCTL_LIT))))
  
  (define-signature java-specials^
    ((terminals special-toks (EXAMPLE TEST_SUITE IMAGE_SPECIAL OTHER_SPECIAL)))) 
  
  ;General purpose signatures
  (define-signature general-productions^ (comma-sep #;variable-declaration #;name))
  
  (define-signature java-variables^ (identifier name variable-declaration))
  
  ;Types, modifiers, operator signatures
  
  (define-signature java-types^ (integer-types inexact-types numeric-type prim-type 
                                               other-type-base value+name-type method-type array-type))
  (define-signature java-access^ (access-mods global-mods method-mods))
  (define-signature java-ops^ (math-ops shift-ops compare-ops bool-ops bit-ops assignment-ops
                                        bin-ops un-assignment un-op))
  
  ;Expression signatures  
  (define-signature expr-lits^ (boolean-lits textual-lits prim-numeric-lits null-lit numeric-lits
                                             double-lits literals all-literals))
  
  (define-signature expr-terms^ (new-class new-array simple-method-call
                                           assignment unary-assignment-front cast
                                           super-call))
  
  (define-signature expr-tails^ (field-access-end 
                                 array-access-end array-init 
                                 binary-expression-end if-expr-end
                                 method-call-end unary-assignment-back instanceof-back))
  
  (define-signature expr-terms+^ extends expr-terms^ (checks))
  
  ;Statement signatures

  (define-signature statements^ (make-statement if-s return-s this-call super-ctor-call
                                           block expression-stmt while-l do-while for-l
                                           break-s cont-s init))
  
  ;Member signatures
  
  (define-signature fields^ (make-field arg args))
  
  (define-signature methods^ (method-signature method-header make-method))
  
  (define-signature ctors^ (make-constructor))
  
  ;Definition signatures
  
  (define-signature interfaces^ (interface-body interface-def))
  
  (define-signature classes^ (class-body implements-dec extend-dec class-def))
  
  (define-signature top-forms^ (top-member import-dec make-program))
  
  )
;    
;    ;                                                                                            
;    ;                                                                                            
;    ;    ;;;;                                                                                    
;    ;       ;                                                                                    
;    ;       ;                                                                                    
;    ;       ;      ;;;;;    ;; ;;;      ;;;; ;; ;;  ;;;    ;;;;;      ;;;; ;;   ;;;;;     ;;;; ; 
;    ;       ;           ;    ;;   ;    ;    ;;   ;    ;         ;    ;    ;;   ;     ;   ;    ;; 
;    ;       ;           ;    ;    ;   ;      ;   ;    ;         ;   ;      ;  ;       ;  ;     ; 
;    ;       ;      ;;;;;;    ;    ;   ;      ;   ;    ;    ;;;;;;   ;      ;  ;;;;;;;;;   ;;;    
;    ;       ;     ;     ;    ;    ;   ;      ;   ;    ;   ;     ;   ;      ;  ;              ;;; 
;    ;       ;     ;     ;    ;    ;   ;      ;   ;    ;   ;     ;   ;      ;  ;          ;     ; 
;    ;       ;     ;    ;;    ;    ;    ;    ;;   ;   ;;   ;    ;;    ;    ;;   ;     ;;  ;;    ; 
;    ;    ;;;;;;;   ;;;; ;;  ;;;  ;;;    ;;;; ;    ;;; ;;   ;;;; ;;    ;;;; ;    ;;;;;    ; ;;;;  
;    ;                                        ;                             ;                     
;    ;                                        ;                             ;                     
;    ;                                   ;;;;;                         ;;;;;                      
;    
;    (define beginner-unique-base
;      (simple-expression
;       (list (literals (list boolean-lits textual-lits prim-numeric-lits double-lits))
;             this
;             IDENTIFIER
;             (new-class IDENTIFIER (eta beginner-expression))
;             (simple-method-call (eta beginner-expression))
;             (sequence (O_PAREN (eta beginner-expression) C_PAREN) id "expression")
;             (sequence (! (eta beginner-expression)) id "unary expression")
;             (checks (eta beginner-expression)))))
;    
;    (define beginner-unique-end
;      (simple-expression
;       (list field-access-end
;             (method-call-end (eta beginner-expression))
;             (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops))
;                                    (eta beginner-expression)))))
;    
;    (define beginner-expression
;      (sequence (beginner-unique-base (repeat beginner-unique-end)) id "expression"))
;    
;    (define beginner-statement
;      (statement (list (if-s beginner-expression (eta beginner-statement) #f)
;                       (return-s beginner-expression #f))))
;    
;    (define beginner-field (field #f value-type beginner-expression #f))
;    
;    (define beginner-method-sig
;      (method-signature #f value-type args))
;    
;    (define beginner-method
;      (method beginner-method-sig beginner-statement))
;    
;    (define beginner-constructor (constructor #f init*))
;    
;    (define beginner-interface
;      (interface-def #f #f (method-header* beginner-method-sig)))
;    
;    (define beginner-class
;      (class-def #f #f (implements-dec IDENTIFIER) 
;                 (repeat (class-body (list beginner-field beginner-method beginner-constructor)))))
;    
;    (define beginner-program 
;      (program #f (repeat import-dec) 
;               (repeat (top-member (list beginner-class beginner-interface)))))
;    
;    (define parse-beginner (parser beginner-program))
;    
;    (define intermediate-unique-base
;      (simple-expression 
;       (list (literals (list boolean-lits textual-lits prim-numeric-lits double-lits))
;             this
;             IDENTIFIER
;             (new-class IDENTIFIER (eta intermediate-expression))
;             (simple-method-call (eta intermediate-expression))
;             (sequence (O_PAREN (eta intermediate-expression) C_PAREN) id "expression")
;             (sequence (! (eta intermediate-expression)) id "unary expression")
;             (cast value-type (eta intermediate-expression))
;             (super-call (eta intermediate-expression))
;             (checks (eta intermediate-expression)))))
;    
;    (define intermediate-unique-end
;      (simple-expression
;       (list field-access-end
;             (method-call-end (eta intermediate-expression))
;             (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops bit-ops))
;                                    (eta intermediate-expression)))))
;    
;    (define intermediate-expression
;      (sequence (intermediate-unique-base (repeat intermediate-unique-end))
;                id "expression"))
;    
;    (define intermediate-stmt-expr
;      (simple-expression (list (new-class IDENTIFIER intermediate-expression)
;                               (super-call intermediate-expression)
;                               (sequence (intermediate-expression 
;                                          (method-call-end intermediate-expression))
;                                         id "method call")
;                               (assignment IDENTIFIER EQUAL intermediate-expression))))
;    
;    (define intermediate-statement
;      (statement (list (if-s intermediate-expression (eta intermediate-statement)  #f)
;                       (return-s intermediate-expression #t)
;                       (variable-declaration value-type intermediate-expression #f "local variable")
;                       (block (repeat (eta intermediate-statement)))
;                       (sequence (intermediate-stmt-expr SEMI_COLON) id "statement"))))                           
;    
;    (define intermediate-field (field access-mods value-type intermediate-expression #t))
;    
;    (define intermediate-method-sig-no-abs
;      (method-signature access-mods
;                        (method-type value-type)
;                        args))
;    (define intermediate-method-sig-abs
;      (method-signature (method-mods access-mods)
;                        (method-type value-type)
;                        args))
;    
;    (define intermediate-method
;      (choose ((method intermediate-method-sig-no-abs intermediate-statement)
;               (method-header intermediate-method-sig-abs)) "method definition top"))
;    
;    (define intermediate-constructor
;      (constructor access-mods
;                   (choose ((sequence ((super-call intermediate-expression) (repeat intermediate-statement)) id)
;                            (sequence ((this-call intermediate-expression) (repeat intermediate-statement)) id)
;                            (repeat intermediate-statement)) "constructor body")))
;    
;    (define intermediate-interface 
;      (interface-def
;       #f
;       (sequence (extends (comma-sep IDENTIFIER "interfaces")) id "extends")
;       (method-header* intermediate-method-sig-no-abs)))
;    
;    (define intermediate-class
;      (class-def abstract (extend-dec IDENTIFIER) (implements-dec (comma-sep IDENTIFIER "interfaces"))
;                 (repeat (class-body (list intermediate-field intermediate-method intermediate-constructor)))))
;    
;    (define intermediate-program
;      (program #f (repeat import-dec)
;               (repeat (top-member (list intermediate-class intermediate-interface)))))
;    
;    (define parse-intermediate (parser intermediate-program))
;    
;    (define advanced-unique-base
;      (simple-expression 
;       (list (literals (list boolean-lits textual-lits prim-numeric-lits double-lits))
;             this
;             IDENTIFIER
;             (new-class IDENTIFIER (eta advanced-expression))
;             (simple-method-call (eta advanced-expression))
;             (new-array value-type (eta advanced-expression))
;             (sequence (O_PAREN (eta advanced-expression) C_PAREN) id "expression")
;             (sequence (! (eta advanced-expression)) id "unary expression")
;             (cast value-type (eta advanced-expression))
;             (super-call (eta advanced-expression))
;             (checks (eta advanced-expression)))))
;    
;    (define advanced-unique-end
;      (simple-expression
;       (list field-access-end
;             (array-access-end (eta advanced-expression))
;             (method-call-end (eta advanced-expression))
;             (if-expr-end (eta advanced-expression))
;             (binary-expression-end (bin-ops (list math-ops compare-ops bool-ops))
;                                    (eta advanced-expression)))))
;    
;    (define advanced-expression
;      (sequence (advanced-unique-base (repeat advanced-unique-end)) id "expression"))
;    
;    
;    (define advanced-stmt-expr
;      (simple-expression (list (new-class IDENTIFIER advanced-expression)
;                               (super-call advanced-expression)
;                               (sequence (advanced-expression
;                                          (method-call-end advanced-expression)) id "method call")
;                               (assignment IDENTIFIER assignment-ops advanced-expression)
;                               (sequence (advanced-expression ++) id "unary mutation")
;                               (sequence (advanced-expression --) id "unary mutation")
;                               (sequence (++ advanced-expression) id "unary mutation")
;                               (sequence (-- advanced-expression) id "unary mutation"))))
;    
;    (define advanced-statement
;      (statement (list (if-s advanced-expression (eta advanced-statement) #t)
;                       (return-s advanced-expression #t)
;                       (variable-declaration value-type advanced-expression #t "local variable")
;                       (block (repeat (eta advanced-statement)))
;                       (sequence (advanced-stmt-expr SEMI_COLON) id "statement")
;                       (for-l (choose ((variable-declaration value-type advanced-expression #t "for loop variable")
;                                       (comma-sep advanced-stmt-expr "initializations")) "for loop initialization") 
;                              #t
;                              advanced-expression #t
;                              (comma-sep advanced-stmt-expr "for loop increments") #t (eta advanced-statement))
;                       (while-l advanced-expression (eta advanced-statement))
;                       (do-while advanced-expression (eta advanced-statement))
;                       (break-s #f)
;                       (cont-s #f))))
;    
;    (define advanced-field (field (global-mods access-mods) value-type advanced-expression #t))  
;    
;    (define advanced-method-sig-no-abs
;      (method-signature (global-mods access-mods)
;                        (method-type value-type)
;                        args))
;    (define advanced-method-sig-abs
;      (method-signature (method-mods (global-mods access-mods))
;                        (method-type value-type)
;                        args))
;    
;    (define advanced-method
;      (choose ((method advanced-method-sig-no-abs advanced-statement)
;               (method-header advanced-method-sig-abs)) "method definition"))
;    
;    (define advanced-constructor
;      (constructor access-mods
;                   (choose ((sequence ((super-call advanced-expression) (repeat advanced-statement)) id)
;                            (sequence ((this-call advanced-expression) (repeat advanced-statement)) id)
;                            (repeat advanced-statement)) "constructor body")))
;    
;    (define advanced-interface 
;      (interface-def
;       #f
;       (sequence (extends (comma-sep IDENTIFIER "interfaces")) id "extends")
;       (method-header* advanced-method-sig-no-abs)))
;    
;    (define advanced-class
;      (class-def abstract (extend-dec IDENTIFIER) (implements-dec (comma-sep IDENTIFIER "interfaces"))
;                 (repeat (class-body (list advanced-field advanced-method advanced-constructor
;                                           (method-header advanced-method-sig-abs))))))
;    
;    (define advanced-program
;      (program (sequence (package name SEMI_COLON) id "package specification")
;               (repeat import-dec)
;               (repeat (top-member (list advanced-class advanced-interface)))))
;    
;    (define parse-advanced
;      (parser advanced-program))
;    
;    (define (old-tokens->new tok-list)
;      (cond
;        [(null? tok-list) null]
;        [else
;         (cons
;          (make-position-token
;           (case (token-name (position-token-token (car tok-list)))
;             [(=) (token-EQUAL)]
;             ((<) (token-LT))
;             ((>) (token-GT))
;             ((<=) (token-LTEQ))
;             ((>=) (token-GTEQ))
;             ((+) (token-PLUS))
;             ((-) (token-MINUS))
;             ((*) (token-TIMES))
;             ((/) (token-DIVIDE))
;             ((^) (token-^T))
;             ((if) (token-ifT))
;             ((do) (token-doT))
;             ((case) (token-caseT))
;             ((else) (token-elseT))
;             ((void) (token-voidT))
;             (else (position-token-token (car tok-list))))
;           (position-token-start-pos (car tok-list))
;           (position-token-end-pos (car tok-list))) 
;          (old-tokens->new (cdr tok-list)))]))  
;    
;    )
;  
;  (define-unit constants@
;    (import)
;    (export error-format-parameters^)
;    (define src? #t)
;    (define input-type "file")
;    (define show-options #f)
;    (define max-depth 1)
;    (define max-choice-depth 3))
;  
;  (define-compound-unit/infer java-parsers@
;    (import)
;    (export teaching-languages^)
;    (link java-dictionary@ combinator-parser-tools@ constants@ java-grammars@))
;  
;  (provide java-parsers@ teaching-languages^)
;  
;  )
;
