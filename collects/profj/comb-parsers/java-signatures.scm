(module java-signatures mzscheme
  
  (require mzlib/unit)
  
  (require parser-tools/lex
           (lib "combinator-unit.ss" "combinator-parser")
           mzlib/string)
  
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

  (define-signature statements^ (if-s return-s this-call super-ctor-call
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
