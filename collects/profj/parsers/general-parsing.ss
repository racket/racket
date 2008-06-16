(module general-parsing scheme/base
  
  (require 
   (for-syntax scheme/base)
   (except-in parser-tools/lex input-port)
   mzlib/string
   (prefix-in class: scheme/class))
  (require "../ast.ss" "../parameters.ss" "lexer.ss")
  
  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Methods used by all generated parsers
  (define-syntax (build-src stx)
    (syntax-case stx ()
      ((_ end)
       (syntax (build-src 1 end)))
      ((_ start end)
       (with-syntax ((start-pos (datum->syntax 
                                 (syntax end)
                                 (string->symbol 
                                  (format "$~a-start-pos"
                                          (syntax->datum (syntax start))))))
                     (end-pos (datum->syntax 
                               (syntax end)
                               (string->symbol 
                                (format "$~a-end-pos"
                                        (syntax->datum (syntax end)))))))
         (syntax
          (make-src (position-line start-pos)
                    (position-col start-pos)
                    (+ (position-offset start-pos) (interactions-offset))
                    (- (position-offset end-pos)
                       (position-offset start-pos))
                    (file-path)
                    ))))))
  
  (define (construct-method-header mods type-parms ret-type declarator throws)
    (make-method mods 
                 (make-type-spec (type-spec-name ret-type)
                                 (+ (type-spec-dim ret-type) (caddr declarator))
                                 (type-spec-src ret-type))
                 type-parms
                 (car declarator)
                 (cadr declarator)
                 throws
                 #f
                 #f
                 #f
                 #f))
  
  (define (name->access n)
    (make-access #f
                 (name-src n)
                 (append (name-path n) (list (name-id n)))))
  
  (define (access->name a)
    (make-name (car (reverse (access-name a)))
               (cdr (access-name a))
               (expr-src a)))
  
  (define (build-name-call name args src-loc)
    (make-call #f src-loc 
               (if (null? (name-path name))
                   #f
                   (make-access #f 
                                (name-src name)
                                (name-path name)))
               (name-id name)
               args 
               #f))
  
  (define (build-field-decl mods type decl)
    (cond
      ((var-decl? decl)
       (make-var-decl (var-decl-name decl)
                      mods
                      (make-type-spec
                       (type-spec-name type)
                       (+ (type-spec-dim type) 
                          (type-spec-dim (var-decl-type-spec decl)))
                       (type-spec-src type))
                      #f
                      (var-decl-src decl)))
      ((var-init? decl)
       (make-var-init
        (build-field-decl mods type (var-init-var-decl decl))
        (var-init-init decl)
        (var-init-src decl)))))
  
;  (define (parse-class-box box box-pos level)
;    (let*-values (((old-input-port) (input-port))
;                  ((old-file-path) (file-path))
;                  ((func _ __) (class:send (class-case-box box) read-one-special 0 #f #f #f #f))
;                  ((parse-port-list) (func 'level)))
;      (input-port (car parse-port-list))
;      (begin0
;        (car (package-defs ((cadr parse-port-list))))
;        (file-path old-file-path)
;        (input-port old-input-port))))

  (define (parse-class-box box box-pos level)
    (let*-values (((old-file-path) (file-path))
                  ((old-lex-stream) (lex-stream))
                  ((parse-func _ __) (class:send (class-case-box box) read-one-special 0 #f #f #f #f))
                  ((class-ast) (parse-func level old-file-path box-pos lex-stream)))
      (begin0
        class-ast
        (lex-stream old-lex-stream)
        (file-path old-file-path))))
;  
;  (lambda (level class-loc box-pos input-spec)
;    (make-class-def (make-header ....)
;                    (list methods, fields, ctor)
;                    #f
;                    box-pos
;                    class-loc
;                    level
;                    null
;                    'top))
;  (anytime before call parse-method
;           (let ((old-input (input-spec)))
;             (input-spec (lambda () get-the-port))
;             (begin0 (parse-method ... )
;                     (input-spec old-input))))
           
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;Token Accessors and Queries for error-messaging parsers
    
  (define-syntax define-token?
    (syntax-rules ()
                  [(_ name check)
                   (define (name token) (eq? (token-name token) check))]))

  ;get-token-name: token -> symbol
  (define get-token-name token-name)
  
  ;Special
  (define-token? eof? 'EOF)
  
  ;Modifiers
  (define-token? abstract? 'abstract)
  (define-token? native? 'native)
  (define-token? private? 'private)
  (define-token? protected? 'protected)
  (define-token? public? 'public)
  (define-token? static? 'static)
  (define-token? strictfp? 'strictfp)
  (define-token? transient? 'transient)
  (define-token? volatile? 'volatile)
  (define-token? final? 'final)
  (define (modifier-token? token)
    (and (symbol? token) (memq token `(abstract native private protected public static strictfp transient volatile))))
  
  ;Literals
  (define (literal-token? token)
    (or (empty-literal? token) (full-literal? token)))
  (define (empty-literal? token)
    (and (symbol? token) (memq token `(NULL_LIT TRUE_LIT FALSE_LIT))))
  (define (full-literal? token)
    (and (token? token) (memq (token-name token) `(STRING_LIT CHAR_LIT INTEGER_LIT LONG_LIT FLOAT_LIT DOUBLE_LIT))))
  
  ;Primitive types
  (define (prim-type? token)
    (and (symbol? token) (memq token `(boolean byte char double float int long short))))
  
  ;Operators
  (define (bin-operator? token)
    (memq (get-token-name token) `(PIPE OR > < == <= >= != && + - * / & ^ % << >> >>>)))
  (define (unary-end? token)
    (memq (get-token-name token) `(++ --)))
  (define (if-exp? token)
    (eq? (get-token-name token) '?))
  (define (teaching-unary-operator? token)
    (and (symbol? token) (memq token `(! ~))))
  (define (unary-operator? token)
    (and (symbol? token) (memq token `(! ~ ++ --))))
  (define (teaching-assignment-operator? token)
    (and (symbol? token) (eq? token '=)))
  (define (assignment-operator? token)
    (and (symbol? token) (memq token `(= += -= *= /= &= ^= %= <<= >>= >>>=))))
  
  ;Separators
  (define-token? o-paren? 'O_PAREN)
  (define-token? c-paren? 'C_PAREN)
  (define-token? o-brace? 'O_BRACE)
  (define-token? c-brace? 'C_BRACE)
  (define-token? o-bracket? 'O_BRACKET)
  (define-token? c-bracket? 'C_BRACKET)
  (define-token? star? '*)
  (define-token? semi-colon? 'SEMI_COLON)
  (define-token? colon? ':)
  (define-token? dot? 'PERIOD)
  (define-token? comma? 'COMMA)
  
  (define (separator? tok)
    (or (open-separator? tok) (close-separator? tok)
        (memq (get-token-name tok) `(SEMI_COLON PERIOD COMMA))))
  (define (open-separator? tok)
    (memq (get-token-name tok) `(O_PAREN O_BRACE O_BRACKET)))
  (define (close-separator? tok)
    (memq (get-token-name tok) `(C_PAREN C_BRACE C_BRACKET)))
  
  ;top-level keywords
  (define-token? package-token? 'package)
  (define-token? import-token? 'import)
  
  ;Definition keywords
  (define-token? class? 'class)
  (define-token? extends? 'extends)
  (define-token? implements? 'implements)
  (define-token? interface? 'interface)
  
  ;Method keywords
  (define-token? const? 'const)
  (define-token? throws-token? 'throws)
  (define-token? void-token? 'void)
  
  ;Statement keywords
  (define-token? break-token? 'break)
  (define-token? case-token? 'case)
  (define-token? catch-token? 'catch)
  (define-token? continue-token? 'continue)
  (define-token? defualt? 'default)
  (define-token? do-token? 'do)
  (define-token? else? 'else)
  (define-token? finally? 'finally)
  (define-token? for-token? 'for)
  (define-token? goto? 'goto)
  (define-token? if-token? 'if)
  (define-token? return-token? 'return)
  (define-token? switch-token? 'switch)
  (define-token? synchronized-token? 'synchronized)
  (define-token? throw-token? 'throw)
  (define-token? try-token? 'try)
  (define-token? while-token? 'while)

  ;Expression tokens
  (define-token? instanceof-token? 'instanceof)
  (define-token? new-token? 'new)
  (define-token? super? 'super)
  (define-token? this? 'this)
  (define-token? cond? '?)
  (define-token? id-token? 'IDENTIFIER)
  
  ;java-keyword? lex-token -> bool
  (define (java-keyword? t)
    (or (memq (get-token-name t) `(? this super new instanceof while try throw synchronized switch return if goto for finally
                                     else do default continue catch case break void throws const interface implements extends
                                     class import package))
        (assignment-operator? t)
        (prim-type? t)
        (modifier-token? t)))
  
  ;only looks for incorrect capitalization at this point, intend to add 1-off spelling errors for at least some keywords
  ;close-to-keyword? token (opt symbol )-> bool
  (define (close-to-keyword? t . args)
    (if (id-token? t)
        (let ((s (string-copy (token-value t))))
          (string-lowercase! s)
          (if (null? args)
              (or (java-keyword? (string->symbol s))
                  (member s all-words))
              (or (eq? (string->symbol s) (car args))
                  (member s (select-words (car args))))))
        #f))
  
  (define (miscapitalized? t key)
    (let ((s (string-copy (token-value t))))
      (string-lowercase! s)
      (equal? s key)))
  
  (define misspelled-list '((import "mport" "iport" "imort" "imprt" "impot" "impor" "improt" "impourt")
                            (class "lass" "cass" "clss" "clas" "calss")
                            (abstract 
                             "bstract" "astract" "abtract" "absract" "abstact" "abstrct" "abstrat" "abstract" "abstarct" "abstracts")
                            (extends "xtends" "etends" "exends" "extnds" "exteds" "extens" "extneds" "extend")
                            (new "nw" "ne" "nwe")
                            (this "his" "tis" "ths" "thi" "tihs" "thsi")
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
                                
  )
