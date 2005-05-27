(module parse mzscheme
  (require (lib "lex.ss" "parser-tools")
           (prefix : (lib "lex-sre.ss" "parser-tools"))
           (lib "yacc.ss" "parser-tools")
           (lib "readerr.ss" "syntax")
           "../ast.ss")
  
  (define (make-struct-type-decls mfidefns)
    (define (convert-to-decl d)
      (cond
        [(honu-init-field? d)
         (make-honu-field-decl (honu-ast-src-stx d)
                               (honu-init-field-name d)
                               (honu-init-field-type d))]
        [(honu-field? d)
         (make-honu-field-decl (honu-ast-src-stx d)
                               (honu-field-name d)
                               (honu-field-type d))]
        [(honu-method? d)
         (make-honu-method-decl (honu-ast-src-stx d)
                                (honu-method-name d)
                                (honu-method-type d)
                                (honu-method-arg-types d))]))
    (map convert-to-decl mfidefns))
  
  (define (make-struct-exports typ mfidefns)
    (define (grab-name d)
      (cond
        [(honu-init-field? d) (honu-init-field-name d)]
        [(honu-field? d)      (honu-field-name d)]
        [(honu-method? d)     (honu-method-name d)]))
    (let ((names (map grab-name mfidefns)))
      (list (make-honu-export #f typ names names))))
  
  (define-lex-abbrevs [lex:letter (:or (:/ #\a #\z) (:/ #\A #\Z))]
    [lex:digit (:/ #\0 #\9)]
    [lex:whitespace (:or #\newline #\return #\tab #\space #\vtab)])
  
  (define-tokens EOF
    (EOF))
  
  (define-empty-tokens for-prec
    (UMINUS))
  
  (define-tokens lex-errors
    (UNPARSEABLE))
  
  (define-tokens keywords
    (type interface class mixin subclass struct
          extends final impl implements
          init export as at with
          this my null isa
          int bool str float char Any void
          if else true false while fun
          new super cast return))
  
  (define-tokens separators
    (O_CURLY C_CURLY O_BRACE C_BRACE O_PAREN C_PAREN COMMA COLON SEMI_COLON BINDS DOT SUBTYPE ARROW))
  
  (define-tokens operators
    (NOT OR AND NEQ EQUALS LT LE GT GE PLUS MINUS TIMES DIV MOD CLS_EQ))
  
  (define-tokens val-tokens
    (character floatnum string integer id))
  
  (define stx-for-original-property (read-syntax #f (open-input-string "original")))
  
  (define (create-src-stx val source-name start-pos end-pos)
    (datum->syntax-object #f val
                          (list
                           source-name
                           (position-line start-pos)
                           (position-col start-pos)
                           (position-offset start-pos)
                           (- (position-offset end-pos)
                              (position-offset start-pos)))
                          stx-for-original-property))
  
  (define (raise-read-error-with-stx str stx)
    (raise-read-error str
                      (syntax-source stx)
                      (syntax-line stx)
                      (syntax-column stx)
                      (syntax-position stx)
                      (syntax-span stx)))
  
  (define-syntax (token stx)
    (syntax-case stx ()
      [(_ name val)
       (identifier? (syntax name))
       (let ([name (syntax name)])
         (with-syntax ([token-name (datum->syntax-object
                                    name
                                    (string->symbol
                                     (format "token-~a" (syntax-e name))))]
                       [source-name (datum->syntax-object name 'source-name)]
                       [start-pos (datum->syntax-object name 'start-pos)]
                       [end-pos (datum->syntax-object name 'end-pos)])
           (syntax 
            (token-name 
             (create-src-stx val source-name start-pos end-pos)))))]))
  
  (define-syntax (ttoken stx)
    (syntax-case stx ()
      [(_ name)
       (identifier? (syntax name))
       (syntax (token name 'name))]))
  
  
  (define (generate-honu-lexer source-name)
    (define honu-lexer 
      (lexer-src-pos
       ["type"       (ttoken type)]
       ["interface"  (ttoken interface)]
       ["class"      (ttoken class)]
       ["mixin"      (ttoken mixin)]
       ["subclass"   (ttoken subclass)]
       ["struct"     (ttoken struct)]
       ["extends"    (ttoken extends)]
       ["final"      (ttoken final)]
       ["impl"       (ttoken impl)]
       ["implements" (ttoken implements)]
       ["init"       (ttoken init)]
       ["export"     (ttoken export)]
       ["as"         (ttoken as)]
       ["at"         (ttoken at)]
       ["with"       (ttoken with)]
       ["fun"        (ttoken fun)]
       ["this"       (ttoken this)]
       ["my"         (ttoken my)]
       ["null"       (ttoken null)]
       ["isa"        (ttoken isa)]
       ["int"        (ttoken int)]
       ["bool"       (ttoken bool)]
       ["str"        (ttoken str)]
       ["float"      (ttoken float)]
       ["char"       (ttoken char)]
       ["Any"        (ttoken Any)]
       ["void"       (ttoken void)]
       ["while"      (ttoken while)]
       ["if"         (ttoken if)]
       ["else"       (ttoken else)]
       ["true"       (token true  #t)]
       ["false"      (token false #f)]
       ["new"        (ttoken new)]
       ["super"      (ttoken super)]
       ["cast"       (ttoken cast)]
       ["return"     (ttoken return)]
       ["{"          (ttoken O_CURLY)]
       ["}"          (ttoken C_CURLY)]
       ["["          (ttoken O_BRACE)]
       ["]"          (ttoken C_BRACE)]
       ["("          (ttoken O_PAREN)]
       [")"          (ttoken C_PAREN)]
       [","          (ttoken COMMA)]
       [":"          (ttoken COLON)]
       [";"          (ttoken SEMI_COLON)]
       ["->"         (token ARROW   'arrow)]
       ["="          (token BINDS   'binds)]
       ["!="         (token NEQ     'neq)]
       ["=="         (token EQUALS  'equal)]
       ["===="       (token CLS_EQ  'cls_eq)]
       ["!"          (token NOT     'not)]
       ["&&"         (token AND     'and)]
       ["||"         (token OR      'or)]
       ["<"          (token LT      'lt)]
       ["<="         (token LE      'le)]
       [">"          (token GT      'gt)]
       [">="         (token GE      'ge)]
       ["+"          (token PLUS    'plus)]
       ["-"          (token MINUS   'minus)]
       ["*"          (token TIMES   'times)]
       ["/"          (token DIV     'div)]
       ["%"          (token MOD     'mod)]
       ["."          (token DOT     'dot)]
       ["<:"         (token SUBTYPE 'subtype)]
       [(:: (:or lex:letter)
            (:* (:or #\_ lex:letter lex:digit)))
        (token id (string->symbol lexeme))]
       [(:: (:? #\-)
            (:+ lex:digit))
        (token integer (string->number lexeme))]
       [(:: (:? #\-)
            (:: (:+ lex:digit) #\. (:+ lex:digit)))
        (token floatnum (string->number lexeme))]
       [(:: #\' any-char #\')
        (token character (string-ref lexeme 1))]
       [(:: #\"                           ;; A quoted string starts with a "
            (:* (:or (:~ #\\ #\")         ;; and has things in it which are
                     (:: #\\ any-char)))  ;; not "s (but \" is okay)
            #\")                          ;; and ends with a ".
        (token string (substring lexeme 1 (- (string-length lexeme) 1)))]
       [(:: "//"
            (:* (:~ #\newline)))
        (return-without-pos (honu-lexer input-port))]
       [(:: #\/ #\*)
        (begin (comment-lexer source-name start-pos input-port) ;; Get the rest of the comment...
               (return-without-pos (honu-lexer input-port)))]   ;; then get the next token.
       [(:+ lex:whitespace)
        (return-without-pos (honu-lexer input-port))]
       [(eof)
        (ttoken EOF)]
       [any-char (token UNPARSEABLE (string->symbol lexeme))]))
    honu-lexer)
  
  (define comment-lexer
    (lambda (source-name first-pos port)
      (letrec ([lxr (lexer-src-pos
                     [(:: #\/ #\*)
                      (begin (lxr input-port)                        ;; once for the nested comment
                             (return-without-pos (lxr input-port)))] ;; now finish out the current one
                     [(:: #\* #\/)
                      #f] ;; This will get ignored by the call to comment-lexer (whether nested or no)
                     [(eof)
                      (raise-read-error-with-stx
                       "Unexpected end of file while inside block comment."
                       (create-src-stx eof source-name first-pos end-pos))]
                     [(:~)
                      (return-without-pos (lxr input-port))])])
        (lxr port))))
  
  (define (generate-honu-parser source-name)
    (define honu-parser
      (parser
       (start program interact)
       (end EOF)
       (src-pos)
       ;; (debug "honu.debug")
       ;; (yacc-output "honu.yacc")
       ;; Since we have things that can look like x.y.z.w(...), we need to
       ;; actually specify a precedence for DOT.  There are 3 shift/reduce
       ;; conflicts for it, so if that warning is seen, it can be safely
       ;; ignored.  I don't want to turn off the warnings yet in case this
       ;; number increases, which means that I've added additional
       ;; conflicts.
       (precs (left else)
              (left BINDS)
              (left OR)
              (left AND)
              (left NEQ EQUALS)
              (nonassoc CLS_EQ)
              (nonassoc LT LE GT GE)
              (left PLUS MINUS)
              (left TIMES DIV MOD)
              (nonassoc NOT UMINUS)
              (right COLON isa)
              (left DOT))
       (tokens keywords separators operators val-tokens lex-errors EOF for-prec)
       (error (lambda (a b stx start end) 
                (raise-read-error-with-stx
                 (format "parse error near ~a" (syntax-e stx))
                 stx)))
       (grammar
        (program
         [(defns)
          (make-honu-program $1)])
        (defns
          [(defn defns)
           (if (honu-ast? $1)
               (cons $1 $2)
               (append $1 $2))]
          [()
           (list)])
        (defn
          [(fun-defn)
           $1]
          [(type-defn)
           $1]
          [(class-defn)
           $1]
          [(struct-defn)
           $1]
          [(mixin-defn)
           $1]
          [(subclass-defn)
           $1])
        
        (fun-defn
         [(any-type id O_PAREN args C_PAREN block)
          (make-honu-function
           (create-src-stx 'honu-function source-name $1-start-pos $6-end-pos)
           $2 $1 (cdr $4) (car $4) $6)])
        
        ;; Type definitions and needed parts
        
        (type-defn
         [(type id ext-clause
                O_CURLY fmdecs C_CURLY)
          (make-honu-type-defn 
           (create-src-stx 'honu-type-defn source-name $1-start-pos $6-end-pos) 
           $2 $3 $5)]
         [(interface id ext-clause
            O_CURLY fmdecs C_CURLY)
          (make-honu-type-defn
           (create-src-stx 'honu-type-defn source-name $1-start-pos $6-end-pos) 
           $2 $3 $5)])
        (type-id
         [(id)
          (make-honu-iface-type $1 $1)]
         [(Any)
          (make-honu-iface-top-type
           (create-src-stx 'Any source-name $1-start-pos $1-end-pos))])
        (any-type
         [(type-id)
          $1]
         [(void)
          (make-honu-top-type 
           (create-src-stx 'void source-name $1-start-pos $1-end-pos))]
         [(int)
          (make-honu-prim-type $1 'int)]
         [(bool)
          (make-honu-prim-type $1 'bool)]
         [(float)
          (make-honu-prim-type $1 'float)]
         [(char)
          (make-honu-prim-type $1 'char)]
         [(str)
          (make-honu-prim-type $1 'str)]
         [(O_BRACE tup-type C_BRACE ARROW any-type)
          (make-honu-func-type 
           (create-src-stx 'honu-func-type source-name $2-start-pos $5-end-pos)
           $2 $5)])
        (tup-type
         [()
          (list)]
         [(tup-type+)
          $1])
        (tup-type+
         [(any-type)
          (list $1)]
         [(any-type COMMA tup-type+)
          (cons $1 $3)])
        (ext-clause
         [(extends type-ids+)
          $2]
         [(SUBTYPE type-ids+)
          $2]
         [()
          '()])
        (ids+
         [(id COMMA ids+)
          (cons $1 $3)]
         [(id)
          (list $1)])
        (type-ids+
         [(type-id COMMA type-ids+)
          (cons $1 $3)]
         [(type-id)
          (list $1)])
        (args
         [(args-cd)
          $1]
         [()
          (cons (list) (list))])
        (args-cd
         [(arg COMMA args-cd)
          (cons (cons (car $1) (car $3)) (cons (cdr $1) (cdr $3)))]
         [(arg)
          (cons (list (car $1)) (list (cdr $1)))])
        (arg
         [(any-type id)
          (cons $1 $2)])
        (fmdecs
         [(fdec fmdecs)
          (cons $1 $2)]
         [(mdec fmdecs)
          (cons $1 $2)]
         [()
          (list)])
        (fdec
         [(any-type field-id SEMI_COLON)
          (make-honu-field-decl
           (create-src-stx 'honu-field-decl source-name $1-start-pos $3-end-pos)
           $2 $1)])
        (field-id
         [(id)
          $1])
        (mdec
         [(any-type meth-id O_PAREN mdec-args C_PAREN SEMI_COLON)
          (make-honu-method-decl
           (create-src-stx 'honu-method-decl source-name $1-start-pos $6-end-pos)
           $2 $1 $4)])
        (meth-id
         [(id)
          $1])
        (mdec-args
         [(mdec-args-cd)
          $1]
         [()
          (list)])
        (mdec-args-cd
         [(mdec-arg COMMA mdec-args-cd)
          (cons $1 $3)]
         [(mdec-arg)
          (list $1)])
        (mdec-arg
         [(any-type)
          $1]
         [(any-type id)
          $1])
        
        
        (struct-defn
         [(struct class-id init-args COLON type-id O_CURLY fmidefns C_CURLY)
          (let ([struct-stx (create-src-stx 'honu-struct source-name $1-start-pos $8-end-pos)])
            (list (make-honu-type-defn struct-stx (honu-iface-type-name $5) (list) (make-struct-type-decls $7))
                  (make-honu-class struct-stx $2 $5 #f (cdr $3) (car $3) (list $5) $7
                                   (make-struct-exports $5 $7))))]
         [(final struct class-id init-args COLON type-id O_CURLY fmidefns C_CURLY)
          (let ([struct-stx (create-src-stx 'honu-struct source-name $1-start-pos $9-end-pos)])
            (list (make-honu-type-defn struct-stx (honu-iface-type-name $6) (list) (make-struct-type-decls $8))
                  (make-honu-class struct-stx $3 $6 #t (cdr $4) (car $4) (list $6) $8
                                   (make-struct-exports $6 $8))))])
        
        ;; Class definitions and needed parts
        
        (class-defn
         [(class class-id init-args COLON type-id imp-clause
            O_CURLY fmidefns exports C_CURLY)
          (make-honu-class 
           (create-src-stx 'honu-class source-name $1-start-pos $10-end-pos)
           $2 $5 #f (cdr $3) (car $3) $6 $8 $9)]
         [(final class class-id init-args COLON type-id imp-clause
                 O_CURLY fmidefns exports C_CURLY)
          (make-honu-class
           (create-src-stx 'honu-class source-name $1-start-pos $11-end-pos)
           $3 $6 #t (cdr $4) (car $4) $7 $9 $10)])
        (class-id
         [(id)
          $1])
        (init-args
         [(O_PAREN args-cd C_PAREN)
          $2]
         [(O_PAREN C_PAREN)
          (cons (list) (list))])
        (imp-clause
         [(impl type-ids+)
          $2]
         [(implements type-ids+)
          $2]
         [()
          '()])
        (fmidefns
         [(fdefn fmidefns)
          (cons $1 $2)]
         [(mdefn fmidefns)
          (cons $1 $2)]
         [(initdefn fmidefns)
          (cons $1 $2)]
         [()
          (list)])
        (fdefn
         [(any-type field-id BINDS expr SEMI_COLON)
          (make-honu-field
           (create-src-stx 'honu-field source-name $1-start-pos $5-end-pos)
           $2 $1 $4)])
        (mdefn
         [(any-type meth-id O_PAREN args C_PAREN block)
          (make-honu-method
           (create-src-stx 'honu-method source-name $1-start-pos $6-end-pos)
           $2 $1 (cdr $4) (car $4) $6)])
        (initdefn
         [(init any-type field-id SEMI_COLON)
          (make-honu-init-field
           (create-src-stx 'honu-init-field source-name $1-start-pos $4-end-pos)
           $3 $2 #f)]
         [(init any-type field-id BINDS expr SEMI_COLON)
          (make-honu-init-field
           (create-src-stx 'honu-init-field source-name $1-start-pos $4-end-pos)
           $3 $2 $5)])
        (exports
         [(expdefn exports)
          (cons $1 $2)]
         [()
          (list)])
        (expdefn
         [(export type-id COLON expdecs SEMI_COLON)
          (make-honu-export
           (create-src-stx 'honu-export source-name $1-start-pos $5-end-pos)
           $2 (car $4) (cdr $4))]
         [(export type-id SEMI_COLON)
          (make-honu-export
           (create-src-stx 'honu-export source-name $1-start-pos $3-end-pos)
           $2 (list) (list))])
        (expdecs
         [(expdec COMMA expdecs)
          (cons (cons (car $1) (car $3)) (cons (cdr $1) (cdr $3)))]
         [(expdec)
          (cons (list (car $1)) (list (cdr $1)))])
        (expdec
         [(id as id)
          (cons $1 $3)]
         [(id)
          (cons $1 $1)])
        
        ;; Mixin definitions
        
        (mixin-defn
         [(mixin mixin-id init-args COLON type-id at type-id imp-clause with-clause
            O_CURLY fmidefns supernew fmidefns exports C_CURLY)
          (make-honu-mixin
           (create-src-stx 'honu-mixin source-name $1-start-pos $15-end-pos)
           $2 $5 $7 #f (cdr $3) (car $3) $8 (cdr $9) (car $9) $11 $12 $13 $14)]
         [(final mixin mixin-id init-args COLON type-id at type-id imp-clause with-clause
                 O_CURLY fmidefns supernew fmidefns exports C_CURLY)
          (make-honu-mixin 
           (create-src-stx 'honu-mixin source-name $1-start-pos $16-end-pos)
           $3 $6 $8 #t (cdr $4) (car $4) $9 (cdr $10) (car $10) $12 $13 $14 $15)])
        (mixin-id
         [(id)
          $1])
        (with-clause
         [(with args-cd)
          $2]
         [()
          (cons (list) (list))])
        (supernew
         [(super O_PAREN newargs C_PAREN SEMI_COLON)
          (make-honu-super-new 
           (create-src-stx 'honu-super-new source-name $1-start-pos $4-end-pos)
           (car $3) (cdr $3))])
        
        ;; Subclass definitions
        
        (subclass-defn
         [(subclass class-id BINDS mixin-id O_PAREN class-id C_PAREN SEMI_COLON)
          (make-honu-subclass
           (create-src-stx 'honu-subclass source-name $1-start-pos $8-end-pos)
           $2 $4 $6)]
         [(subclass class-id init-args COLON type-id extends class-id at type-id imp-clause with-clause
                    O_CURLY fmidefns supernew fmidefns exports C_CURLY)
          (let ([mixin-name (datum->syntax-object $2 (string->symbol (string-append "$" (symbol->string (syntax-e $2)))) $2)]
                [subclass-stx (create-src-stx 'honu-subclass source-name $1-start-pos $17-end-pos)])
            (list (make-honu-mixin subclass-stx mixin-name $5 $9 #f (cdr $3) (car $3) $10 (cdr $11) (car $11) $13 $14 $15 $16)
                  (make-honu-subclass subclass-stx $2 mixin-name $7)))]
         [(final subclass class-id init-args COLON type-id extends class-id at type-id imp-clause with-clause
                 O_CURLY fmidefns supernew fmidefns exports C_CURLY)
          (let ([mixin-name (datum->syntax-object $3 (string->symbol (string-append "$" (symbol->string (syntax-e $3)))))]
                [subclass-stx (create-src-stx 'honu-subclass source-name $1-start-pos $18-end-pos)])
            (list (make-honu-mixin subclass-stx mixin-name $6 $10 #f (cdr $4) (car $4) $11 (cdr $12) (car $12) $14 $15 $16 $17)
                  (make-honu-subclass subclass-stx $3 mixin-name $8)))])
        
        ;; Expressions
        
        (block
         [(O_CURLY bindings expr-sc+ C_CURLY)
          (make-honu-block
           (create-src-stx 'honu-block source-name $1-start-pos $4-end-pos)
           (reverse $2) $3)])
        (expr-sc+
         [(expr-sc expr-sc+)
          (cons $1 $2)]
         [(expr-sc)
          (list $1)])
        (expr-sc
         [(expr SEMI_COLON)
          $1]
         [(return SEMI_COLON)
          (make-honu-return 
           (create-src-stx 'honu-return source-name $1-start-pos $2-end-pos)
           #f)]
         [(return expr SEMI_COLON)
          (make-honu-return
           (create-src-stx 'honu-return source-name $1-start-pos $3-end-pos)
           $2)])
        (expr
         [(MINUS expr)
          (prec UMINUS)
          (make-honu-uprim 
           (create-src-stx 'honu-uprim source-name $1-start-pos $2-end-pos)
           'minus $1 #f $2)]
         [(NOT expr)
          (make-honu-uprim 
           (create-src-stx 'honu-uprim source-name $1-start-pos $2-end-pos)
           'not $1 #f $2)]
         [(expr OR expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'or $2 #f $1 $3)]
         [(expr AND expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'and $2 #f $1 $3)]
         [(expr CLS_EQ expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'clseq $2 #f $1 $3)]
         [(expr NEQ expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'neq $2 #f $1 $3)]
         [(expr EQUALS expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'equal $2 #f $1 $3)]
         [(expr LT expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'lt $2 #f $1 $3)]
         [(expr LE expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'le $2 #f $1 $3)]
         [(expr GT expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'gt $2 #f $1 $3)]
         [(expr GE expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'ge $2 #f $1 $3)]
         [(expr PLUS expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'plus $2 #f $1 $3)]
         [(expr MINUS expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'minus $2 #f $1 $3)]
         [(expr TIMES expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'times $2 #f $1 $3)]
         [(expr DIV expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'div $2 #f $1 $3)]
         [(expr MOD expr)
          (make-honu-prim
           (create-src-stx 'honu-prim source-name $1-start-pos $3-end-pos)
           'mod $2 #f $1 $3)]
         [(expr DOT field-id)
          (make-honu-facc
           (create-src-stx 'honu-facc source-name $1-start-pos $3-end-pos)
           $1 #f $3)]
         [(expr DOT field-id BINDS expr)
          (make-honu-fassn
           (create-src-stx 'honu-fassn source-name $1-start-pos $5-end-pos)
           $1 #f $3 $5)]
         [(expr DOT meth-id O_PAREN exprs C_PAREN)
          (make-honu-mcall
           (create-src-stx 'honu-mcall source-name $1-start-pos $6-end-pos)
           $1 #f $3 $5)]
         [(my DOT field-id)
          (make-honu-facc
           (create-src-stx 'honu-facc source-name $1-start-pos $3-end-pos)
           'my #f $3)]
         [(my DOT field-id BINDS expr)
          (make-honu-fassn
           (create-src-stx 'honu-fassn source-name $1-start-pos $5-end-pos)
           'my #f $3 $5)]
         [(my DOT meth-id O_PAREN exprs C_PAREN)
          (make-honu-mcall
           (create-src-stx 'honu-mcall source-name $1-start-pos $6-end-pos)
           'my #f $3 $5)]
         [(fun O_PAREN args C_PAREN block)
          (make-honu-lambda 
           (create-src-stx 'honu-lambda source-name $1-start-pos $5-end-pos)
           (cdr $3) (car $3) $5)]
         [(null)
          (make-honu-null $1)]
         [(literal)
          $1]
         [(this)
          (make-honu-this $1)]
         [(id)
          (make-honu-var $1 $1 #f)]
         [(id BINDS expr)
          (make-honu-assn 
           (create-src-stx 'honu-assn source-name $1-start-pos $3-end-pos)
           $1 $3)]
         [(id O_PAREN exprs C_PAREN)
          (make-honu-call
           (create-src-stx 'honu-call source-name $1-start-pos $4-end-pos)
           $1 $3 #f)]
         [(new class-id COLON type-id O_PAREN newargs C_PAREN)
          (make-honu-new 
           (create-src-stx 'honu-new source-name $1-start-pos $7-end-pos)
           $2 $4 (car $6) (cdr $6))]
         [(new class-id O_PAREN newargs C_PAREN)
          (make-honu-new 
           (create-src-stx 'honu-new source-name $1-start-pos $5-end-pos)
           $2 #f (car $4) (cdr $4))]
         [(expr COLON type-id)
          (make-honu-cast 
           (create-src-stx 'honu-cast source-name $1-start-pos $3-end-pos)
           $1 $3)]
         [(expr isa type-id)
          (make-honu-isa 
           (create-src-stx 'honu-isa source-name $1-start-pos $3-end-pos)
           $1 $3)]
         [(if expr block else block)
          (make-honu-if 
           (create-src-stx 'honu-if source-name $1-start-pos $5-end-pos)
           $2 $3 $5)]
         [(while expr block)
          (make-honu-while
           (create-src-stx 'honu-while source-name $1-start-pos $3-end-pos)
           $2 $3)]
         [(O_PAREN expr C_PAREN)
          $2]
         [(block)
          $1])
        (literal
         [(true)
          (make-honu-bool $1 (syntax-e $1))]
         [(false)
          (make-honu-bool $1 (syntax-e $1))]
         [(integer)
          (make-honu-int $1 (syntax-e $1))]
         [(floatnum)
          (make-honu-float $1 (syntax-e $1))]
         [(character)
          (make-honu-char $1 (syntax-e $1))]
         [(string)
          (make-honu-str $1 (syntax-e $1))])
        (newargs
         [(newargs-cd)
          $1]
         [()
          (cons (list) (list))])
        (newargs-cd
         [(newarg COMMA newargs-cd)
          (cons (cons (car $1) (car $3))
                (cons (cdr $1) (cdr $3)))]
         [(newarg)
          (cons (list (car $1)) (list (cdr $1)))])
        (newarg
         [(id BINDS expr)
          (cons $1 $3)])
        (exprs
         [(exprs-cd)
          $1]
         [()
          '()])
        (exprs-cd
         [(expr COMMA exprs-cd)
          (cons $1 $3)]
         [(expr)
          (list $1)])
        (bindings
         [(bindings binding)
          (cons $2 $1)]
         [()
          '()])
        (binding
         [(any-type id BINDS expr SEMI_COLON)
          (make-honu-binding
           (create-src-stx 'honu-binding source-name $1-start-pos $5-end-pos)
           $2 $1 $4)])
        (interact
         [(binding)
          $1]
         [(expr)
          $1]))))
    honu-parser)
  
  (define (parse-interaction port file)
    (let ([lexer  (generate-honu-lexer file)]
          [parser (cadr (generate-honu-parser file))])
      (port-count-lines! port)
      (parser
       (lambda ()
         (lexer port)))))
  
  (define (parse-port port file)
    (let ([lexer  (generate-honu-lexer  file)]
          [parser (car (generate-honu-parser file))])
      (port-count-lines! port)
      (parser
       (lambda () 
         (lexer port)))))
  
  (define (parse-file file)
    (with-input-from-file file
      (lambda ()
        (parse-port (current-input-port)
                    (simplify-path (path->complete-path file))))))
  
  (define (parse-stdin)
    (parse-port (current-input-port) #f))
  
  (define (parse-string string)
    (parse-port (open-input-string string) #f))
  
  (define (read-cm port)
    (let loop ((filenames '())
               (val (read port)))
      (if (eof-object? val)
          (reverse filenames)
          (loop (cons (string-append val ".honu") filenames)
                (read port)))))
  
  (define (parse-group port name)
    (let ([filenames (read-cm port)])
      (if (null? filenames)
          (make-honu-program '())
          (let loop ((filenames filenames)
                     (defns     '()))
            (let ((parsed (parse-file 
                           (simplify-path
                            (path->complete-path (car filenames))))))
              (if (null? (cdr filenames))
                  (make-honu-program 
                   (append (honu-program-defns parsed) defns))
                  (loop (cdr filenames)
                        (append (honu-program-defns parsed) defns))))))))
  
  (define (parse-group-file dirname filename)
    (let ([filenames (call-with-input-file
                         (string-append dirname "/" filename)
                       read-cm)])
      (if (null? filenames)
          (make-honu-program '())
          (let loop ((filenames filenames)
                     (defns     '()))
            (let ((parsed (parse-file (string-append dirname "/"
                                                     (car filenames)))))
              (if (null? (cdr filenames))
                  (make-honu-program 
                   (append (honu-program-defns parsed) defns))
                  (loop (cdr filenames)
                        (append (honu-program-defns parsed) defns))))))))
  
  (provide parse-file parse-port parse-stdin parse-string parse-group parse-group-file parse-interaction)
  
  )

