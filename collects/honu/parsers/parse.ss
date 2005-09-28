(module parse mzscheme
  
  (require (lib "yacc.ss" "parser-tools")
           "lex.ss"
           "../readerr.ss"
           "../ast.ss"
           "../utils.ss"
           "../private/typechecker/type-utils.ss")
  
  (define (generate-honu-parser source-name)
    (define honu-parser
      (parser
       (start <program> <interact>)
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
       (precs (right ARROW) ;; for types
        
              (nonassoc return)
              (left else)       ;; for expressions
              (left BINDS)
              (left OR)
              (left AND)
              (left NEQ EQUALS)
              (nonassoc CLS_EQ)
              (nonassoc LT LE GT GE)
              (left PLUS MINUS)
              (left TIMES DIV MOD)
              (nonassoc NOT UMINUS) ;; unary operators
              (right COLON isa)
              (nonassoc selector)
              (left O_PAREN)  ;; this gives application a precedence
              (left DOT))
       (tokens keywords separators operators val-tokens lex-errors EOF for-prec)
       (error (lambda (a b stx start end) 
                (raise-read-error-with-stx
                 (format "parse error near ~a" (syntax-e stx))
                 stx)))
       (grammar
        (<program>
         [(<defns>)
          $1])
        (<defns>
          [(<defn> <defns>)
           (if (honu:ast? $1)
               (cons $1 $2)
               (append $1 $2))]
          [()
           (list)])
        (<defn>
          [(<function-defn>)
           $1]
          [(<iface-defn>)
           $1]
          [(<class-defn>)
           $1]
          [(<struct-defn>)
           $1]
          [(<mixin-defn>)
           $1]
          [(<binding-defn>)
           $1])
          
        (<binding-defn>
         [(<bind> BINDS <expr> SEMI_COLON)
          (make-honu:bind-top 
           (create-src-stx 'honu:bind-top source-name $1-start-pos $4-end-pos)
           (list (honu:formal-name $1)) (list (honu:formal-type $1)) $3)]
         [(O_PAREN <binds-cd> C_PAREN BINDS <expr> SEMI_COLON)
          (let-values ([(names types) (map-two-values (lambda (f)
                                                        (values (honu:formal-name f)
                                                                (honu:formal-type f)))
                                                      $2)])
            (make-honu:bind-top
             (create-src-stx 'honu:bind-top source-name $1-start-pos $6-end-pos)
             names types $5))])
        (<binds-cd>
         [(<bind> COMMA <binds-cd>)
          (cons $1 $3)]
         [(<bind>)
          (list $1)])
        (<bind>
         [(<any-type> id)
          (make-honu:formal
           (create-src-stx 'honu:formal source-name $1-start-pos $2-end-pos)
           $2 $1)]
         [(USCORE)
          (make-honu:formal
           (create-src-stx 'honu:formal source-name $1-start-pos $1-end-pos)
           #f (make-top-type $1))])
        
        (<function-defn>
         [(<any-type> id O_PAREN <args> C_PAREN <block>)
          (make-honu:function
           (create-src-stx 'honu:function source-name $1-start-pos $6-end-pos)
           $2 $1 $4 $6)])
        
        ;; Type definitions and needed parts
        
        (<iface-defn>
         [(<iface-tag> id <ext-clause> O_CURLY <fmdecs> C_CURLY)
          (make-honu:iface
           (create-src-stx 'honu:iface source-name $1-start-pos $6-end-pos) 
           $2 $3 $5)])
        (<iface-tag>
         [(type)      (void)]
         [(interface) (void)])
        (<type-id>
         [(id)
          (make-iface-type $1 $1)]
         [(Any)
          (make-any-type $1)])
        (<any-type>
         [(<type-id>)
          $1]
         [(void)
          (make-void-type $1)]
         [(int)
          (make-int-type $1)]
         [(bool)
          (make-bool-type $1)]
         [(float)
          (make-float-type $1)]
         [(char)
          (make-char-type $1)]
         [(string)
          (make-string-type $1)]
         [(<tup-type>)
          $1]
         [(<any-type> ARROW <any-type>)
          (make-func-type
           (create-src-stx 'honu:func-type source-name $1-start-pos $3-end-pos)
           $1 $3)])
        (<tup-type>
         [(LT GT)
          (make-tuple-type
           (create-src-stx 'honu:type-tuple source-name $1-start-pos $2-end-pos)
           (list))]
         [(LT <any-type+> GT)
          (if (null? (cdr $2))
              (car $2)
              (make-tuple-type
               (create-src-stx 'honu:type-tuple source-name $1-start-pos $3-end-pos)
               $2))])
        (<any-type+>
         [(<any-type>)
          (list $1)]
         [(<any-type> COMMA <any-type+>)
          (cons $1 $3)])
        (<ext-clause>
         [(extends <type-ids+>)
          $2]
         [(SUBTYPE <type-ids+>)
          $2]
         [()
          '()])
        (<type-ids+>
         [(<type-id> COMMA <type-ids+>)
          (cons $1 $3)]
         [(<type-id>)
          (list $1)])
        (<args>
         [(<args-cd>)
          $1]
         [()
          (list)])
        (<args-cd>
         [(<arg> COMMA <args-cd>)
          (cons $1 $3)]
         [(<arg>)
          (list $1)])
        (<arg>
         [(<any-type> id)
          (make-honu:formal
           (create-src-stx 'honu:formal source-name $1-start-pos $2-end-pos)
           $2 $1)])
        (<fmdecs>
         [(<fdec> <fmdecs>)
          (cons $1 $2)]
         [(<mdec> <fmdecs>)
          (cons $1 $2)]
         [()
          (list)])
        (<fdec>
         [(<any-type> id SEMI_COLON)
          (make-honu:field-decl
           (create-src-stx 'honu:field-decl source-name $1-start-pos $3-end-pos)
           $2 $1)])
        (<mdec>
         [(<any-type> id O_PAREN <mdec-args> C_PAREN SEMI_COLON)
          (make-honu:method-decl
           (create-src-stx 'honu:method-decl source-name $1-start-pos $6-end-pos)
           $2 $1 $4)])
        (<mdec-args>
         [(<mdec-args-cd>)
          $1]
         [()
          (list)])
        (<mdec-args-cd>
         [(<mdec-arg> COMMA <mdec-args-cd>)
          (cons $1 $3)]
         [(<mdec-arg>)
          (list $1)])
        (<mdec-arg>
         [(<any-type>)
          $1]
         [(<any-type> id)
          $1])
        
        (<struct-defn>
         [(struct id <init-slots> COLON <type-id> <imp-clause> O_CURLY <fmidefns> <exports> C_CURLY)
          (make-honu:struct 
           (create-src-stx 'honu:struct source-name $1-start-pos $10-end-pos)
           $2 $5 #f $6 $3 $8 $9)]
         [(final struct id <init-slots> COLON <type-id> <imp-clause> O_CURLY <fmidefns> <exports> C_CURLY)
          (make-honu:struct 
           (create-src-stx 'honu:struct source-name $1-start-pos $11-end-pos)
           $3 $6 #t $7 $4 $9 $10)]
         [(struct id <init-slots> COLON <type-id> extends id <init-slots> COLON <type-id> <imp-clause>
                  O_CURLY <fmidefns> <supernew> <fmidefns> <exports> C_CURLY)
          (make-honu:substruct 
           (create-src-stx 'honu:substruct source-name $1-start-pos $17-end-pos)
           $2 $5 $7 $10 #f $11 $3 $8 $14 $13 $15 $16)]
         [(final struct id <init-slots> COLON <type-id> extends id <init-slots> COLON <type-id> <imp-clause>
                 O_CURLY <fmidefns> <supernew> <fmidefns> <exports> C_CURLY)
          (make-honu:substruct 
           (create-src-stx 'honu:substruct source-name $1-start-pos $18-end-pos)
           $3 $6 $8 $11 #t $12 $4 $9 $15 $14 $16 $17)])
        
        ;; Class and subclass definitions and needed parts
        
        (<class-defn>
         [(class id <init-slots> COLON <type-id> <imp-clause> O_CURLY <fmidefns> <exports> C_CURLY)
          (make-honu:class 
           (create-src-stx 'honu:class source-name $1-start-pos $10-end-pos)
           $2 $5 #f $6 $3 $8 $9)]
         [(final class id <init-slots> COLON <type-id> <imp-clause> O_CURLY <fmidefns> <exports> C_CURLY)
          (make-honu:class
           (create-src-stx 'honu:class source-name $1-start-pos $11-end-pos)
           $3 $6 #t $7 $3 $9 $10)]
         [(class id BINDS id O_PAREN id C_PAREN SEMI_COLON)
          (make-honu:subclass
           (create-src-stx 'honu:subclass source-name $1-start-pos $8-end-pos)
           $2 $6 $4)]
         [(class id <init-slots> COLON <type-id> extends id <init-slots> COLON <type-id> <imp-clause>
                    O_CURLY <fmidefns> <supernew> <fmidefns> <exports> C_CURLY)
          (let ([mixin-name (datum->syntax-object $2 (string->symbol (string-append "$" (symbol->string (syntax-e $2)))) $2)]
                [subclass-stx (create-src-stx 'honu:subclass source-name $1-start-pos $17-end-pos)])
            (list (make-honu:mixin subclass-stx mixin-name $5 $10 #f $11 $3 $8 $14 $13 $15 $16)
                  (make-honu:subclass subclass-stx $2 $7 mixin-name)))]
         [(final class id <init-slots> COLON <type-id> extends id <init-slots> COLON <type-id> <imp-clause>
                 O_CURLY <fmidefns> <supernew> <fmidefns> <exports> C_CURLY)
          (let ([mixin-name (datum->syntax-object $3 (string->symbol (string-append "$" (symbol->string (syntax-e $3)))))]
                [subclass-stx (create-src-stx 'honu:subclass source-name $1-start-pos $18-end-pos)])
            (list (make-honu:mixin subclass-stx mixin-name $6 $10 #t $12 $4 $9 $15 $14 $16 $17)
                  (make-honu:subclass subclass-stx $3 $8 mixin-name)))])
        
        (<imp-clause>
         [(impl <type-ids+>)
          $2]
         [(implements <type-ids+>)
          $2]
         [()
          '()])
        (<init-slots>
         [(O_PAREN <args> C_PAREN)
          $2])
        (<at-clause>
         [(at <type-id>)
          $2]
         [(AT <type-id>)
          $2])
        (<fmidefns>
         [(<fdefn> <fmidefns>)
          (cons $1 $2)]
         [(<mdefn> <fmidefns>)
          (cons $1 $2)]
         [(<initdefn> <fmidefns>)
          (cons $1 $2)]
         [()
          (list)])
        (<fdefn>
         [(<any-type> id BINDS <expr> SEMI_COLON)
          (make-honu:field
           (create-src-stx 'honu:field source-name $1-start-pos $5-end-pos)
           $2 $1 $4)])
        (<mdefn>
         [(<any-type> id O_PAREN <args> C_PAREN <block>)
          (make-honu:method
           (create-src-stx 'honu:method source-name $1-start-pos $6-end-pos)
           $2 $1 $4 $6)])
        (<initdefn>
         [(init <any-type> id SEMI_COLON)
          (make-honu:init-field
           (create-src-stx 'honu:init-field source-name $1-start-pos $4-end-pos)
           $3 $2 #f)]
         [(init <any-type> id BINDS <expr> SEMI_COLON)
          (make-honu:init-field
           (create-src-stx 'honu:init-field source-name $1-start-pos $4-end-pos)
           $3 $2 $5)])
        (<exports>
         [(<expdefn> <exports>)
          (cons $1 $2)]
         [()
          (list)])
        (<expdefn>
         [(export <type-id> COLON <expdecs> SEMI_COLON)
          (make-honu:export
           (create-src-stx 'honu:export source-name $1-start-pos $5-end-pos)
           $2 $4)]
         [(export <type-id> SEMI_COLON)
          (make-honu:export
           (create-src-stx 'honu:export source-name $1-start-pos $3-end-pos)
           $2 (list))])
        (<expdecs>
         [(<expdec> COMMA <expdecs>)
          (cons $1 $3)]
         [(<expdec>)
          (list $1)])
        (<expdec>
         [(id as id)
          (make-honu:exp-bind $1 $3)]
         [(id)
          (make-honu:exp-bind $1 $1)])
        
        ;; Mixin definitions
        
        (<mixin-defn>
         [(mixin id <init-slots> COLON <type-id> <init-slots> ARROW <type-id> <imp-clause>
            O_CURLY <fmidefns> <supernew> <fmidefns> <exports> C_CURLY)
          (make-honu:mixin
           (create-src-stx 'honu:mixin source-name $1-start-pos $15-end-pos)
           $2 $8 $5 #f $9 $3 $6 $12 $11 $13 $14)]
         [(final mixin id <init-slots> COLON <type-id> <init-slots> ARROW <type-id> <imp-clause>
                 O_CURLY <fmidefns> <supernew> <fmidefns> <exports> C_CURLY)
          (make-honu:mixin 
           (create-src-stx 'honu:mixin source-name $1-start-pos $16-end-pos)
           $3 $9 $6 #t $10 $4 $7 $13 $12 $14 $15)])
        (<with-clause>
         [(with <args-cd>)
          $2]
         [()
          (list)])
        (<supernew>
         [(super O_PAREN <newargs> C_PAREN SEMI_COLON)
          (make-honu:super-new 
           (create-src-stx 'honu:super-new source-name $1-start-pos $4-end-pos)
           $3)])
        (<newargs>
         [(<newargs-cd>)
          $1]
         [()
          (list)])
        (<newargs-cd>
         [(<newarg> COMMA <newargs-cd>)
          (cons $1 $3)]
         [(<newarg>)
          (list $1)])
        (<newarg>
         [(id BINDS <expr>)
          (make-honu:name-arg $1 $3)])
        
        ;; Expressions
        
        (<block>
         [(O_CURLY <block-stmts*> C_CURLY)
          (if $2
              $2
              (raise-read-error-with-stx
               "Blocks must have at least one expression"
               (create-src-stx 'honu:block source-name $1-start-pos $3-end-pos)))])
        (<block-stmts*>
         [(<expr> SEMI_COLON <block-stmts*>)
          (if $3
              (make-honu:seq
               (create-src-stx 'honu:seq source-name $1-start-pos $3-end-pos)
               (list $1) $3)
              $1)]
         [(<binding> <block-stmts*>)
          (if $2
              (make-honu:let
               (create-src-stx 'honu:let source-name $1-start-pos $2-end-pos)
               (list $1) $2)
              (raise-read-error-with-stx
               "Block must end with an expression"
               (create-src-stx 'honu:block source-name $1-start-pos $1-end-pos)))]
         [()
          #f])
        (<expr>
         ;; unary operators
         [(selector <expr>)
          (make-honu:select 
            (create-src-stx 'honu:select source-name $1-start-pos $2-end-pos)
            (syntax-e $1) $2)]
         [(MINUS <expr>)
          (prec UMINUS)
          (make-honu:un-op
           (create-src-stx 'honu:un-op source-name $1-start-pos $2-end-pos)
           'minus $1 #f $2)]
         [(NOT <expr>)
          (make-honu:un-op
           (create-src-stx 'honu:un-op source-name $1-start-pos $2-end-pos)
           'not $1 #f $2)]
         ;; binary operators
         [(<expr> OR <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'or $2 #f $1 $3)]
         [(<expr> AND <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'and $2 #f $1 $3)]
         [(<expr> CLS_EQ <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'clseq $2 #f $1 $3)]
         [(<expr> NEQ <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'neq $2 #f $1 $3)]
         [(<expr> EQUALS <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'equal $2 #f $1 $3)]
         [(<expr> LT <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'lt $2 #f $1 $3)]
         [(<expr> LE <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'le $2 #f $1 $3)]
         [(<expr> GT <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'gt $2 #f $1 $3)]
         [(<expr> GE <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'ge $2 #f $1 $3)]
         [(<expr> PLUS <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'plus $2 #f $1 $3)]
         [(<expr> MINUS <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'minus $2 #f $1 $3)]
         [(<expr> TIMES <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'times $2 #f $1 $3)]
         [(<expr> DIV <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'div $2 #f $1 $3)]
         [(<expr> MOD <expr>)
          (make-honu:bin-op
           (create-src-stx 'honu:bin-op source-name $1-start-pos $3-end-pos)
           'mod $2 #f $1 $3)]
         ;; member access
         [(<expr> DOT id)
          (make-honu:member
           (create-src-stx 'honu-member source-name $1-start-pos $3-end-pos)
           $1 #f $3 #f)]
         [(my DOT id)
          (make-honu:member
           (create-src-stx 'honu:member source-name $1-start-pos $3-end-pos)
           'my #f $3 #f)]
         [(<any-type> fun O_PAREN <args> C_PAREN <block>)
          (make-honu:lambda 
           (create-src-stx 'honu:lambda source-name $1-start-pos $6-end-pos)
           $1 $4 $6)]
         [(<literal>)
          $1]
         [(this)
          (make-honu:this $1)]
         [(id)
          (make-honu:var $1 $1)]
         [(<expr> BINDS <expr>)
          (make-honu:assn 
           (create-src-stx 'honu:assn source-name $1-start-pos $3-end-pos)
           $1 $3)]
         ;; application
         [(<expr> <tuple>)
          (make-honu:call
           (create-src-stx 'honu:call source-name $1-start-pos $2-end-pos)
           $1 $2)]
         [(new id COLON <type-id> O_PAREN <newargs> C_PAREN)
          (make-honu:new 
           (create-src-stx 'honu:new source-name $1-start-pos $7-end-pos)
           $2 $4 $6)]
         [(new id O_PAREN <newargs> C_PAREN)
          (make-honu:new 
           (create-src-stx 'honu:new source-name $1-start-pos $5-end-pos)
           $2 #f $4)]
         [(<expr> COLON <type-id>)
          (make-honu:cast 
           (create-src-stx 'honu:cast source-name $1-start-pos $3-end-pos)
           $1 $3)]
         [(<expr> isa <type-id>)
          (make-honu:isa 
           (create-src-stx 'honu:isa source-name $1-start-pos $3-end-pos)
           $1 $3)]
         [(if <expr> <block>)
          (make-honu:if
           (create-src-stx 'honu:if source-name $1-start-pos $3-end-pos)
           $2 $3 #f)]
         [(if <expr> <block> else <block>)
          (make-honu:if 
           (create-src-stx 'honu:if source-name $1-start-pos $5-end-pos)
           $2 $3 $5)]
         [(cond O_CURLY <cond-clauses> C_CURLY)
          (make-honu:cond
           (create-src-stx 'honu:cond source-name $1-start-pos $4-end-pos)
           (car $3) (cadr $3))]
         [(while <expr> <block>)
          (make-honu:while
           (create-src-stx 'honu:while source-name $1-start-pos $3-end-pos)
           $2 $3)]
         [(<tuple>)
          $1]
         [(return <expr>)
          (make-honu:return
           (create-src-stx 'honu:return source-name $1-start-pos $2-end-pos)
           $2)]         
         [(<block>)
          $1])
        (<tuple>
         [(O_PAREN C_PAREN)
          (make-honu:tuple
           (create-src-stx 'honu:tuple source-name $1-start-pos $2-end-pos)
           (list))]
         [(O_PAREN <exprs-cd> C_PAREN)
          (if (null? (cdr $2))
              (car $2)
              (make-honu:tuple
               (create-src-stx 'honu:tuple source-name $1-start-pos $3-end-pos)
               $2))])
        (<literal>
         [(true)
          (make-honu:lit $1 (make-bool-type $1)   $1)]
         [(false)
          (make-honu:lit $1 (make-bool-type $1)   $1)]
         [(integer)
          (make-honu:lit $1 (make-int-type $1)    $1)]
         [(floatnum)
          (make-honu:lit $1 (make-float-type $1)  $1)]
         [(character)
          (make-honu:lit $1 (make-char-type $1)   $1)]
         [(string-lit)
          (make-honu:lit $1 (make-string-type $1) $1)]
         [(null)
          (make-honu:lit $1 (make-null-type $1)  (datum->syntax-object $1 'null-obj $1))])
        (<cond-clauses>
         [(<expr> THICK_ARROW <expr> SEMI_COLON <cond-clauses>)
          (list (cons (make-honu:cond-clause
                       (create-src-stx 'honu:cond-clause source-name $1-start-pos $4-end-pos)
                       $1 $3)
                      (car $5))
                (cadr $5))]
         [(<expr> THICK_ARROW <expr> SEMI_COLON)
          (list (list (make-honu:cond-clause
                       (create-src-stx 'honu:cond-clause source-name $1-start-pos $4-end-pos)
                       $1 $3))
                #f)]
         [(else <expr> SEMI_COLON)
          (list '() $2)])
        (<exprs-cd>
         [(<expr> COMMA <exprs-cd>)
          (cons $1 $3)]
         [(<expr>)
          (list $1)])
        (<binding>
         [(<bind> BINDS <expr> SEMI_COLON)
          (make-honu:binding
           (create-src-stx 'honu:binding source-name $1-start-pos $4-end-pos)
           (list (honu:formal-name $1)) (list (honu:formal-type $1)) $3)]
         [(O_PAREN <binds-cd> C_PAREN BINDS <expr> SEMI_COLON)
          (let-values ([(names types) (map-two-values (lambda (f)
                                                        (values (honu:formal-name f)
                                                                (honu:formal-type f)))
                                                      $2)])
            (make-honu:binding
             (create-src-stx 'honu:binding source-name $1-start-pos $6-end-pos)
             names types $5))])
        (<interact>
         [(<binding-defn>)
          $1]
         [(<expr>)
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
          (list)
          (let loop ((filenames filenames)
                     (defns     '()))
            (let ((parsed (parse-file 
                           (simplify-path
                            (path->complete-path (car filenames))))))
              (if (null? (cdr filenames))
                  (append parsed defns)
                  (loop (cdr filenames)
                        (append parsed defns))))))))
  
  (define (parse-group-file dirname filename)
    (let ([filenames (call-with-input-file
                         (string-append dirname "/" filename)
                       read-cm)])
      (if (null? filenames)
          (list)
          (let loop ((filenames filenames)
                     (defns     '()))
            (let ((parsed (parse-file (string-append dirname "/"
                                                     (car filenames)))))
              (if (null? (cdr filenames))
                  (append parsed defns)
                  (loop (cdr filenames)
                        (append parsed defns))))))))
  
  (provide parse-file parse-port parse-stdin parse-string parse-group parse-group-file parse-interaction)
  )
