(module beginner-parser mzscheme
  
  (require "general-parsing.ss"
           "lexer.ss"
           "../ast.ss"
           "../parameters.ss")
  
  (require (lib "yacc.ss" "parser-tools")
           (all-except (lib "lex.ss" "parser-tools") input-port)           
           (lib "readerr.ss" "syntax")
           (prefix class: (lib "class.ss")))
  
  ;(require (lib "build-grammar.ss" "tester"))
  
  #;(define-syntax testing-parser
    (syntax-rules ()
      ((_ parse-info ...) (parser parse-info ...))))
    
  (provide parse-beginner parse-beginner-interactions parse-beginner-expression parse-beginner-type)
  ;(provide beginner-grammar)
  
  (define parsers
    (parser
     ;(debug "out2.ss")
     (start CompilationUnit BeginnerInteractions Expression Type)
     (tokens java-vals special-toks Keywords Separators EmptyLiterals Operators ExtraKeywords)
     ;(terminals val-tokens special-tokens keyword-tokens separator-tokens literal-tokens operator-tokens)
     (error (lambda (tok-ok name val start-pos end-pos)
              (if ((determine-error))
                  (raise-read-error (format "Parse error near <~a:~a>" name val)
                                    (file-path)
                                    (position-line start-pos)
                                    (position-col start-pos)
                                    (+ (position-offset start-pos) (interactions-offset))
                                    (- (position-offset end-pos)
                                       (position-offset start-pos))))))
     
     (end EOF)
     (src-pos)
     
     (grammar
      
      ;; 19.3
      (Literal
       [(INTEGER_LIT) (make-literal 'int (build-src 1) $1)]
       [(LONG_LIT) (make-literal 'long (build-src 1) $1)]
       [(FLOAT_LIT) (make-literal 'float (build-src 1) $1)]
       [(DOUBLE_LIT) (make-literal 'double (build-src 1)  $1)]
       [(TRUE_LIT) (make-literal 'boolean (build-src 1) #t)]
       [(FALSE_LIT) (make-literal 'boolean (build-src 1) #f)]
       [(CHAR_LIT) (make-literal 'char (build-src 1) $1)]
       [(STRING_LIT) (make-literal 'string 
                                   (make-src (position-line $1-start-pos)
                                             (position-col $1-start-pos)
                                             (+ (position-offset $1-start-pos) (interactions-offset))
                                             (- (position-offset (cadr $1)) (position-offset $1-start-pos))
                                             (file-path))
                                   (car $1))]
       [(IMAGE_SPECIAL) (make-literal 'image (build-src 1) $1)])
      
      ;; 19.4
      (Type
       [(PrimitiveType) $1]
       [(ReferenceType) $1])
      
      (PrimitiveType
       [(NumericType) $1]
       [(boolean) (make-type-spec 'boolean 0 (build-src 1))])
      
      (NumericType
       [(IntegralType) $1]
       [(FloatingPointType) $1])
      
      (IntegralType
       [(byte) (make-type-spec 'byte 0 (build-src 1))]
       [(short) (make-type-spec 'short 0 (build-src 1))]
       [(int) (make-type-spec 'int 0 (build-src 1))]
       [(long) (make-type-spec 'long 0 (build-src 1))]
       [(char) (make-type-spec 'char 0 (build-src 1))])
      
      (FloatingPointType
       [(float) (make-type-spec 'float 0 (build-src 1))]
       [(double) (make-type-spec 'double 0 (build-src 1))])
      
      (ReferenceType
       [(IDENTIFIER) (make-type-spec 
                      (make-name (make-id $1 (build-src 1)) null (build-src 1)) 0 (build-src 1))]
       )
      
      (ClassOrInterfaceType
       [(IDENTIFIER) (make-name (make-id $1 (build-src 1)) null (build-src 1))])
            
      (ClassType
       [(ClassOrInterfaceType) $1])
      
      (InterfaceType
       [(ClassOrInterfaceType) $1])
      
      ;;19.5
      (Name
       [(IDENTIFIER) (make-name (make-id $1 (build-src 1)) null (build-src 1))]
       [(Name PERIOD IDENTIFIER)
	(make-name (make-id $3 (build-src 3 3)) 
                   (append (name-path $1) (list (name-id $1)))
                   (build-src 3))])
      ;; 19.6
      (CompilationUnit
       [(ImportDeclarations TypeDeclarations) (make-package #f (reverse $1) (reverse $2))]
       [(ImportDeclarations) (make-package #f (reverse $1) null)]
       [(TypeDeclarations) (make-package #f null (reverse $1))]
       [() (make-package #f null null)])
      
      (TypeDeclarations
       [(TypeDeclaration) (if $1 (list $1) null)]
       [(TypeDeclarations TypeDeclaration) (if $2 (cons $2 $1) $1)])
      
      (TypeDeclaration
       [(ClassDeclaration) $1]
       [(InterfaceDeclaration) $1]
       #;[(INTERACTIONS_BOX) $1]
       [(EXAMPLE) $1]
       #;[(CLASS_BOX) (parse-class-box $1 (build-src 1) 'beginner)]
       [(TEST_SUITE) $1]
       [(SEMI_COLON) #f])
      
      ;; 19.7
      #;(Modifiers
         [(Modifier) (list $1)])
      
        #;(Modifier
           [(abstract) (make-modifier 'abstract (build-src 1))])

      (ImportDeclarations
       [(ImportDeclaration) (list $1)]
       [(ImportDeclarations ImportDeclaration) (cons $2 $1)])
      
      (ImportDeclaration
       [(import Name SEMI_COLON) (make-import $2 #f (build-src 1) (build-src 3) (file-path))]
       [(import Name PERIOD * SEMI_COLON)
	(make-import $2 #t (build-src 1) (build-src 5) (file-path))])
      
      ;; 19.8.1
      (ClassDeclaration
       [(class IDENTIFIER Interface ClassBody)
	(make-class-def (make-header (make-id $2 (build-src 2 2))
                                     (list (make-modifier 'public #f))
                                     null $3 null (build-src 3))
                        $4
                        (build-src 1)
                        (build-src 4)
                        (file-path)
                        'beginner
                        null 'top null)])

      (Interface
       [() null]
       [(implements InterfaceType) (list $2)])
      
      (ClassBody
       [(O_BRACE ClassBodyDeclarations C_BRACE) (reverse $2)])
      
      (ClassBodyDeclarations
       [() null]
       [(ClassBodyDeclarations ClassBodyDeclaration)
        (cond
          ((not $2) $1)
          ((list? $2) (append $2 $1))
          (else (cons $2 $1)))])
      
      (ClassBodyDeclaration
       [(ClassMemberDeclaration) $1]
       [(ConstructorDeclaration) $1]
       [(SEMI_COLON) #f])
      
      (ClassMemberDeclaration
       [(FieldDeclaration) $1]
       [(MethodDeclaration) $1])
      
      ;; 19.8.2
      (FieldDeclaration
       [(Type VariableDeclaratorId SEMI_COLON)
        (build-field-decl (list (make-modifier 'public #f)) $1 $2)]
       [(Type VariableDeclaratorId = Expression SEMI_COLON)
        (build-field-decl (list (make-modifier 'public #f)) $1
                          (make-var-init $2 $4 (build-src 2 4)))])

      (InteractFieldDeclaration
       [(Type VariableDeclaratorId = Expression SEMI_COLON)
        (build-field-decl (list (make-modifier 'public #f)) $1 
                          (make-var-init $2 $4 (build-src 2 4)))])
      
      (VariableDeclaratorId
       [(IDENTIFIER)
	(make-var-decl (make-id $1 (build-src 1)) null (make-type-spec #f 0 (build-src 1)) #f (build-src 1))])			
      
      ;; 19.8.3
      (MethodDeclaration
       [(MethodHeader Block) (make-method (method-modifiers $1)
                                          (method-type $1)
                                          (method-type-parms $1)
                                          (method-name $1)
                                          (method-parms $1)
                                          (method-throws $1)
                                          $2
                                          #t
                                          #f
                                          (build-src 2))]
       )
      
      (MethodHeader
       [(Type MethodDeclarator) (construct-method-header (list (make-modifier 'public #f)) null $1 $2 null)])
      
      (MethodDeclarator
       [(IDENTIFIER O_PAREN FormalParameterList C_PAREN) (list (make-id $1 (build-src 1)) (reverse $3) 0)]
       [(IDENTIFIER O_PAREN C_PAREN) (list (make-id $1 (build-src 1)) null 0)])
      
      (FormalParameterList
       [(FormalParameter) (list $1)]
       [(FormalParameterList COMMA FormalParameter) (cons $3 $1)])
      
      (FormalParameter
       [(Type VariableDeclaratorId) (build-field-decl null $1 $2)])
      
      ;; 19.8.5      
      (ConstructorDeclaration
       [(ConstructorDeclarator ConstructorBody)
	(make-method (list (make-modifier 'public #f)) (make-type-spec 'ctor 0 (build-src 2)) null (car $1)
                     (cadr $1) null $2 #t #f (build-src 2))])
      
      (ConstructorDeclarator
       [(IDENTIFIER O_PAREN FormalParameterList C_PAREN) (list (make-id $1 (build-src 1)) (reverse $3))]
       [(IDENTIFIER O_PAREN C_PAREN) (list (make-id $1 (build-src 1)) null)])
      
      (ConstructorBody
       #;[(O_BRACE ExplicitConstructorInvocation BlockStatements C_BRACE)
	(make-block (cons $2 (reverse $3)) (build-src 4))]
      #; [(O_BRACE ExplicitConstructorInvocation C_BRACE)
	(make-block (list $2) (build-src 3))]
       [(O_BRACE BlockStatements C_BRACE)
	(make-block 
	 (cons (make-call #f (build-src 3) #f (make-special-name #f #f "super") null #f)
	       (reverse $2))
	 (build-src 3))]
       [(O_BRACE C_BRACE) 
        (make-block (cons (make-call #f (build-src 2) #f (make-special-name #f #f "super") null #f)
                          null) (build-src 2))])
 
      #;(ExplicitConstructorInvocation
       [(super O_PAREN ArgumentList C_PAREN SEMI_COLON)
	(make-call #f (build-src 5) 
		       #f (make-special-name #f (build-src 1) "super") (reverse $3) #f)]
       [(super O_PAREN C_PAREN SEMI_COLON)
	(make-call #f (build-src 4) 
		       #f (make-special-name #f (build-src 1) "super") null #f)])
      
      ;; 19.9.1
      
      (InterfaceDeclaration
       [(interface IDENTIFIER InterfaceBody)
	(make-interface-def (make-header (make-id $2 (build-src 2 2))(list (make-modifier 'public #f))
                                         null null null (build-src 2))
                                $3
                                (build-src 1)
                                (build-src 3)
                                (file-path)
                                'beginner
                                null 'top null)])
      
      (InterfaceBody
       [(O_BRACE InterfaceMemberDeclarations C_BRACE) $2])
      
      (InterfaceMemberDeclarations
       [() null]
       [(InterfaceMemberDeclarations InterfaceMemberDeclaration)
        (cond
          ((not $2) $1)
          ((list? $2) (append $2 $1))
          (else (cons $2 $1)))])
      
      (InterfaceMemberDeclaration
       [(AbstractMethodDeclaration) $1]
       [(SEMI_COLON) #f])
      
      (AbstractMethodDeclaration
       [(MethodHeader SEMI_COLON) $1])
            
      ;; 19.11
      (Block
       [(O_BRACE Statement C_BRACE) (make-block (list $2) (build-src 3))])
      
      (BlockStatements
       [(Assignment SEMI_COLON) (if (list? $1) $1 (list $1))]
       [(BlockStatements Assignment SEMI_COLON) (if (list? $2) (append (reverse $2) $1) (cons $2 $1))])
      
      (BeginnerInteractions
       [(Statement) $1]
       [(Expression) $1]
       [(InteractFieldDeclaration) $1]
       [() null])
      
      (Statement
       [(StatementWithoutTrailingSubstatement) $1]
       [(IfThenElseStatement) $1])
      
      (StatementNoShortIf
       [(StatementWithoutTrailingSubstatement) $1]
       [(IfThenElseStatementNoShortIf) $1])
      
      (StatementWithoutTrailingSubstatement
       [(Assignment SEMI_COLON) $1]
       [(ReturnStatement) $1])
      
      (IfThenElseStatement
       [(if O_PAREN Expression C_PAREN O_BRACE StatementNoShortIf C_BRACE else O_BRACE Statement C_BRACE)
	(make-ifS $3 $6 $10 (build-src 1) (build-src 11))])
      
      (IfThenElseStatementNoShortIf
       [(if O_PAREN Expression C_PAREN O_BRACE StatementNoShortIf C_BRACE else O_BRACE StatementNoShortIf C_BRACE)
	(make-ifS $3 $6 $10 (build-src 1) (build-src 11))])
      
      (ReturnStatement
       [(return Expression SEMI_COLON) (make-return $2 #f #t (build-src 3))])
      
      ;; 19.12
      (Primary
       [(Literal) $1]
       [(this) (make-special-name #f (build-src 1) "this")]
       [(O_PAREN Expression C_PAREN) $2]
       [(ClassInstanceCreationExpression) $1]
       [(MethodInvocation) $1]
       [(FieldAccess) $1])
      
      (ClassInstanceCreationExpression
       [(new ClassOrInterfaceType O_PAREN ArgumentList C_PAREN)
	(make-class-alloc #f (build-src 5) $2 (reverse $4) #f #f #f)]
       [(new ClassOrInterfaceType O_PAREN C_PAREN) 
	(make-class-alloc #f (build-src 4) $2 null #f #f #f)])
      
      (ArgumentList
       [(Expression) (list $1)]
       [(ArgumentList COMMA Expression) (cons $3 $1)])
      
      (MethodInvocation
       [(Name O_PAREN ArgumentList C_PAREN) (build-name-call $1 (reverse $3) (build-src 4))]
       [(Name O_PAREN C_PAREN) (build-name-call $1 null (build-src 3))]
       [(Primary PERIOD IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-call #f (build-src 6) $1 (make-id $3 (build-src 3 3)) (reverse $5) #f)]
       [(Primary PERIOD IDENTIFIER O_PAREN C_PAREN)
        (make-call #f (build-src 5) $1 (make-id $3 (build-src 3 3)) null #f)]
       #;[(super PERIOD IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-call #f (build-src 6) 
                   (make-special-name #f (build-src 1) "super") 
                   (make-id $3 (build-src 3 3)) (reverse $5) #f)]
      #; [(super PERIOD IDENTIFIER O_PAREN C_PAREN)
        (make-call #f (build-src 5) 
                   (make-special-name #f (build-src 1) "super") 
                   (make-id $3 (build-src 3 3)) null #f)])
      
      (FieldAccess
       [(Primary PERIOD IDENTIFIER) 
        (make-access #f (build-src 3) (make-field-access $1 
                                                         (make-id $3 (build-src 3 3)) #f))])
      
      (PostfixExpression
       [(Primary) $1]
       [(Name) (name->access $1)])
      
      (UnaryExpression
       [(PostfixExpression) $1]
       [(- UnaryExpression) (make-unary #f (build-src 2) '- $2 (build-src 1))]
       [(~ UnaryExpression) (make-unary #f (build-src 2) '~ $2 (build-src 1))]
       [(! UnaryExpression) (make-unary #f (build-src 2) '! $2 (build-src 1))])
      
      (MultiplicativeExpression
       [(UnaryExpression) $1]
       [(MultiplicativeExpression * UnaryExpression)
        (make-bin-op #f (build-src 3) '* $1 $3 (build-src 2 2))]
       [(MultiplicativeExpression / UnaryExpression)
	(make-bin-op #f (build-src 3) '/ $1 $3 (build-src 2 2))]
       [(MultiplicativeExpression % UnaryExpression)
	(make-bin-op #f (build-src 3) '% $1 $3 (build-src 2 2))])
      
      (AdditiveExpression
       [(MultiplicativeExpression) $1]
       [(AdditiveExpression + MultiplicativeExpression)
	(make-bin-op #f (build-src 3) '+ $1 $3 (build-src 2 2))]
       [(AdditiveExpression - MultiplicativeExpression)
	(make-bin-op #f (build-src 3) '- $1 $3 (build-src 2 2))])
      
      (ShiftExpression
       [(AdditiveExpression) $1]
       [(ShiftExpression << AdditiveExpression)
	(make-bin-op #f (build-src 3) '<< $1 $3 (build-src 2 2))]
       [(ShiftExpression >> AdditiveExpression)
	(make-bin-op #f (build-src 3) '>> $1 $3 (build-src 2 2))]	
       [(ShiftExpression >>> AdditiveExpression)
	(make-bin-op #f (build-src 3) '>>> $1 $3 (build-src 2 2))])
      
      
      (RelationalExpression
       [(ShiftExpression) $1]
       [(ShiftExpression < ShiftExpression)
        (make-bin-op #f (build-src 3) '< $1 $3 (build-src 2 2))]		
       [(RelationalExpression > ShiftExpression)
	(make-bin-op #f (build-src 3) '> $1 $3 (build-src 2 2))]	
       [(RelationalExpression <= ShiftExpression)
	(make-bin-op #f (build-src 3) '<= $1 $3 (build-src 2 2))]	
       [(RelationalExpression >= ShiftExpression)
	(make-bin-op #f (build-src 3) '>= $1 $3 (build-src 2 2))])
      
      (EqualityExpression
       [(RelationalExpression) $1]
       [(EqualityExpression == RelationalExpression)
	(make-bin-op #f (build-src 3) '== $1 $3 (build-src 2 2))]	
       [(EqualityExpression != RelationalExpression)
	(make-bin-op #f (build-src 3) '!= $1 $3 (build-src 2 2))])
      
      (AndExpression
       [(EqualityExpression) $1]
       [(AndExpression & EqualityExpression)
	(make-bin-op #f (build-src 3) '& $1 $3 (build-src 2 2))])
      
      
      (ExclusiveOrExpression
       [(AndExpression) $1]
       [(ExclusiveOrExpression ^ AndExpression)
	(make-bin-op #f (build-src 3) '^ $1 $3 (build-src 2 2))])
      
      
      (InclusiveOrExpression
       [(ExclusiveOrExpression) $1]
       [(InclusiveOrExpression PIPE ExclusiveOrExpression)
	(make-bin-op #f (build-src 3) 'or $1 $3 (build-src 2 2))])
      
      (ConditionalAndExpression
       [(InclusiveOrExpression) $1]
       [(ConditionalAndExpression && InclusiveOrExpression)
	(make-bin-op #f (build-src 3) '&& $1 $3 (build-src 2 2))])
      
      (ConditionalOrExpression
       [(ConditionalAndExpression) $1]
       [(ConditionalOrExpression OR ConditionalAndExpression)
	(make-bin-op #f (build-src 3) 'oror $1 $3 (build-src 2 2))])

      (CheckExpression
       [(ConditionalOrExpression) $1]
       [(check ConditionalOrExpression expect ConditionalOrExpression) 
        (make-check-expect #f (build-src 4) $2 $4 #f (build-src 2 4))]
       [(check ConditionalOrExpression expect ConditionalOrExpression within ConditionalOrExpression) 
        (make-check-expect #f (build-src 6) $2 $4 $6 (build-src 2 4))])
      
      (Assignment
       [(LeftHandSide AssignmentOperator #;CheckExpression IDENTIFIER)
	(make-assignment #f (build-src 3) $1 $2 #;$3 
                         (make-access #f (build-src 3 3)
                                      (make-local-access 
                                       (make-id $3 (build-src 3 3)))) (build-src 2 2))])
      
      (LeftHandSide
       [(Name) (name->access $1)]
       [(FieldAccess) $1])
            
      (AssignmentOperator
       [(=) '=])            
      
      (Expression
       ;[(ConditionalOrExpression) $1]
       [(CheckExpression) $1])
      
      )))
  
  ;(define beginner-grammar (cadr parsers))
  ;(set! parsers (car parsers))
  
  (define parse-beginner (car parsers))
  (define parse-beginner-interactions (cadr parsers))
  (define parse-beginner-expression (caddr parsers))
  (define parse-beginner-type (cadddr parsers))
  )