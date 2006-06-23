(module full-parser mzscheme

  (require "general-parsing.ss"
           "lexer.ss"
           "../ast.ss"
           "../parameters.ss")
  
  (require (lib "yacc.ss" "parser-tools")
           (all-except (lib "lex.ss" "parser-tools") input-port)
           (lib "readerr.ss" "syntax"))
  
  (provide parse-full parse-full-interactions parse-full-expression parse-full-type)
    
  ;; A parser for Java based on the LALR(1) grammar in the Java
  ;; Language Specification First Edition, extended to support Java 1.1
  ;; Bugs: Does not handle @deprecated in documentation comments, t.class where
  ;;       t is an array type
  
  (define parsers
    (parser
     (start CompilationUnit Interactions VariableInitializer Type)
     ;;(debug "parser.output")
     (tokens java-vals special-toks Keywords ExtraKeywords Separators EmptyLiterals Operators)
     (error (lambda (tok-ok name val start-pos end-pos)
              (raise-read-error (format "Parse error near <~a:~a>" name val)
                                (file-path)
                                (position-line start-pos)
                                (position-col start-pos)
                                (+ (position-offset start-pos) (interactions-offset))
                                (- (position-offset end-pos)
                                   (position-offset start-pos)))))

     (end EOF)
     (src-pos)
     
     (grammar
      
      ;; 19.3
      (Literal
       [(INTEGER_LIT) (make-literal 'int (build-src 1) $1)]
       [(HEX_LIT) (make-literal 'int (build-src 1) $1)]
       [(OCT_LIT) (make-literal 'int (build-src 1) $1)]
       [(LONG_LIT) (make-literal 'long (build-src 1) $1)]
       [(HEXL_LIT) (make-literal 'long (build-src 1) $1)]
       [(OCTL_LIT) (make-literal 'long (build-src 1) $1)]
       [(FLOAT_LIT) (make-literal 'float (build-src 1) $1)]
       [(DOUBLE_LIT) (make-literal 'double (build-src 1)  $1)]
       [(TRUE_LIT) (make-literal 'boolean (build-src 1) #t)]
       [(FALSE_LIT) (make-literal 'boolean (build-src 1) #f)]
       [(CHAR_LIT) (make-literal 'char (build-src 1) $1)]
       [(STRING_LIT) (make-literal 'string (make-src (position-line $1-start-pos)
                                                     (position-col $1-start-pos)
                                                     (+ (position-offset $1-start-pos) (interactions-offset))
                                                     (- (position-offset (cadr $1)) (position-offset $1-start-pos))
                                                     (file-path))
                                   (car $1))]
       [(NULL_LIT) (make-literal 'null (build-src 1) #f)]
       [(IMAGE_SPECIAL) (make-literal 'image (build-src 1) $1)])
      
      ;; 19.4
      (Type
       [(PrimitiveType) $1]
       [(ReferenceType) $1]
       [(dynamic) (make-type-spec 'dynamic 0 (build-src 1))])
      
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
       [(Name) (make-type-spec $1 0 (build-src 1))]
       [(ArrayType) $1]
       )
            
      (ClassOrInterfaceType
       [(Name) $1])
      
      (ClassType
       [(ClassOrInterfaceType) $1])
      
      (InterfaceType
       [(ClassOrInterfaceType) $1])
      
      (ArrayType
       [(PrimitiveType Dims) (make-type-spec (type-spec-name $1) $2 (build-src 2))]
       [(Name Dims) (make-type-spec $1 $2 (build-src 2))])

      ;;19.5
      (Name
       [(IDENTIFIER) (make-name (make-id $1 (build-src 1)) null (build-src 1))]
       [(Name PERIOD IDENTIFIER)
	(make-name (make-id $3 (build-src 3 3)) 
		       (append (name-path $1) (list (name-id $1)))
		       (build-src 3))])
      ;; 19.6
      (CompilationUnit
       [(PackageDeclaration ImportDeclarations TypeDeclarations) 
        (make-package $1 (reverse $2) (reverse $3))]
       [(ImportDeclarations TypeDeclarations) (make-package #f (reverse $1) (reverse $2))]
       [(PackageDeclaration TypeDeclarations) (make-package $1 null (reverse $2))]
       [(PackageDeclaration ImportDeclarations) (make-package $1 (reverse $2) null)]
       [(PackageDeclaration) (make-package $1 null null)]
       [(ImportDeclarations) (make-package #f (reverse $1) null)]
       [(TypeDeclarations) (make-package #f null (reverse $1))]
       [() (make-package #f null null)])

      (Interactions
       [(Statement) $1]
       [(Expression) $1]
       [(FieldDeclaration) $1]
       [() null])
      
      (ImportDeclarations
       [(ImportDeclaration) (list $1)]
       [(ImportDeclarations ImportDeclaration) (cons $2 $1)])

      (TypeDeclarations
       [(TypeDeclaration) (if $1
                              (list $1)
                              null)]
       [(TypeDeclarations TypeDeclaration) (if $2
                                               (cons $2 $1)
                                               $1)])
      
      (PackageDeclaration
       [(package Name SEMI_COLON) $2])
      
      (ImportDeclaration
       [(SingleTypeImportDeclaration) $1]
       [(TypeImportOnDemandDeclaration) $1])
      
      (SingleTypeImportDeclaration
       [(import Name SEMI_COLON) (make-import $2 #f (build-src 1) (build-src 3) (file-path))])
      
      (TypeImportOnDemandDeclaration
       [(import Name PERIOD * SEMI_COLON)
	(make-import $2 #t (build-src 1) (build-src 5) (file-path))])
      
      (TypeDeclaration
       [(ClassDeclaration) $1]
       [(InterfaceDeclaration) $1]
       [(TestDeclaration) $1]
       [(TEST_SUITE) $1]
       [(EXAMPLE) $1]
       [(SEMI_COLON) #f])
      
      ;; 19.7
      (Modifiers
       [(Modifier) (list $1)]
       [(Modifiers Modifier) (cons $2 $1)])
      
      (Modifier
       [(public) (make-modifier 'public (build-src 1))]
       [(protected) (make-modifier 'protected (build-src 1))]
       [(private) (make-modifier 'private (build-src 1))]
       [(static) (make-modifier 'static (build-src 1))]
       [(abstract) (make-modifier 'abstract (build-src 1))]
       [(final) (make-modifier 'final (build-src 1))]
       [(strictfp) (make-modifier 'strictfp (build-src 1))]
       [(native) (make-modifier 'native (build-src 1))]
       [(synchronized) (make-modifier 'synchronized (build-src 1))]
       [(transient) (make-modifier 'transient (build-src 1))]
       [(volatile) (make-modifier 'volatile (build-src 1))])
      
      ;; 19.8.1
      (ClassDeclaration
       [(Modifiers class IDENTIFIER Super Interfaces ClassBody)
	(make-class-def (make-header (make-id $3 (build-src 3 3)) $1 $4 $5 null (build-src 5))
                            $6
                            (build-src 2 2)
                            (build-src 6)
                            (file-path)
                            'full
                            null 'top null)]
       [(class IDENTIFIER Super Interfaces ClassBody)
	(make-class-def (make-header (make-id $2 (build-src 2 2)) null $3 $4 null (build-src 4))
                            $5
                            (build-src 1)
                            (build-src 5)
                            (file-path)
                            'full
                            null 'top null)])
      
      (Super
       [() null]
       [(extends ClassType) (list $2)])
      
      (Interfaces
       [() null]
       [(implements InterfaceTypeList) $2])
      
      (InterfaceTypeList
       [(InterfaceType) (list $1)]
       [(InterfaceTypeList COMMA InterfaceType) (cons $3 $1)])
      
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
       ;; 1.1
       [(ClassDeclaration) (begin (set-def-kind! $1 'member) $1)]
       ;; 1.1
       [(InterfaceDeclaration) (begin (set-def-kind! $1 'member) $1)]
       [(StaticInitializer) $1]
       [(ConstructorDeclaration) $1]
       [(SEMI_COLON) #f])
      
      (ClassMemberDeclaration
       [(FieldDeclaration) $1]
       [(MethodDeclaration) $1])
      
      ;; 19.8.2
      (FieldDeclaration
       [(Modifiers Type VariableDeclarators SEMI_COLON)
        (map (lambda (d) (build-field-decl $1 $2 d)) (reverse $3))]
       [(Type VariableDeclarators SEMI_COLON)
        (map (lambda (d) (build-field-decl null $1 d)) (reverse $2))])

      (VariableDeclarators
       [(VariableDeclarator) (list $1)]
       [(VariableDeclarators COMMA VariableDeclarator) (cons $3 $1)])
      
      (VariableDeclarator
       [(VariableDeclaratorId) $1]
       [(VariableDeclaratorId = VariableInitializer)
        (make-var-init $1 $3 (build-src 3))])

      (VariableDeclaratorId
       [(IDENTIFIER)
	(make-var-decl (make-id $1 (build-src 1)) null (make-type-spec #f 0 (build-src 1)) #f (build-src 1))]
       [(IDENTIFIER Dims)
	(make-var-decl (make-id $1 (build-src 1)) null (make-type-spec #f $2 (build-src 2)) #f (build-src 2))])
			
      (VariableInitializer
       [(Expression) $1]
       [(ArrayInitializer) $1])

      ;; 19.8.3
      (MethodDeclaration
       [(MethodHeader MethodBody) (make-method (method-modifiers $1)
                                               (method-type $1)
                                               (method-type-parms $1)
                                               (method-name $1)
                                               (method-parms $1)
                                               (method-throws $1)
                                               $2
                                               #f
                                               #f
                                               (build-src 2))])

      (MethodHeader
       [(Modifiers Type MethodDeclarator Throws) (construct-method-header $1 null $2 $3 $4)]
       [(Modifiers void MethodDeclarator Throws)
	(construct-method-header $1 
				 null 
				 (make-type-spec 'void 0 (build-src 2 2)) 
				 $3 
				 $4)]
       [(Type MethodDeclarator Throws) (construct-method-header null null $1 $2 $3)]
       [(void MethodDeclarator Throws)
	(construct-method-header null
				 null 
				 (make-type-spec 'void 0 (build-src 2 2))
				 $2 
				 $3)])
      
      (MethodDeclarator
       [(IDENTIFIER O_PAREN FormalParameterList C_PAREN) (list (make-id $1 (build-src 1)) (reverse $3) 0)]
       [(IDENTIFIER O_PAREN C_PAREN) (list (make-id $1 (build-src 1)) null 0)]
       [(IDENTIFIER O_PAREN FormalParameterList C_PAREN Dims) (list (make-id $1 (build-src 1)) (reverse $3) $5)]
       [(IDENTIFIER O_PAREN C_PAREN Dims) (list (make-id $1 (build-src 1)) null $4)])
      
      (FormalParameterList
       [(FormalParameter) (list $1)]
       [(FormalParameterList COMMA FormalParameter) (cons $3 $1)])
      
      (FormalParameter
       [(Type VariableDeclaratorId) (build-field-decl null $1 $2)]
       ;; 1.1
       [(final Type VariableDeclaratorId) (build-field-decl '(final) $2 $3)])
      
      (Throws
       [() null]
       [(throws ClassTypeList) $2])
      
      (ClassTypeList
       [(ClassType) (list $1)]
       [(ClassTypeList COMMA ClassType) (cons $3 $1)])
      
      (MethodBody
       [(Block) $1]
       [(SEMI_COLON) #f])
      
      ;; 19.8.4
      
      (StaticInitializer
       [(static Block) (make-initialize #t $2 (build-src 2))]
       ;; 1.1
       [(Block) (make-initialize #f $1 (build-src 1))])
      
      ;; 19.8.5
      
      (ConstructorDeclaration
       [(Modifiers ConstructorDeclarator Throws ConstructorBody)
	(make-method $1 (make-type-spec 'ctor 0 (build-src 4)) null (car $2) 
                         (cadr $2) $3 $4 #f #f (build-src 4))]
       [(ConstructorDeclarator Throws ConstructorBody)
	(make-method null (make-type-spec 'ctor 0 (build-src 3)) null (car $1)
                         (cadr $1) $2 $3 #f #f (build-src 3))])
      
      (ConstructorDeclarator
       [(IDENTIFIER O_PAREN FormalParameterList C_PAREN) (list (make-id $1 (build-src 1)) (reverse $3))]
       [(IDENTIFIER O_PAREN C_PAREN) (list (make-id $1 (build-src 1)) null)])
      
      (ConstructorBody
       [(O_BRACE ExplicitConstructorInvocation BlockStatements C_BRACE)
	(make-block (cons $2 (reverse $3)) (build-src 4))]
       [(O_BRACE ExplicitConstructorInvocation C_BRACE)
	(make-block (list $2) (build-src 3))]
       [(O_BRACE BlockStatements C_BRACE)
	(make-block 
	 (cons (make-call #f #f #f (make-special-name #f #f "super") null #f)
	       (reverse $2))
	 (build-src 3))]
       [(O_BRACE C_BRACE)
	(make-block
	 (list (make-call #f (build-src 1) 
			      #f (make-special-name #f (build-src 1) "super") null #f))
	 (build-src 2))])
      
      (ExplicitConstructorInvocation
       [(this O_PAREN ArgumentList C_PAREN SEMI_COLON)
	(make-call #f (build-src 5) 
		       #f (make-special-name #f (build-src 1) "this") (reverse $3) #f)]
       [(this O_PAREN C_PAREN SEMI_COLON)
	(make-call #f (build-src 4) 
		       #f (make-special-name #f (build-src 1) "this") null #f)]
       [(super O_PAREN ArgumentList C_PAREN SEMI_COLON)
	(make-call #f (build-src 5) 
		       #f (make-special-name #f (build-src 1) "super") (reverse $3) #f)]
       [(super O_PAREN C_PAREN SEMI_COLON)
	(make-call #f (build-src 4) 
		       #f (make-special-name #f (build-src 1) "super") null #f)])
      
      ;; 19.9.1
      
      (InterfaceDeclaration
       [(Modifiers interface IDENTIFIER ExtendsInterfaces InterfaceBody)
	(make-interface-def (make-header (make-id $3 (build-src 3 3)) $1 $4 null null (build-src 4))
                                $5
                                (build-src 2 2)
                                (build-src 5)
                                (file-path)
                                'full
                                null 'top null)]
       [(Modifiers interface IDENTIFIER InterfaceBody)
	(make-interface-def (make-header (make-id $3 (build-src 3 3)) $1 null null null (build-src 3))
                                $4
                                (build-src 2 2)
                                (build-src 4)
                                (file-path)
                                'full
                                null 'top null)]
       [(interface IDENTIFIER ExtendsInterfaces InterfaceBody)
       	(make-interface-def (make-header (make-id $2 (build-src 2 2)) null $3 null null (build-src 3))
                                $4
                                (build-src 1)
                                (build-src 4)
                                (file-path)
                                'full
                                null 'top null)]
       [(interface IDENTIFIER InterfaceBody)
	(make-interface-def (make-header (make-id $2 (build-src 2 2)) null null null null (build-src 2))
                                $3
                                (build-src 1)
                                (build-src 3)
                                (file-path)
                                'full
                                null 'top null)])
       
      
      (ExtendsInterfaces
       [(extends InterfaceType) (list $2)]
       [(ExtendsInterfaces COMMA InterfaceType) (cons $3 $1)])

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
       [(ConstantDeclaration) $1]
       ;; 1.1
       [(ClassDeclaration) $1]
       ;; 1.1
       [(InterfaceDeclaration) $1]
       [(AbstractMethodDeclaration) $1]
       [(SEMI_COLON) #f])
      
      (ConstantDeclaration
       [(FieldDeclaration) $1])
      
      (AbstractMethodDeclaration
       [(MethodHeader SEMI_COLON) $1])
      
      ;;test extension stuff

      (TestDeclaration
       [(test IDENTIFIER TestBody) 
        (make-test-def (make-test-header (make-id $2 (build-src 2 2))
                                         (list (make-modifier 'public #f)) 
                                         null null null (build-src 2) null)
                       $3
                       (build-src 1)
                       (build-src 3)
                       (file-path)
                       'full null 'top null)]
       [(test IDENTIFIER tests TestClasses TestBody) 
        (make-test-def (make-test-header (make-id $2 (build-src 2 2))
                                         (list (make-modifier 'public #f)) 
                                         null null null (build-src 4) $4)
                       $5 
                       (build-src 1)
                       (build-src 5)
                       (file-path)
                       'full null 'top null)]
       [(test IDENTIFIER extends ClassType TestBody)
        (make-test-def (make-test-header (make-id $2 (build-src 2 2))
                                         (list (make-modifier 'public #f))
                                         (list $4) null null (build-src 4) null)
                       $5
                       (build-src 1)
                       (build-src 5)
                       (file-path)
                       'full null 'top null)]
       [(test IDENTIFIER extends ClassType tests TestClasses TestBody) 
        (make-test-def (make-test-header (make-id $2 (build-src 2 2))
                                         (list (make-modifier 'public #f))
                                         (list $4) null null (build-src 6) $6)
                       $7
                       (build-src 1)
                       (build-src 7)
                       (file-path)
                       'full null 'top null)])
      
      (TestClasses
       [(ClassType) (list $1)]
       [(TestClasses COMMA ClassType) (cons $3 $1)])
      
      (TestBody
       [(O_BRACE TestMemberDeclarations C_BRACE) $2])
      
      (TestMemberDeclarations
       [() null]
       [(TestMemberDeclarations TestMemberDeclaration)
        (cond
          ((not $2) $1)
          ((list? $2) (append $2 $1))
          (else (cons $2 $1)))])
            
      (TestMemberDeclaration
       [(FieldDeclaration) $1]
       [(MethodDeclaration) $1]
       [(TestcaseDeclaration) $1]
       [(ConstructorDeclaration) $1]
       [(SEMI_COLON) #f])
      
      (TestcaseDeclaration
       [(testcase MethodDeclarator Block) 
        (let ([method-header (construct-method-header (list (make-modifier 'public (build-src 1)))
                                                      null
                                                      (make-type-spec 'boolean 0 (build-src 1))
                                                      $2
                                                      null)])
          (make-test-method (method-modifiers method-header)
                            (method-type method-header)
                            null
                            (method-name method-header)
                            (method-parms method-header)
                            null
                            $3
                            #f #f (build-src 3)))])
      
      ;; 19.10
      
      (ArrayInitializer
       [(O_BRACE VariableInitializers COMMA C_BRACE) (make-array-init $2 (build-src 3))]
       [(O_BRACE VariableInitializers C_BRACE) (make-array-init $2 (build-src 3))]
       [(O_BRACE COMMA C_BRACE) (make-array-init null (build-src 3))]
       [(O_BRACE C_BRACE) (make-array-init null (build-src 2))])
      
      (VariableInitializers
       [(VariableInitializer) (list $1)]
       [(VariableInitializers COMMA VariableInitializer) (cons $3 $1)])
      
      ;; 19.11
      
      (Block
       [(O_BRACE BlockStatements C_BRACE) (make-block (reverse $2) (build-src 3))]
       [(O_BRACE C_BRACE) (make-block null (build-src 2))])
      
      (BlockStatements
       [(BlockStatement) (cond
			  ((list? $1) $1)
			  (else (list $1)))]
       [(BlockStatements BlockStatement) (cond
                                           ((list? $2)
                                            (append (reverse $2) $1))
                                           (else
                                            (cons $2 $1)))])
      
      (BlockStatement
       ;; 1.1
       [(ClassDeclaration) (begin (set-def-kind! $1 'statement) $1)]
       [(InterfaceDeclaration) (begin (set-def-kind! $1 'statement) $1)]

       [(LocalVariableDeclarationStatement) $1]
       [(Statement) $1])
      
      (LocalVariableDeclarationStatement
       [(LocalVariableDeclaration SEMI_COLON) $1]
       ;; 1.1
       [(final LocalVariableDeclaration SEMI_COLON)
        (map (lambda (d)
               (let* ((decl (cond
                              ((var-decl? d) d)
                              (else (var-init-var-decl d))))
                      (new-decl (make-var-decl (var-decl-name decl)
                                               `(final)
                                               (var-decl-type-spec decl)
                                               #f
                                               (var-decl-src decl))))
                                 
                 (cond
                   ((var-decl? d) new-decl)
                   (else (make-var-init new-decl
                                            (var-init-init d)
                                            (var-init-src d))))))
             $2)])
      
      (LocalVariableDeclaration
       [(Type VariableDeclarators)
        (map (lambda (d) (build-field-decl null $1 d)) (reverse $2))])
      
      (Statement
       [(StatementWithoutTrailingSubstatement) $1]
       [(LabeledStatement) $1]
       [(IfThenStatement) $1]
       [(IfThenElseStatement) $1]
       [(WhileStatement) $1]
       [(ForStatement) $1])
      
      (StatementNoShortIf
       [(StatementWithoutTrailingSubstatement) $1]
       [(LabeledStatementNoShortIf) $1]
       [(IfThenElseStatementNoShortIf) $1]
       [(WhileStatementNoShortIf) $1]
       [(ForStatementNoShortIf) $1])
      
      (StatementWithoutTrailingSubstatement
       [(Block) $1]
       [(EmptyStatement) $1]
       [(ExpressionStatement) $1]
       [(SwitchStatement) $1]
       [(DoStatement) $1]
       [(BreakStatement) $1]
       [(ContinueStatement) $1]
       [(ReturnStatement) $1]
       [(SynchronizedStatement) $1]
       [(ThrowStatement) $1]
       [(TryStatement) $1])
      
      (EmptyStatement
       [(SEMI_COLON) (make-block null (build-src 1))])
      
      (LabeledStatement
       [(IDENTIFIER : Statement) (make-label (make-id $1 (build-src 1)) $3 (build-src 3))])
      
      (LabeledStatementNoShortIf
       [(IDENTIFIER : StatementNoShortIf) (make-label (make-id $1 (build-src 1)) $3 (build-src 3))])
      
      (ExpressionStatement
       [(StatementExpression SEMI_COLON) $1])
      
      (StatementExpression
       [(Assignment) $1]
       [(PreIncrementExpression) $1]
       [(PreDecrementExpression) $1]
       [(PostIncrementExpression) $1]
       [(PostDecrementExpression) $1]
       [(MethodInvocation) $1]
       [(ClassInstanceCreationExpression) $1])
      
      (IfThenStatement
       [(if O_PAREN Expression C_PAREN Statement) (make-ifS $3 $5 #f (build-src 1) (build-src 5))])
      
      (IfThenElseStatement
       [(if O_PAREN Expression C_PAREN StatementNoShortIf else Statement)
	(make-ifS $3 $5 $7 (build-src 1) (build-src 7))])
      
      (IfThenElseStatementNoShortIf
       [(if O_PAREN Expression C_PAREN StatementNoShortIf else StatementNoShortIf)
	(make-ifS $3 $5 $7 (build-src 1) (build-src 7))])
      
      (SwitchStatement
       [(switch O_PAREN Expression C_PAREN SwitchBlock)
	(make-switch $3 $5 (build-src 5))])
      
      (SwitchBlock
       [(O_BRACE SwitchBlockStatementGroups SwitchLabels C_BRACE)
	(reverse (cons (make-caseS $3 
                                       (list (make-block null (build-src 4 4)))
				       (build-src 3 3))
		       $2))]
       [(O_BRACE SwitchBlockStatementGroups C_BRACE) (reverse $2)]
       [(O_BRACE SwitchLabels C_BRACE)
	(list (make-caseS $2 
                              (list (make-block null (build-src 2 2)))
			      (build-src 3)))]
       [(O_BRACE C_BRACE) null])
       
      (SwitchBlockStatementGroups
       [(SwitchBlockStatementGroup) (list $1)]
       [(SwitchBlockStatementGroups SwitchBlockStatementGroup) (cons $2 $1)])
      
      (SwitchBlockStatementGroup
       [(SwitchLabels BlockStatements) (make-caseS $1 (reverse $2) (build-src 2))])
      
      (SwitchLabels
       [(SwitchLabel) $1]
       [(SwitchLabels SwitchLabel) (cons $2 $1)])
      
      (SwitchLabel
       [(case ConstantExpression :) $2]
       [(default :) 'default])
      
      (WhileStatement
       [(while O_PAREN Expression C_PAREN Statement)
        (make-while $3 $5 (build-src 5))])
      
      (WhileStatementNoShortIf
       [(while O_PAREN Expression C_PAREN StatementNoShortIf)
	(make-while $3 $5 (build-src 5))])
      
      (DoStatement
       [(do Statement while O_PAREN Expression C_PAREN SEMI_COLON)
	(make-doS $2 $5 (build-src 7))])
      
      (ForStatement
       [(for O_PAREN ForInit SEMI_COLON Expression SEMI_COLON ForUpdate C_PAREN Statement)
	(make-for $3 $5 $7 $9 (build-src 9))]
       [(for O_PAREN ForInit SEMI_COLON SEMI_COLON ForUpdate C_PAREN Statement)
	(make-for $3 
                      (make-literal 'boolean (build-src 4 5) #t) 
		      $6 
                      $8 
                      (build-src 8))])
	
      
      (ForStatementNoShortIf
       [(for O_PAREN ForInit SEMI_COLON Expression SEMI_COLON ForUpdate C_PAREN StatementNoShortIf)
	(make-for $3 $5 $7 $9 (build-src 9))]
       [(for O_PAREN ForInit SEMI_COLON SEMI_COLON ForUpdate C_PAREN StatementNoShortIf)
      	(make-for $3 (make-literal 'boolean #t (build-src 4 5)) 
		      $6 $8 (build-src 8))])
      (ForInit
       [() null]
       [(StatementExpressionList) (reverse $1)]
       [(LocalVariableDeclaration) (reverse $1)])
      
      (ForUpdate
       [() null]
       [(StatementExpressionList) (reverse $1)])
      
      (StatementExpressionList
       [(StatementExpression) (list $1)]
       [(StatementExpressionList COMMA StatementExpression) (cons $3 $1)])
      
      (BreakStatement
       [(break IDENTIFIER SEMI_COLON) (make-break (make-id $2 (build-src 2 2)) (build-src 3))]
       [(break SEMI_COLON) (make-break #f (build-src 2))])
      
      (ContinueStatement
       [(continue IDENTIFIER SEMI_COLON) (make-continue (make-id $2 (build-src 2 2)) (build-src 3))]
       [(continue SEMI_COLON) (make-continue #f (build-src 2))])
       
      (ReturnStatement
       [(return Expression SEMI_COLON) (make-return $2 #f #f (build-src 3))]
       [(return SEMI_COLON) (make-return #f #f #f (build-src 2))])
      
      (ThrowStatement
       [(throw Expression SEMI_COLON) (make-throw $2 (build-src 1) (build-src 3))])
      
      (SynchronizedStatement
       [(synchronized O_PAREN Expression C_PAREN Block)
        (make-synchronized $3 $5 (build-src 4))])
      
      (TryStatement
       [(try Block Catches) (make-try $2 (reverse $3) #f (build-src 1) (build-src 3))]
       [(try Block Catches Finally) (make-try $2 (reverse $3) $4 (build-src 1) (build-src 4))]
       [(try Block Finally) (make-try $2 null $3 (build-src 1) (build-src 3))])
      
      (Catches
       [(CatchClause) (list $1)]
       [(Catches CatchClause) (cons $2 $1)])
      
      (CatchClause
       [(catch O_PAREN FormalParameter C_PAREN Block)
	(make-catch $3 $5 (build-src 5))])
      
      (Finally
       [(finally Block) $2])
      
      ;; 19.12
      
      (Primary
       [(PrimaryNoNewArray) $1]
       [(ArrayCreationExpression) $1])
      
      (PrimaryNoNewArray
       [(Literal) $1]
       [(this) (make-special-name #f (build-src 1) "this")]
       [(O_PAREN Expression C_PAREN) $2]
       [(ClassInstanceCreationExpression) $1]
       [(FieldAccess) $1]
       [(MethodInvocation) $1]
       [(ArrayAccess) $1]
       ;; 1.1
       [(PrimitiveType PERIOD class) (unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Name PERIOD class) (unimplemented-1.1 (build-src 1))]
       ;; 1.1
       ;;[(ArrayType PERIOD class)
       ;; 1.1
       ;;[(PrimitiveType Dims PERIOD class)
       ;; 1.1
       ;;[(Name Dims PERIOD class)
       ;; 1.1
       [(void PERIOD class) (unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Name PERIOD this) (make-specified-this #f (build-src 3) $1 #f)])
      
      (ClassInstanceCreationExpression
       [(new ClassOrInterfaceType O_PAREN ArgumentList C_PAREN)
	(make-class-alloc #f (build-src 5) $2 (reverse $4) #f #f #f)]
       [(new ClassOrInterfaceType O_PAREN C_PAREN) 
	(make-class-alloc #f (build-src 4) $2 null #f #f #f)]
       ;; 1.1
       [(new ClassOrInterfaceType O_PAREN ArgumentList C_PAREN ClassBody)
        (make-anon-class-alloc (build-src 5)
			       (build-src 2 6)
			       (build-src 2 6)
			       $2 $4 $6)]
       ;; 1.1
       [(new ClassOrInterfaceType O_PAREN C_PAREN ClassBody)
        (make-anon-class-alloc (build-src 4)
			       (build-src 2 4)
			       (build-src 2 5)
			       $2 null $5)]
       ;; 1.1
       [(Primary PERIOD new IDENTIFIER O_PAREN ArgumentList C_PAREN ClassBody)
	(unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Primary PERIOD new IDENTIFIER O_PAREN C_PAREN ClassBody)
	(unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Primary PERIOD new IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-inner-alloc #f (build-src 7) $1 (make-id $4 (build-src 4 4)) (reverse $6) #f)]
;	(unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Primary PERIOD new IDENTIFIER O_PAREN C_PAREN)
        (make-inner-alloc #f (build-src 6) $1 (make-id $4 (build-src 4 4)) null #f)]
;	(unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Name PERIOD new IDENTIFIER O_PAREN ArgumentList C_PAREN ClassBody)
	(unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Name PERIOD new IDENTIFIER O_PAREN C_PAREN ClassBody)
	(unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Name PERIOD new IDENTIFIER O_PAREN ArgumentList C_PAREN)
	(unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Name PERIOD new IDENTIFIER O_PAREN C_PAREN)
	(unimplemented-1.1 (build-src 1))])
      
      (ArgumentList
       [(Expression) (list $1)]
       [(ArgumentList COMMA Expression) (cons $3 $1)])
      
      (ArrayCreationExpression
       [(new PrimitiveType DimExprs Dims) (make-array-alloc #f (build-src 4) $2 (reverse $3) $4)]
       [(new PrimitiveType DimExprs) (make-array-alloc #f (build-src 3) $2 (reverse $3) 0)]
       [(new ClassOrInterfaceType DimExprs Dims)
        (make-array-alloc #f (build-src 4) (make-type-spec $2 0 (build-src 2 2)) (reverse $3) $4)]
       [(new ClassOrInterfaceType DimExprs)
        (make-array-alloc #f (build-src 3) (make-type-spec $2 0 (build-src 2 2)) (reverse $3) 0)]
       ;; 1.1
       [(new PrimitiveType Dims ArrayInitializer) (make-array-alloc-init #f (build-src 4) $2 $3 $4)]
       ;; 1.1
       [(new ClassOrInterfaceType Dims ArrayInitializer) 
        (make-array-alloc-init #f (build-src 4) (make-type-spec $2  $3 (build-src 2 2)) $3 $4)])
      
      (DimExprs
       [(DimExpr) (list $1)]
       [(DimExprs DimExpr) (cons $2 $1)])

      (DimExpr
       [(O_BRACKET Expression C_BRACKET) $2])
      
      (Dims
       [(O_BRACKET C_BRACKET) 1]
       [(Dims O_BRACKET C_BRACKET) (add1 $1)])
      
      (FieldAccess
       [(Primary PERIOD IDENTIFIER) 
        (make-access #f (build-src 3) (make-field-access $1 
                                                         (make-id $3 (build-src 3 3)) #f))]
       [(super PERIOD IDENTIFIER) 
        (make-access #f (build-src 3)
                         (make-field-access (make-special-name #f (build-src 1)
                                                                       "super")
                                                (make-id $3 (build-src 3 3))
                                                #f))]
       ;; 1.1
       [(Name PERIOD super PERIOD IDENTIFIER) (unimplemented-1.1 (build-src 1))])
      
      (MethodInvocation
       [(Name O_PAREN ArgumentList C_PAREN) (build-name-call $1 (reverse $3) (build-src 4))]
       [(Name O_PAREN C_PAREN) (build-name-call $1 null (build-src 3))]
       [(Primary PERIOD IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-call #f (build-src 6) $1 (make-id $3 (build-src 3 3)) (reverse $5) #f)]
       [(Primary PERIOD IDENTIFIER O_PAREN C_PAREN)
        (make-call #f (build-src 5) $1 (make-id $3 (build-src 3 3)) null #f)]
       [(super PERIOD IDENTIFIER O_PAREN ArgumentList C_PAREN)
        (make-call #f (build-src 6) 
                       (make-special-name #f (build-src 1) "super") 
                       (make-id $3 (build-src 3 3)) (reverse $5) #f)]
       [(super PERIOD IDENTIFIER O_PAREN C_PAREN)
        (make-call #f (build-src 5) 
                       (make-special-name #f (build-src 1) "super") 
                       (make-id $3 (build-src 3 3)) null #f)]
       ;; 1.1
       [(Name PERIOD super PERIOD IDENTIFIER O_PAREN ArgumentList C_PAREN)
	(unimplemented-1.1 (build-src 1))]
       ;; 1.1
       [(Name PERIOD super PERIOD IDENTIFIER O_PAREN C_PAREN) (unimplemented-1.1 (build-src 1))])
      
      (ArrayAccess
       [(Name O_BRACKET Expression C_BRACKET)
        (make-array-access #f (build-src 4) (name->access $1) $3)]
       [(PrimaryNoNewArray O_BRACKET Expression C_BRACKET)
	(make-array-access #f (build-src 4) $1 $3)])
      
      (PostfixExpression
       [(Primary) $1]
       [(Name) (name->access $1)]
       [(PostIncrementExpression) $1]
       [(PostDecrementExpression) $1])
      
      (PostIncrementExpression
       [(PostfixExpression ++) (make-post-expr #f (build-src 2) $1 '++ (build-src 2 2))])
      
      (PostDecrementExpression
       [(PostfixExpression --) (make-post-expr #f (build-src 2) $1 '-- (build-src 2 2))])
      
      (UnaryExpression
       [(PreIncrementExpression) $1]
       [(PreDecrementExpression) $1]
       [(+ UnaryExpression) (make-unary #f (build-src 2) '+ $2 (build-src 1))]
       [(- UnaryExpression) (make-unary #f (build-src 2) '- $2 (build-src 1))]
       [(UnaryExpressionNotPlusMinus) $1])
      
      (PreIncrementExpression
       [(++ UnaryExpression) (make-pre-expr #f (build-src 2) '++ $2 (build-src 1))])

      (PreDecrementExpression
       [(-- UnaryExpression) (make-pre-expr #f (build-src 2) '-- $2 (build-src 1))])
      
      (UnaryExpressionNotPlusMinus
       [(PostfixExpression) $1]
       [(~ UnaryExpression) (make-unary #f (build-src 2) '~ $2 (build-src 1))]
       [(! UnaryExpression) (make-unary #f (build-src 2) '! $2 (build-src 1))]
       [(CastExpression) $1])
      
      (CastExpression
       [(O_PAREN PrimitiveType Dims C_PAREN UnaryExpression)
	(make-cast #f (build-src 5) 
		       (make-type-spec (type-spec-name $2)
                                           $3
                                           (build-src 2 3))
		       $5)]
       [(O_PAREN PrimitiveType C_PAREN UnaryExpression)
	(make-cast #f (build-src 4) $2 $4)]
       [(O_PAREN dynamic C_PAREN UnaryExpression)
        (make-cast #f (build-src 4) (make-type-spec 'dynamic 0 (build-src 2 2)) $4)]
       [(O_PAREN Expression C_PAREN UnaryExpressionNotPlusMinus)
        (if (access? $2)
            (make-cast #f (build-src 4) 
                           (make-type-spec (access->name $2) 0 (build-src 2 2)) $4)
            (raise-read-error "An operator is needed to combine these expressions."
                              (file-path)
                              (position-line $1-start-pos)
                              (position-col $1-start-pos)
                              (+ (position-offset $1-start-pos) (interactions-offset))
                              (- (position-offset $4-end-pos)
                                 (position-offset $1-start-pos))))]
       [(O_PAREN Name Dims C_PAREN UnaryExpressionNotPlusMinus)
	(make-cast #f (build-src 4)
		       (make-type-spec $2 $3 (build-src 2 3))
		       $5)])

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
       ;; GJ - changed to remove shift/reduce conflict
       [(ShiftExpression < ShiftExpression)
        (make-bin-op #f (build-src 3) '< $1 $3 (build-src 2 2))]		
       [(RelationalExpression > ShiftExpression)
	(make-bin-op #f (build-src 3) '> $1 $3 (build-src 2 2))]	
       [(RelationalExpression <= ShiftExpression)
	(make-bin-op #f (build-src 3) '<= $1 $3 (build-src 2 2))]	
       [(RelationalExpression >= ShiftExpression)
	(make-bin-op #f (build-src 3) '>= $1 $3 (build-src 2 2))]	
       [(RelationalExpression instanceof ReferenceType)
	(make-instanceof #f (build-src 3) $1 $3 (build-src 2 2))])
      

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
      
      (ConditionalExpression
       [(ConditionalOrExpression) $1]
       [(ConditionalOrExpression ? Expression : ConditionalExpression)
	(make-cond-expression #f (build-src 5) $1 $3 $5 (build-src 2 2))])
      
      (CheckExpression
       [(ConditionalExpression) $1]
       [(check ConditionalExpression expect ConditionalExpression) 
        (make-check-expect #f (build-src 4) $2 $4 #f (build-src 2 4))]
       [(check ConditionalExpression expect ConditionalExpression within ConditionalExpression) 
        (make-check-expect #f (build-src 6) $2 $4 $6 (build-src 2 4))]
       [(check ConditionalExpression catch Type)
        (make-check-catch #f (build-src 4) $2 $4)])
       
      (MutateExpression
       [(CheckExpression) $1]
       [(CheckExpression -> CheckExpression) 
        (make-check-mutate #f (build-src 3) $1 $3 (build-src 2 2))])
      
      (AssignmentExpression
       [#;(ConditionalExpression) #;(CheckExpression) (MutateExpression) $1]
       [(Assignment) $1])
      
      (Assignment
       [(LeftHandSide AssignmentOperator AssignmentExpression)
	(make-assignment #f (build-src 3) $1 $2 $3 (build-src 2 2))])      
      
      (LeftHandSide
       [(Name) (name->access $1)]
       [(FieldAccess) $1]
       [(ArrayAccess) $1])
      
      (AssignmentOperator
       [(=) '=]
       [(*=) '*=]
       [(/=) '/=]
       [(%=) '%=]
       [(+=) '+=]
       [(-=) '-=]
       [(<<=) '<<=]
       [(>>=) '>>=]
       [(>>>=) '>>>=]
       [(&=) '&=]
       [(^=) '^=]
       [(OREQUAL) 'or=])
      
      (Expression
       [(AssignmentExpression) $1])
      
      (ConstantExpression
       [(Expression) $1]))))
  
  (define (unimplemented-1.1 src)
    (raise-read-error "Unimplemented 1.1"
 		      (file-path)
 		      (src-line src)
 		      (src-col src)
 		      (src-pos src)
 		      (src-span src)))
  
  ;make-anon-class-alloc: src src src id (list field) statement -> class-alloc
  (define (make-anon-class-alloc all-src head-src class-src super args body)
    (let ([anon (symbol->string (gensym 'anonymous$))])
      (make-class-alloc #f all-src
			(make-class-def (make-header (make-id anon all-src) null (list super) null null head-src)
					body
					class-src
					class-src
					(file-path)
					'full
					null 'anon null)
			(reverse args) #t #f #t)))
  
  (define parse-full (car parsers))
  (define parse-full-interactions (cadr parsers))
  (define parse-full-expression (caddr parsers))
  (define parse-full-type (cadddr parsers))

  )
