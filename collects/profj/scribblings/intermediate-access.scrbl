#lang scribble/doc

@(require scribble/manual "java-scribble.ss")

@title[#:style 'toc #:tag "intermediateAcc"]{ProfessorJ Intermediate + Access}

@javagrammar[Program [Import ... Def ...]]

@javagrammar[ #:literals(import *)
              #:tag '(inta "import")
              Import 
              [import Name (code:comment "")]
              [import Name.* (code:comment "")]]

@javagrammar[#:literals(public)
             Def [Class] [Interface]
                 [public Class] [public Interface]]

@javagrammar[#:literals (class implements extends abstract) #:tag '(inta "class") Class
             [class Id { Member ... } ]
             [class Id implements Id , Id ... { Member ... } ]
             [class Id extends Id { Member ... } ]
             [class Id extends Id implements Id , Id ... { Member ... } ]
             [abstract class Id { Member ... }]
             [abstract class Id implements Id , Id ... { Member ... } ]
             [abstract class Id extends Id { Member ... } ]
             [abstract class Id extends Id implements Id , Id ... { Member ... } ]]

@javagrammar[#:literals (interface extends) #:tag '(inta "iface") Interface
             [interface Id { Signature ... } ]
             [interface Id extends Id , Id ... { Signature ... } ]]

@javagrammar[#:literals (abstract) #:tag '(inta "sig")
             Signature
             [MethodReturn Id( Type Id , ... )(code:comment "")]
             [abstract MethodReturn Id(Type Id , ...)(code:comment "")]]

@javagrammar[Member [Field][Modifier Field][Method][Modifier Method][Constructor][Modifier Constructor]]

@javagrammar[#:literals(public private protected) #:tag '(inta "mods")
             Modifier
             [public][private][protected]]

@javagrammar[#:literals(=) #:tag '(inta "field") 
             Field 
             [Type Id = Expression (code:comment "")]
             [Type Id (code:comment "")]]

@javagrammar[#:literals (abstract) #:tag '(inta "method")
             Method 
             [MethodReturn Id( Type Id , ...) { Statement ... } ]
             [abstract MethodReturn Id(Type Id , ...) (code:comment "")]]

@javagrammar[#:literals(void)
             MethodReturn
             [void]
             [Type]]

@javagrammar[#:tag '(inta "ctor")
             Constructor [Id( Type Id , ...) { Statement ... } ]]

@javagrammar[#:literals(if else return super = this) 
             Statement
             [#:tag '(inta "if") if (Expression) { Statement ...} else { Statement ...} ]
             [#:tag '(inta "return") return Expression (code:comment "")]
             [#:tag '(inta "return") return (code:comment "")]
             [#:tag '(inta "block") { Statement ... }]
             [#:tag '(inta "super") super(Expression , ...)(code:comment "")]
             [#:tag '(inta "thisC") this(Expression , ...)(code:comment "")]
             [#:tag '(inta "varDecl") Type Id (code:comment "")]
             [#:tag '(inta "varDecl") Type Id = Expression (code:comment "")]
             [#:tag '(inta "stmtExpr") StatementExpression (code:comment "")]]

@javagrammar[#:literals(super)
             StatementExpression
             [Id(Expression , ...)]
             [Expression.Id(Expression , ...)]
             [super.Id(Expression , ...)]]

@javagrammar[#:literals(- this ! new true false check expect within super instanceof)
             Expression
             [#:tag '(inta "op") Expression Op Expression]
             [#:tag '(inta "op") - Expression]
             [#:tag '(inta "op") ! Expression]
             [#:tag '(inta "this") this]
             [#:tag '(inta "call") Id.(expression , ...)]
             [#:tag '(inta "call") Expression.Id(Expression , ...)]
             [#:tag '(inta "supercall") super.Id(Expression , ...)]
             [#:tag '(inta "acc") Expression.Id]
             [#:tag '(inta "new") new Id(Expression , ...)]
             [#:tag '(inta "cast") (Type) Expression]
             [#:tag '(inta "instof") Expression instanceof Type]
             [#:tag '(inta "check") check Expression expect Expression]
             [#:tag '(inta "check") check Expression expect Expression within Expression]
             [#:tag '(inta "misc") (Expression)]
             [#:tag '(inta "misc") Id]
             [#:tag '(inta "misc") Number]
             [#:tag '(inta "misc") Character]
             [#:tag '(inta "misc") String]
             [#:tag '(inta "misc") null]
             [#:tag '(inta "misc") true]
             [#:tag '(inta "misc") false]]
                       
@javagrammar[Name [ Id. ... Id]]

@javagrammar[Op [+][-][*][/][<][<=][==][>][>=][&&][||]]

@javagrammar[#:literals(int boolean float short double long byte char String)
                       Type 
                       [Id] [boolean] [int] [char] [double] [float]
                       [long] [byte] [short]]


An @(scheme Id) is a sequence of alphanumeric characters, _, and $.

@section[#:tag "intermedAcc:import"]{@scheme[import]}

@elemtag['(inta "import")]
@itemize[
@item{@(scheme import Name (code:comment ""))
       
       Imports a specified class to use within the program.}
@item{@(scheme import Name.* (code:comment ""))
       
       Imports a group of classes that can all be used within the program.}

]

@section[#:tag "intermedAcc:class"]{@scheme[class]}

@elemtag['(inta "class")]
@itemize[
 @item{@(scheme class Id { Member ...})
        
        Creates a class named Id. If no constructor is present, one is generated that takes no arguments.
        }
 @item{@(scheme class Id implements Id , Id ... { Member ...})
        
        Creates a class named Id that implements the listed @elemref['(inta "iface")]{interfaces}
        named by (scheme implements). If no constructor is present, one is generated that takes no arguments.
        Any @elemref['(inta "sig")]{method} defined by the listed interface must be a member of this class.
        }
 @item{@(scheme class Id extends Id { Member ... })
        
        Creates a class named Id that inherits and expands the behavior of the extended class. 
        If no constructor is present, one is generated that takes no arguments. If the parent
        class contains a constructor that requires arguments, then none can be generated and
        the current class must contain a @elemref['(inta "ctor")]{constructor that contains @(scheme super)}.
        }
 @item{@(scheme class Id extends Id implements Id , Id ... { Member ... })
        
        Creates a class named Id that inherits from the extended class and implements the listed interfaces.
        }
 @item{@(scheme abstract class Id { Member ... })
        
        Creates a class named Id that cannot be instantiated. Members may contain @elemref['(inta "method")]{abstract methods}. 
        Non-abstract classes extending this class are required to implement all @(scheme abstract) methods.
        
        }
 @item{@(scheme abstract class Id implements Id , Id ... { Member ... })
        
        Creates an abstract class named Id that implements the listed interfaces. Members can include abstract methods. 
        This class need not implement all methods in the interfaces, but all non-abstract subclasses must. 
        }
 @item{@(scheme abstract class Id extends Id { Member ... })
        
        Creates an abstract class named Id that inherits from the extended class. Members can include
        abstract methods. If the parent is abstract, the current class does not need to implement all
        inherited abstract methods, but all non-abstract subclasses must.
        }
 @item{@(scheme abstract class Id extends Id implements Id , Id ... { Member ... })
        
        Creates an abstract class named Id, that inherits from the extended class and implements
        the listed interfaces.
        }

 ]

@section[#:tag "intermedAcc:iface"]{@scheme[interface]}

@elemtag['(inta "iface")]

@itemize[

@item{@(scheme interface Id { Signature ... })

       Creates an interface named Id that specifies a set of method signatures for classes to implement.
       }
@item{@(scheme interface Id extends Id , Id ... { Signature ... })
       
       Creates an interface named Id that specifies a set of method signatures for classes to implement, and
       inherits the method signatures of the interfaces specified in the extends list.
       }
]

@elemtag['(inta "sig")]

@(scheme MethodReturn Id(Type Id , ...) (code:comment ""))

The signature specifies a method named Id, expecting the listed arguments. All @elemref['(inta "class")]{classes}
implementing the (scheme interface) must contain a @elemref['(inta "method")]{method} with the same name, 
return type, and argument types. A method that does not return a value uses the @(scheme void) designation instead
of a Type.

@(scheme abstract MethodReturn Id(Type Id , ...)(code:comment ""))

A signature may be declared @(scheme abstract). This does not impact the method behavior; 
all signatures are by default abstract.

@section[#:tag "intermedAcc:mods"]{@scheme[Modifiers]}

@elemtag['(inta "mods")]

The modifiers @(scheme public), @(scheme private), and @(scheme protected) controll access to the 
modified member. A public member can be accessed by any class. A private member can only be accessed
by the containing class. A protected member can be accessed by the containing class and subclasses.

@section[#:tag "intermedAcc:field"]{@scheme[Field]}

@elemtag['(inta "field")]

@itemize[
  @item{@(scheme Type Id (code:comment "")) 
         
         Creates a field, bound to Id, that can be used within the current class, or on instances of the 
         current class using an @elemref['(inta "acc")]{expression}.
         This field will have the declared type and will contain a default value of this type if uninitialized.
         }
  @item{@(scheme Type Id = Expression (code:comment ""))
  
         Creates a field, bound to Id, that can be used within the current class, or on instances of the 
         current class using an @elemref['(inta "acc")]{expression}.
         This field will have the declared type and the value of the evaluated @(scheme Expression). 
  }
  ]

@section[#:tag "intermedAcc:method"]{@scheme[Method]}

@elemtag['(inta "method")]
@(scheme MethodReturn Id( Type Id , ...) { Statement ... })

Creates a method, bound to Id, that can be called on the current object, or instances of this class.
The body of the method, the @elemref['(inta "stmt")]{statements}, will be evaluated sequentially when the method is called.
The method name may not be the name of any classes defined in the same program or of any fields or methods in the same class.
A method that does not return a value uses the @(scheme void) designation instead of a Type for MethodReturn.

@(scheme abstract MethodReturn Id( Type Id , ...) (code:comment ""))

Creates a method, bount to Id, inside an @elemref['(inta "class")]{abstract class}. Like an @elemref['(inta "sig")]{interface signature},
non-abstract classes that inherit this method must provide an implementation.

@section[#:tag "intermedAcc:ctor"]{@scheme[Constructor]}

@elemtag['(inta "ctor")]
@(scheme Id( Type Id , ...) { Statement ... } )

Creates a constructor that is used in creating an @elemref['(inta "new")]{instance} of a @elemref['(inta "class")]{class} (called an object). 
The arguments given when creating an instance must be of the same type, and in the same order, as that specified by the constructor.
The statements are executed in sequence in intializing the object. If the parent of the current class contains a constructor, that expects
parameters, then the first statement in the constructor must be a @elemref['(inta "super")]{@(scheme super) call}. 

Multiple constructors can appear in a class body, provided that for each constructor the type of arguments or the number of arguments 
us unique. Each constructor may set its own @elemref['(inta "mods")]{access}. A constructor in the same class can be called using a
@elemref['(inta "thisC")]{@(scheme this) call}. This must be the first statement.

@section[#:tag "intermedAcc:stmt"]{@scheme[Statement]}

@elemtag['(inta "stmt")]{}

@itemize[
 @item{@elemtag['(inta "if")] @(scheme if (Expression) { Statement ... } else { Statement  ...})
               
       In this statement the expression should have a boolean type. It is evaluated first. 
       If the expression evaluates to @(scheme true), then the first group of statements (known as the then clause) are evaluated. 
       If the expression evaluates to @(scheme false), the group of statements following else (the else clause) are evaluated.
               }
 @item{@elemtag['(inta "return")] @(scheme return Expression (code:comment ""))
               
               This form evaluates the expression, and then returns the value of the expression 
               as the result of the @elemref['(inta "method")]{method} in which it is contained. 
               }
 @item{ @(scheme return (code:comment ""))
               
               This form causes the method to cease evaluation, without producing a value. Should be used
               in conjunction with @(scheme void) for the MethodReturn.}
  @item{@elemtag['(inta "block")] @(scheme { Statement ... })
                
        This statement groups the sequence of statements together, commonly called a block. 
        The statements evaluate sequentially.
        }
  @item{@elemtag['(inta "super")]@(scheme super(Expression , ...)(code:comment ""))
   
        May only appear as the first statement of a @elemref['(inta "ctor")]{constructor}. Calls the
        constructor for the parent class using the given expressions as arguments. Expressions
        are evaluated left to right.
        }
  @item{@elemtag['(inta "thisC")]@(scheme this(Expression , ...)(code:comment ""))
                
        May only appear as the first statement of a @elemref['(inta "ctor")]{constructor}. Calls
        a different constructor from the same class, chosen by analyzing the given expressions.
   }
   @item{@elemtag['(inta "varDecl")] @(scheme Type Id (code:comment ""))
         
         Creates a local variable Id within a method body or a block statement; 
         it is not visible outside the block or method, or to statements the preceed
         the declaration. The variable must be initialized prior to use.
         }
   @item{@(scheme Type Id = Expression (code:comment ""))

          Creates a local variable Id within a method body or a block statement.
          }
   @item{@elemtag['(inta "stmtExpr")]@(scheme StatementExpression (code:comment ""))
          
          This set of expressions can be used in a statement position, provided they
          are followed by ';'.
          }
   ]

@section[#:tag "intermedAcc:expr"]{@scheme[Expression]}

@itemize[
 
   @item{@elemtag['(inta "op")]@(scheme Expression Op Expression)
                 
                 Performs the mathematical or logical operation Op on the value of the two expressions.
                 }
   @item{@(scheme - Expression)}
   @item{@(scheme ! Expression)
          
          Performs logical negation on the value of the expression.
          }
   @item{ @elemtag['(inta "this")]@(scheme this)
                  
          Allows access to the current object. Within a class, fields and methods of the current 
          class can be accessed through @(scheme this).
                  }
   @item{ @elemtag['(inta "call")]@(scheme Id(Expression , ...))
                  
                  Id names a method of the current class to be called by the current expression.
                  The expressions following Id are 
                  evaluated from left to right and passed in to the method as its arguments. 
                  The number and types of the arguments must match the @elemref['(inta "method")]{method's declaration}. 
                  These values replace the argument names in the body of the method, and the result of the body is the result of this expression. 
                  }

   @item{@(scheme Expression.Id(Expression , ...))
                  
                  The first expression must evaluate to an object value. Id names a method of this
                  object to be called by the current expression. The expressions following Id are 
                  evaluated from left to right and passed in to the method as its arguments. 
                  The number and types of the arguments must match the @elemref['(inta "method")]{method's declaration}. 
                  These values replace the argument names in the body of the method, and the result of the body is the result of this expression. 
                  }
   @item{@elemtag['(inta "supercall")] @(scheme super.Id (Expression , ...))
         
         Evaluates the overridden method body using the provided expressions as its arguments.
         }                                         
   @item{ @elemtag['(inta "acc")]@(scheme Expression.Id)
                  
                  The first expression must evaluate to an object value. Id names a field of this
                  object, whose value is retrieved by this expression. 
                  }
   @item{ @elemtag['(inta "new")]@(scheme new Id(Expression , ...))
                  
                  Evaluates to a new instance (object) of the Id class. 
                  The class's @elemref['(inta "ctor")]{constructor} will be run with the given values 
                  (evaluated from left to right) as its arguments. The number and types of these values 
                  select which constructor is used. 
                  }
   @item{@elemtag['(inta "cast")]@(scheme (Type) Expression)
                 
                 Evaluates Expression and then confirms that the value matches the specified type.
                 During compilation, the resulting expression has the specified type. 
                 If during evaluation, this is not true, an error is raised; 
                 otherwise the result of this expression is the result of Expression.
                 }
   @item{@elemtag['(inta "instof")]@(scheme Expression instanceof Type)
                 
                 Evaluates Expression and then confirms that the value matches the specified type.
                 Returns @(scheme true) when the type matches and @(scheme false) otherwise.
                 }
   @item{ @elemtag['(inta "check")]@(scheme check Expression expect Expression)
                  
                  Compares the resulting values of the two expressions through a deep comparison, including the fields of objects. 
                  The resulting value will be a boolean. Neither expression can have type float or double. 
                  When test reporting is enabled, results of checks appear in the testing window. 
                  }
   @item{@(scheme check Expression expect Expression within Expression)
          
          Compares the resulting values of the first two expressions through a deep comparison. 
          The third value must be numeric. If the resulting values of the compared expressions are numeric, 
          their values must be within the third value of each other. 
          For example, in @(scheme check a expect b within c), the absolute value of a-b must be less than or 
          equal to c. If the compared expressions evaluate to objects, any numeric fields will be compared 
          with this formula. The resulting value will be a boolean. When test reporting is enabled, results of 
          checks appear in the testing window. 
          
          }
   @item{ @elemtag['(inta "misc")]@(scheme (Expression))}
   @item{@(scheme Id)
          
          May refer to either a local variable, method parameter, or field of the current class.
          }
   @item{@(scheme Number)}
   @item{@(scheme Character)
          
          Values of type @(scheme char) are ASCII characters enclosed by single quotes such as 'a' is the character a. 
          They can be used as numbers as well as characters. 
          }
   @item{@(scheme String)
          
          Strings are created through placing text inside of double quotes. For example "I am a string" is a String. 
          A String value is an instance of the class String, which descends from Object, and can also be created with a constructor.
          }
   @item{@(scheme null)
          
          A value representing an object with no fields or methods. It should be used as a placeholder for
          uninitialized fields.
          }
   @item{@(scheme true)}
   @item{@(scheme false)}
   
   
   ]
