#lang scribble/doc

@(require scribble/manual "java-scribble.ss")

@title[#:style 'toc #:tag "advanced"]{ProfessorJ Advanced}

@javagrammar[Program 
             [Import ... Def ...]
             [PackageDec Import ... Def ...]]

@javagrammar[#:literals(package) #:tag '(adv "package")
             PackageDec
             [package Name(code:comment "")]]

@javagrammar[ #:literals(import *)
              #:tag '(adv "import")
              Import 
              [import Name (code:comment "")]
              [import Name.* (code:comment "")]]

@javagrammar[#:literals(public)
             Def [Class] [Interface]
                 [public Class] [public Interface]]

@javagrammar[#:literals (class implements extends abstract) #:tag '(adv "class") Class
             [class Id { Member ... } ]
             [class Id implements Id , Id ... { Member ... } ]
             [class Id extends Id { Member ... } ]
             [class Id extends Id implements Id , Id ... { Member ... } ]
             [abstract class Id { Member ... }]
             [abstract class Id implements Id , Id ... { Member ... } ]
             [abstract class Id extends Id { Member ... } ]
             [abstract class Id extends Id implements Id , Id ... { Member ... } ]]

@javagrammar[#:literals (interface extends) #:tag '(adv "iface") Interface
             [interface Id { Signature ... } ]
             [interface Id extends Id , Id ... { Signature ... } ]]

@javagrammar[#:literals (abstract) #:tag '(adv "sig")
             Signature
             [MethodReturn Id( Type Id , ... )(code:comment "")]
             [abstract MethodReturn Id(Type Id , ...)(code:comment "")]]

@javagrammar[Member [Field][Modifier Field][Method][Modifier Method]
                    [Constructor][Modifier Constructor]
                    [{ Statement ... }]]

@javagrammar[#:literals(public private protected) #:tag '(adv "mods")
             Modifier
             [public][private][protected]]

@javagrammar[#:literals(= static) #:tag '(adv "field") 
             Field 
             [Type Id = Expression (code:comment "")]
             [Type Id = ArrayInit (code:comment "")]
             [Type Id (code:comment "")]
             [static Type Id = Expression (code:comment "")]
             [static Type Id = ArrayInit (code:comment "")]
             [static Type Id (code:comment "")]]

@javagrammar[#:literals (abstract final static) #:tag '(adv "method")
             Method 
             [MethodReturn Id( Type Id , ...) { Statement ... } ]
             [abstract MethodReturn Id(Type Id , ...) (code:comment "")]
             [final MethodReturn Id(Type Id , ...) {Statement ...}]
             [static MethodReturn Id(Type Id , ...) {Statement ...}]]

@javagrammar[#:literals(void)
             MethodReturn
             [void]
             [Type]]

@javagrammar[#:tag '(adv "ctor")
             Constructor [Id( Type Id , ...) { Statement ... } ]]

@javagrammar[#:literals(if else return super = this while for do break continue) 
             Statement
             [#:tag '(adv "asgn") Expression = Expression (code:comment "")]
             [#:tag '(adv "if") if (Expression) Statement else Statement ]
             [#:tag '(adv "if") if (Expression) Statement ]
             [#:tag '(adv "return") return Expression (code:comment "")]
             [#:tag '(adv "return") return (code:comment "")]
             [#:tag '(adv "block") { Statement ... }]
             [#:tag '(adv "while") while (Expression) { Statement ... }]
             [#:tag '(adv "do") do { Statement ... } while (Expression)]
             [#:tag '(adv "for") for ( ForInit ForExpression ForUpdate ... ) { Statement ... }]
             [#:tag '(adv "brk") break (code:comment "")]
             [#:tag '(adv "cont") continue (code:comment "")]
             [#:tag '(adv "super") super(Expression , ...)(code:comment "")]
             [#:tag '(adv "thisC") this(Expression , ...)(code:comment "")]
             [#:tag '(adv "varDecl") Type Id (code:comment "")]
             [#:tag '(adv "varDecl") Type Id = Expression (code:comment "")]
             [#:tag '(adv "varDecl") Type Id = ArrayInit (code:comment "")]
             [#:tag '(adv "stmtExpr") StatementExpression (code:comment "")]]

@javagrammar[#:literals(super ++)
             StatementExpression
             [Id(Expression , ...)]
             [Expression.Id(Expression , ...)]
             [super.Id(Expression , ...)]
             [Expression ++]
             [++ Expression]
             [Expression --]
             [-- Expression]]

@javagrammar[ForInit
             [Type Id = Expression (code:comment "")]
             [Type Id = ArrayInit (code:comment "")]
             [StatementExpression , StatementExpression ... (code:comment "")]
             [(code:comment "")]]

@javagrammar[ForExpression
             [Expression (code:comment "")]
             [(code:comment "")]]

@javagrammar[ForUpdate 
             [StatementExpression]
             [Expression = Expression]]

@javagrammar[#:tag '(adv "ainit")
             ArrayInit
             [{ArrayInit , ...}]
             [{Expression , ...}]]

@javagrammar[#:literals(- this ! new true false check expect within super instanceof ++ -- ? :)
             Expression
             [#:tag '(adv "op") Expression Op Expression]
             [#:tag '(adv "op") - Expression]
             [#:tag '(adv "op") + Expression]
             [#:tag '(adv "op") ! Expression]
             [#:tag '(adv "opAs") ++ Expression]
             [#:tag '(adv "opAs") -- Expression]
             [#:tag '(adv "opAs") Expression --]
             [#:tag '(adv "opAs") Expression ++]
             [#:tag '(adv "this") this]
             [#:tag '(adv "call") Id.(expression , ...)]
             [#:tag '(adv "call") Expression.Id(Expression , ...)]
             [#:tag '(adv "supercall") super.Id(Expression , ...)]
             [#:tag '(adv "acc") Expression.Id]
             [#:tag '(adv "aacc") Expression[Expression][Expression]...]
             [#:tag '(adv "new") new Id(Expression , ...)]
             [#:tag '(adv "newA") new Type[Expression][Expression] ...]
             [#:tag '(adv "cast") (Type) Expression]
             [#:tag '(adv "instof") Expression instanceof Type]
             [#:tag '(adv "?") Expression ? Expression : Expression]
             [#:tag '(adv "check") check Expression expect Expression]
             [#:tag '(adv "check") check Expression expect Expression within Expression]
             [#:tag '(adv "misc") (Expression)]
             [#:tag '(adv "misc") Id]
             [#:tag '(adv "misc") Number]
             [#:tag '(adv "misc") Character]
             [#:tag '(adv "misc") String]
             [#:tag '(adv "misc") null]
             [#:tag '(adv "misc") true]
             [#:tag '(adv "misc") false]]
                       
@javagrammar[Name [ Id. ... Id]]

@javagrammar[Op [+][-][*][/][<][<=][==][>][>=][&&][||]]

@javagrammar[#:literals(int boolean float short double long byte char String)
                       Type 
                       [Id] [boolean] [int] [char] [double] [float]
                       [long] [byte] [short]
                       [Type[]]]


An @(scheme Id) is a sequence of alphanumeric characters, _, and $.

@section[#:tag "advanced:pack"]{@scheme[package]}

@elemtag['(adv "package")]

@(scheme package Name(code:comment ""))

This declaration asserts that all classes contained in the current file 
are members of the named package.

@section[#:tag "advanced:import"]{@scheme[import]}

@elemtag['(adv "import")]
@itemize{
@item{@(scheme import Name (code:comment ""))
       
       Imports a specified class to use within the program.}
@item{@(scheme import Name.* (code:comment ""))
       
       Imports a group of classes that can all be used within the program.}

}

@section[#:tag "advanced:class"]{@scheme[class]}

@elemtag['(adv "class")]
@itemize{
 @item{@(scheme class Id { Member ...})
        
        Creates a class named Id. If no constructor is present, one is generated that takes no arguments.
        }
 @item{@(scheme class Id implements Id , Id ... { Member ...})
        
        Creates a class named Id that implements the listed @elemref['(adv "iface")]{interfaces}
        named by (scheme implements). If no constructor is present, one is generated that takes no arguments.
        Any @elemref['(adv "sig")]{method} defined by the listed interface must be a member of this class.
        }
 @item{@(scheme class Id extends Id { Member ... })
        
        Creates a class named Id that inherits and expands the behavior of the extended class. 
        If no constructor is present, one is generated that takes no arguments. If the parent
        class contains a constructor that requires arguments, then none can be generated and
        the current class must contain a @elemref['(adv "ctor")]{constructor that contains @(scheme super)}.
        }
 @item{@(scheme class Id extends Id implements Id , Id ... { Member ... })
        
        Creates a class named Id that inherits from the extended class and implements the listed interfaces.
        }
 @item{@(scheme abstract class Id { Member ... })
        
        Creates a class named Id that cannot be instantiated. Members may contain @elemref['(adv "method")]{abstract methods}. 
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

 }

@section[#:tag "advanced:iface"]{@scheme[interface]}

@elemtag['(adv "iface")]

@itemize{

@item{@(scheme interface Id { Signature ... })

       Creates an interface named Id that specifies a set of method signatures for classes to implement.
       }
@item{@(scheme interface Id extends Id , Id ... { Signature ... })
       
       Creates an interface named Id that specifies a set of method signatures for classes to implement, and
       inherits the method signatures of the interfaces specified in the extends list.
       }
}

@elemtag['(adv "sig")]

@(scheme MethodReturn Id(Type Id , ...) (code:comment ""))

The signature specifies a method named Id, expecting the listed arguments. All @elemref['(adv "class")]{classes}
implementing the (scheme interface) must contain a @elemref['(adv "method")]{method} with the same name, 
return type, and argument types. A method that does not return a value uses the @(scheme void) designation instead
of a Type.

@(scheme abstract MethodReturn Id(Type Id , ...)(code:comment ""))

A signature may be declared @(scheme abstract). This does not impact the method behavior; 
all signatures are by default abstract.

@section[#:tag "advanced:mods"]{@scheme[Modifiers]}

@elemtag['(adv "mods")]

The modifiers @(scheme public), @(scheme private), and @(scheme protected) controll access to the 
modified member. A public member can be accessed by any class. A private member can only be accessed
by the containing class. A protected member can be accessed by the containing class and subclasses 
and all classes that are members of the same package. A member without a modifier can be accessed
by all members of the same package.

@section[#:tag "advanced:field"]{@scheme[Field]}

@elemtag['(adv "field")]

@itemize{
  @item{@(scheme Type Id (code:comment "")) 
         
         Creates a field, bound to Id, that can be used within the current class, or on instances of the 
         current class using an @elemref['(adv "acc")]{expression}.
         This field will have the declared type and will contain a default value of this type if uninitialized.
         }
  @item{@(scheme Type Id = Expression (code:comment ""))
  
         Creates a field, bound to Id, that can be used within the current class, or on instances of the 
         current class using an @elemref['(adv "acc")]{expression}.
         This field will have the declared type and the value of the evaluated @(scheme Expression). 
  }
  @item{@(scheme Type Id = ArrayInit (code:comment ""))
         
         Creates a field, bound to Id, that can be used within the current class, or on instances of the
         current class using an @elemref['(adv "acc")]{expression}.
         This field must have an array type and the value is that of the evaluated @elemref['(adv "ainit")]{array initialization specification}.
  }
  }
  
  All fields with @(scheme static) preceeding their declaration are tied to the class and not tied
  to an instance of the class. They can be accessed and initialized using the standard techniques
  for non-static fields. They may also be accessed with the class name preceeding the field name:
  @(scheme Id.Id). An initializing expression cannot use the @(scheme this) expression.

@section[#:tag "advanced:method"]{@scheme[Method]}

@elemtag['(adv "method")]
@(scheme MethodReturn Id( Type Id , ...) { Statement ... })

Creates a method, bound to Id, that can be called on the current object, or instances of this class.
The body of the method, the @elemref['(adv "stmt")]{statements}, will be evaluated sequentially when the method is called.
The method name may not be the name of any classes defined in the same program or of any fields or methods in the same class.
A method that does not return a value uses the @(scheme void) designation instead of a Type for MethodReturn.

@(scheme abstract MethodReturn Id( Type Id , ...) (code:comment ""))

Creates a method, bount to Id, inside an @elemref['(adv "class")]{abstract class}. Like an @elemref['(adv "sig")]{interface signature},
non-abstract classes that inherit this method must provide an implementation.

@(scheme final MethodReturn Id( Type Id , ...) { Statement ... })

Creates a method, bound to Id, that cannot be overridden by future classes.

@(scheme static MethodReturn Id( Type Id , ...) { Statement ...})
Creates a method, bound to Id, that is tied to the class, not the instance of the
class. This method cannot use the @(scheme this) expression within the Statement body.

Multiple methods can appear in a class body with the same name, provided that for 
each method with a given name the type of arguments or the number of arguments is unique.

@section[#:tag "advanced:ctor"]{@scheme[Constructor]}

@elemtag['(adv "ctor")]
@(scheme Id( Type Id , ...) { Statement ... } )

Creates a constructor that is used in creating an @elemref['(adv "new")]{instance} of a @elemref['(adv "class")]{class} (called an object). 
The arguments given when creating an instance must be of the same type, and in the same order, as that specified by the constructor.
The statements are executed in sequence in intializing the object. If the parent of the current class contains a constructor, that expects
parameters, then the first statement in the constructor must be a @elemref['(adv "super")]{@(scheme super) call}. 

Multiple constructors can appear in a class body, provided that for each constructor the type of arguments or the number of arguments 
is unique. Each constructor may set its own @elemref['(adv "mods")]{access}. A constructor in the same class can be called using a
@elemref['(adv "thisC")]{@(scheme this) call}. This must be the first statement.

@section[#:tag "advanced:stmt"]{@scheme[Statement]}

@elemtag['(adv "stmt")]{}

@itemize{
 @item{@elemtag['(adv "asgn")] @(scheme Expression = Expression (code:comment ""))
               
       The first expression must be a field reference, array position reference, or a variable. 
       The value of this variable, field, or array position will be changed to be the value of the 
       evalauated expression on the right-hand side of =. 
       }
 @item{@elemtag['(adv "if")] @(scheme if (Expression) Statement else Statement )
               
       In this statement the expression should have a boolean type. It is evaluated first. 
       If the expression evaluates to @(scheme true), then the first statement (known as the then clause) is evaluated. 
       If the expression evaluates to @(scheme false), the statement following else (the else clause) is evaluated. Both
       statements may be blocks, including { }.
               }
  @item{@(scheme if (Expression) Statement)
               
       In this statement the expression should have a boolean type. It is evaluated first.
       If the expression evaluates to @(scheme true), then the statement is evaluated;
       otherwise the statement has completed evaluation.
       }

 @item{@elemtag['(adv "return")] @(scheme return Expression (code:comment ""))
               
               This form evaluates the expression, and then returns the value of the expression 
               as the result of the @elemref['(adv "method")]{method} in which it is contained. 
               }
 @item{ @(scheme return (code:comment ""))
               
               This form causes the method to cease evaluation, without producing a value. Should be used
               in conjunction with @(scheme void) for the MethodReturn.}
  @item{@elemtag['(adv "block")] @(scheme { Statement ... })
                
        This statement groups the sequence of statements together, commonly called a block. 
        The statements evaluate sequentially.
        }
  @item{@elemtag['(adv "while")]@(scheme while(Expression) { Statement ... })
                
        Evaluates the expression, which must have type boolean. 
        If the resulting value is true, then the statements are evaluated.
        After evaluating the statements, the expression is evaluated again.
        This repeats until the resulting value is false;
        once false, the statements are not evaluated and the while statement has completed evaluation.
                }
  @item{@elemtag['(adv "do")]@(scheme do { Statement ... } while (expression))
                
       The do statement evaluates the Statements, and then evaluates the
       expression, which must have type boolean. If the expression is true
       the statements evaluate again. This repeats until the expression is
       false, afterwhich the do statement has completed evaluation.
       }
  @item{@elemtag['(adv "for")]@(scheme for (ForInit ForExpression ForUpdate , ...) { Statement ... } )
                
                The ForInit first initializes a variable, or evaluates a set of expressions. The variable
                can only be seen within the ForExpression, ForUpdate, and within the Statements.
                Then the ForExpression is evaluated, when true the Statements are evaluated.
                Subsequently, the ForUpdate evaluates a set of statement expressions or assignments before
                evaluating the expression again. Other than the ForInit stage, this repeats until the
                expression evaluates to false, afterwhich the for statement has completed evaluation.
                }
  @item{@elemtag['(adv "brk")]@(scheme break (code:comment ""))
                
                Can only appear within the statement group of a while loop, do loop, or for loop. Causes
                the loop evaluation to complete without checking the expression again.
                }
  @item{@elemtag['(adv "cont")]@(scheme continue (code:comment ""))
                
                Can only appear within the statement group of a while loop, do loop, or for loop. Causes
                the statement group evaluation to complete, repeating to the evaluation of the conditional.
                }
  @item{@elemtag['(adv "super")]@(scheme super(Expression , ...)(code:comment ""))
   
        May only appear as the first statement of a @elemref['(adv "ctor")]{constructor}. Calls the
        constructor for the parent class using the given expressions as arguments. Expressions
        are evaluated left to right.
        }
  @item{@elemtag['(adv "thisC")]@(scheme this(Expression , ...)(code:comment ""))
                
        May only appear as the first statement of a @elemref['(adv "ctor")]{constructor}. Calls
        a different constructor from the same class, chosen by analyzing the given expressions.
   }
   @item{@elemtag['(adv "varDecl")] @(scheme Type Id (code:comment ""))
         
         Creates a local variable Id within a method body or a block statement; 
         it is not visible outside the block or method, or to statements the preceed
         the declaration. The variable must be initialized prior to use.
         }
   @item{@(scheme Type Id = Expression (code:comment ""))

          Creates a local variable Id within a method body or a block statement.
          }
   @item{@elemtag['(adv "stmtExpr")]@(scheme StatementExpression (code:comment ""))
          
          This set of expressions can be used in a statement position, provided they
          are followed by ';'.
          }
   }

@section[#:tag "advanced:arrayInt"]{@scheme[ArrayInit]}

@elemtag['(adv "ainit")]

This syntax specifies an array to be created holding initial values as specified.

@(scheme { Expression , ... })

This form creates a one dimensional array, where the values are the result of evaluating
each expression, left to right.

@(scheme { ArrayInit , ...})

This form creates a multi-dimensional array, where the values for this array are arrays,
with their values specified by the ArrayInit.

@section[#:tag "advanced:expr"]{@scheme[Expression]}

@itemize{
 
   @item{@elemtag['(adv "op")]@(scheme Expression Op Expression)
                 
                 Performs the mathematical or logical operation Op on the value of the two expressions.
                 }
   @item{@(scheme - Expression)}
   @item{@(scheme ! Expression)
          
          Performs logical negation on the value of the expression.
          }
   @item{@elemtag['(adv "opAs")]@(scheme ++ Expression)
                 
                 The expression must be a field access, variable, or array position access, where the type must be a number. 
                 This form causes 1 to be added to the value of the field, variable, or array position. Returns the augmented
                 number.}
   @item{@(scheme Expression ++)
          
          Like @(scheme ++ Expression), except the returned value is the initial number held by Expression.
          }
   @item{@(scheme -- Expression)
                 
                 The expression must be a field access, variable, or array position access, where the type must be a number. 
                 This form causes 1 to be subtracted from the value of the field, variable, or array position. Returns the decremented
                 number.}
   @item{@(scheme Expression --)
          
          Like @(scheme -- Expression), except the returned value is the initial number held by Expression.
          }
   
   @item{ @elemtag['(adv "this")]@(scheme this)
                  
          Allows access to the current object. Within a class, fields and methods of the current 
          class can be accessed through @(scheme this).
                  }
   @item{ @elemtag['(adv "call")]@(scheme Id(Expression , ...))
                  
                  Id names a method of the current class to be called by the current expression.
                  The expressions following Id are 
                  evaluated from left to right and passed in to the method as its arguments. 
                  The number and types of the arguments must match the @elemref['(adv "method")]{method's declaration}. 
                  These values replace the argument names in the body of the method, and the result of the body is the result of this expression. 
                  }

   @item{@(scheme Expression.Id(Expression , ...))
                  
                  The first expression must evaluate to an object value. Id names a method of this
                  object to be called by the current expression. The expressions following Id are 
                  evaluated from left to right and passed in to the method as its arguments. 
                  The number and types of the arguments must match the @elemref['(adv "method")]{method's declaration}. 
                  These values replace the argument names in the body of the method, and the result of the body is the result of this expression. 
                  }
   @item{@elemtag['(adv "supercall")] @(scheme super.Id (Expression , ...))
         
         Evaluates the overridden method body using the provided expressions as its arguments.
         }                                         
   @item{ @elemtag['(adv "acc")]@(scheme Expression.Id)
                  
                  The first expression must evaluate to an object value. Id names a field of this
                  object, whose value is retrieved by this expression. 
                  }
   @item{ @elemtag['(adv "aacc")]@(scheme Expression[Expression])
                  
                  The first expression must evaluate into an array object, and the
                  second expression must evaluate into an integer. Evaluation
                  of the full expression retrieves the value stored in the corresponding position in the array.
                  If the integer value is equal to or greater than the size of the array a runtime error will occur. 
                  The indexing of the array begins at 0. 
                  }
   @item{ @elemtag['(adv "new")]@(scheme new Id(Expression , ...))
                  
                  Evaluates to a new instance (object) of the Id class. 
                  The class's @elemref['(adv "ctor")]{constructor} will be run with the given values 
                  (evaluated from left to right) as its arguments. The number and types of these values 
                  select which constructor is used. 
                  }
   @item{ @elemtag['(adv "newA")]@(scheme new Type[Expression][Expression] ...)
                  
         The expressions must all evaluate to integers. Evaluates to a new array value, where the base array holds values of the
         specified type, and the size is specified by the integer values.
         }
   @item{@elemtag['(adv "cast")]@(scheme (Type) Expression)
                 
                 Evaluates Expression and then confirms that the value matches the specified type.
                 During compilation, the resulting expression has the specified type. 
                 If during evaluation, this is not true, an error is raised; 
                 otherwise the result of this expression is the result of Expression.
                 }
   @item{@elemtag['(adv "instof")]@(scheme Expression instanceof Type)
                 
                 Evaluates Expression and then confirms that the value matches the specified type.
                 Returns @(scheme true) when the type matches and @(scheme false) otherwise.
                 }
   @item{@elemtag['(adv "?")]@(scheme Expression ? Expression : Expression)
                 
                 This form is an expression form of @(scheme if). The first expression is evaluated (and must have type boolean), 
                 if it is true then the second expression is evaluated and this is the result of the ? expression. 
                 If the first expression is false, then the third expression is evaluated and this is the result of the ? expression. 
                 The second and third expressions must have types that are assignable to one another.
                 }
   @item{ @elemtag['(adv "check")]@(scheme check Expression expect Expression)
                  
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
   @item{ @elemtag['(adv "misc")]@(scheme (Expression))}
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
   
   
   }