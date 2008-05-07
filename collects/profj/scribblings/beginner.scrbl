#lang scribble/doc

@(require scribble/manual "java-scribble.ss")

@title[#:style 'toc #:tag "beginner"]{ProfessorJ Beginner}

@javagrammar[Program [Import ... Def ...]]

@javagrammar[ #:literals(import *)
              #:tag '(beg "import")
              Import 
              [import Name (code:comment "")]
              [import Name.* (code:comment "")]]

@javagrammar[ #:literals(class interface implements)
              Def
              [#:tag '(beg "class") class Id { Member Member ... } ]
              [#:tag '(beg "class") class Id implements Id { Member Member ... } ]
              [#:tag '(beg "iface") interface Id { Signature ... } ]]

@javagrammar[#:tag '(beg "sig")
             Signature
             [Type Id( Type Id , ... )(code:comment "")]]

@javagrammar[Member [Field][Method][Constructor]]

@javagrammar[#:literals(=) #:tag '(beg "field") 
             Field 
             [Type Id = Expression (code:comment "")]
             [Type Id (code:comment "")]]

@javagrammar[#:tag '(beg "method")
             Method [Type Id( Type Id , ...) { Statement } ] ]

@javagrammar[#:tag '(beg "ctor")
             Constructor [Id( Type Id , ...) { Init ... } ]]

@javagrammar[#:literals(this =) #:tag '(beg "init")
             Init
             [this.Id = Id (code:comment "")]]

@javagrammar[#:literals(if else return) 
             Statement
             [#:tag '(beg "if") if (Expression) { Statement } else { Statement } ]
             [#:tag '(beg "return") return Expression (code:comment "")]]

@javagrammar[#:literals(- this ! new true false check expect within)
             Expression
             [#:tag '(beg "op") Expression Op Expression]
             [#:tag '(beg "op") - Expression]
             [#:tag '(beg "op") ! Expression]
             [#:tag '(beg "this") this]
             [#:tag '(beg "call") Expression.Id(Expression , ...)]
             [#:tag '(beg "acc") Expression.Id]
             [#:tag '(beg "new") new Id(Expression , ...)]
             [#:tag '(beg "check") check Expression expect Expression]
             [#:tag '(beg "check") check Expression expect Expression within Expression]
             [#:tag '(beg "misc") (Expression)]
             [#:tag '(beg "misc") Id]
             [#:tag '(beg "misc") Number]
             [#:tag '(beg "misc") Character]
             [#:tag '(beg "misc") String]
             [#:tag '(beg "misc") true]
             [#:tag '(beg "misc") false]]
                       
@javagrammar[Name [ Id. ... Id]]

@javagrammar[Op [+][-][*][/][<][<=][==][>][>=][&&][||]]

@javagrammar[#:literals(int boolean float short double long byte char String)
                       Type 
                       [Id] [boolean] [int] [char] [double] [float]
                       [long] [byte] [short]]


An @(scheme Id) is a sequence of alphanumeric characters, _, and $.

@section{@scheme[import]}

@elemtag['(beg "import")]
@itemize{
@item{@(scheme import Name (code:comment ""))
       
       Imports a specified class to use within the program.}
@item{@(scheme import Name.* (code:comment ""))
       
       Imports a group of classes that can all be used within the program.}

}

@section{@scheme[class]}

@elemtag['(beg "class")]
@itemize{
 @item{@(scheme class Id { Member Member ...})
        
        Creates a class named Id. One member is required and must be a @elemref['(beg "ctor")]{constructor.}
        }
 @item{@(scheme class Id implements Id { Member Member ...})
        
        Creates a class named Id implements the @elemref['(beg "iface")]{interface}
        named by (scheme implements). One member must be a @elemref['(beg "ctor")]{constructor}. 
        Any @elemref['(beg "sig")]{method} defined by the interface must be a member of this class.
        }
 }

@section{@scheme[interface]}

@elemtag['(beg "iface")]

@(scheme interface Id { Signature ... })

Creates an interface named Id that specifies a set of method signatures for classes to implement.

@elemtag['(beg "sig")]

@(scheme Type Id(Type Id , ...) (code:comment ""))

The signature specifies a method named Id, expecting the listed arguments. All @elemref['(beg "class")]{classes}
implementing the (scheme interface) must contain a @elemref['(beg "method")]{method} with the same name, 
return type, and argument types.

@section{@scheme[Field]}

@elemtag['(beg "field")]

@itemize{
  @item{@(scheme Type Id (code:comment "")) 
         
         Creates a field, bound to Id, that can be used within the current class, or on instances of the 
         current class using an @elemref['(beg "acc")]{expression}.
         This field will have the declared type and must be initialized to its value using an @elemref['(beg "init")]{Init}
         in the constructor. 
         }
  @item{@(scheme Type Id = Expression (code:comment ""))
  
         Creates a field, bound to Id, that can be used within the current class, or on instances of the 
         current class using an @elemref['(beg "acc")]{expression}.
         This field will have the declared type and the value of the evaluated @(scheme Expression). 
         @(scheme Expression) may not refer to other fields in the current class.   
  }
  }

@section{@scheme[Method]}

@elemtag['(beg "method")]
@(scheme Type Id( Type Id , ...) { Statement })

Creates a method, bound to Id, that can be called on the current object, or instances of this class.
The body of the method, the @elemref['(beg "stmt")]{statement}, will be evaluated when the method is called.
The method name may not be the name of any classes defined in the same program or of any fields or methods in the same class.

@section{@scheme[Constructor]}

@elemtag['(beg "ctor")]
@(scheme Id( Type Id , ...) { Init ... } )

Creates a constructor that is used in creating an @elemref['(beg "new")]{instance} of a @elemref['(beg "class")]{class} (called an object). 
The arguments given when creating an instance must be of the same type, and in the same order, as that specified by the constructor. 
All of the uninitialized @elemref['(beg "field")]{fields} of the class must be set in the constructor by the Init sequence.

@elemtag['(beg "init")]
@(scheme this.Id = Id (code:comment ""))

The initialization statements pass the value provided to the constructor to the specified field for later use.

@section{@scheme[Statement]}

@elemtag['(beg "stmt")]
@itemize{
 @item{@elemtag['(beg "if")] @(scheme if (Expression) { Statement } else { Statement })
               
       In this statement the expression should have a boolean type. It is evaluated first. 
       If the expression evaluates to @(scheme true), then the first statement (known as the then clause) is evaluated. 
       If the expression evaluates to @(scheme false), the statement following else (the else clause) is evaluated.
               }
 @item{@elemtag['(beg "return")] @(scheme return Expression (code:comment ""))
               
               This form evaluates the expression, and then returns the value of the expression 
               as the result of the @elemref['(beg "method")]{method} in which it is contained. 
               }

}

@section{@scheme[Expression]}

@itemize{
 
   @item{@elemtag['(beg "op")]@(scheme Expression Op Expression)
                 
                 Performs the mathematical or logical operation Op on the value of the two expressions.
                 }
   @item{@(scheme - Expression)}
   @item{@(scheme ! Expression)
          
          Performs logical negation on the value of the expression.
          }
   @item{ @elemtag['(beg "this")]@(scheme this)
                  
          Allows access to the current object. Within a class, fields and methods of the current 
          class must be accessed through @(scheme this).
                  }
   @item{ @elemtag['(beg "call")]@(scheme Expression.Id(Expression , ...))
                  
                  The first expression must evaluate to an object value. Id names a method of this
                  object to be called by the current expression. The expressions following Id are 
                  evaluated from left to right and passed in to the method as its arguments. 
                  The number and types of the arguments must match the @elemref['(beg "method")]{method's declaration}. 
                  These values replace the argument names in the body of the method, and the result of the body is the result of this expression. 
                  }
   @item{ @elemtag['(beg "acc")]@(scheme Expression.Id)
                  
                  The first expression must evaluate to an object value. Id names a field of this
                  object, whose value is retrieved by this expression. 
                  }
   @item{ @elemtag['(beg "new")]@(scheme new Id(Expression , ...))
                  
                  Evaluates to a new instance (object) of the Id class. 
                  The class's @elemref['(beg "ctor")]{constructor} will be run with the given values 
                  (evaluated from left to right) as its arguments. These values must be the correct number 
                  and type as specified by the constructor. 
                  }
   @item{ @elemtag['(beg "check")]@(scheme check Expression expect Expression)
                  
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
   @item{ @elemtag['(beg "misc")]@(scheme (Expression))}
   @item{@(scheme Id)}
   @item{@(scheme Number)}
   @item{@(scheme Character)
          
          Values of type @(scheme char) are ASCII characters enclosed by single quotes such as 'a' is the character a. 
          They can be used as numbers as well as characters. 
          }
   @item{@(scheme String)
          
          Strings are created through placing text inside of double quotes. For example "I am a string" is a String. 
          A String value is an instance of the class String, which descends from Object, and can also be created with a constructor.
          }
   @item{@(scheme true)}
   @item{@(scheme false)}
   }