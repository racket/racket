#lang scribble/doc
@(require "web-server.rkt")
@(require (for-label web-server/servlet
                     xml))

@(define xexpr @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression})

@title[#:tag "formlets"]{Formlets: Functional Form Abstraction}

@defmodule[web-server/formlets]

The @web-server provides a kind of Web form abstraction called a @tech{formlet}.

@margin-note{@tech{Formlet}s originate in the work of the @link["http://groups.inf.ed.ac.uk/links/"]{Links} research group in
their paper @link["http://groups.inf.ed.ac.uk/links/formlets/"]{The Essence of Form Abstraction}.}

@section{Basic Formlet Usage}

Suppose we want to create an abstraction of entering a date in an HTML form. The following
@tech{formlet} captures this idea:

@racketblock[
(define date-formlet
  (formlet
   (div
    "Month:" ,{input-int . => . month}
    "Day:" ,{input-int . => . day})
   (list month day)))
]

The first part of the @racket[formlet] syntax is the template of an @xexpr that is the rendering
of the formlet. It can contain elements like @racket[,(_formlet . => . _name)] where @racket[_formlet]
is a formlet expression and @racket[_name] is an identifier bound in the second part of the @racket[formlet]
syntax.

This formlet is displayed (with @racket[formlet-display]) as the following @xexpr forest (list):

@racketblock[
(list 
 '(div "Month:" (input ([name "input_0"]))
       "Day:" (input ([name "input_1"]))))
]

@racket[date-formlet] not only captures the rendering of the form, but also the request processing
logic. If we send it an HTTP request with bindings for @racket["input_0"] to @racket["10"] and
@racket["input_1"] to @racket["3"], with @racket[formlet-process], then it returns:

@racketblock[
(list 10 3)
]

which is the second part of the @racket[formlet] syntax, where @racket[month] has been replaced with the
integer represented by the @racket["input_0"] and @racket[day] has been replaced with the
integer represented by the @racket["input_1"].

The real power of formlet is that they can be embedded within one another. For instance, suppose we want to
combine two date forms to capture a travel itinerary. The following formlet does the job:

@racketblock[
(define travel-formlet
  (formlet
   (div
    "Name:" ,{input-string . => . name}
    (div
     "Arrive:" ,{date-formlet . => . arrive}
     "Depart:" ,{date-formlet . => . depart})
   (list name arrive depart))))
]

(Notice that @racket[date-formlet] is embedded twice.) This is rendered as:

@racketblock[
(list 
 '(div
   "Name:"
   (input ([name "input_0"]))
   (div
    "Arrive:"
    (div "Month:" (input ([name "input_1"]))
         "Day:" (input ([name "input_2"])))
    "Depart:"
    (div "Month:" (input ([name "input_3"]))
         "Day:" (input ([name "input_4"]))))))
]

Observe that @racket[formlet-display] has automatically generated unique names for each input element. When we pass
bindings for these names to @racket[formlet-process], the following list is returned:

@racketblock[
(list "Jay"
      (list 10 3)
      (list 10 6))
]

The rest of the manual gives the details of @tech{formlet} usage and extension.

@section{Syntactic Shorthand}

@(require (for-label web-server/formlets/syntax))
@defmodule[web-server/formlets/syntax]{

Most users will want to use the syntactic shorthand for creating @tech{formlet}s.

@defform[(formlet rendering yields-expr)]{
 Constructs a @tech{formlet} with the specified @racket[rendering] and the processing
 resulting in the @racket[yields-expr] expression. The @racket[rendering] form is a quasiquoted
 @xexpr, with two special caveats:
 
 @racket[,{_formlet-expr . => . _name}] embeds the
 @tech{formlet} given by @racket[_formlet-expr]; the result of this processing this formlet is
 available in the @racket[yields-expr] as @racket[_name]. 
 
 @racket[,{_formlet-expr . => . (values _name ...)}] embeds the
 @tech{formlet} given by @racket[_formlet-expr]; the results of this processing this formlet is
 available in the @racket[yields-expr] as @racket[_name ...]. 
 
 @racket[(#%# _xexpr ...)] renders an @xexpr forest.
}

@defidform[#%#]{Only allowed inside @racket[formlet].}

}

@section{Functional Usage}

@(require (for-label web-server/formlets/lib))
@defmodule[web-server/formlets/lib]{

The syntactic shorthand abbreviates the construction of @deftech{formlet}s with the following library.
These combinators may be used directly to construct low-level formlets, such as those for new INPUT element
types. Refer to @secref["input-formlets"] for example low-level formlets using these combinators.

@defthing[xexpr-forest/c contract?]{
 Equivalent to @racket[(listof xexpr/c)]
}

@defproc[(formlet/c [content any/c] ...) contract?]{
 Equivalent to @racket[(integer? . -> . 
            (values xexpr-forest/c
                    ((listof binding?) . -> . (values (coerce-contract 'formlet/c content) ...))
                    integer?))].
                               
 A @tech{formlet}'s internal representation is a function from an initial input number
 to an @xexpr forest rendering, a processing function, and the next allowable
 input number.
}

@defthing[formlet*/c contract?]{
  Equivalent to @racket[(formlet/c any/c ...)].
}

@defproc[(pure [value any/c]) (formlet/c any/c)]{
 Constructs a @tech{formlet} that has no rendering and always returns @racket[value] in
 the processing stage.
}

@defproc[(cross [f (formlet/c procedure?)]
                [g (formlet/c any/c ...)])
         (formlet/c any/c ...)]{
 Constructs a @tech{formlet} with a rendering equal to the concatenation of the renderings of @tech{formlet}s @racket[f] and @racket[g];
 a processing stage that applies @racket[g]'s processing results to @racket[f]'s processing result.
}

@defproc[(cross* [f (formlet/c (() () #:rest (listof any/c) . ->* . any/c))]
                 [g (formlet/c any/c)] ...)
         (formlet/c any/c)]{
 Equivalent to @racket[cross] lifted to many arguments.
}

@defproc[(xml-forest [r xexpr-forest/c])
         (formlet/c procedure?)]{
 Constructs a @tech{formlet} with the rendering @racket[r] and the identity procedure as the processing step.
}

@defproc[(xml [r xexpr/c])
         (formlet/c procedure?)]{
 Equivalent to @racket[(xml-forest (list r))].
}
                                
@defproc[(text [r string?])
         (formlet/c procedure?)]{
 Equivalent to @racket[(xml r)].
}
                                
@defproc[(tag-xexpr [tag symbol?]
                    [attrs (listof (list/c symbol? string?))]
                    [inner (formlet/c any/c)])
         (formlet/c any/c)]{
 Constructs a @tech{formlet} with the rendering @racket[(list (list* tag attrs inner-rendering))] where @racket[inner-rendering] is
 the rendering of @racket[inner] and the processing stage identical to @racket[inner].
}

@defproc[(formlet-display [f (formlet/c any/c)])
         xexpr-forest/c]{
 Renders @racket[f].
}
                        
@defproc[(formlet-process [f (formlet/c any/c ...)]
                          [r request?])
         (values any/c ...)]{
 Runs the processing stage of @racket[f] on the bindings in @racket[r].
}
               
}

@section[#:tag "input-formlets"]{Predefined Formlets}

@(require (for-label web-server/formlets/input))
@defmodule[web-server/formlets/input]{

These @tech{formlet}s are the main combinators for form input.
      
@defproc[(make-input [render (string? . -> . xexpr/c)])
         (formlet/c (or/c false/c binding?))]{
 This @tech{formlet} is rendered with @racket[render], which is passed the input name, and results in the
 extracted @racket[binding].
}
                                             
@defproc[(make-input* [render (string? . -> . xexpr/c)])
         (formlet/c (listof binding?))]{
 This @tech{formlet} is rendered with @racket[render], which is passed the input name, and results in all the
 @racket[binding]s that use the name.
}
                                             
@defproc[(text-input [#:value value (or/c false/c bytes?) #f]
                     [#:size size (or/c false/c exact-nonnegative-integer?) #f]
                     [#:max-length max-length (or/c false/c exact-nonnegative-integer?) #f]
                     [#:read-only? read-only? boolean? #f]
                     [#:attributes attrs (listof (list/c symbol? string?)) empty])
        (formlet/c (or/c false/c binding?))]{
 This @tech{formlet} renders using an INPUT element with the TEXT type and the attributes given in the arguments.
}
                                                                                        
@defproc[(password-input [#:value value (or/c false/c bytes?) #f]
                         [#:size size (or/c false/c exact-nonnegative-integer?) #f]
                         [#:max-length max-length (or/c false/c exact-nonnegative-integer?) #f]
                         [#:read-only? read-only? boolean? #f]
                         [#:attributes attrs (listof (list/c symbol? string?)) empty])
        (formlet/c (or/c false/c binding?))]{
 This @tech{formlet} renders using an INPUT element with the PASSWORD type and the attributes given in the arguments.
}

@defproc[(textarea-input)
        (formlet/c string?)]{
 This @tech{formlet} renders using an TEXTAREA element.
}
                                            
@defproc[(checkbox [value bytes?]
                   [checked? boolean?]
                   [#:attributes attrs (listof (list/c symbol? string?)) empty])
         (formlet/c (or/c false/c binding?))]{
 This @tech{formlet} renders using a INPUT elemen with the CHECKBOX type and the attributes given in the arguments.
}
                                             
@defproc[(multiselect-input [l sequence?]
                            [#:multiple? multiple? boolean? #t]
                            [#:selected? selected? (any/c . -> . boolean?) (λ (x) #f)]
                            [#:display display (any/c . -> . xexpr/c) (λ (x) x)])
        (formlet/c list?)]{
 This @tech{formlet} renders using an SELECT element with an OPTION for each element of the sequence. If @racket[multiple?] is @racket[#t], then multiple options may be selected. An element is selected if @racket[selected?] returns @racket[#t]. Elements are displayed with @racket[display].
}

@defproc[(select-input [l sequence?]
                       [#:selected? selected? (any/c . -> . boolean?) (λ (x) #f)]
                       [#:display display (any/c . -> . xexpr/c) (λ (x) x)])
        (formlet/c any/c)]{
 This @tech{formlet} renders using an SELECT element with an OPTION for each element of the sequence. An element is selected if @racket[selected?] returns @racket[#t]. Elements are displayed with @racket[display].
}
                          
@defproc[(required [f (formlet/c (or/c false/c binding?))])
         (formlet/c bytes?)]{
 Constructs a @tech{formlet} that extracts the @racket[binding:form-value] from the binding produced by @racket[f], or errors.
}

@defproc[(default 
           [def bytes?]
           [f (formlet/c (or/c false/c binding?))])
         (formlet/c bytes?)]{
 Constructs a @tech{formlet} that extracts the @racket[binding:form-value] from the binding produced by @racket[f], or returns @racket[def].
}
 
@defproc[(to-string [f (formlet/c bytes?)])
         (formlet/c string?)]{
 Converts @racket[f]'s output to a string. Equivalent to @racket[(cross (pure bytes->string/utf-8) f)].
}          
 
@defproc[(to-number [f (formlet/c string?)])
         (formlet/c number?)]{
 Converts @racket[f]'s output to a number. Equivalent to @racket[(cross (pure string->number) f)].
}                             

@defproc[(to-symbol [f (formlet/c string?)])
         (formlet/c symbol?)]{
 Converts @racket[f]'s output to a symbol. Equivalent to @racket[(cross (pure string->symbol) f)].
}                             

@defproc[(to-boolean [f (formlet/c bytes?)])
         (formlet/c boolean?)]{
 Converts @racket[f]'s output to a boolean, if it is equal to @racket[#"on"].
}
                             
@defthing[input-string (formlet/c string?)]{
 Equivalent to @racket[(to-string (required (text-input)))].
}

@defthing[input-int (formlet/c integer?)]{
 Equivalent to @racket[(to-number input-string)].
}

@defthing[input-symbol (formlet/c symbol?)]{
 Equivalent to @racket[(to-symbol input-string)].
}

}

@section{Utilities}

@(require (for-label web-server/formlets/servlet))
@defmodule[web-server/formlets/servlet]{

A few utilities are provided for using @tech{formlet}s in Web applications.

@defproc[(send/formlet [f (formlet/c any/c ...)]
                       [#:wrap wrapper
                               (xexpr/c . -> . response/c)
                               (lambda (form-xexpr)
                                 `(html (head (title "Form Entry"))
                                        (body ,form-xexpr)))])
         (values any/c ...)]{
 Uses @racket[send/suspend] to send @racket[f]'s rendering (wrapped in a FORM tag whose action is
 the continuation URL (wrapped again by @racket[wrapper])) to the client.
 When the form is submitted, the request is passed to the
 processing stage of @racket[f].
}
               
@defproc[(embed-formlet [embed/url embed/url/c]
                        [f (formlet/c any/c ...)])
         xexpr/c]{
 Like @racket[send/formlet], but for use with @racket[send/suspend/dispatch].
}

}
