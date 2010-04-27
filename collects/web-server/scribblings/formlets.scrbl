#lang scribble/doc
@(require "web-server.ss")
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

@schemeblock[
(define date-formlet
  (formlet
   (div
    "Month:" ,{input-int . => . month}
    "Day:" ,{input-int . => . day})
   (list month day)))
]

The first part of the @scheme[formlet] syntax is the template of an @xexpr that is the rendering
of the formlet. It can contain elements like @scheme[,(_formlet . => . _name)] where @scheme[_formlet]
is a formlet expression and @scheme[_name] is an identifier bound in the second part of the @scheme[formlet]
syntax.

This formlet is displayed (with @scheme[formlet-display]) as the following @xexpr forest (list):

@schemeblock[
(list 
 '(div "Month:" (input ([name "input_0"]))
       "Day:" (input ([name "input_1"]))))
]

@scheme[date-formlet] not only captures the rendering of the form, but also the request processing
logic. If we send it an HTTP request with bindings for @scheme["input_0"] to @scheme["10"] and
@scheme["input_1"] to @scheme["3"], with @scheme[formlet-process], then it returns:

@schemeblock[
(list 10 3)
]

which is the second part of the @scheme[formlet] syntax, where @scheme[month] has been replaced with the
integer represented by the @scheme["input_0"] and @scheme[day] has been replaced with the
integer represented by the @scheme["input_1"].

The real power of formlet is that they can be embedded within one another. For instance, suppose we want to
combine two date forms to capture a travel itinerary. The following formlet does the job:

@schemeblock[
(define travel-formlet
  (formlet
   (div
    "Name:" ,{input-string . => . name}
    (div
     "Arrive:" ,{date-formlet . => . arrive}
     "Depart:" ,{date-formlet . => . depart})
   (list name arrive depart))))
]

(Notice that @scheme[date-formlet] is embedded twice.) This is rendered as:

@schemeblock[
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

Observe that @scheme[formlet-display] has automatically generated unique names for each input element. When we pass
bindings for these names to @scheme[formlet-process], the following list is returned:

@schemeblock[
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
 Constructs a @tech{formlet} with the specified @scheme[rendering] and the processing
 resulting in the @scheme[yields-expr] expression. The @scheme[rendering] form is a quasiquoted
 @xexpr, with two special caveats:
 
 @scheme[,{_formlet-expr . => . _name}] embeds the
 @tech{formlet} given by @scheme[_formlet-expr]; the result of this processing this formlet is
 available in the @scheme[yields-expr] as @scheme[_name]. 
 
 @scheme[,{_formlet-expr . => . (values _name ...)}] embeds the
 @tech{formlet} given by @scheme[_formlet-expr]; the results of this processing this formlet is
 available in the @scheme[yields-expr] as @scheme[_name ...]. 
 
 @scheme[(#%# _xexpr ...)] renders an @xexpr forest.
}

@defidform[#%#]{Only allowed inside @scheme[formlet].}

}

@section{Functional Usage}

@(require (for-label web-server/formlets/lib))
@defmodule[web-server/formlets/lib]{

The syntactic shorthand abbreviates the construction of @deftech{formlet}s with the following library.
These combinators may be used directly to construct low-level formlets, such as those for new INPUT element
types. Refer to @secref["input-formlets"] for example low-level formlets using these combinators.

@defthing[xexpr-forest/c contract?]{
 Equivalent to @scheme[(listof xexpr/c)]
}

@defproc[(formlet/c [content any/c] ...) contract?]{
 Equivalent to @scheme[(integer? . -> . 
            (values xexpr-forest/c
                    ((listof binding?) . -> . (values (coerce-contract 'formlet/c content) ...))
                    integer?))].
                               
 A @tech{formlet}'s internal representation is a function from an initial input number
 to an @xexpr forest rendering, a processing function, and the next allowable
 input number.
}

@defthing[formlet*/c contract?]{
  Equivalent to @scheme[(formlet/c any/c ...)].
}

@defproc[(pure [value any/c]) (formlet/c any/c)]{
 Constructs a @tech{formlet} that has no rendering and always returns @scheme[value] in
 the processing stage.
}

@defproc[(cross [f (formlet/c procedure?)]
                [g (formlet/c any/c ...)])
         (formlet/c any/c ...)]{
 Constructs a @tech{formlet} with a rendering equal to the concatenation of the renderings of @tech{formlet}s @scheme[f] and @scheme[g];
 a processing stage that applies @scheme[g]'s processing results to @scheme[f]'s processing result.
}

@defproc[(cross* [f (formlet/c (() () #:rest (listof any/c) . ->* . any/c))]
                 [g (formlet/c any/c)] ...)
         (formlet/c any/c)]{
 Equivalent to @scheme[cross] lifted to many arguments.
}

@defproc[(xml-forest [r xexpr-forest/c])
         (formlet/c procedure?)]{
 Constructs a @tech{formlet} with the rendering @scheme[r] and the identity procedure as the processing step.
}

@defproc[(xml [r xexpr/c])
         (formlet/c procedure?)]{
 Equivalent to @scheme[(xml-forest (list r))].
}
                                
@defproc[(text [r string?])
         (formlet/c procedure?)]{
 Equivalent to @scheme[(xml r)].
}
                                
@defproc[(tag-xexpr [tag symbol?]
                    [attrs (listof (list/c symbol? string?))]
                    [inner (formlet/c any/c)])
         (formlet/c any/c)]{
 Constructs a @tech{formlet} with the rendering @scheme[(list (list* tag attrs inner-rendering))] where @scheme[inner-rendering] is
 the rendering of @scheme[inner] and the processing stage identical to @scheme[inner].
}

@defproc[(formlet-display [f (formlet/c any/c)])
         xexpr-forest/c]{
 Renders @scheme[f].
}
                        
@defproc[(formlet-process [f (formlet/c any/c ...)]
                          [r request?])
         (values any/c ...)]{
 Runs the processing stage of @scheme[f] on the bindings in @scheme[r].
}
               
}

@section[#:tag "input-formlets"]{Predefined Formlets}

@(require (for-label web-server/formlets/input))
@defmodule[web-server/formlets/input]{

These @tech{formlet}s are the main combinators for form input.
      
@defproc[(make-input [render (string? . -> . xexpr/c)])
         (formlet/c (or/c false/c binding?))]{
 This @tech{formlet} is rendered with @scheme[render], which is passed the input name, and results in the
 extracted @scheme[binding].
}
                                             
@defproc[(make-input* [render (string? . -> . xexpr/c)])
         (formlet/c (listof binding?))]{
 This @tech{formlet} is rendered with @scheme[render], which is passed the input name, and results in all the
 @scheme[binding]s that use the name.
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
 This @tech{formlet} renders using an SELECT element with an OPTION for each element of the sequence. If @scheme[multiple?] is @scheme[#t], then multiple options may be selected. An element is selected if @scheme[selected?] returns @scheme[#t]. Elements are displayed with @scheme[display].
}

@defproc[(select-input [l sequence?]
                       [#:selected? selected? (any/c . -> . boolean?) (λ (x) #f)]
                       [#:display display (any/c . -> . xexpr/c) (λ (x) x)])
        (formlet/c any/c)]{
 This @tech{formlet} renders using an SELECT element with an OPTION for each element of the sequence. An element is selected if @scheme[selected?] returns @scheme[#t]. Elements are displayed with @scheme[display].
}
                          
@defproc[(required [f (formlet/c (or/c false/c binding?))])
         (formlet/c bytes?)]{
 Constructs a @tech{formlet} that extracts the @scheme[binding:form-value] from the binding produced by @scheme[f], or errors.
}

@defproc[(default 
           [def bytes?]
           [f (formlet/c (or/c false/c binding?))])
         (formlet/c bytes?)]{
 Constructs a @tech{formlet} that extracts the @scheme[binding:form-value] from the binding produced by @scheme[f], or returns @scheme[def].
}
 
@defproc[(to-string [f (formlet/c bytes?)])
         (formlet/c string?)]{
 Converts @scheme[f]'s output to a string. Equivalent to @scheme[(cross (pure bytes->string/utf-8) f)].
}          
 
@defproc[(to-number [f (formlet/c string?)])
         (formlet/c number?)]{
 Converts @scheme[f]'s output to a number. Equivalent to @scheme[(cross (pure string->number) f)].
}                             

@defproc[(to-symbol [f (formlet/c string?)])
         (formlet/c symbol?)]{
 Converts @scheme[f]'s output to a symbol. Equivalent to @scheme[(cross (pure string->symbol) f)].
}                             

@defproc[(to-boolean [f (formlet/c bytes?)])
         (formlet/c boolean?)]{
 Converts @scheme[f]'s output to a boolean, if it is equal to @scheme[#"on"].
}
                             
@defthing[input-string (formlet/c string?)]{
 Equivalent to @scheme[(to-string (required (text-input)))].
}

@defthing[input-int (formlet/c integer?)]{
 Equivalent to @scheme[(to-number input-string)].
}

@defthing[input-symbol (formlet/c symbol?)]{
 Equivalent to @scheme[(to-symbol input-string)].
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
 Uses @scheme[send/suspend] to send @scheme[f]'s rendering (wrapped in a FORM tag whose action is
 the continuation URL (wrapped again by @scheme[wrapper])) to the client.
 When the form is submitted, the request is passed to the
 processing stage of @scheme[f].
}
               
@defproc[(embed-formlet [embed/url embed/url/c]
                        [f (formlet/c any/c ...)])
         xexpr/c]{
 Like @scheme[send/formlet], but for use with @scheme[send/suspend/dispatch].
}

}
