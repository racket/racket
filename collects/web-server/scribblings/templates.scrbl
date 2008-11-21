#lang scribble/doc
@(require "web-server.ss")
@(require (for-label web-server/servlet
                     web-server/templates
                     scheme/list
                     xml))

@(define xexpr @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression})
@(define at-reader-ref @secref[#:doc '(lib "scribblings/scribble/scribble.scrbl")]{reader})

@title[#:tag "templates"]{Templates}

@defmodule[web-server/templates]

The @web-server provides a powerful Web template system for separating the presentation logic of a Web application
and enabling non-programmers to contribute to PLT-based Web applications.

@local-table-of-contents[]

@section{Static}

Suppose we have a file @filepath{static.html} with the contents:
@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest Templates in the West!</title></head>
  <body>
   <h1>Bang!</h1>
   <h2>Bang!</h2>
  </body>
 </html>
}|

If we write the following in our code:
@schemeblock[
 (include-template "static.html")
]

Then the contents of @filepath{static.html} will be read @emph{at compile time} and compiled into a
Scheme program that returns the contents of @filepath{static.html} as a string:
@schemeblock[
 "<html>\n  <head><title>Fastest Templates in the West!</title></head>\n  <body>\n    <h1>Bang!</h1>\n    <h2>Bang!</h2>\n  </body>\n</html>"
]

@section{Dynamic}

@scheme[include-template] gives the template access to the @emph{complete lexical context} of the including program. This context can be
accessed via the @at-reader-ref syntax. For example, if @filepath{simple.html} contains:
@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest @thing in the West!</title></head>
  <body>
   <h1>Bang!</h1>
   <h2>Bang!</h2>
  </body>
 </html>
}|

Then
@schemeblock[
 (let ([thing "Templates"])
   (include-template "simple.html"))
]
evaluates to the same content as the static example.

There is no constraints on the values, the way they are used, or the way they are defined, that are made accessible to the template.
For example,
@schemeblock[
 (define (fast-template thing)
   (include-template "simple.html"))
 
 (fast-template "Templates")
 (fast-template "Noodles")
]
evalutes to two strings with the predictable contents:
@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest Templates in the West!</title></head>
  <body>
   <h1>Bang!</h1>
   <h2>Bang!</h2>
  </body>
 </html>
}|

and

@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest Noodles in the West!</title></head>
  <body>
   <h1>Bang!</h1>
   <h2>Bang!</h2>
  </body>
 </html>
}|

@section{Gotchas}

One of the most important things to remember about the @at-reader-ref syntax is that the @"@" symbol must be escaped in content:
@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest @"@"s in the West!</title></head>
  <body>
   <h1>Bang!</h1>
   <h2>Bang!</h2>
  </body>
 </html>
}|

The other gotcha is that since the template is compiled into a Scheme program, only its results will be printed. For example, suppose 
we have the template:
@verbatim[#:indent 2]|{
 <table>
  @for[([c clients])]{
   <tr><td>@(car c), @(cdr c)</td></tr>
  }
 </table>
}|

If this is included in a lexical context with @scheme[clients] bound to @scheme[(list (cons "Young" "Brigham") (cons "Smith" "Joseph"))],
then the template will be printed as:
@verbatim[#:indent 2]|{
 <table>
 </table>
}|
because @scheme[for] does not return the value of the body.
Suppose that we change the template to use @scheme[for/list] (which combines them into a list):
@verbatim[#:indent 2]|{
 <table>
  @for/list[([c clients])]{
   <tr><td>@(car c), @(cdr c)</td></tr>
  }
 </table>
}|

Now the result is:
@verbatim[#:indent 2]|{
 <table>
  </tr>
  </tr>
 </table>
}|
because only the final expression of the body of the @scheme[for/list] is included in the result. We can capture all the sub-expressions
by using @scheme[list] in the body:
@verbatim[#:indent 2]|{
 <table>
  @for/list[([c clients])]{
   @list{
    <tr><td>@(car c), @(cdr c)</td></tr>
   }
  }
 </table>
}|
Now the result is:
@verbatim[#:indent 2]|{
 <table>
  <tr><td>Young, Brigham</td></tr>
  <tr><td>Smith, Joseph</td></tr>
 </table>
}|

The templating library provides a syntactic form to deal with this issue for you called @scheme[in]:
@verbatim[#:indent 2]|{
 <table>
  @in[c clients]{
   <tr><td>@(car c), @(cdr c)</td></tr>
  }
 </table>
}|
Notice how it also avoids the absurd amount of punctuation on line two.

@section{HTTP Responses}

The quickest way to generate an HTTP response from a template is using the @scheme[list] response type:
@schemeblock[
 (list #"text/html" (include-template "static.html"))
]

If you want more control then you can generate a @scheme[response/full] struct:
@schemeblock[
 (make-response/full
  200 "Okay"
  (current-seconds) TEXT/HTML-MIME-TYPE
  empty
  (list (include-template "static.html")))
]

Finally, if you want to include the contents of a template inside a larger @xexpr :
@schemeblock[
 `(html ,(include-template "static.html"))
]
will result in the literal string being included (and entity-escaped). If you actually want
the template to be unescaped, then create a @scheme[cdata] structure:
@schemeblock[
 `(html ,(make-cdata #f #f (include-template "static.html")))
]

@section{API Details}

@defform[(include-template path)]{
 Compiles the template at @scheme[path] using the @at-reader-ref syntax within the enclosing lexical context.
          
 Example:
 @schemeblock[
  (include-template "static.html")
 ]                    
}

@defform[(in x xs e ...)]{
 Expands into
 @schemeblock[
  (for/list ([x xs])
   (list e ...))
 ]
 
 Template Example:
 @verbatim[#:indent 2]|{
  @in[c clients]{
   <tr><td>@(car c), @(cdr c)</td></tr>
  }
 }|
 
 Scheme Example:
 @schemeblock[
  (in c clients "<tr><td>" (car c) ", " (cdr c) "</td></tr>")
 ]
}
         