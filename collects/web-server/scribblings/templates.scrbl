#lang scribble/doc
@(require "web-server.ss")
@(require (for-label web-server/servlet
                     web-server/templates
                     scheme/promise
                     scheme/list
                     xml))

@(define xexpr @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression})
@(define at-reader-ref @secref[#:doc '(lib "scribblings/scribble/scribble.scrbl")]{reader})
@(define text-ref @secref[#:doc '(lib "scribblings/scribble/scribble.scrbl")]{preprocessor})

@title[#:tag "templates"]{Templates}

@defmodule[web-server/templates]

The @web-server provides a powerful Web template system for separating the presentation logic of a Web application
and enabling non-programmers to contribute to PLT-based Web applications.

@margin-note{Although all the examples here generate HTML, the template language and the @text-ref it is based on can
             be used to generate any text-based format: C, SQL, form emails, reports, etc.} 

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

There are no constraints on how the lexical context of the template is populated. For instance, you can built template abstractions
by wrapping the inclusion of a template in a function:
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

Furthermore, there are no constraints on the Scheme used by templates: they can use macros, structs, continuation marks, threads, etc.
However, Scheme values that are ultimately returned must be printable by the @text-ref@"."
For example, consider the following outputs of the 
title line of different calls to @scheme[fast-template]:

@itemize{

@item{
@schemeblock[
 (fast-template 'Templates)
] 
@verbatim[#:indent 2]|{
  <head><title>Fastest Templates in the West!</title></head>
}|
}

@item{
@schemeblock[
 (fast-template 42)
] 
@verbatim[#:indent 2]|{
  <head><title>Fastest 42 in the West!</title></head>
}|
}

@item{
@schemeblock[
 (fast-template (list "Noo" "dles"))
] 
@verbatim[#:indent 2]|{
  <head><title>Fastest Noodles in the West!</title></head>
}|
}

@item{
@schemeblock[
 (fast-template (lambda () "Thunks"))
] 
@verbatim[#:indent 2]|{
  <head><title>Fastest Thunks in the West!</title></head>
}|
}

@item{
@schemeblock[
 (fast-template (delay "Laziness"))
] 
@verbatim[#:indent 2]|{
  <head><title>Fastest Laziness in the West!</title></head>
}|
}
}

@section{Gotchas}

To obtain an @"@" symbol in template output, you must escape the @"@" symbol, because it is the escape character of the @at-reader-ref syntax.
For example, to obtain:
@verbatim[#:indent 2]|{
  <head><title>Fastest @s in the West!</title></head>
}|
You must write:
@verbatim[#:indent 2]|{
  <head><title>Fastest @"@"s in the West!</title></head>
}|
as your template: literal @"@"s must be replaced with @"@\"@\"".

The other gotcha is that since the template is compiled into a Scheme program, only its results will be printed. For example, suppose 
we have the template:
@verbatim[#:indent 2]|{
 <table>
  @for[([c clients])]{
   <tr><td>@(car c), @(cdr c)</td></tr>
  }
 </table>
}|

If this is included in a lexical context with @scheme[clients] bound to @schemeblock[(list (cons "Young" "Brigham") (cons "Smith" "Joseph"))]
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
         
@section{Conversion Example}

Alonzo Church has been maintaining a blog with PLT Scheme for some years and would like to convert to @schememodname[web-server/templates].

Here's the code he starts off with:
@schememod[
 scheme
(require xml
         web-server/servlet 
         web-server/servlet-env)

(code:comment "He actually Church-encodes them, but we'll use structs.")
(define-struct post (title body comments))

(define posts
  (list 
   (make-post
    "(Y Y) Works: The Why of Y"
    "..."
    (list 
     "First post! - A.T."
     "Didn't I write this? - Matthias"))
   (make-post
    "Church and the States"
    "As you may know, I grew up in DC, not technically a state..."
    (list
     "Finally, A Diet That Really Works! As Seen On TV"))))

(code:comment "A function that is the generic template for the site")
(define (template section body)
  `(html
    (head (title "Alonzo's Church: " ,section)
          (style ([type "text/css"])
                 (code:comment "CDATA objects were useful for returning raw data")
                 ,(make-cdata #f #f "\n   body {\n    margin: 0px;\n    padding: 10px;\n   }\n\n   #main {\n    background: #dddddd;\n   }")))
    (body
     (script ([type "text/javascript"])
             (code:comment "Which is particularly useful for JavaScript")
             ,(make-cdata #f #f "\n   var gaJsHost = ((\"https:\" == document.location.protocol) ?\n     \"https://ssl.\" : \"http://www.\");\n   document.write(unescape(\"%3Cscript src='\" + gaJsHost +\n     \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));\n"))
     (script ([type "text/javascript"])
             ,(make-cdata #f #f "\n   var pageTracker = _gat._getTracker(\"UA-YYYYYYY-Y\");\n   pageTracker._trackPageview();\n"))
     
     (h1 "Alonzo's Church: " ,section)
     (div ([id "main"])
          (code:comment "He had to be careful to use splicing here")
          ,@body))))

(define (blog-posted title body k-url)
  `((h2 ,title)
    (p ,body)
    (h1 (a ([href ,k-url]) "Continue"))))

(define (extract-post req)
  (define binds
    (request-bindings req))
  (define title 
    (extract-binding/single 'title binds))
  (define body
    (extract-binding/single 'body binds))
  (set! posts
        (list* (make-post title body empty)
               posts))
  (send/suspend
   (lambda (k-url)
     (template "Posted" (blog-posted title body k-url))))
  (display-posts))

(define (blog-posts k-url)
  (code:comment "append or splicing is needed")
  (append
   (code:comment "Each element of the list is another list")
   (apply append 
          (for/list ([p posts])
            `((h2 ,(post-title p))
              (p ,(post-body p))
              (ul
               ,@(for/list ([c (post-comments p)])
                   `(li ,c))))))
   `((h1 "New Post")
     (form ([action ,k-url])
           (input ([name "title"]))
           (input ([name "body"]))
           (input ([type "submit"]))))))

(define (display-posts)
  (extract-post
   (send/suspend
    (lambda (k-url)
      (template "Posts" (blog-posts k-url))))))

(define (start req)
  (display-posts))

(serve/servlet start)
]

Luckily, Alonzo has great software engineering skills, so he's already separated the presentation logic into the functions
@scheme[blog-posted], @scheme[blog-posts], and @scheme[template]. Each one of these will turn into a different
template.

@filepath{blog.html}:
@verbatim[#:indent 2]|{
<html>
 <head>
  <title>Alonzo's Church: @|section|</title>
  <style type="text/css">
   body {
    margin: 0px;
    padding: 10px;
   }

   #main {
    background: #dddddd;
   }
  </style>
 </head>
 <body>
  <script type="text/javascript">
   var gaJsHost = (("https:" == document.location.protocol) ?
     "https://ssl." : "http://www.");
   document.write(unescape("%3Cscript src='" + gaJsHost +
     "google-analytics.com/ga.js' 
      type='text/javascript'%3E%3C/script%3E"));
  </script>
  <script type="text/javascript">
   var pageTracker = _gat._getTracker("UA-YYYYYYY-Y");
   pageTracker._trackPageview();
  </script>

  <h1>Alonzo's Church: @|section|</h1>
  <div id="main">
    @body
  </div>
 </body>
</html>
}|

Notice that this part of the presentation is much simpler, because the CSS and JavaScript
can be included verbatim, without resorting to any special escape-escaping patterns.
Similarly, since the @scheme[body] is represented as a string, there is no need to
remember if splicing is necessary.

@filepath{blog-posts.html}:
@verbatim[#:indent 2]|{
@in[p posts]{
 <h2>@(post-title p)</h2>
 <p>@(post-body p)</p>
 <ul>
  @in[c (post-comments p)]{
   <li>@|c|</li>
  }
 </ul>
}

<h1>New Post</h1>
<form action="@|k-url|">
 <input name="title" />
 <input name="body" />
 <input type="submit" />
</form>
}|

This template is even simpler, because there is no list management whatsoever. The defaults "just work".
For completeness, we show the final template:

@filepath{blog-posted.html}:
@verbatim[#:indent 2]|{
<h2>@|title|</h2>
<p>@|body|</p>

<h1><a href="@|k-url|">Continue</a></h1>
}|

The code associated with these templates is very simple as well:
@schememod[
 scheme
(require web-server/templates
         web-server/servlet
         web-server/servlet-env)

(define-struct post (title body comments))

(define posts ....)

(define (template section body)
  (list TEXT/HTML-MIME-TYPE
        (include-template "blog.html")))

(define (extract-post req)
  (define binds
    (request-bindings req))
  (define title
    (extract-binding/single 'title binds))
  (define body 
    (extract-binding/single 'body binds))
  (set! posts
        (list* (make-post title body empty)
               posts))
  (send/suspend
   (lambda (k-url)
     (template "Posted" (include-template "blog-posted.html"))))
  (display-posts))

(define (display-posts)
  (extract-post
   (send/suspend
    (lambda (k-url)
      (template "Posts" (include-template "blog-posts.html"))))))

(define (start req)
  (display-posts))

(serve/servlet start)
] 