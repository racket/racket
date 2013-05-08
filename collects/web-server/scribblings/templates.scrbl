#lang scribble/doc
@(require "web-server.rkt")
@(require (for-label web-server/servlet
                     web-server/templates
                     racket/include
                     racket/promise
                     racket/list
                     xml))

@(define xexpr @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression})
@(define at-reader-ref @secref[#:doc '(lib "scribblings/scribble/scribble.scrbl")]{reader})
@(define text-ref
   @secref[#:doc '(lib "scribblings/scribble/scribble.scrbl") "text"])

@title[#:tag "templates"]{Templates: Separation of View}

@defmodule[web-server/templates]

The @web-server provides a powerful Web template system for separating
the presentation logic of a Web application and enabling non-programmers
to contribute to Racket-based Web applications.

@margin-note{Although all the examples here generate HTML, the template
  language and the @text-ref it is based on can be used to generate any
  text-based format: C, SQL, form emails, reports, etc.}

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
@racketblock[
 (include-template "static.html")
]

Then the contents of @filepath{static.html} will be read @emph{at compile time} and compiled into a
racket program that returns the contents of @filepath{static.html} as a string:
@racketblock[
 "<html>\n  <head><title>Fastest Templates in the West!</title></head>\n  <body>\n    <h1>Bang!</h1>\n    <h2>Bang!</h2>\n  </body>\n</html>"
]

@section{Dynamic}

@racket[include-template] gives the template access to the @emph{complete lexical context} of the including program. This context can be
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
@racketblock[
 (let ([thing "Templates"])
   (include-template "simple.html"))
]
evaluates to the same content as the static example.

There are no constraints on how the lexical context of the template is populated. For instance, you can built template abstractions
by wrapping the inclusion of a template in a function:
@racketblock[
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

Furthermore, there are no constraints on the Racket used by templates: they can use macros, structs, continuation marks, threads, etc.
However, Racket values that are ultimately returned must be printable by the @|text-ref|.
For example, consider the following outputs of the 
title line of different calls to @racket[fast-template]:

@itemize[

@item{
@racketblock[
 (fast-template 'Templates)
] 
@verbatim[#:indent 2]|{
  ...<title>Fastest Templates in the West!</title>...
}|
}

@item{
@racketblock[
 (fast-template 42)
] 
@verbatim[#:indent 2]|{
  ...<title>Fastest 42 in the West!</title>...
}|
}

@item{
@racketblock[
 (fast-template (list "Noo" "dles"))
] 
@verbatim[#:indent 2]|{
  ...<title>Fastest Noodles in the West!</title>...
}|
}

@item{
@racketblock[
 (fast-template (lambda () "Thunks"))
] 
@verbatim[#:indent 2]|{
  ...<title>Fastest Thunks in the West!</title>...
}|
}

@item{
@racketblock[
 (fast-template (delay "Laziness"))
] 
@verbatim[#:indent 2]|{
  ...<title>Fastest Laziness in the West!</title>...
}|
}
 
@item{
@racketblock[
 (fast-template (fast-template "Embedding"))
] 
@verbatim[#:indent 2]|{
  ...<title>Fastest ...<title>Fastest Embedding in the West!</title>... in the West!</title>...
}|
}
]

@section{Gotchas}

To obtain an @litchar["@"] character in template output, you must
escape the it, because it is the escape character of the
@at-reader-ref syntax.  For example, to obtain:
@verbatim[#:indent 2]|{
  <head><title>Fastest @s in the West!</title></head>
}|
You must write:
@verbatim[#:indent 2]|{
  <head><title>Fastest @"@"s in the West!</title></head>
}|
as your template: literal @litchar["@"]s must be replaced with
@litchar["@\"@\""].  (Note that the double-quotes are basically a Racket
expression, which can be used for longer strings too.)

The @at-reader-ref will read Racket identifiers, so it does not terminate identifiers on punctuations or XML angle brackets.  So,
@verbatim[#:indent 2]|{
  <head><title>Fastest @thing in the @place!</title></head>
}|
will complain that the identifier @racket[place!</title></head>] is
undefined. You can subvert this by explicitly delimiting the
identifier:
@verbatim[#:indent 2]|{
  <head><title>Fastest @thing in the @|place|!</title></head>
}|

Another gotcha is that since the template is compiled into a Racket
program, only its results will be printed. For example, suppose we
have the template:
@verbatim[#:indent 2]|{
 <table>
  @for[([c clients])]{
   <tr><td>@(car c), @(cdr c)</td></tr>
  }
 </table>
}|

If this is included in a lexical context with @racket[clients] bound
to
@racketblock[(list (cons "Young" "Brigham") (cons "Smith" "Joseph"))]
then the template will be printed as:
@verbatim[#:indent 2]|{
 <table>
 </table>
}|
because @racket[for] does not return the value of the body.
Suppose that we change the template to use @racket[for/list] (which combines them into a list):
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
because only the final expression of the body of the @racket[for/list]
is included in the result. We can capture all the sub-expressions by
using @racket[list] in the body:
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

The templating library provides a syntactic form to deal with this
issue for you called @racket[in]:
@verbatim[#:indent 2]|{
 <table>
  @in[c clients]{
   <tr><td>@(car c), @(cdr c)</td></tr>
  }
 </table>
}|
Notice how it also avoids the absurd amount of punctuation on line two.

@section{Escaping}

@margin-note{Thanks to Michael W. for this section.}

Because templates are useful for many things (scripts, CSS, HTML,
etc), the Web Server does not assume that the template is for XML-like
content. Therefore when when templates are expanded, no
XML escaping is done by default. Beware of @emph{cross-site scripting}
vulnerabilities! For example, suppose a servlet serves the following
template where @racket[_some-variable] is an input string supplied by
the client:

@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest Templates in the West!</title></head>
  <body>
   @some-variable
  </body>
 </html>
}|

If the servlet contains something like the following:

@racketblock[
 (let ([some-variable (get-input-from-user)])
  (include-template "static.htm"))
]

There is nothing to prevent an attacker from entering
@litchar["<script type=\"text/javascript\">...</script>"] to make the
template expand into:

@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest Templates in the West!</title></head>
  <body>
   <script type="text/javascript">...</script>
  </body>
 </html>
}|

Now the server will send the attacker's code to millions of innocent
users. To keep this from happening when serving HTML, use the
@racket[xexpr->string] function from the @racketmodname[xml] module.

This can be done in the servlet:

@racketblock[
 (require xml)

 (let ([some-variable (xexpr->string (get-input-from-user))])
  (include-template "static.htm"))
]

Alternatively, make the template responsible for its own escaping:

@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest Templates in the West!</title></head>
  <body>
   @(xexpr->string some-variable)
  </body>
 </html>
}|

The improved version renders as:

@verbatim[#:indent 2]|{
 <html>
  <head><title>Fastest Templates in the West!</title></head>
  <body>
    &lt;script type=\"text/javascript\"&gt;...&lt;/script&gt;
  </body>
 </html>
}|

When writing templates, always remember to escape user-supplied input.

@section{HTTP Responses}

The quickest way to generate an HTTP response from a template is using
a @racket[response?] struct:
@racketblock[
 (response/full
  200 #"Okay"
  (current-seconds) TEXT/HTML-MIME-TYPE
  empty
  (list (string->bytes/utf-8 (include-template "static.html"))))
]

Finally, if you want to include the contents of a template inside a larger @xexpr :
@racketblock[
 `(html ,(include-template "static.html"))
]
will result in the literal string being included (and entity-escaped). If you actually want
the template to be unescaped, then create a @racket[cdata] structure:
@racketblock[
 `(html ,(make-cdata #f #f (include-template "static.html")))
]

@section{API Details}

@defform*[((include-template path-spec)
           (include-template #:command-char command-char path-spec))]{
 Compiles the template at @racket[path-spec] using the @at-reader-ref syntax within the enclosing lexical context. The @racket[path-spec] is the same format used by @racket[include]. Use the @racket[command-char] keyword to customize the escape character.

 Examples:
 @racketblock[
  (include-template "static.html")
  (include-template #:command-char #\$ "dollar-static.html")
 ]
}

@defform[(in x xs e ...)]{
 Expands into
 @racketblock[
  (for/list ([x xs])
    (begin/text e ...))
 ]

 Template Example:
 @verbatim[#:indent 2]|{
  @in[c clients]{
   <tr><td>@(car c), @(cdr c)</td></tr>
  }
 }|

 Racket Example:
 @racketblock[
  (in c clients "<tr><td>" (car c) ", " (cdr c) "</td></tr>")
 ]
}
         
@section{Conversion Example}

Al Church has been maintaining a blog with Racket for some years and would like to convert to @racketmodname[web-server/templates].

The data-structures he uses are defined as:
@racketblock[
(define-struct post (title body))

(define posts
  (list 
   (make-post
    "(Y Y) Works: The Why of Y"
    "Why is Y, that is the question.")
   (make-post
    "Church and the States"
    "As you may know, I grew up in DC, not technically a state.")))
]
Actually, Al Church-encodes these posts, but for explanatory reasons, we'll use structs.

He has divided his code into presentation functions and logic functions. We'll look at the presentation functions first.

The first presentation function defines the common layout of all pages.
@racketblock[
(define (template section body)
  (response/xexpr
   `(html
     (head (title "Al's Church: " ,section))
     (body
      (h1 "Al's Church: " ,section)
      (div ([id "main"])
           ,@body)))))
]

One of the things to notice here is the @racket[unquote-splicing] on the @racket[body] argument.
This indicates that the @racket[body] is list of @|xexpr|s. If he had accidentally used only @racket[unquote]
then there would be an error in converting the return value to an HTTP response.

@racketblock[
(define (blog-posted title body k-url)
  `((h2 ,title)
    (p ,body)
    (h1 (a ([href ,k-url]) "Continue"))))
]

Here's an example of simple body that uses a list of @|xexpr|s to show the newly posted blog entry, before continuing to redisplay
the main page. Let's look at a more complicated body:

@racketblock[
(define (blog-posts k-url)
  (append
   (apply append 
          (for/list ([p posts])
            `((h2 ,(post-title p))
              (p ,(post-body p)))))
   `((h1 "New Post")
     (form ([action ,k-url])
           (input ([name "title"]))
           (input ([name "body"]))
           (input ([type "submit"]))))))
]

This function shows a number of common patterns that are required by @|xexpr|s. First, @racket[append] is used to combine
different @|xexpr| lists. Second, @racket[apply append] is used to collapse and combine the results of a @racket[for/list] 
where each iteration results in a list of @|xexpr|s. We'll see that these patterns are unnecessary with templates. Another
annoying patterns shows up when Al tries to add CSS styling and some JavaScript from Google Analytics to all the pages of
his blog. He changes the @racket[template] function to:

@racketblock[
(define (template section body)
  (response/xexpr
   `(html
     (head 
      (title "Al's Church: " ,section)
      (style ([type "text/css"])                 
             "body {margin: 0px; padding: 10px;}"
             "#main {background: #dddddd;}"))
     (body
      (script 
       ([type "text/javascript"])             
       ,(make-cdata 
         #f #f
         "var gaJsHost = ((\"https:\" =="
         "document.location.protocol)"
         "? \"https://ssl.\" : \"http://www.\");"
         "document.write(unescape(\"%3Cscript src='\" + gaJsHost"
         "+ \"google-analytics.com/ga.js' "
         "type='text/javascript'%3E%3C/script%3E\"));"))
      (script
       ([type "text/javascript"])
       ,(make-cdata 
         #f #f
         "var pageTracker = _gat._getTracker(\"UA-YYYYYYY-Y\");"
         "pageTracker._trackPageview();"))     
      (h1 "Al's Church: " ,section)
      (div ([id "main"])
           ,@body)))))
]

@margin-note{Some of these problems go away by using here strings, as described in the documentation on
                  @secref[#:doc '(lib "scribblings/reference/reference.scrbl")]{parse-string}.}

The first thing we notice is that encoding CSS as a string is rather primitive. Encoding JavaScript with strings is even worse for two
reasons: first, we are more likely to need to manually escape characters such as @"\""; second, we need to use a CDATA object, because most
JavaScript code uses characters that "need" to be escaped in XML, such as &, but most browsers will fail if these characters are
entity-encoded. These are all problems that go away with templates.


Before moving to templates, let's look at the logic functions:
@racketblock[
(define (extract-post req)
  (define binds
    (request-bindings req))
  (define title 
    (extract-binding/single 'title binds))
  (define body
    (extract-binding/single 'body binds))
  (set! posts
        (list* (make-post title body)
               posts))
  (send/suspend
   (lambda (k-url)
     (template "Posted" (blog-posted title body k-url))))
  (display-posts))

(define (display-posts)
  (extract-post
   (send/suspend
    (lambda (k-url)
      (template "Posts" (blog-posts k-url))))))

(define (start req)
  (display-posts))
]

To use templates, we need only change @racket[template], @racket[blog-posted], and @racket[blog-posts]:

@racketblock[
(define (template section body)
  (response/full
   200 #"Okay"
   (current-seconds) TEXT/HTML-MIME-TYPE
   empty
   (list (string->bytes/utf-8 (include-template "blog.html")))))

(define (blog-posted title body k-url)
  (include-template "blog-posted.html"))

(define (blog-posts k-url)
  (include-template "blog-posts.html"))
]

Each of the templates are given below:

@filepath{blog.html}:
@verbatim[#:indent 2]|{
<html>
 <head>
  <title>Al's Church: @|section|</title>
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

  <h1>Al's Church: @|section|</h1>
  <div id="main">
    @body
  </div>
 </body>
</html>
}|

Notice that this part of the presentation is much simpler, because the CSS and JavaScript
can be included verbatim, without resorting to any special escape-escaping patterns.
Similarly, since the @racket[body] is represented as a string, there is no need to
remember if splicing is necessary.

@filepath{blog-posted.html}:
@verbatim[#:indent 2]|{
<h2>@|title|</h2>
<p>@|body|</p>

<h1><a href="@|k-url|">Continue</a></h1>
}|

@filepath{blog-posts.html}:
@verbatim[#:indent 2]|{
@in[p posts]{
 <h2>@(post-title p)</h2>
 <p>@(post-body p)</p>
}

<h1>New Post</h1>
<form action="@|k-url|">
 <input name="title" />
 <input name="body" />
 <input type="submit" />
</form>
}|

Compare this template with the original presentation function: there is no need to worry about managing how lists
are nested: the defaults @emph{just work}.
