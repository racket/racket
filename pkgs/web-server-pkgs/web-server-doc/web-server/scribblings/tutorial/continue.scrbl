#lang scribble/doc
@(require scribble/manual
          (for-label racket
                     (except-in web-server/servlet make-url)
                     db)
          "tutorial-util.rkt")

@(define xexpr @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression})

@title{Continue: Web Applications in Racket}

@author[(author+email "Danny Yoo" "dyoo@hashcollision.org")
        (author+email "Jay McCarthy" "jay@cs.byu.edu")]

How do we make dynamic web applications?
In this tutorial, we show how to use Racket to achieve this goal.
We explain how to start up a web server, how to generate dynamic web content,
and how to interact with the user. Our working example will be a simple
web journal---a ``blog.''

This tutorial is intended for students who have read enough of
@italic{@link["http://www.htdp.org/"]{How to Design Programs}} to know how to
use structures, higher-order functions, the @racket[local] syntax, and a little
bit of mutation.

@section{Getting Started}

Everything you need in this tutorial is provided in
@link["http://racket-lang.org/"]{Racket}; we will be using the DrRacket module
language.

Enter the following into DrRacket's Definitions window, then press the
@onscreen{Run} button.

@racketmod[
web-server/insta
(define (start request)
  (response/xexpr
   '(html
     (head (title "My Blog"))
     (body (h1 "Under construction")))))
]

If a web browser comes up with an ``Under Construction'' page, then clap your
hands with delight, because you've built your first web application!  We
haven't yet gotten it to do much, but we'll get there.  For now,
press the @onscreen{Stop} button to shut the server down.

@section{The Application}

We want to motivate this tutorial by showing you how to develop a blog.
Users of the blog should be able to create new posts and add comments to
existing posts. We'll approach the task iteratively, pointing out one or two
pitfalls along the way. The game plan will be approximately as follows:

@itemize[
         @item{Show a static list of posts.}
         @item{Allow a user to add new posts to the system.}
         @item{Extend the model to let a user add comments to a post.}
         @item{Allow all users to share the same set of posts.}
         @item{Serialize our data structures to disk.}
         ]

By the end of the tutorial, we'll have a simple blogging application up and
running.

@section{Basic Blog}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-1)]

We begin by defining the necessary data structures. A post is:

@racketblock[(struct post (title body))]

@(defstruct* post ([title string?] [body string?]))

@bold{Exercise.} Make a few examples of posts.

Next we define a blog to be simply a list of posts:

@(defthing blog (listof post?))

Here, then, is a very simple example of a blog:

@racketblock[
(define BLOG (list (post "First Post!"
                         "Hey, this is my first post!")))
]

Now let's get our web application to show it.

@section{Rendering HTML}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-1)]

When a web browser visits our blog's URL, the browser constructs a request
structure and sends it across the network to our application.  We need a
function, which we'll call @racket[start], to consume such requests and produce
responses to them. One basic kind of response is to show an HTML page; this is
done by the function @racket[response/xexpr], which takes an @xexpr
representing the desired HTML. An @xexpr is defined as

@; Very unsure of this change.  A brief look through the xml and web-server
@; code did not turn up a flat-rec-contract definition of xexpr/c.
@; -AWest, 18-Dec-2011.
@racketblock[
 (define xexpr/c
   (flat-rec-contract
    xexpr
    (or/c string?
          (cons/c symbol? (listof xexpr))
          (cons/c symbol?
                  (cons/c (listof (list/c symbol? string?))
                          (listof xexpr))))))]

and the following examples illustrate how natural it is to use @|xexpr|s
to represent HTML.

The first alternative in @racket[xexpr/c] is @racket[string?]. For example,
the HTML @tt{hello} is represented as @racket["hello"]. To guarantee valid 
HTML, strings are automatically escaped when output. Thus, the @xexpr
@racket["<b>Unfinished tag"] is rendered as the HTML @tt{&lt;b&gt;Unfinished
tag}, and not as @tt{<b>Unfinished tag}.  Similarly, @racket["<i>Finished
tag</i>"] is rendered as @tt{&lt;i&gt;Finished tag&lt;/i&gt;}, and not as
@tt{<i>Finished tag</i>}.

The second alternative in @racket[xexpr/c] is the recursive contract
@racket[(cons/c symbol? (listof xexpr))].  For example, the HTML @tt{<p>This is
an example</p>} is represented by the @xexpr

@racket['(p "This is an example")].

And finally, the third alternative in @racket[xexpr/c] allows for parameters in
HTML tags.  As examples, @tt{<a href="link.html">Past</a>} is represented by

@racket['(a ((href "link.html")) "Past")]

and @tt{<p>This is <div class="emph">another</div> example.</p>} is represented by

@racket['(p "This is " (div ((class "emph")) "another") " example.")].

We could also have produced these @|xexpr|s ``manually,'' using
@racket[cons] and @racket[list], but that can get notationally heavy.  For 
example, the following Racket expressions both evaluate to the same
@|xexpr|:

@racketblock[
  (list 'html (list 'head (list 'title "Some title"))
         (list 'body (list 'p "This is a simple static page.")))
]

@racketblock[] @; an ugly hack to insert some vertical space -AWest,
@; 18-Dec-2011


@racketblock[
  '(html (head (title "Some title"))
         (body (p "This is a simple static page.")))
]

But the latter is much easier to read and type, because it uses a leading
forward quote mark to express the list structure concisely. This is
how to construct static html responses with aplomb! (For more on
the extended list abbreviation form, see
@link["http://htdp.org/2003-09-26/Book/curriculum-Z-H-17.html#node_chap_13"]{Section
13} of @link["http://htdp.org/"]{How to Design Programs}.)

It turns out, however, that this simple kind of list abbreviation cannot
produce web content that is dynamic. For if we try to inject expressions into
an @|xexpr| constructed by simple list abbreviation, those expressions will be
treated as part of the list structure, literally! What we need instead is a
notation that gives us the convenience of quoted list abbreviations, but that
also allows us to treat portions of the list structure as normal
expressions.  That is, we would like to define a @emph{template} whose
placeholders can be expressed easily and filled in dynamically.

Racket provides this templating functionality, in the form of a notation called
quasiquote. In quasiquotation a list is abbreviated not with a leading forward
quote but with a leading back quote. If we wish any subexpression of this
backquoted list to be evaluated normally (``unquoted''), then all we 
have to do is place a comma in front that subexpression. For example:

@racketblock[
@code:comment{render-greeting: string -> response}
@code:comment{Consumes a name, and produces a dynamic response.}
(define (render-greeting a-name)
  (response/xexpr
   `(html (head (title "Welcome"))
          (body (p ,(string-append "Hello " a-name))))))
]

@; Suggestion: insert here a reference to the Guide's discussion of
@; quasiquotation, akin to the reference to HtDP a couple of paragraphs
@; above. -AWest, 18-Dec-2011
@bold{Exercise.} Write a function that consumes a @racket[post] and produces
an @xexpr representing that content.

@defthing[render-post (post? . -> . xexpr/c)]

As an example, we want:

@racketblock[
    (render-post (post "First post!" "This is a first post."))
]

to produce:

@racketblock[
   '(div ((class "post")) "First post!" (p "This is a first post."))
]

@centerline{------------}

We will sometimes want to embed a list of @|xexpr|s into
another list that acts as a template.  For example, given the list of
@|xexpr|s @racket['((li "Larry") (li "Curly") (li "Moe"))], we may want
to create the single @|xexpr|

@racketblock[
'(ul (li "Larry")
     (li "Curly")
     (li "Moe"))
]

This can't be done using plain unquoting, because placing a comma in
front of @racket['((li "Larry") (li "Curly") (li "Moe"))] will unquote
the entire list, yielding the malformed expression
@racket['(ul ((li "Larry") (li "Curly") (li
"Moe")))].

Instead, we must splice the list in, like so: @racket[`(ul
,@'((li "Larry") (li "Curly") (li "Moe")))]. The
@racket[unquote-splicing] form, @racket[,@expression], allows us
conveniently to splice a list of @|xexpr| fragments into a larger
template list. To generalize the example, here are two helper
functions that convert any list of @|xexpr|s into one @|xexpr|
representing an unordered, itemized HTML list:

@racketblock[
@code:comment{render-as-itemized-list: (listof xexpr) -> xexpr}
@code:comment{Consumes a list of items, and produces a rendering}
@code:comment{as an unordered list.}
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

@code:comment{render-as-item: xexpr -> xexpr}
@code:comment{Consumes an xexpr, and produces a rendering}
@code:comment{as a list item.}
(define (render-as-item a-fragment)
  `(li ,a-fragment))
]

@bold{Exercise.} Write a function @racket[render-posts] that consumes a @racket[(listof post?)]
and produces an @|xexpr| for that content.

@defthing[render-posts ((listof post?) . -> . xexpr/c)]

As examples,

@racketblock[
(render-posts empty)
]

should produce

@racketblock[
'(div ((class "posts")))
]

and

@racketblock[
(render-posts (list (post "Post 1" "Body 1")
                    (post "Post 2" "Body 2")))
]

should produce

@racketblock[
'(div ((class "posts"))
      (div ((class "post")) "Post 1" (p "Body 1"))
      (div ((class "post")) "Post 2" (p "Body 2")))
]

@centerline{------------}

Now that we have the @racket[render-posts] function handy, let's revisit our
web application and change our @racket[start] function to return an interesting
@racket[response].

@external-file["iteration-1.rkt"]

If we press Run, we should see the blog posts in our web browser.

@section{Inspecting Requests}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-2
                                  web-server/servlet)]

Our application is still a bit too static: we build the 
page dynamically, but we don't yet provide a way for the user
to create new posts.  Let's tackle that now, by providing a form that
lets the user add a new blog entry.  When the user presses the
submit button, we want the new post to appear at the top of the
page.

We haven't yet done anything with the @racket[request] object that
we've been passing around.  As you may already have guessed, it isn't
really supposed to be ignored so much!
When a user fills out a web form and submits it, the user's browser constructs
a new @racket[request] that contains the form's 
values, which we can extract on our end, using the function
@racket[request-bindings]:

@defthing[request-bindings (request? . -> . bindings?)]

To extract a single web form value from a set of bindings, Racket provides
the function @racket[extract-binding/single], which also takes the name
of the corresponding field of the web form:

@defthing[extract-binding/single (symbol? bindings? . -> . string?)]

To verify that a set of bindings contains a particular field, use
@racket[exists-binding?]:

@defthing[exists-binding? (symbol? bindings? . -> . boolean?)]

With these functions, we can design functions that consume @racket[request]s
and respond to them usefully.

@bold{Exercise.} Write a function @racket[can-parse-post?] that consumes a set
of bindings.
It should produce @racket[#t] if there exist bindings both for the symbols
@racket['title] and @racket['body], and @racket[#f] otherwise.

@defthing[can-parse-post? (bindings? . -> . boolean?)]

@bold{Exercise.} Write a function @racket[parse-post] that consumes a set of
bindings.
Assuming that the bindings structure has values for the symbols @racket['title]
and @racket['body], @racket[parse-post] should produce a post containing those
values.

@defthing[parse-post (bindings? . -> . post?)]

Now that we have these helper functions, we can extend our web
application to handle form input.  We'll add a small form at the
bottom of the web page, and we'll adjust our program to handle the addition of new posts. 
So our new @racket[start] method will check that the @racket[request] has a 
parsable post, will then try to extend the set of posts, and will finally display the new set of blog posts:

@external-file["iteration-2.rkt"]

This solution seems to work, but it has a flaw!  Try to add
two new posts.  What happens?

@section{Advanced Control Flow}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-3)]

For the moment, let's ignore the admittedly huge problem of having a
blog that accepts only one new blog entry.  Don't worry, we'll fix
this!

The more pressing problem right now is a higher-level one: although we
do 
have a function, @racket[start], that responds to @racket[request]s directed
at our application's URL, that function has begun to take on 
too much responsibility.  In particular, @racket[start] now handles
two different kinds of @racket[request]s: those for showing a
blog, and those for adding new blog posts. It has become a kind of traffic cop
--- a dispatcher --- for all of our web application's behaviors, including any
new functionality we may want to add later. Life would be easier for
@racket[start] 
(and for us) if different kinds of @racket[request]s were instead
directed automatically to different functions. Is this possible in Racket?

Yes! The web server library provides a function,
@racket[send/suspend/dispatch], that allows us to create URLs that direct
@racket[request]s aimed at them to specific functions in our application.  We
demonstrate with a dizzying example. In a new file, enter the following in
DrRacket's Definitions window.

@racketmod[
web-server/insta
@code:comment{start: request -> response}
(define (start request)
  (phase-1 request))

@code:comment{phase-1: request -> response}
(define (phase-1 request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html 
               (body (h1 "Phase 1")
                     (a ((href ,(embed/url phase-2)))
                        "click me!")))))]
    (send/suspend/dispatch response-generator)))

@code:comment{phase-2: request -> response}
(define (phase-2 request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html 
               (body (h1 "Phase 2")
                     (a ((href ,(embed/url phase-1)))
                        "click me!")))))]    
    (send/suspend/dispatch response-generator)))
]

This is a web application that goes round and round.  When a user
first visits the application, the user starts off in @racket[phase-1].  The
generated page has a hyperlink that, when clicked, continues to
@racket[phase-2].  The user can click back, and falls back to @racket[phase-1], and the
cycle repeats.

Let's look more closely at the @racket[send/suspend/dispatch] mechanism.
@racket[send/suspend/dispatch] consumes a response-generating function and
gives it another function, called @racket[embed/url], that we'll
use to build special URLs.  What makes these URLs special is this:
when a web browser visits one of them, our web application restarts,
not from @racket[start], but from the handler that we associate with the
URL. In the handler @racket[phase-1], the use of @racket[embed/url] associates
the link with the handler @racket[phase-2], and vice versa.

We can be even more sophisticated about the handlers associated with
@racket[embed/url].  Because a handler is just a request-consuming function,
it can be defined within a @racket[local] and so can see
all the other variables in the scope of its definition.  Here's another loopy example:

@racketmod[
web-server/insta
@code:comment{start: request -> response}
(define (start request)
  (show-counter 0 request))

@code:comment{show-counter: number request -> doesn't return}
@code:comment{Displays a number that's hyperlinked: when the link is pressed,}
@code:comment{returns a new page with the incremented number.}
(define (show-counter n request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Counting example"))
                    (body
                     (a ((href ,(embed/url next-number-handler)))
                        ,(number->string n))))))

          (define (next-number-handler request)
            (show-counter (+ n 1) request))]

    (send/suspend/dispatch response-generator)))
]

This example shows that we can accumulate the results of an
interaction.  Even though the user starts off by visiting and seeing
zero, the handlers produced by @racket[next-number-handler] continue the interaction, accumulating a larger and larger number.

We're going in circles now, so let's move forward and return to our blog
application.  We'll adjust the form's action so that it directs a submission
@racket[request] to a URL associated with a separate handler, called
@racket[insert-post-handler].

@external-file["iteration-3.rkt"]

Note that the structure of the @racket[render-blog-page] function is very
similar to that of our last @racket[show-counter] example.  The user can 
finally add and see multiple posts to the blog.

Unfortunately, our design still suffers from a problem, which can be seen by
adding a few posts to the system, and then visiting the web application's URL
in a new browser window.  What happens when you try this?

@section{Share and Share Alike}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-4)]

The problem with our application is that each browser window keeps
track of its own distinct blog.  For most people, this defeats the
purpose of a blog, which is to share with others!  When we insert a
new post, rather than creating a new blog value, we'd like to modify
@emph{the} blog. In other words, we'd like to make a structural
change. (@italic{@link["http://www.htdp.org/"]{How to Design
Programs}}, Chapter 41).  So let's switch from just the @racket[BLOG]
binding to a list and instead bind it to a mutable structure. If we
were to just use a structure, we'd write the following:

@racketblock[(struct blog (posts))]

But, by default, structures in Racket are immutable.  To gain access
to structure mutators, we'll need to override this default, by adding
the @racket[#:mutable] keyword to some of our structure
definitions. In particular, if we want to allow changes to a blog, we
must change our definition of the blog structure to the following:

@racketblock[(struct blog (posts) #:mutable)]

@defstruct*[blog ([posts (listof post?)])]

A mutable structure provides functions that change its fields; in this case, we
are provided the structure mutator @racket[set-blog-posts!],
which allows us to change the posts of a blog:

@defthing[set-blog-posts! (blog? (listof post?) . -> . void)]



@bold{Exercise.} Write a function @racket[blog-insert-post!]

@defthing[blog-insert-post! (blog? post? . -> . void)]

whose intended side effect is to extend a blog's posts.

@centerline{------------}

We must now modify the web application to use our new data representation of a
blog. Since the blog is now referred to by the global variable
@racket[BLOG], it no longer needs to be passed as a parameter to handlers like
@racket[render-blog-page].  Here is our updated web application, after
adjustments that incorporate @racket[insert-blog-post!], and after a bit of
variable cleanup:

@external-file["iteration-4.rkt"]

Now visit the blog from two separate browser windows and add
posts from each of them.  You'll be glad to see that both windows share
the same blog!

@section{Extending the Model}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-5)]

Next, let's extend the application so that a post can include a list
of comments. The data definition becomes:

@defstruct*[post ([title string?] [body string?] [comments (listof string?)]) #:mutable]

@bold{Exercise.} Write the updated data structure definition for posts.  Make
sure to make the structure mutable, since we intend to add comments to
posts.

@bold{Exercise.} Make up a few examples of posts.

@bold{Exercise.} Define a function @racket[post-add-comment!]

@defthing[post-add-comment! (post? string? . -> . void)]

whose intended side effect is to add a new comment to the end of the post's
list of comments.

@bold{Exercise.} Adjust @racket[render-post] so that the produced fragment will include the
comments in an itemized list.

@bold{Exercise.} Because we've extended @racket[post] to include comments, you
also need to adjust other, post-manipulating parts of the application,
such as uses of @racket[post].

@centerline{------------}

Now that we've adjusted our functions to accommodate @racket[post]'s new
structure, our web application should be runnable.  The user may even see some
of the fruits of our labor: if the initial @racket[BLOG] has a post with
comments, the user should now see them. On the other hand, something is
obviously missing: the user is given no interface through which to add
comments!


@section{Breaking Up the Display}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-5)]

How should we incorporate comments more fully into the user's web
experience?  Seeing all the posts and comments on one page may
be a bit overwhelming, so maybe we should hold off on showing the
comments on the main blog page.  Instead, let's make a secondary
``detail'' view of a post and present its comments there.
Accordingly, the top-level view of a blog will show only the title and body of
a post, and the number of its comments.

So now we need a way to visit a post's detail page.  One way to do
this is to hyperlink a post's title: if one wishes to see a post's
detail page, one should only have to click the post's title. In that post's
detail page, we can even add a form to let the user add new comments.
The page flow of this new version of our web application is then depicted
simply as:

@image{scribblings/tutorial/images/flow1.png}

Each node (bubble) in this diagram corresponds to a request-consuming handler.
As you might expect, we'll be using @racket[send/suspend/dispatch] some more.
Every arrow in the diagram will be realized as a URL that we generate
with @racket[embed/url].

This approach has a slightly messy consequence. Previously we rendered the list
of posts without any hyperlinks.  But since any function that generates a
special dispatching URL must use @racket[embed/url] to do so, we'll need to
adjust @racket[render-posts] and @racket[render-post] to consume and use
@racket[embed/url] itself when it makes those hyperlinked titles.

We now have a pretty sophisticated web application, one that permits the
creation of posts and the addition of comments.  Here is what it looks like:

@external-file["iteration-5.rkt"]

But it still suffers from a problem: once in a @racket[post-detail-page], the
only way for the user to return to the blog is to use the Back button!
That's disruptive, and it might allow the user get ``stuck'' in a
dark corner of the web application. To solve this problem, let's improve the
page flow.

@section{Adding a Back Button}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-6)]

Perhaps we should simply add a BACK link from the
@racket[render-post-detail-page], one that returns us to the top-level
blog. Here's the corresponding page flow diagram:

@image{scribblings/tutorial/images/flow2.png}

@bold{Exercise.} Adjust @racket[render-post-detail-page] to include another link that goes
back to @racket[render-blog-page].

And since a user may have a change of heart about a comment,
let's enrich the flow to give the user a chance to back out of submitting one.

@image{scribblings/tutorial/images/flow3.png}

Note that, although this change may seem complicated, it doesn't affect the
general shape of our handlers:

@external-file["iteration-6.rkt"]


@section{Decorating With Style!}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-7
                                  web-server/insta/insta)]

Our web application is now functionally complete.  But it's visually
lacking, so let's try to improve its appearance.  One way to add visual panache
to our web pages is to use a cascading style sheet.  For example, if we'd like
to make all of our paragraphs green, we might insert the following style
declaration into a response.

@racket['(style ((type "text/css")) "p { color: green }")]

It is tempting to embed such declarations directly into our
@racket[response]s. But our source file is already quite busy, and, as a
matter of principle, we should separate logical representation
from visual presentation.  So, rather than embed the
.css in the HTML response directly, let's instead add a link reference to a
separate .css file.

Up till now, all the content produced by our web application has
come from a response-generating handler.  But this dynamic generation of HTML
is not necessary for content that doesn't change.
Examples of such static resources include images, documents, and .css files.
To serve them alongside our web applications, we inform the web server of a
directory that we have created specially for static files. The function
@racket[static-files-path],

@defthing[static-files-path (path-string? -> void)]

tells the web server to look in the given path whenever it receives a URL
that looks like a request for a static resource.

@bold{Exercise.} Create a simple web application called @filepath{test-static.rkt} with the
following content:

@racketmod[
web-server/insta
(define (start request)
  (response/xexpr
   '(html (head (title "Testing")
                (link ((rel "stylesheet")
                       (href "/test-static.css")
                       (type "text/css"))))          
          (body (h1 "Testing")
                (h2 "This is a header")
                (p "This is " (span ((class "hot")) "hot") ".")))))

(static-files-path "htdocs")
]

Make a subdirectory called @filepath{htdocs}, rooted in the same directory as
the @filepath{test-static.rkt} source.  Just to see that we can serve
this .css page, create a very simple .css file @filepath{test-static.css}
in @filepath{htdocs/} with the following content:

@verbatim{
body {
  margin-left: 10%;
  margin-right: 10%;
}
p { font-family: sans-serif }
h1 { color: green }
h2 { font-size: small }
span.hot { color: red }
}

Now run the application and look at the browser's output.
A Spartan web page should appear, but it should still have some color
in its cheeks.

@centerline{------------}

@bold{Exercise.}
Improve the presentation of the blog web application by writing
an external style sheet that suits your tastes.  Adjust all of the HTML
response handlers to include a link to the style sheet.

@section{The Double Submit Error}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-7
                                  web-server/servlet)]

Our application has yet another subtle problem.  To see it,
bring the blog application up again, and add a post.  Then reload the
page.  Reload the page again.

What you are observing is the well known ``double-submit'' problem.
Whenever a user presses Reload, a request is sent to our application, and the
problem is that some requests make the application mutate data structures.

A common technique that web developers use to dodge the double-submission
problem is to redirect state-mutating requests to a different URL, one that is
safe to reload.  This trick is implemented in Racket by the function
@racket[redirect/get]:

@defthing[redirect/get (-> request?)]

Its immediate side effect is to force the user's browser to follow a
redirection to a safe URL, and it gives us back that fresh new request.

For example, consider a toy application that lets the user add
names to a roster:

@external-file["no-use-redirect.rkt"]

This application suffers from the same problem as our blog does: if the user
adds a name, and then presses reload, then the same name will be added
twice.

We can fix this by changing a single expression; can you find it
below?

@external-file["use-redirect.rkt"]

So the double-submit error is easy to prevent: whenever you have
handlers that mutate the state of the system, use @racket[redirect/get] when
sending back your response.

@bold{Exercise.}
Use @racket[redirect/get] to fix the double-submit error in the blog
application.

With these minor fixes, our blog application now looks like this:

@external-file["iteration-7.rkt"]

@section{Abstracting the Model}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-8
                                  web-server/scribblings/tutorial/examples/model)]

If we ``turn off the lights'' by closing the program, the state of
our application disappears into the ether.  How do we get our
ephemeral state to stick around?  Before we tackle this problem, note that it
does not apply to all of the application's state, for we have no long-term
interest in things like requests. What we do care about saving is our model of
the blog.

If we look closely at our web application program, we see a seam
between the model of our blog, and the web application that uses that
model.  Let's isolate the model; it's all the stuff near the top:

@racketblock[
    (struct blog (posts) #:mutable)
    (struct post (title body comments) #:mutable)
    (define BLOG ...)
    (define (blog-insert-post! ...) ...)
    (define (post-insert-comment! ...) ...)
]

In realistic web applications, the model and the web application are
separated by a wall of abstraction.  In theory, this separation
allows us to make isolated changes in future without breaking the entire
system. So let's start separating. First we'll rip the model out into a
separate file, and then we'll look into making the model persistent.

Create a new file called @filepath{model.rkt} with the following content.

@external-file["model.rkt"]

This is essentially a cut-and-paste of the lines we identified as our
model.  It's written in the @racketmodname[racket] language because
the model shouldn't need to worry about web-server stuff.  There's one
additional expression that looks a little odd at first:

@racketblock[
    (provide (all-defined-out))
]

It tells Racket to grant other files access to
everything that's defined in the @filepath{model.rkt} file.

Now we go back to our web application and change it to use this model, by
replacing the deleted model code with the expression

@racketblock[
    (require "model.rkt")
]

which hooks our web application module up to the @racketmodname["model.rkt"]
module.

@external-file["iteration-8.rkt"]

@section{A Persistent Model}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/iteration-9
                                  web-server/scribblings/tutorial/examples/model-2)]

Now that the model resides in a separate module, we can more easily modify
it and, in particular, can make it persistent.

The first step is to make the model structures serializable. Earlier, we made the
structures mutable by adding @racket[#:mutable] to their
definitions. Similarly, when the keyword @racket[#:prefab] is added to the
definition of a structure, Racket understands that the structure can be
``previously fabricated,'' that is, created before the program started
running---which is exactly what we want when restoring the blog data from
disk. Our blog structure definition now looks like:

@racketblock[
    (struct blog (posts) #:mutable #:prefab)
]

@racket[blog] structures can now be read from the outside world with
@racket[read] and written to it with @racket[write]. But we also need to make
sure that everything inside a @racket[blog] structure is also (transitively) marked
as @racket[#:prefab].

@bold{Exercise.} Write the new structure definition for posts.

At this point, we @emph{can} read and write the blog to disk. So let's do it.
First, we'll add to the model a path pointing to where the blog resides on
disk:

@defstruct*[blog ([home string?] [posts (listof post?)]) #:mutable #:prefab]

Notice that we will need to convert the path into a string. Why didn't we just
make the blog structure contain paths? Answer: They can't be used with
@racket[read] and @racket[write].

@bold{Exercise.} Write the new structure definition for blogs.

Next we create a function that allows our application to initialize the blog:

@racketblock[
@code:comment{initialize-blog! : path? -> blog}
@code:comment{Reads a blog from a path, if not present, returns default}
(define (initialize-blog! home)
  (local [(define (log-missing-exn-handler exn)
            (blog
             (path->string home)
             (list (post "First Post"
                         "This is my first post"
                         (list "First comment!"))
                   (post "Second Post"
                         "This is another post"
                         (list)))))
          (define the-blog
            (with-handlers ([exn? log-missing-exn-handler])
              (with-input-from-file home read)))]
    (set-blog-home! the-blog (path->string home))
    the-blog))
]

@racket[initialize-blog!] takes a path and tries to @racket[read] from it. If
the path contains a @racket[blog] structure, then @racket[read] will parse it,
because @racket[blog]s are @racket[#:prefab]. If there is no file at the path,
or if the file has some spurious data, then @racket[read] or
@racket[with-input-from-file] will throw an exception. @racket[with-handlers]
supplies an exception handler that reacts to any error by returning the default
@racket[blog] structure.  After @racket[the-blog] is bound to the newly read
(or default) structure, we set the home to the correct path. 

Next we need a function to save the model to the disk:

@racketblock[
@code:comment{save-blog! : blog -> void}
@code:comment{Saves the contents of a blog to its home}
(define (save-blog! a-blog)
  (local [(define (write-to-blog)
            (write a-blog))]
    (with-output-to-file (blog-home a-blog) 
      write-to-blog
      #:exists 'replace)))
]

@racket[save-blog!] @racket[write]s the model to its home; by supplying an
@racket[#:exists] clause to @racket[with-output-to-file], it ensures that the
old contents on disk will be overwritten.

This function can now be used to save the blog structure whenever it is
modified by the user. Since modifications are made only by the model, only
@racket[blog-insert-post!] and @racket[post-insert-comment!] will need to be
updated.

@bold{Exercise.} Change @racket[blog-insert-post!] and
@racket[post-insert-comment!] to call @racket[save-blog!].

@centerline{------------}

You may have noticed a problem when trying to update
@racket[post-insert-comment!]: the function has no blog to pass to
@racket[save-blog!]. We will therefore need to give it a blog argument and
change the application appropriately. While we're at it, let's change
@racket[blog-insert-post!] to accept the contents of the post 
structure, rather the structure itself. This improves the model's interface, by
making it more abstract:

@defthing[blog-insert-post! (blog? string? string? . -> . void)]
@defthing[post-insert-comment! (blog? post? string? . -> . void)]

@bold{Exercise.} Write the new definitions of @racket[blog-insert-post!] and @racket[post-insert-comment!].
Remember to call @racket[save-blog!].


In the previous iteration of the model, we used @racket[(provide
(all-defined-out))] to expose all of the model's definitions.  This
transgresses the principle of abstraction, which tells us to hide
implementation details like private functions and internal data structures.
We'll conform to that principle now, by using a form of 
@racket[provide] that names the exposed definitions explicitly.

For example, if we wanted to limit the module's exposure to the functions
@racket[blog-insert-post!] and @racket[post-insert-comment!], we could
do this:
@racketblock[
    (provide blog-insert-post!
             post-insert-comment!)
]

But this is exposing too little! So let's change the @racket[provide] line in
the model to:
@racketblock[
(provide blog? blog-posts
         post? post-title post-body post-comments
         initialize-blog!
         blog-insert-post! post-insert-comment!)
]

Since these nine functions are all we need from the module, this degree of
exposure is just right.


@centerline{------------}

The last step is to change the application. We need to call
@racket[initialize-blog!] to read in the blog structure, and, since there is no
longer a a @racket[BLOG] export, we need to pass the returned blog value around
the application.

First, change @racket[start] to call @racket[initialize-blog!] with a path in
our home directory:

@racketblock[
 (define (start request)  
   (render-blog-page 
    (initialize-blog! 
     (build-path (current-directory)
                 "the-blog-data.db"))
    request))
]

@bold{Exercise.} Thread the @racket[blog] structure through the application
appropriately to give @racket[blog-insert-post!] and
@racket[post-insert-comment!] the correct values. You'll also need to change
how @racket[render-blog-page] adds new posts.

@centerline{------------}

Our model is now:

@external-file["model-2.rkt"]

And our application is:

@external-file["iteration-9.rkt"]

@centerline{------------}

This approach to persistence can work surprisingly well for simple
applications. But as our application's needs grow, we will have to deal with
concurrency issues, the lack of a simple query language over our data model,
etc. So, in the next section, we'll explain how to use an SQL database to
store our blog model.

@section{Using an SQL database}
@declare-exporting[#:use-sources (web-server/scribblings/tutorial/examples/dummy-10
                                  web-server/scribblings/tutorial/examples/dummy-3
                                  web-server/scribblings/tutorial/dummy-sqlite)]
@(require (for-label web-server/scribblings/tutorial/dummy-sqlite))

To employ an SQL database, we use the following bindings from the
@racketmodname[db] library: @racket[connection?], @racket[sqlite3-connect],
@racket[table-exists?], @racket[query-exec], @racket[query-list], and
@racket[query-value].  Import them by adding the following to the top of the
model:

@racketblock[
(require db)
]

Next, we define a relational structure for our model using the following
tables:

@verbatim{
 CREATE TABLE posts (id INTEGER PRIMARY KEY, title TEXT, body TEXT)
 CREATE TABLE comments (pid INTEGER, content TEXT)
}

Like the Racket structure, a post in the database has a title and a body,
but it also has an identifier. (Actually, the Racket structure had an
identifier as well---the memory pointer---but the database requires it to be
explicit.)

As for the comments, each has some textual content and is connected to a post
via identifier. We could have chosen to serialize comments with
@racket[write] and add a new TEXT column to the posts table to store
the value. But a separate comments table conforms better to
relational style.

A @racket[blog] structure is now simply a container for the database
handle:

@defstruct*[blog ([db connection?])]

@bold{Exercise.} Write the @racket[blog] structure definition. It
does not need to be mutable or serializable.

We can now write the code to initialize a @racket[blog] structure:
@racketblock[
@code:comment{initialize-blog! : path? -> blog?}
@code:comment{Sets up a blog database (if it doesn't exist)}
(define (initialize-blog! home)
  (define db (sqlite3-connect #:database home #:mode 'create))
  (define the-blog (blog db))
  (unless (table-exists? db "posts")
    (query-exec db
     (string-append
      "CREATE TABLE posts "
      "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)"))
    (blog-insert-post!
     the-blog "First Post" "This is my first post")
    (blog-insert-post!
     the-blog "Second Post" "This is another post"))
  (unless (table-exists? db "comments")
    (query-exec db
     "CREATE TABLE comments (pid INTEGER, content TEXT)")
    (post-insert-comment!
     the-blog (first (blog-posts the-blog))
     "First comment!"))
  the-blog)
]

Given the @racket['create] flag, @racket[sqlite3-connect] creates a database if
one does not already exist at the @racket[home] path.

We still need to initialize the database with the table
definitions and initial data. Previously we used @racket[blog-insert-post!] and
@racket[post-insert-comment!] for this purpose; here are their new
implementations:

@racketblock[
@code:comment{blog-insert-post!: blog? string? string? -> void}
@code:comment{Consumes a blog and a post, adds the post at the top of the blog.}
(define (blog-insert-post! a-blog title body)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO posts (title, body) VALUES (?, ?)"
   title body))

@code:comment{post-insert-comment!: blog? post string -> void}
@code:comment{Consumes a blog, a post and a comment string.  As a side-effect,}
@code:comment{adds the comment to the bottom of the post's list of comments.}
(define (post-insert-comment! a-blog p a-comment)
  (query-exec
   (blog-db a-blog)
   "INSERT INTO comments (pid, content) VALUES (?, ?)"
   (post-id p) a-comment))
]

@centerline{------------}

Note that the SQL queries above use the SQL placeholder, @litchar{?}, to
perform string substitution. If they had performed it with
@racket[format] and @litchar{~a} instead, a malicious user could submit a post with a
title like @racket["null', 'null') and INSERT INTO accounts (username,
password) VALUES ('ur','hacked"] and get @racket[query-exec] to make two
INSERTs instead of one. This is called an SQL injection attack.

SQL placeholders prevent such attacks by ensuring that the query is submitted
as-is to SQLite, which then parses it and applies the arguments. This approach
ensures that the arguments are treated strictly as data.


@centerline{------------}

In @racket[post-insert-comment!] we use @racket[post-id] but we
have not yet defined the new @racket[post] structure.  Since the post table
schema uses an integer as identifier, it would seem sufficient
to do the same for the @racket[post] structure.
However, a structure so defined would not indicate which blog, and consequently
which database, a post belongs to. We would thus be unable to extract
the title or body values.

The solution, of course, is to associate the blog with each post:

@defstruct*[post ([blog blog?] [id integer?])]

@bold{Exercise.} Write the structure definition for posts.

The only function that creates posts is @racket[blog-posts]:

@racketblock[
@code:comment{blog-posts : blog -> (listof post?)}
@code:comment{Queries for the post ids}
(define (blog-posts a-blog)
  (local [(define (id->post an-id)
            (post a-blog an-id))]
    (map id->post
         (query-list
          (blog-db a-blog)
          "SELECT id FROM posts"))))
]

@racket[query-list] can be used for queries that return a single
column (e.g., @racket["SELECT id FROM posts"]), and it returns a list of that
column's values.

At this point we can write the functions that operate on posts:
@racketblock[
@code:comment{post-title : post -> string?}
@code:comment{Queries for the title}
(define (post-title a-post)
  (query-value
   (blog-db (post-blog a-post))
   "SELECT title FROM posts WHERE id = ?"
   (post-id a-post)))
]

@racket[query-value] is used with queries that return a single
value (that is, one row and one column).

@bold{Exercise.} Write the definition of @racket[post-body].
             
@bold{Exercise.} Write the definition of @racket[post-comments].
(Hint: Use @racket[blog-posts] as a template, not @racket[post-title].)

@centerline{------------}

The only change that we need to make to the application is to require
the new model. Note that its interface remains unchanged!

@centerline{------------}

Our model is now:

@external-file["model-3.rkt"]

And our application is:

@racketmod[
web-server/insta

(require "model-3.rkt")

....
]

See @secref[#:doc '(lib "db/scribblings/db.scrbl") "intro-servlets"]
for more information on writing database-backed web servlets.

@section{Using Formlets}

@(require (for-label web-server/formlets))

Now let's go back to the application code. One of our poor design choices is
to have made a loose connection between the name used to identify a form
element in the rendering code, and the name used for it in the extracting code:

@racketblock[
@code:comment{render-blog-page: blog request -> doesnt'}
@code:comment{Send an HTML page of the content of the}
@code:comment{blog.}
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url) 
            (response/xexpr
             `(html (head (title "My Blog"))
                    (body 
                     (h1 "My Blog")
                     ,(render-posts a-blog embed/url)
                     (form ((action 
                             ,(embed/url insert-post-handler)))
                           @code:comment{"title" is used here}
                           (input ((name "title")))
                           (input ((name "body")))
                           (input ((type "submit"))))))))          
          
          (define (insert-post-handler request)
            (define bindings (request-bindings request))
            (blog-insert-post!
             a-blog
             @code:comment{And "title" is used here.}
             (extract-binding/single 'title bindings)
             (extract-binding/single 'body bindings))
            (render-blog-page a-blog (redirect/get)))]

    (send/suspend/dispatch response-generator)))
]

@(define formlet
   @tech[#:doc '(lib "web-server/scribblings/web-server.scrbl")]{formlet})
@(define formlets
   @tech[#:doc '(lib "web-server/scribblings/web-server.scrbl")]{formlets})

The Racket Web framework provides @formlets to abstract these names away, by
adjusting them automatically in the HTML, and by presenting the following
interface for the display and processing of forms:
@itemize[
@item{@racket[formlet-display] takes a @formlet and returns its rendering as a
list of X-expressions. This will generate unique names for its form elements.}
 
@item{@racket[formlet-process] takes a @formlet and a request and processes the
@formlet, i.e., extracts the bindings from the request using the
names generated by @racket[formlet-display].}
]

A @formlet is created using the @racket[formlet] syntax. For example, here is
a @formlet for @racket[render-blog-page]:

@racketblock[
@code:comment{new-post-formlet : formlet (values string? string?)}
@code:comment{A formlet for requesting a title and body of a post}
(define new-post-formlet
  (formlet
   (#%# ,{input-string . => . title}
        ,{input-string . => . body})
   (values title body)))
]

The first argument in the @racket[formlet] syntax determines how
@racket[formlet-display] should display the @|formlet|.
It is a quoted @|xexpr|, with two important differences:
@itemize[
@item{@racket[#%#] introduces a list of @|xexpr|s}
@item{
@racket[,{_formlet . => . _id}] embeds the @formlet @racket[_formlet] as a
sub@|formlet| and it attaches the name @racket[_id] to the result of processing
this sub@|formlet|.

For example, @racket[input-string] is itself a library
@formlet that yields a string, and @racket[,{input-string . => . title}] embeds
@racket[input-string] in @racket[new-post-formlet] and associates the name
@racket[title] to that string.

@racket[input-string] is rendered as @racketresult[`(input ([type "text"] [name
,_fresh_name]))], so @racket[(formlet-display
new-post-formlet)] is rendered as:
@racketresultblock[
(list '(input ([type "text"] [name "input_0"]))
      '(input ([type "text"] [name "input_1"])))
]
}


]

The second argument of @racket[formlet] determines how @racket[formlet-process]
should process the @|formlet|. That is, it specifies how to group and order the
results of processing the @formlet's sub@|formlet|s: the identifiers on the
right-hand side of @racket[=>] are bound to the results of processing the
sub@|formlets|.

For example, @racket[input-string] is processed as
@racket[(extract-binding/single _fresh_name (request-bindings
request))]. Thus, if @racket[_request] binds @racketresult["input_0"] to
@racketresult["Title"] and @racketresult["input_1"] to @racketresult["Body"],
then @racket[(formlet-process new-post-formlet _request)] returns
@racketresult[(values "Title" "Body")].


Finally, here is how to use @racket[new-post-formlet] in @racket[render-blog-page]:
@racketblock[
@code:comment{render-blog-page: blog request -> doesn't return}
@code:comment{Sends an HTML page of the content of the}
@code:comment{blog.}
(define (render-blog-page a-blog request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "My Blog"))
                    (body 
                     (h1 "My Blog")
                     ,(render-posts a-blog embed/url)
                     (form ([action 
                             ,(embed/url insert-post-handler)])
                           ,@(formlet-display new-post-formlet)
                           (input ([type "submit"])))))))
          
          (define (insert-post-handler request)
            (define-values (title body) 
              (formlet-process new-post-formlet request))
            (blog-insert-post! a-blog title body)
            (render-blog-page a-blog (redirect/get)))]
    
    (send/suspend/dispatch response-generator)))
]

@bold{Alternative.} The formlet shown above uses the
@racket[input-string] combinator, which combines a bunch of other
formlets into one container with sensible defaults. Sometimes it is
useful to break it apart to provide different arguments to the
subpieces. For example, suppose we wanted to add a CSS class to the
form elements? Here's how we'd do that:

@racketblock[
(define new-post-formlet
  (formlet
   (#%# ,((to-string 
           (required 
            (text-input
             #:attributes '([class "form-text"]))))
          . => . title)
        ,((to-string
           (required
            (text-input
             #:attributes '([class "form-text"]))))
          . => . body))
   (values title body)))
]

@bold{Exercise.} Write a @formlet and use it in @racket[render-post-detail-page].

@centerline{------------}

Our application is now:

@external-file["iteration-11.rkt"]

@section{Leaving DrRacket}

We've been in the habit of pressing the @onscreen{Run} button to run our
application in DrRacket. But if we were actually to deploy an application, we'd
need to launch it by a different method.

@(require (for-label web-server/servlet-env)
          (for-label web-server/managers/lru))

The simplest alternative is to use @racketmodname[web-server/servlet-env].
First, change the first lines in your application from
@racketmod[
web-server/insta
]

to
@racketmod[
racket

(require web-server/servlet)
(provide/contract (start (request? . -> . response?)))
]

Second, add the following at the bottom of your application:

@racketblock[
(require web-server/servlet-env)
(serve/servlet start 
               #:launch-browser? #f
               #:quit? #f
               #:listen-ip #f
               #:port 8000 
               #:extra-files-paths
               (list (build-path _your-path-here "htdocs"))
               #:servlet-path
               "/servlets/APPLICATION.rkt")
]

Regarding the parameters of @racket[serve/servlet]:
@itemize[
@item{You can change the value of the @racket[#:port] parameter to use a
different port.}

@item{@racket[#:listen-ip] is set to @racket[#f] so that the server will listen
on @emph{all} available IPs.}

@item{You should change @racket[_your-path-here] to be the path to the parent
of your @racket[htdocs] directory.}

@item{You should change @racket["APPLICATION.rkt"] to be the name of your
application.}
]

Third, to run your server, you can either press @onscreen{Run} in DrRacket, or type

@commandline{racket -t <file.rkt>}

(using your own file name, of course). Both of these will start a Web server
for your application. 

@centerline{------------}

@racket[serve/servlet] takes other parameters and there are more advanced ways
of starting the Web Server, but you'll have to refer to the Racket Web Server
Reference Manual for details.

@section{Using HTTPS}

Finally, here are instructions for using the server in HTTPS mode.
This requires an SSL certificate and a private key.  It is also very
platform-specific, but here are the details for using OpenSSL on UNIX:

@commandline{openssl genrsa -des3 -out private-key.pem 1024}

This will generate a new private key, but with a passphrase, which you
can remove as follows:

@commandline{openssl rsa -in private-key.pem -out private-key.pem}
@commandline{chmod 400 private-key.pem}

Now we generate a self-signed certificate:

@commandline{openssl req -new -x509 -nodes -sha1 -days 365 -key private-key.pem > server-cert.pem}

(Each certificate authority has a different way of generating
certificate-signing requests.)

We can now start the server with:

@commandline{plt-web-server --ssl}

The Web Server will start on port 443 (which can be overridden with the @exec{-p} option) using the
@filepath{private-key.pem} and @filepath{server-cert.pem} we've created.

@section{Moving Forward}

As you move forward with your own applications, you may find many PLaneT packages
to be useful. There are interfaces to other databases, many tools for generating
output in HTML, XML, Javascript, etc.

There is also an active community of users on the Racket mailing list. We
welcome new users!
