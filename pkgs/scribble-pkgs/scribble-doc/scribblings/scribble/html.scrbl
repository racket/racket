#lang scribble/doc
@(require scribble/manual
          scribble/core
          scribble/eval
          (only-meta-in 0 "utils.rkt")
          (for-label (except-in racket/base #%top #%module-begin)
                     racket/contract/base
                     racket/string
                     scribble/html))

@(define html-eval (make-base-eval))
@interaction-eval[#:eval html-eval (require scribble/html)]
@interaction-eval[#:eval html-eval (require racket/string)]

@title[#:tag "html" #:style 'toc]{HTML Generation}

@defmodulelang[scribble/html]{The @racketmodname[scribble/html]
language provides a way to generate HTML that is different from
@racketmodname[scribble/base]. The @racketmodname[scribble/base]
approach involves describing a document that can be rendered to HTML,
Latex, or other formats. The @racketmodname[scribble/html] approach,
in contrast, treats the document content as HTML format plus escapes.}

Specifically, @racketmodname[scribble/html] is like
@racketmodname[scribble/text], but with the following changes:

@itemize[

  @item{The @racketmodname[scribble/html/html],
        @racketmodname[scribble/html/xml], and
        @racketmodname[scribble/html/resource] are re-exported,
        in addition to @racketmodname[scribble/text].}

  @item{Free identifiers that end with @litchar{:} are implicitly
        quoted as symbols.}

]

When @racketmodname[scribble/html] is used via @racket[require]
instead of @hash-lang[], then it does not change the printing of
values, and it does not include the bindings of @racket[racket/base].

The @racketmodname[scribble/html/resource],
@racketmodname[scribble/html/xml], and
@racketmodname[scribble/html/html] libraries provide forms for
generating HTML as strings to be output in the same way as
@racketmodname[scribble/text].

@local-table-of-contents[]

@; ----------------------------------------

@section[#:tag "html-html"]{Generating HTML Strings}

@defmodule[scribble/html/html]{The @racketmodname[scribble/html/html]
provides functions for HTML representations that render to string form
via @racket[output-xml].}

@defproc[(doctype [s (or/c string 'html 'xhtml)]) procedure?]{

Produces a value that @tech{XML-renders} as a DOCTYPE declaration.

@examples[#:eval html-eval
(output-xml (doctype "?"))
(output-xml (doctype 'html))
(regexp-split #rx"\n|((?<=\") (?=\"))"
              (xml->string (doctype 'xhtml)))]}


@defproc[(xhtml [content outputable/c] ...) procedure?]{

Produces a value that @tech{XML-renders} as the given content wrapped
as XHTML.

@examples[#:eval html-eval
(regexp-split #rx"\n|((?<=\") (?=\"))"
              (xml->string (xhtml "Hello")))]}

@(define-syntax-rule (def-tags tag ...)
   @deftogether[(
   @defproc[(tag [v outputable/c] (... ...)) procedure?] ...
   )]{

   Like @racket[element/not-empty], but with the symbolic form of the function
   name added as the first argument.

   @examples[#:eval html-eval
    (output-xml (title "The Book"))]})

@(def-tags
  html
  head
  title
  style ; style info, which may include CDATA sections
  script ; script statements, which may include CDATA sections
  noscript ; alternate content container for non script-based rendering
  frameset ; only one noframes element permitted per document
  frame ; tiled window within frameset
  iframe ; inline subwindow
  noframes ; alternate content container for non frame-based rendering
  body
  div ; generic language/style container
  p
  h1
  h2
  h3
  h4
  h5
  h6
  ul ; Unordered list
  ol ; Ordered (numbered) list
  menu ; single column list (DEPRECATED)
  dir ; multiple column list (DEPRECATED)
  li ; list item
  dl ; definition lists - dt for term, dd for its definition
  dt
  dd
  address ; information on author
  pre
  blockquote
  center ; center content
  ins
  del
  a ; content is inline; except that anchors shouldn't be nested
  span ; generic language/style container
  bdo ; I18N BiDi over-ride
  em ; emphasis
  strong ; strong emphasis
  dfn ; definitional
  code ; program code
  samp ; sample
  kbd ; something user would type
  var ; variable
  cite ; citation
  abbr ; abbreviation
  acronym ; acronym
  q ; inlined quote
  sub ; subscript
  sup ; superscript
  tt ; fixed pitch font
  i ; italic font
  b ; bold font
  big ; bigger font
  small ; smaller font
  u ; underline
  s ; strike-through
  strike ; strike-through
  font ; local change to font
  object ; embeded objects
  applet ; Java applet
  form ; forms shouldn't be nested
  label ; text that belongs to a form control
  select ; option selector
  optgroup ; option group
  option ; selectable choice
  textarea ; multi-line text field
  fieldset ; group form fields
  legend ; fieldset label (one per fieldset)
  button ; push button
  table ; holds caption?, (col*|colgroup*), thead?, tfoot?, (tbody+|tr+)
  caption ; caption text
  thead ; header part, holds tr
  tfoot ; footer part, holds tr
  tbody ; body part, holds tr
  colgroup ; column group, olds col
  tr ; holds th or td
  th ; header cell
  td)

@(define-syntax-rule (def-tags/empty tag ...)
   @deftogether[(
   @defproc[(tag [v outputable/c] (... ...)) procedure?] ...
   )]{

   Like @racket[element], but with the symbolic form of the function
   name added as the first argument.

   @examples[#:eval html-eval
    (output-xml (hr))]})

@(def-tags/empty
  base meta link hr br basefont param img area input isindex col)

@(define-syntax-rule (def-entities ent ...)
   @deftogether[(
   @defthing[ent procedure?] ...
   )]{

  The result of @racket[(entity '_id)] for each @racket[_id].

   @examples[#:eval html-eval
    (output-xml nbsp)]})

@(def-entities
  nbsp ndash mdash bull middot sdot lsquo rsquo sbquo ldquo rdquo bdquo
  lang rang dagger Dagger plusmn deg)


@defproc[(script/inline [v outputable/c] ...) procedure?]{

Procedures a value that renders as an inline script.

@examples[#:eval html-eval
(output-xml (script/inline type: "text/javascript" "var x = 5;"))]}


@defproc[(style/inline [v outputable/c] ...) procedure?]{

Procedures a value that renders as an inline style sheet.

@examples[#:eval html-eval
(output-xml (style/inline type: "text/css"
                          ".racket { font-size: xx-large; }"))]}


@; ----------------------------------------

@section[#:tag "html-xml"]{Generating XML Strings}

@defmodule[scribble/html/xml]{The @racketmodname[scribble/html/xml]
provides functions for XML representations that @deftech{XML-render} to string form
via @racket[output-xml] or @racket[xml->string].}


@defproc[(output-xml [content outputable/c] [port output-port? (current-output-port)])
         void?]{

Renders @racket[content] in the same way as @racket[output], but using
the value of @racket[xml-writer] as the @tech{current writer} so that
special characters are escaped as needed.}


@defproc[(xml->string [content outputable/c]) string?]{

Renders @racket[content] to a string via @racket[output-xml].}


@defparam[xml-writer writer ((string? output-port? . -> . void))]{

A parameter for a function that is used with @racket[with-writer] by
@racket[output-xml]. The default value is a function that escapes
@litchar{&}, @litchar{<}, @litchar{>}, and @litchar{"} to entity form.}


@defproc[(make-element [tag symbol?]
                       [attrs (listof (cons/c symbol? outputable/c))]
                       [content outputable/c])
         (and/c procedure outputable/c?)]{

Produces a value that @tech{XML-renders} as XML for the
given tag, attributes, and content.

When an attribute in @racket[attrs] is mapped to @racket[#f], then it
is skipped. When an attribute is mapped to @racket[#t], then it is
rendered as present, but without a value.

@examples[#:eval html-eval
(output-xml (make-element 'b '() '("Try" #\space "Racket")))
(output-xml (make-element 'a '((href . "http://racket-lang.org")) "Racket"))
(output-xml (make-element 'div '((class . "big") (overlay . #t)) "example"))
]}


@defproc[(element [tag symbol?] [attrs-and-content any/c] ...)
         (and procedure outputable/c?)]{

Like @racket[make-element], but the list of @racket[attrs-and-content]
is parsed via @racket[attributes+body] to separate the attributes and
content.

@examples[#:eval html-eval
(output-xml (element 'b "Try" #\space "Racket"))
(output-xml (element 'a 'href: "http://racket-lang.org" "Racket"))
(output-xml (element 'div 'class: "big" 'overlay: #t "example"))
(require scribble/html)
(output-xml (element 'div class: "big" overlay: #t "example"))
]}


@defproc[(element/not-empty [tag symbol?] [attrs-and-content any/c] ...)
         (and/c procedure? outputable/c)]{

Like @racket[element], but the result always renders with an separate
closing tag.

@examples[#:eval html-eval
(output-xml (element 'span))
(output-xml (element/not-empty 'span))
]}


@defproc[(attribute? [v any/c]) (or/c #f symbol?)]{

Returns a symbol without if @racket[v] is a symbol that ends with
@litchar{:}, @racket[#f] otherwise. When a symbol is returned, it is
the same as @racket[v], but without the trailing @litchar{:}.

@examples[#:eval html-eval
(attribute? 'a:)
(attribute? 'a)
(require scribble/html)
(attribute? a:)
]}


@defproc[(attributes+body [lst list?]) (values (listof (cons/c symbol? any/c))
                                               list?)]{

Parses @racket[lst] into an association list mapping attributes to
list elements plus a list of remaining elements. The first
even-positioned (counting from 0) non-@racket[attribute?] element of
@racket[lst] is the start of the ``remaining elements'' list, while
each preceding even-positioned attribute is mapped in the association
list to the immediately following element of @racket[lst]. In the
association list, the trailing @litchar{:} is stripped for each
attribute.}


@defproc[(split-attributes+body [lst list?]) (values list? list?)]{

Like @racket[attributes+body], but produces a flat list (of
alternating attributes and value) instead of an association list as
the first result.}


@defproc[(literal [content any/c] ...) procedure?]{

Produces a value that @tech{XML-renders} without escapes
for special characters.

@examples[#:eval html-eval
(output-xml (literal "a->b"))
(output-xml "a->b")]}


@defproc[(entity [v (or/c exact-integer? symbol?)]) procedure?]{

Produces a value that @tech{XML-renders} as a numeric or
symbolic entity.

@examples[#:eval html-eval
(output-xml (entity 'gt))]}


@defproc[(comment [content outputable/c] ... [#:newlines? newlines? any/c #f])
         procedure?]{

Produces a value that @tech{XML-renders} as a comment with
literal content. If @racket[newlines?] is true, then newlines are
inserted before and after the content.

@examples[#:eval html-eval
(output-xml (comment "testing" 1 2 3))]}


@defproc[(cdata [content outputable/c] ... 
                [#:newlines? newlines? any/c #t]
                [#:line-pfx line-pfx any/c #f])
         procedure?]{

Produces a value that @tech{XML-renders} as CDATA with
literal content. If @racket[newlines?] is true, then newlines are
inserted before and after the content. The @racket[line-pfx] value is
rendered before the CDATA opening and closing markers.

@examples[#:eval html-eval
(output-xml (cdata "testing" 1 2 3))]}


@defform[(define/provide-elements/empty tag-id ...)]{

Defines and exports @racket[tag-id] as a function that is like
@racket[element], but with @racket['tag-id] added as the first argument.}


@defform[(define/provide-elements/not-empty tag-id ...)]{

Defines and exports @racket[tag-id] as a function that is like
@racket[element/not-empty], but with @racket['_tag-id] added as the
first argument.}


@defform[(define/provide-entities entity-id ...)]{

Defines and exports @racket[entity-id] as the 
result of @racket[(entity '_entity-id)].}

@; ----------------------------------------

@section[#:tag "html-resources"]{HTML Resources}

@defmodule[scribble/html/resource]

@defproc[(resource [path string?]
                   [renderer (or/c (path-string? . -> . any) #f)]
                   [#:exists exists (or/c 'delete-file #f) 'delete-file])
         (and/c resource?
                (->* () (outputable/c) -> string?))]{

Creates and returns a new @deftech{resource} value. Creating a
resource registers @racket[renderer] (if non-@racket[#f]) to be called when rendering is
initiated by @racket[render-all], while calling the result resource as
a function generates a URL for the resource.

For example, a typical use of @racket[resource] is to register the
generation of a CSS file, where the value produced by
@racket[resource] itself renders as the URL for the generated CSS
file. Another possible use of @racket[resource] is to generate an HTML
file, where the @racket[resource] result renders as the URL of the
generated HTML page.

The @racket[path] argument specifies the path of the output file,
relative to the working directory, indicating where the resource file
should be placed. Though @racket[url-roots], @racket[path] also
determines the ultimate URL. The @racket[path] string must be a
@litchar{/}-separated relative path with no @litchar{..}, @litchar{.},
or @litchar{//}. The @racket[path] string can end in @litchar{/}, in
which case @racket["index.html"] is effectively added to the string.
Using @racket[resource] with @racket[#f] as @racket[renderer] is
useful for converting a path to a URL according to @racket[url-roots].

The @racket[renderer] argument (when non-@racket[#f]) renders the resource, receiving the
path for the file to be created. The path provided to
@racket[renderer] will be different from @racket[path], because the
function is invoked in the target directory.

The resulting resource value is a function that returns the URL for
the resource. The function accepts an optional boolean; if a true
value is provided, the result is an absolute URL, instead of relative.
Note that the function can be used as a value for @racket[output],
which uses the resource value as a thunk (that renders as the relative
URL for the resource).  The default relative resulting URL is, of
course, a value that depends on the currently rendered resource that
uses this value.

When @racket[renderer] is called by @racket[render-all], more
resources can be created while rendering; the newly created resources
will also be rendered, in turn, until no more new resources are
created.

If @racket[exists] is @racket['delete-file] and the target file exists
when @racket[renderer] is to be called, then the file is deleted
before @racket[renderer] is called.}


@defparam[url-roots roots (or/c #f
                                (listof (cons/c path-string?
                                                (cons/c string?
                                                        (listof (or/c 'abs 'index))))))]{

A parameter that determines how resource paths are converted to URLs
for reference. A @racket[#f] value is equivalent to an empty list.

The parameter value is a mapping from path prefixes to URLs (actually,
any string). When two paths have the same prefix, links from one to
the other are relative (unless absolute links are requested); if they
have different prefixes, the full URL is used. The paths enclosed by
two root paths must be disjoint (e.g., the list must not include
both @racket["/colors"] and @racket["/colors/red"], but it can include
both @racket["/colors/red"] and @racket["/colors/blue"]).

If an item in the parameter's list includes @racket['abs], then a
site-local, absolute URL (i.e., a URL that starts with @litchar{/}) is
produced for references among files within the corresponding prefix.

If an item in the parameter's list includes @racket['index], then a
reference to a directory path is converted to a reference to
@filepath{index.html}, otherwise a reference to @filepath{index.html}
is converted to a directory path.}


@defproc[(resource? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a procedure (that takes 0 or 1
arguments) produced by @racket[resource].}


@defproc[(render-all) void?]{

Generates all resources registered via @racket[resource].}


@defproc[(file-writer [content-writer (outputable/c output-port? . -> . any)]
                      [content outputable/c])
         (path-string? . -> . any)]{

Produces a function that is useful as a @racket[_writer] argument to
@racket[resource]. Given a path, the produced function writes
@racket[content] to the path by passing @racket[content] and an output
port for the file to @racket[content-writer].}

@; ------------------------------------------------------------

@close-eval[html-eval]
