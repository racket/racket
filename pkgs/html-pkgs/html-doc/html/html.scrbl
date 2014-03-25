#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label html
                     xml
                     racket/contract))

@(define xexpr @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression})

@title{HTML: Parsing Library}

@defmodule[html]{The @racketmodname[html] library provides
functions to read html documents and structures to represent them.}


@deftogether[(
@defproc[(read-xhtml [port input-port?])
         html?]{}

@defproc[(read-html [port input-port?])
         html?]{}
)]{

Reads (X)HTML from a port, producing an @racket[html] instance.}


@defproc[(read-html-as-xml [port input-port?])
         (listof content/c)]{
 
Reads HTML from a port, producing a list of XML content, each of which could be
turned into an @|xexpr|, if necessary, with @racket[xml->xexpr].}

@defboolparam[read-html-comments v]{
 If @racket[v] is not @racket[#f], then comments are read and returned. Defaults to @racket[#f].
}

@defboolparam[use-html-spec v]{
 If @racket[v] is not @racket[#f], then the HTML must respect the HTML specification 
 with regards to what elements are allowed to be the children of
 other elements. For example, the top-level @racket["<html>"]
 element may only contain a @racket["<body>"] and @racket["<head>"]
 element. Defaults to @racket[#t].
}

@section{Example}
@(require (only-in (for-label racket)
                   open-input-string string-append
                   list cond match apply append map printf define require module)
          (for-label (prefix-in h: html))
          (for-label (prefix-in x: xml)))
@def+int[
(module html-example racket

  (code:comment @#,t{Some of the symbols in @racketmodname[html] and @racketmodname[xml] conflict with})
  (code:comment @#,t{each other and with racket/base language, so we prefix})
  (code:comment @#,t{to avoid namespace conflict.})
  (require (prefix-in h: html)
           (prefix-in x: xml))

  (define an-html
    (h:read-xhtml 
     (open-input-string 
      (string-append 
       "<html><head><title>My title</title></head><body>"
       "<p>Hello world</p><p><b>Testing</b>!</p>"
       "</body></html>"))))
  
  (code:comment @#,t{extract-pcdata: html-content/c -> (listof string)})
  (code:comment @#,t{Pulls out the pcdata strings from some-content.})
  (define (extract-pcdata some-content)
    (cond [(x:pcdata? some-content)
           (list (x:pcdata-string some-content))]
          [(x:entity? some-content)
           (list)]
          [else
           (extract-pcdata-from-element some-content)]))
  
  (code:comment @#,t{extract-pcdata-from-element: html-element -> (listof string)})
  (code:comment @#,t{Pulls out the pcdata strings from an-html-element.})
  (define (extract-pcdata-from-element an-html-element)
    (match an-html-element
      [(struct h:html-full (attributes content))
       (apply append (map extract-pcdata content))]
      
      [(struct h:html-element (attributes))
       '()]))
  
  (printf "~s\n" (extract-pcdata an-html)))
(require 'html-example)
]



@section{HTML Structures}

@racket[pcdata], @racket[entity], and @racket[attribute] are defined
in the @racketmodname[xml] documentation.

@defthing[html-content/c contract?]{
A @racket[html-content/c] is either
@itemize[
  @item[@racket[html-element]]
  @item[@racket[pcdata]]
  @item[@racket[entity]]]
}

@defstruct[html-element ([attributes (listof attribute)])]{
  Any of the structures below inherits from @racket[html-element].}

@defstruct[(html-full struct:html-element) ([content (listof html-content/c)])]{
  Any html tag that may include content also inherits from
  @racket[html-full] without adding any additional fields.}

@defstruct[(mzscheme html-full) ()]{
  A @racket[mzscheme] is special legacy value for the old documentation system.
}

@defstruct[(html html-full) ()]{
  A @racket[html] is
  @racket[(make-html (listof attribute) (listof Contents-of-html))]
}

A @racket[Contents-of-html] is either
@itemize[
  @item{@racket[body]}
  @item{@racket[head]}
]


@defstruct[(div html-full)()]{
  A @racket[div] is
  @racket[(make-div (listof attribute) (listof G2))]}


@defstruct[(center html-full)()]{
  A @racket[center] is
  @racket[(make-center (listof attribute) (listof G2))]
}


@defstruct[(blockquote html-full) ()]{
  A @racket[blockquote] is
  @racket[(make-blockquote (listof attribute) (listof G2))]
}

@defstruct[(ins html-full) ()]{
  An Ins is
  @racket[(make-ins (listof attribute) (listof G2))]
}

@defstruct[(del html-full) ()]{
  A @racket[del] is
  @racket[(make-del (listof attribute) (listof G2))]
}

@defstruct[(dd html-full) ()]{
  A @racket[dd] is
  @racket[(make-dd (listof attribute) (listof G2))]
}

@defstruct[(li html-full) ()]{
  A @racket[li] is
  @racket[(make-li (listof attribute) (listof G2))]
}

@defstruct[(th html-full) ()]{
  A @racket[th] is
  @racket[(make-th (listof attribute) (listof G2))]
}

@defstruct[(td html-full) ()]{
A @racket[td] is
@racket[(make-td (listof attribute) (listof G2))]
}

@defstruct[(iframe html-full) ()]{
An @racket[iframe] is
@racket[(make-iframe (listof attribute) (listof G2))]
}

@defstruct[(noframes html-full) ()]{
A @racket[noframes] is
@racket[(make-noframes (listof attribute) (listof G2))]
}


@defstruct[(noscript html-full) ()]{
A @racket[noscript] is
@racket[(make-noscript (listof attribute) (listof G2))]
}


@defstruct[(style html-full) ()]{
A @racket[style] is
@racket[(make-style (listof attribute) (listof pcdata))]
}


@defstruct[(script html-full) ()]{
A @racket[script] is
@racket[(make-script (listof attribute) (listof pcdata))]
}


@defstruct[(basefont html-element) ()]{
A @racket[basefont] is
@racket[(make-basefont (listof attribute))]
}


@defstruct[(br html-element) ()]{
A @racket[br] is
@racket[(make-br (listof attribute))]
}

@defstruct[(area html-element) ()]{
An @racket[area] is
@racket[(make-area (listof attribute))]
}

@defstruct[(alink html-element) ()]{
A @racket[alink] is
@racket[(make-alink (listof attribute))]
}

@defstruct[(img html-element) ()]{
An @racket[img] is
@racket[(make-img (listof attribute))]
}

@defstruct[(param html-element) ()]{
A @racket[param] is
@racket[(make-param (listof attribute))]
}

@defstruct[(hr html-element) ()]{
A @racket[hr] is
@racket[(make-hr (listof attribute))]
}

@defstruct[(input html-element) ()]{
An @racket[input] is
@racket[(make-input (listof attribute))]
}

@defstruct[(col html-element) ()]{
A @racket[col] is
@racket[(make-col (listof attribute))]
}

@defstruct[(isindex html-element) ()]{
An @racket[isindex] is
@racket[(make-isindex (listof attribute))]
}

@defstruct[(base html-element) ()]{
A @racket[base] is
@racket[(make-base (listof attribute))]
}

@defstruct[(meta html-element) ()]{
A @racket[meta] is
@racket[(make-meta (listof attribute))]
}

@defstruct[(option html-full) ()]{
An @racket[option] is
@racket[(make-option (listof attribute) (listof pcdata))]
}


@defstruct[(textarea html-full) ()]{
A @racket[textarea] is
@racket[(make-textarea (listof attribute) (listof pcdata))]
}


@defstruct[(title html-full) ()]{
A @racket[title] is
@racket[(make-title (listof attribute) (listof pcdata))]
}


@defstruct[(head html-full) ()]{
  A @racket[head] is
  @racket[(make-head (listof attribute) (listof Contents-of-head))]
}


A @racket[Contents-of-head] is either
@itemize[
  @item[@racket[base]]
  @item[@racket[isindex]]
  @item[@racket[alink]]
  @item[@racket[meta]]
  @item[@racket[object]]
  @item[@racket[script]]
  @item[@racket[style]]
  @item[@racket[title]]
]


@defstruct[(tr html-full) ()]{
A @racket[tr] is
@racket[(make-tr (listof attribute) (listof Contents-of-tr))]
}


A @racket[Contents-of-tr] is either
@itemize[
  @item[@racket[td]]
  @item[@racket[th]]
]


@defstruct[(colgroup html-full) ()]{
A @racket[colgroup] is
@racket[(make-colgroup (listof attribute) (listof col))]
}


@defstruct[(thead html-full) ()]{
A @racket[thead] is
@racket[(make-thead (listof attribute) (listof tr))]
}


@defstruct[(tfoot html-full) ()]{
A @racket[tfoot] is
@racket[(make-tfoot (listof attribute) (listof tr))]
}


@defstruct[(tbody html-full) ()]{
A @racket[tbody] is
@racket[(make-tbody (listof attribute) (listof tr))]
}


@defstruct[(tt html-full) ()]{
A @racket[tt] is
@racket[(make-tt (listof attribute) (listof G5))]
}


@defstruct[(i html-full) ()]{
An @racket[i] is
@racket[(make-i (listof attribute) (listof G5))]
}


@defstruct[(b html-full) ()]{
A @racket[b] is
@racket[(make-b (listof attribute) (listof G5))]
}


@defstruct[(u html-full) ()]{
An @racket[u] is
@racket[(make-u (listof attribute) (listof G5))]
}


@defstruct[(s html-full) ()]{
A @racket[s] is
@racket[(make-s (listof attribute) (listof G5))]
}


@defstruct[(strike html-full) ()]{
A @racket[strike] is
@racket[(make-strike (listof attribute) (listof G5))]
}


@defstruct[(big html-full) ()]{
A @racket[big] is
@racket[(make-big (listof attribute) (listof G5))]
}


@defstruct[(small html-full) ()]{
A @racket[small] is
@racket[(make-small (listof attribute) (listof G5))]
}


@defstruct[(em html-full) ()]{
An @racket[em] is
@racket[(make-em (listof attribute) (listof G5))]
}


@defstruct[(strong html-full) ()]{
A @racket[strong] is
@racket[(make-strong (listof attribute) (listof G5))]
}


@defstruct[(dfn html-full) ()]{
A @racket[dfn] is
@racket[(make-dfn (listof attribute) (listof G5))]
}


@defstruct[(code html-full) ()]{
A @racket[code] is
@racket[(make-code (listof attribute) (listof G5))]
}


@defstruct[(samp html-full) ()]{
A @racket[samp] is
@racket[(make-samp (listof attribute) (listof G5))]
}


@defstruct[(kbd html-full) ()]{
A @racket[kbd] is
@racket[(make-kbd (listof attribute) (listof G5))]
}


@defstruct[(var html-full) ()]{
A @racket[var] is
@racket[(make-var (listof attribute) (listof G5))]
}


@defstruct[(cite html-full) ()]{
A @racket[cite] is
@racket[(make-cite (listof attribute) (listof G5))]
}


@defstruct[(abbr html-full) ()]{
An @racket[abbr] is
@racket[(make-abbr (listof attribute) (listof G5))]
}


@defstruct[(acronym html-full) ()]{
An @racket[acronym] is
@racket[(make-acronym (listof attribute) (listof G5))]
}


@defstruct[(sub html-full) ()]{
A @racket[sub] is
@racket[(make-sub (listof attribute) (listof G5))]
}


@defstruct[(sup html-full) ()]{
A @racket[sup] is
@racket[(make-sup (listof attribute) (listof G5))]
}


@defstruct[(span html-full) ()]{
A @racket[span] is
@racket[(make-span (listof attribute) (listof G5))]
}


@defstruct[(bdo html-full) ()]{
A @racket[bdo] is
@racket[(make-bdo (listof attribute) (listof G5))]
}


@defstruct[(font html-full) ()]{
A @racket[font] is
@racket[(make-font (listof attribute) (listof G5))]
}


@defstruct[(p html-full) ()]{
A @racket[p] is
@racket[(make-p (listof attribute) (listof G5))]
}


@defstruct[(h1 html-full) ()]{
A @racket[h1] is
@racket[(make-h1 (listof attribute) (listof G5))]
}


@defstruct[(h2 html-full) ()]{
A @racket[h2] is
@racket[(make-h2 (listof attribute) (listof G5))]
}


@defstruct[(h3 html-full) ()]{
A @racket[h3] is
@racket[(make-h3 (listof attribute) (listof G5))]
}


@defstruct[(h4 html-full) ()]{
A @racket[h4] is
@racket[(make-h4 (listof attribute) (listof G5))]
}


@defstruct[(h5 html-full) ()]{
A @racket[h5] is
@racket[(make-h5 (listof attribute) (listof G5))]
}


@defstruct[(h6 html-full) ()]{
A @racket[h6] is
@racket[(make-h6 (listof attribute) (listof G5))]
}


@defstruct[(q html-full) ()]{
A @racket[q] is
@racket[(make-q (listof attribute) (listof G5))]
}


@defstruct[(dt html-full) ()]{
A @racket[dt] is
@racket[(make-dt (listof attribute) (listof G5))]
}


@defstruct[(legend html-full) ()]{
A @racket[legend] is
@racket[(make-legend (listof attribute) (listof G5))]
}


@defstruct[(caption html-full) ()]{
A @racket[caption] is
@racket[(make-caption (listof attribute) (listof G5))]
}


@defstruct[(table html-full) ()]{
A @racket[table] is
@racket[(make-table (listof attribute) (listof Contents-of-table))]
}


A @racket[Contents-of-table] is either
@itemize[
  @item[@racket[caption]]
  @item[@racket[col]]
  @item[@racket[colgroup]]
  @item[@racket[tbody]]
  @item[@racket[tfoot]]
  @item[@racket[thead]]
]

@defstruct[(button html-full) ()]{
A @racket[button] is
@racket[(make-button (listof attribute) (listof G4))]
}


@defstruct[(fieldset html-full) ()]{
A @racket[fieldset] is
@racket[(make-fieldset (listof attribute) (listof Contents-of-fieldset))]
}


A @racket[Contents-of-fieldset] is either
@itemize[
  @item[@racket[legend]]
  @item{G2}
]


@defstruct[(optgroup html-full) ()]{
An @racket[optgroup] is
@racket[(make-optgroup (listof attribute) (listof option))]
}


@defstruct[(select html-full) ()]{
A @racket[select] is
@racket[(make-select (listof attribute) (listof Contents-of-select))]
}


A @racket[Contents-of-select] is either
@itemize[
  @item[@racket[optgroup]]
  @item[@racket[option]]
]

@defstruct[(label html-full) ()]{
A @racket[label] is
@racket[(make-label (listof attribute) (listof G6))]
}


@defstruct[(form html-full) ()]{
A @racket[form] is
@racket[(make-form (listof attribute) (listof G3))]
}


@defstruct[(ol html-full) ()]{
An @racket[ol] is
@racket[(make-ol (listof attribute) (listof li))]
}


@defstruct[(ul html-full) ()]{
An @racket[ul] is
@racket[(make-ul (listof attribute) (listof li))]
}


@defstruct[(dir html-full) ()]{
A @racket[dir] is
@racket[(make-dir (listof attribute) (listof li))]
}


@defstruct[(menu html-full) ()]{
A @racket[menu] is
@racket[(make-menu (listof attribute) (listof li))]
}


@defstruct[(dl html-full) ()]{
A @racket[dl] is
@racket[(make-dl (listof attribute) (listof Contents-of-dl))]
}


A @racket[Contents-of-dl] is either
@itemize[
  @item[@racket[dd]]
  @item[@racket[dt]]
]


@defstruct[(pre html-full) ()]{
A @racket[pre] is
@racket[(make-pre (listof attribute) (listof Contents-of-pre))]
}


A @racket[Contents-of-pre] is either
@itemize[
  @item{G9}
  @item{G11}
]


@defstruct[(object html-full) ()]{
An @racket[object] is
@racket[(make-object (listof attribute) (listof Contents-of-object-applet))]
}


@defstruct[(applet html-full) ()]{
An @racket[applet] is
@racket[(make-applet (listof attribute) (listof Contents-of-object-applet))]
}


A @racket[Contents-of-object-applet] is either
@itemize[
  @item[@racket[param]]
  @item{G2}
]


@defstruct[(-map html-full) ()]{
A Map is
@racket[(make--map (listof attribute) (listof Contents-of-map))]
}


A @racket[Contents-of-map] is either
@itemize[
  @item[@racket[area]]
  @item[@racket[fieldset]]
  @item[@racket[form]]
  @item[@racket[isindex]]
  @item{G10}
]


@defstruct[(a html-full) ()]{
An @racket[a] is
@racket[(make-a (listof attribute) (listof Contents-of-a))]
}


A @racket[Contents-of-a] is either
@itemize[
  @item[@racket[label]]
  @item{G7}
]

@defstruct[(address html-full) ()]{
An @racket[address] is
@racket[(make-address (listof attribute) (listof Contents-of-address))]
}


A @racket[Contents-of-address] is either
@itemize[
  @item[@racket[p]]
  @item{G5}
]


@defstruct[(body html-full) ()]{
  A @racket[body] is
  @racket[(make-body (listof attribute) (listof Contents-of-body))]
}

A @racket[Contents-of-body] is either
@itemize[
  @item[@racket[del]]
  @item[@racket[ins]]
  @item{G2}
]


A @racket[G12] is either
@itemize[
  @item[@racket[button]]
  @item[@racket[iframe]]
  @item[@racket[input]]
  @item[@racket[select]]
  @item[@racket[textarea]]
]


A @racket[G11] is either
@itemize[
  @item[@racket[a]]
  @item[@racket[label]]
  @item[@racket[G12]]
]

A @racket[G10] is either
@itemize[
  @item[@racket[address]]
  @item[@racket[blockquote]]
  @item[@racket[center]]
  @item[@racket[dir]]
  @item[@racket[div]]
  @item[@racket[dl]]
  @item[@racket[h1]]
  @item[@racket[h2]]
  @item[@racket[h3]]
  @item[@racket[h4]]
  @item[@racket[h5]]
  @item[@racket[h6]]
  @item[@racket[hr]]
  @item[@racket[menu]]
  @item[@racket[noframes]]
  @item[@racket[noscript]]
  @item[@racket[ol]]
  @item[@racket[p]]
  @item[@racket[pre]]
  @item[@racket[table]]
  @item[@racket[ul]]
]


A @racket[G9] is either
@itemize[
  @item[@racket[abbr]]
  @item[@racket[acronym]]
  @item[@racket[b]]
  @item[@racket[bdo]]
  @item[@racket[br]]
  @item[@racket[cite]]
  @item[@racket[code]]
  @item[@racket[dfn]]
  @item[@racket[em]]
  @item[@racket[i]]
  @item[@racket[kbd]]
  @item[@racket[-map]]
  @item[@racket[pcdata]]
  @item[@racket[q]]
  @item[@racket[s]]
  @item[@racket[samp]]
  @item[@racket[script]]
  @item[@racket[span]]
  @item[@racket[strike]]
  @item[@racket[strong]]
  @item[@racket[tt]]
  @item[@racket[u]]
  @item[@racket[var]]
]


A @racket[G8] is either
@itemize[
  @item[@racket[applet]]
  @item[@racket[basefont]]
  @item[@racket[big]]
  @item[@racket[font]]
  @item[@racket[img]]
  @item[@racket[object]]
  @item[@racket[small]]
  @item[@racket[sub]]
  @item[@racket[sup]]
  @item{G9}
]


A @racket[G7] is either
@itemize[
  @item{G8}
  @item{G12}
]

A @racket[G6] is either
@itemize[
  @item[@racket[a]]
  @item{G7}
]

A @racket[G5] is either
@itemize[
  @item[@racket[label]]
  @item{G6}
]

A @racket[G4] is either
@itemize[
  @item{G8}
  @item{G10}
]

A @racket[G3] is either
@itemize[
  @item[@racket[fieldset]]
  @item[@racket[isindex]]
  @item{G4}
  @item{G11}
]

A @racket[G2] is either
@itemize[
  @item[@racket[form]]
  @item{G3}
]

