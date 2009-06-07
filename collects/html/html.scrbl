#lang scribble/doc
@(require scribble/manual
          scribble/eval
          (for-label html)
          (for-label xml))

@(define xexpr @tech[#:doc '(lib "xml/xml.scrbl")]{X-expression})

@title{@bold{HTML}: Parsing Library}

@defmodule[html]{The @schememodname[html] library provides
functions to read html documents and structures to represent them.}


@deftogether[(
@defproc[(read-xhtml [port input-port?])
         html?]{}

@defproc[(read-html [port input-port?])
         html?]{}
)]{

Reads (X)HTML from a port, producing an @scheme[html] instance.}


@defproc[(read-html-as-xml [port input-port?])
         (listof content/c)]{
 
Reads HTML from a port, producing an @xexpr compatible with the
@schememodname[xml] library (which defines @scheme[content/c]).}

@defboolparam[read-html-comments v]{
 If @scheme[v] is not @scheme[#f], then comments are read and returned. Defaults to @scheme[#f].
}

@defboolparam[use-html-spec v]{
 If @scheme[v] is not @scheme[#f], then the HTML must respect the HTML specification 
 with regards to what elements are allowed to be the children of
 other elements. For example, the top-level @scheme["<html>"]
 element may only contain a @scheme["<body>"] and @scheme["<head>"]
 element. Defaults to @scheme[#f].
}                               

@section{Example}
@(require (only-in (for-label scheme)
                   open-input-string string-append
                   list cond match apply append map printf define require module)
          (for-label (prefix-in h: html))
          (for-label (prefix-in x: xml)))
@def+int[
(module html-example scheme

  (code:comment @#,t{Some of the symbols in @schememodname[html] and @schememodname[xml] conflict with})
  (code:comment @#,t{each other and with scheme/base language, so we prefix})
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
  
  (code:comment @#,t{extract-pcdata: html-content -> (listof string)})
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
  
  (printf "~s~n" (extract-pcdata an-html)))
(require 'html-example)
]



@section{HTML Structures}

@scheme[pcdata], @scheme[entity], and @scheme[attribute] are defined
in the @schememodname[xml] documentation.

A @scheme[html-content] is either
@itemize[
  @item[@scheme[html-element]]
  @item[@scheme[pcdata]]
  @item[@scheme[entity]]]


@defstruct[html-element ([attributes (listof attribute)])]{
  Any of the structures below inherits from @scheme[html-element].}

@defstruct[(html-full struct:html-element) ([content (listof html-content)])]{
  Any html tag that may include content also inherits from
  @scheme[html-full] without adding any additional fields.}



@defstruct[(html html-full) ()]{
  A @scheme[html] is
  @scheme[(make-html (listof attribute) (listof Contents-of-html))]
}

A @scheme[Contents-of-html] is either
@itemize[
  @item{@scheme[body]}
  @item{@scheme[head]}
]


@defstruct[(div html-full)()]{
  A @scheme[div] is
  @scheme[(make-div (listof attribute) (listof G2))]}


@defstruct[(center html-full)()]{
  A @scheme[center] is
  @scheme[(make-center (listof attribute) (listof G2))]
}


@defstruct[(blockquote html-full) ()]{
  A @scheme[blockquote] is
  @scheme[(make-blockquote (listof attribute) G2)]
}

@defstruct[(ins html-full) ()]{
  An Ins is
  @scheme[(make-ins (listof attribute) (listof G2))]
}

@defstruct[(del html-full) ()]{
  A @scheme[del] is
  @scheme[(make-del (listof attribute) (listof G2))]
}

@defstruct[(dd html-full) ()]{
  A @scheme[dd] is
  @scheme[(make-dd (listof attribute) (listof G2))]
}

@defstruct[(li html-full) ()]{
  A @scheme[li] is
  @scheme[(make-li (listof attribute) (listof G2))]
}

@defstruct[(th html-full) ()]{
  A @scheme[th] is
  @scheme[(make-th (listof attribute) (listof G2))]
}

@defstruct[(td html-full) ()]{
A @scheme[td] is
@scheme[(make-td (listof attribute) (listof G2))]
}

@defstruct[(iframe html-full) ()]{
An @scheme[iframe] is
@scheme[(make-iframe (listof attribute) (listof G2))]
}

@defstruct[(noframes html-full) ()]{
A @scheme[noframes] is
@scheme[(make-noframes (listof attribute) (listof G2))]
}


@defstruct[(noscript html-full) ()]{
A @scheme[noscript] is
@scheme[(make-noscript (listof attribute) (listof G2))]
}


@defstruct[(style html-full) ()]{
A @scheme[style] is
@scheme[(make-style (listof attribute) (listof pcdata))]
}


@defstruct[(script html-full) ()]{
A @scheme[script] is
@scheme[(make-script (listof attribute) (listof pcdata))]
}


@defstruct[(basefont html-element) ()]{
A @scheme[basefont] is
@scheme[(make-basefont (listof attribute))]
}


@defstruct[(br html-element) ()]{
A @scheme[br] is
@scheme[(make-br (listof attribute))]
}

@defstruct[(area html-element) ()]{
An @scheme[area] is
@scheme[(make-area (listof attribute))]
}

@defstruct[(alink html-element) ()]{
A @scheme[alink] is
@scheme[(make-alink (listof attribute))]
}

@defstruct[(img html-element) ()]{
An @scheme[img] is
@scheme[(make-img (listof attribute))]
}

@defstruct[(param html-element) ()]{
A @scheme[param] is
@scheme[(make-param (listof attribute))]
}

@defstruct[(hr html-element) ()]{
A @scheme[hr] is
@scheme[(make-hr (listof attribute))]
}

@defstruct[(input html-element) ()]{
An @scheme[input] is
@scheme[(make-input (listof attribute))]
}

@defstruct[(col html-element) ()]{
A @scheme[col] is
@scheme[(make-col (listof attribute))]
}

@defstruct[(isindex html-element) ()]{
An @scheme[isindex] is
@scheme[(make-isindex (listof attribute))]
}

@defstruct[(base html-element) ()]{
A @scheme[base] is
@scheme[(make-base (listof attribute))]
}

@defstruct[(meta html-element) ()]{
A @scheme[meta] is
@scheme[(make-meta (listof attribute))]
}

@defstruct[(option html-full) ()]{
An @scheme[option] is
@scheme[(make-option (listof attribute) (listof pcdata))]
}


@defstruct[(textarea html-full) ()]{
A @scheme[textarea] is
@scheme[(make-textarea (listof attribute) (listof pcdata))]
}


@defstruct[(title html-full) ()]{
A @scheme[title] is
@scheme[(make-title (listof attribute) (listof pcdata))]
}


@defstruct[(head html-full) ()]{
  A @scheme[head] is
  @scheme[(make-head (listof attribute) (listof Contents-of-head))]
}


A @scheme[Contents-of-head] is either
@itemize[
  @item[@scheme[base]]
  @item[@scheme[isindex]]
  @item[@scheme[alink]]
  @item[@scheme[meta]]
  @item[@scheme[object]]
  @item[@scheme[script]]
  @item[@scheme[style]]
  @item[@scheme[title]]
]


@defstruct[(tr html-full) ()]{
A @scheme[tr] is
@scheme[(make-tr (listof attribute) (listof Contents-of-tr))]
}


A @scheme[Contents-of-tr] is either
@itemize[
  @item[@scheme[td]]
  @item[@scheme[th]]
]


@defstruct[(colgroup html-full) ()]{
A @scheme[colgroup] is
@scheme[(make-colgroup (listof attribute) (listof col))]
}


@defstruct[(thead html-full) ()]{
A @scheme[thead] is
@scheme[(make-thead (listof attribute) (listof tr))]
}


@defstruct[(tfoot html-full) ()]{
A @scheme[tfoot] is
@scheme[(make-tfoot (listof attribute) (listof tr))]
}


@defstruct[(tbody html-full) ()]{
A @scheme[tbody] is
@scheme[(make-tbody (listof attribute) (listof tr))]
}


@defstruct[(tt html-full) ()]{
A @scheme[tt] is
@scheme[(make-tt (listof attribute) (listof G5))]
}


@defstruct[(i html-full) ()]{
An @scheme[i] is
@scheme[(make-i (listof attribute) (listof G5))]
}


@defstruct[(b html-full) ()]{
A @scheme[b] is
@scheme[(make-b (listof attribute) (listof G5))]
}


@defstruct[(u html-full) ()]{
An @scheme[u] is
@scheme[(make-u (listof attribute) (listof G5))]
}


@defstruct[(s html-full) ()]{
A @scheme[s] is
@scheme[(make-s (listof attribute) (listof G5))]
}


@defstruct[(strike html-full) ()]{
A @scheme[strike] is
@scheme[(make-strike (listof attribute) (listof G5))]
}


@defstruct[(big html-full) ()]{
A @scheme[big] is
@scheme[(make-big (listof attribute) (listof G5))]
}


@defstruct[(small html-full) ()]{
A @scheme[small] is
@scheme[(make-small (listof attribute) (listof G5))]
}


@defstruct[(em html-full) ()]{
An @scheme[em] is
@scheme[(make-em (listof attribute) (listof G5))]
}


@defstruct[(strong html-full) ()]{
A @scheme[strong] is
@scheme[(make-strong (listof attribute) (listof G5))]
}


@defstruct[(dfn html-full) ()]{
A @scheme[dfn] is
@scheme[(make-dfn (listof attribute) (listof G5))]
}


@defstruct[(code html-full) ()]{
A @scheme[code] is
@scheme[(make-code (listof attribute) (listof G5))]
}


@defstruct[(samp html-full) ()]{
A @scheme[samp] is
@scheme[(make-samp (listof attribute) (listof G5))]
}


@defstruct[(kbd html-full) ()]{
A @scheme[kbd] is
@scheme[(make-kbd (listof attribute) (listof G5))]
}


@defstruct[(var html-full) ()]{
A @scheme[var] is
@scheme[(make-var (listof attribute) (listof G5))]
}


@defstruct[(cite html-full) ()]{
A @scheme[cite] is
@scheme[(make-cite (listof attribute) (listof G5))]
}


@defstruct[(abbr html-full) ()]{
An @scheme[abbr] is
@scheme[(make-abbr (listof attribute) (listof G5))]
}


@defstruct[(acronym html-full) ()]{
An @scheme[acronym] is
@scheme[(make-acronym (listof attribute) (listof G5))]
}


@defstruct[(sub html-full) ()]{
A @scheme[sub] is
@scheme[(make-sub (listof attribute) (listof G5))]
}


@defstruct[(sup html-full) ()]{
A @scheme[sup] is
@scheme[(make-sup (listof attribute) (listof G5))]
}


@defstruct[(span html-full) ()]{
A @scheme[span] is
@scheme[(make-span (listof attribute) (listof G5))]
}


@defstruct[(bdo html-full) ()]{
A @scheme[bdo] is
@scheme[(make-bdo (listof attribute) (listof G5))]
}


@defstruct[(font html-full) ()]{
A @scheme[font] is
@scheme[(make-font (listof attribute) (listof G5))]
}


@defstruct[(p html-full) ()]{
A @scheme[p] is
@scheme[(make-p (listof attribute) (listof G5))]
}


@defstruct[(h1 html-full) ()]{
A @scheme[h1] is
@scheme[(make-h1 (listof attribute) (listof G5))]
}


@defstruct[(h2 html-full) ()]{
A @scheme[h2] is
@scheme[(make-h2 (listof attribute) (listof G5))]
}


@defstruct[(h3 html-full) ()]{
A @scheme[h3] is
@scheme[(make-h3 (listof attribute) (listof G5))]
}


@defstruct[(h4 html-full) ()]{
A @scheme[h4] is
@scheme[(make-h4 (listof attribute) (listof G5))]
}


@defstruct[(h5 html-full) ()]{
A @scheme[h5] is
@scheme[(make-h5 (listof attribute) (listof G5))]
}


@defstruct[(h6 html-full) ()]{
A @scheme[h6] is
@scheme[(make-h6 (listof attribute) (listof G5))]
}


@defstruct[(q html-full) ()]{
A @scheme[q] is
@scheme[(make-q (listof attribute) (listof G5))]
}


@defstruct[(dt html-full) ()]{
A @scheme[dt] is
@scheme[(make-dt (listof attribute) (listof G5))]
}


@defstruct[(legend html-full) ()]{
A @scheme[legend] is
@scheme[(make-legend (listof attribute) (listof G5))]
}


@defstruct[(caption html-full) ()]{
A @scheme[caption] is
@scheme[(make-caption (listof attribute) (listof G5))]
}


@defstruct[(table html-full) ()]{
A @scheme[table] is
@scheme[(make-table (listof attribute) (listof Contents-of-table))]
}


A @scheme[Contents-of-table] is either
@itemize[
  @item[@scheme[caption]]
  @item[@scheme[col]]
  @item[@scheme[colgroup]]
  @item[@scheme[tbody]]
  @item[@scheme[tfoot]]
  @item[@scheme[thead]]
]

@defstruct[(button html-full) ()]{
A @scheme[button] is
@scheme[(make-button (listof attribute) (listof G4))]
}


@defstruct[(fieldset html-full) ()]{
A @scheme[fieldset] is
@scheme[(make-fieldset (listof attribute) (listof Contents-of-fieldset))]
}


A @scheme[Contents-of-fieldset] is either
@itemize[
  @item[@scheme[legend]]
  @item{G2}
]


@defstruct[(optgroup html-full) ()]{
An @scheme[optgroup] is
@scheme[(make-optgroup (listof attribute) (listof option))]
}


@defstruct[(select html-full) ()]{
A @scheme[select] is
@scheme[(make-select (listof attribute) (listof Contents-of-select))]
}


A @scheme[Contents-of-select] is either
@itemize[
  @item[@scheme[optgroup]]
  @item[@scheme[option]]
]

@defstruct[(label html-full) ()]{
A @scheme[label] is
@scheme[(make-label (listof attribute) (listof G6))]
}


@defstruct[(form html-full) ()]{
A @scheme[form] is
@scheme[(make-form (listof attribute) (listof G3))]
}


@defstruct[(ol html-full) ()]{
An @scheme[ol] is
@scheme[(make-ol (listof attribute) (listof li))]
}


@defstruct[(ul html-full) ()]{
An @scheme[ul] is
@scheme[(make-ul (listof attribute) (listof li))]
}


@defstruct[(dir html-full) ()]{
A @scheme[dir] is
@scheme[(make-dir (listof attribute) (listof li))]
}


@defstruct[(menu html-full) ()]{
A @scheme[menu] is
@scheme[(make-menu (listof attribute) (listof li))]
}


@defstruct[(dl html-full) ()]{
A @scheme[dl] is
@scheme[(make-dl (listof attribute) (listof Contents-of-dl))]
}


A @scheme[Contents-of-dl] is either
@itemize[
  @item[@scheme[dd]]
  @item[@scheme[dt]]
]


@defstruct[(pre html-full) ()]{
A @scheme[pre] is
@scheme[(make-pre (listof attribute) (listof Contents-of-pre))]
}


A @scheme[Contents-of-pre] is either
@itemize[
  @item{G9}
  @item{G11}
]


@defstruct[(object html-full) ()]{
An @scheme[object] is
@scheme[(make-object (listof attribute) (listof Contents-of-object-applet))]
}


@defstruct[(applet html-full) ()]{
An @scheme[applet] is
@scheme[(make-applet (listof attribute) (listof Contents-of-object-applet))]
}


A @scheme[Contents-of-object-applet] is either
@itemize[
  @item[@scheme[param]]
  @item{G2}
]


@defstruct[(map html-full) ()]{
A Map is
@scheme[(make-map (listof attribute) (listof Contents-of-map))]
}


A @scheme[Contents-of-map] is either
@itemize[
  @item[@scheme[area]]
  @item[@scheme[fieldset]]
  @item[@scheme[form]]
  @item[@scheme[isindex]]
  @item{G10}
]


@defstruct[(a html-full) ()]{
An @scheme[a] is
@scheme[(make-a (listof attribute) (listof Contents-of-a))]
}


A @scheme[Contents-of-a] is either
@itemize[
  @item[@scheme[label]]
  @item{G7}
]

@defstruct[(address html-full) ()]{
An @scheme[address] is
@scheme[(make-address (listof attribute) (listof Contents-of-address))]
}


A @scheme[Contents-of-address] is either
@itemize[
  @item[@scheme[p]]
  @item{G5}
]


@defstruct[(body html-full) ()]{
  A @scheme[body] is
  @scheme[(make-body (listof attribute) (listof Contents-of-body))]
}

A @scheme[Contents-of-body] is either
@itemize[
  @item[@scheme[del]]
  @item[@scheme[ins]]
  @item{G2}
]


A @scheme[G12] is either
@itemize[
  @item[@scheme[button]]
  @item[@scheme[iframe]]
  @item[@scheme[input]]
  @item[@scheme[select]]
  @item[@scheme[textarea]]
]


A @scheme[G11] is either
@itemize[
  @item[@scheme[a]]
  @item[@scheme[label]]
  @item[@scheme[G12]]
]

A @scheme[G10] is either
@itemize[
  @item[@scheme[address]]
  @item[@scheme[blockquote]]
  @item[@scheme[center]]
  @item[@scheme[dir]]
  @item[@scheme[div]]
  @item[@scheme[dl]]
  @item[@scheme[h1]]
  @item[@scheme[h2]]
  @item[@scheme[h3]]
  @item[@scheme[h4]]
  @item[@scheme[h5]]
  @item[@scheme[h6]]
  @item[@scheme[hr]]
  @item[@scheme[menu]]
  @item[@scheme[noframes]]
  @item[@scheme[noscript]]
  @item[@scheme[ol]]
  @item[@scheme[p]]
  @item[@scheme[pre]]
  @item[@scheme[table]]
  @item[@scheme[ul]]
]


A @scheme[G9] is either
@itemize[
  @item[@scheme[abbr]]
  @item[@scheme[acronym]]
  @item[@scheme[b]]
  @item[@scheme[bdo]]
  @item[@scheme[br]]
  @item[@scheme[cite]]
  @item[@scheme[code]]
  @item[@scheme[dfn]]
  @item[@scheme[em]]
  @item[@scheme[i]]
  @item[@scheme[kbd]]
  @item[@scheme[map]]
  @item[@scheme[pcdata]]
  @item[@scheme[q]]
  @item[@scheme[s]]
  @item[@scheme[samp]]
  @item[@scheme[script]]
  @item[@scheme[span]]
  @item[@scheme[strike]]
  @item[@scheme[strong]]
  @item[@scheme[tt]]
  @item[@scheme[u]]
  @item[@scheme[var]]
]


A @scheme[G8] is either
@itemize[
  @item[@scheme[applet]]
  @item[@scheme[basefont]]
  @item[@scheme[big]]
  @item[@scheme[font]]
  @item[@scheme[img]]
  @item[@scheme[object]]
  @item[@scheme[small]]
  @item[@scheme[sub]]
  @item[@scheme[sup]]
  @item{G9}
]


A @scheme[G7] is either
@itemize[
  @item{G8}
  @item{G12}
]

A @scheme[G6] is either
@itemize[
  @item[@scheme[a]]
  @item{G7}
]

A @scheme[G5] is either
@itemize[
  @item[@scheme[label]]
  @item{G6}
]

A @scheme[G4] is either
@itemize[
  @item{G8}
  @item{G10}
]

A @scheme[G3] is either
@itemize[
  @item[@scheme[fieldset]]
  @item[@scheme[isindex]]
  @item{G4}
  @item{G11}
]

A @scheme[G2] is either
@itemize[
  @item[@scheme[form]]
  @item{G3}
]

