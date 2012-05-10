#lang racket/base
(require racket/contract
         xml)

(define-struct html-element (attributes))
(define-struct (html-full html-element) (content))
(define-struct (mzscheme html-full) ())
(define-struct (html html-full) ())
(define-struct (div html-full) ())
(define-struct (center html-full) ())
(define-struct (blockquote html-full) ())
(define-struct (ins html-full) ())
(define-struct (del html-full) ())
(define-struct (dd html-full) ())
(define-struct (li html-full) ())
(define-struct (th html-full) ())
(define-struct (td html-full) ())
(define-struct (iframe html-full) ())
(define-struct (noframes html-full) ())
(define-struct (noscript html-full) ())
(define-struct (style html-full) ())
(define-struct (script html-full) ())
(define-struct (option html-full) ())
(define-struct (textarea html-full) ())
(define-struct (title html-full) ())
(define-struct (head html-full) ())
(define-struct (tr html-full) ())
(define-struct (colgroup html-full) ())
(define-struct (thead html-full) ())
(define-struct (tfoot html-full) ())
(define-struct (tbody html-full) ())
(define-struct (tt html-full) ())
(define-struct (i html-full) ())
(define-struct (b html-full) ())
(define-struct (u html-full) ())
(define-struct (s html-full) ())
(define-struct (strike html-full) ())
(define-struct (big html-full) ())
(define-struct (small html-full) ())
(define-struct (em html-full) ())
(define-struct (strong html-full) ())
(define-struct (dfn html-full) ())
(define-struct (code html-full) ())
(define-struct (samp html-full) ())
(define-struct (kbd html-full) ())
(define-struct (var html-full) ())
(define-struct (cite html-full) ())
(define-struct (abbr html-full) ())
(define-struct (acronym html-full) ())
(define-struct (sub html-full) ())
(define-struct (sup html-full) ())
(define-struct (span html-full) ())
(define-struct (bdo html-full) ())
(define-struct (font html-full) ())
(define-struct (p html-full) ())
(define-struct (h1 html-full) ())
(define-struct (h2 html-full) ())
(define-struct (h3 html-full) ())
(define-struct (h4 html-full) ())
(define-struct (h5 html-full) ())
(define-struct (h6 html-full) ())
(define-struct (q html-full) ())
(define-struct (dt html-full) ())
(define-struct (legend html-full) ())
(define-struct (caption html-full) ())
(define-struct (table html-full) ())
(define-struct (button html-full) ())
(define-struct (fieldset html-full) ())
(define-struct (optgroup html-full) ())
(define-struct (select html-full) ())
(define-struct (label html-full) ())
(define-struct (form html-full) ())
(define-struct (ol html-full) ())
(define-struct (ul html-full) ())
(define-struct (dir html-full) ())
(define-struct (menu html-full) ())
(define-struct (dl html-full) ())
(define-struct (pre html-full) ())
(define-struct (object html-full) ())
(define-struct (applet html-full) ())
(define-struct (-map html-full) ())
(define-struct (a html-full) ())
(define-struct (address html-full) ())
(define-struct (body html-full) ())
(define-struct (basefont html-element) ())
(define-struct (br html-element) ())
(define-struct (area html-element) ())
(define-struct (alink html-element) ())
(define-struct (img html-element) ())
(define-struct (param html-element) ())
(define-struct (hr html-element) ())
(define-struct (input html-element) ())
(define-struct (col html-element) ())
(define-struct (isindex html-element) ())
(define-struct (base html-element) ())
(define-struct (meta html-element) ())

;; Html-content = Html-element | Pc-data | Entity   
(define html-content/c
  (or/c html-element? pcdata? entity?))

(provide/contract
 [html-content/c contract?]
 [struct html-element ([attributes (listof attribute?)])]
 [struct (html-full html-element)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
  [struct (mzscheme html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (html html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (div html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (center html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (blockquote html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (ins html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (del html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (dd html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (li html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (th html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (td html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (iframe html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (noframes html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (noscript html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (style html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (script html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (option html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (textarea html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (title html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (head html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (tr html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (colgroup html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (thead html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (tfoot html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (tbody html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (tt html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (i html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (b html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (u html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (s html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (strike html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (big html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (small html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (em html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (strong html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (dfn html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (code html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (samp html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (kbd html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (var html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (cite html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (abbr html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (acronym html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (sub html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (sup html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (span html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (bdo html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (font html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (p html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (h1 html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (h2 html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (h3 html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (h4 html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (h5 html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (h6 html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (q html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (dt html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (legend html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (caption html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (table html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (button html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (fieldset html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (optgroup html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (select html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (label html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (form html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (ol html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (ul html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (dir html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (menu html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (dl html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (pre html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (object html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (applet html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (-map html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (a html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (address html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (body html-full)
         ([attributes (listof attribute?)]
          [content (listof html-content/c)])]
 [struct (basefont html-element)
         ([attributes (listof attribute?)])]
 [struct (br html-element)
         ([attributes (listof attribute?)])]
 [struct (area html-element)
         ([attributes (listof attribute?)])]
 [struct (alink html-element)
         ([attributes (listof attribute?)])]
 [struct (img html-element)
         ([attributes (listof attribute?)])]
 [struct (param html-element)
         ([attributes (listof attribute?)])]
 [struct (hr html-element)
         ([attributes (listof attribute?)])]
 [struct (input html-element)
         ([attributes (listof attribute?)])]
 [struct (col html-element)
         ([attributes (listof attribute?)])]
 [struct (isindex html-element)
         ([attributes (listof attribute?)])]
 [struct (base html-element)
         ([attributes (listof attribute?)])]
 [struct (meta html-element)
         ([attributes (listof attribute?)])])
