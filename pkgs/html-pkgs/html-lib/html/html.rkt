#lang racket/base
;; copyright by Paul Graunke June 2000 AD

(require "html-structs.rkt"
         "html-spec.rkt"
         "sgml-reader.rkt"
         racket/contract
         xml)

(provide (all-from-out "html-structs.rkt")
         read-html-comments)
(provide/contract
 [use-html-spec (parameter/c boolean?)]
 [read-html (() (input-port?) . ->* . html?)]
 [read-xhtml (() (input-port?) . ->* . html?)]
 [read-html-as-xml (() (input-port?) . ->* . (listof content/c))])

;; xml-single-content->html : Content (listof Html-content) -> (listof Html-content)
(define (xml-single-content->html x acc)
  (cond
   ((element? x)
    (case (element-name x)
      ((basefont) (cons (make-basefont (element-attributes x)) acc))
      ((br) (cons (make-br (element-attributes x)) acc))
      ((area) (cons (make-area (element-attributes x)) acc))
      ((link) (cons (make-alink (element-attributes x)) acc))
      ((img) (cons (make-img (element-attributes x)) acc))
      ((param) (cons (make-param (element-attributes x)) acc))
      ((hr) (cons (make-hr (element-attributes x)) acc))
      ((input) (cons (make-input (element-attributes x)) acc))
      ((col) (cons (make-col (element-attributes x)) acc))
      ((isindex) (cons (make-isindex (element-attributes x)) acc))
      ((base) (cons (make-base (element-attributes x)) acc))
      ((meta) (cons (make-meta (element-attributes x)) acc))
      ((mzscheme)
       (cons
        (make-mzscheme
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((html)
       (cons
        (make-html
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((div)
       (cons
        (make-div
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((center)
       (cons
        (make-center
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((blockquote)
       (cons
        (make-blockquote
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((ins)
       (cons
        (make-ins
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((del)
       (cons
        (make-del
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((dd)
       (cons
        (make-dd
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((li)
       (cons
        (make-li
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((th)
       (cons
        (make-th
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((td)
       (cons
        (make-td
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((iframe)
       (cons
        (make-iframe
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((noframes)
       (cons
        (make-noframes
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((noscript)
       (cons
        (make-noscript
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((style)
       (cons
        (make-style
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((script)
       (cons
        (make-script
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((option)
       (cons
        (make-option
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((textarea)
       (cons
        (make-textarea
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((title)
       (cons
        (make-title
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((head)
       (cons
        (make-head
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((tr)
       (cons
        (make-tr
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((colgroup)
       (cons
        (make-colgroup
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((thead)
       (cons
        (make-thead
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((tfoot)
       (cons
        (make-tfoot
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((tbody)
       (cons
        (make-tbody
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((tt)
       (cons
        (make-tt
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((i)
       (cons
        (make-i
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((b)
       (cons
        (make-b
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((u)
       (cons
        (make-u
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((s)
       (cons
        (make-s
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((strike)
       (cons
        (make-strike
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((big)
       (cons
        (make-big
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((small)
       (cons
        (make-small
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((em)
       (cons
        (make-em
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((strong)
       (cons
        (make-strong
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((dfn)
       (cons
        (make-dfn
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((code)
       (cons
        (make-code
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((samp)
       (cons
        (make-samp
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((kbd)
       (cons
        (make-kbd
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((var)
       (cons
        (make-var
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((cite)
       (cons
        (make-cite
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((abbr)
       (cons
        (make-abbr
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((acronym)
       (cons
        (make-acronym
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((sub)
       (cons
        (make-sub
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((sup)
       (cons
        (make-sup
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((span)
       (cons
        (make-span
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((bdo)
       (cons
        (make-bdo
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((font)
       (cons
        (make-font
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((p)
       (cons
        (make-p
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((h1)
       (cons
        (make-h1
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((h2)
       (cons
        (make-h2
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((h3)
       (cons
        (make-h3
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((h4)
       (cons
        (make-h4
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((h5)
       (cons
        (make-h5
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((h6)
       (cons
        (make-h6
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((q)
       (cons
        (make-q
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((dt)
       (cons
        (make-dt
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((legend)
       (cons
        (make-legend
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((caption)
       (cons
        (make-caption
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((table)
       (cons
        (make-table
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((button)
       (cons
        (make-button
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((fieldset)
       (cons
        (make-fieldset
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((optgroup)
       (cons
        (make-optgroup
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((select)
       (cons
        (make-select
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((label)
       (cons
        (make-label
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((form)
       (cons
        (make-form
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((ol)
       (cons
        (make-ol
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((ul)
       (cons
        (make-ul
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((dir)
       (cons
        (make-dir
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((menu)
       (cons
        (make-menu
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((dl)
       (cons
        (make-dl
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((pre)
       (cons
        (make-pre
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((object)
       (cons
        (make-object (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((applet)
       (cons
        (make-applet
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((map)
       (cons
        (make--map
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((a)
       (cons
        (make-a
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((address)
       (cons
        (make-address
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      ((body)
       (cons
        (make-body
          (element-attributes x)
          (xml-contents->html (element-content x)))
        acc))
      (else acc)))
   ((or (pcdata? x) (entity? x)) (cons x acc))
   (else acc)))

;; xml->html : Document -> Html
(define (xml->html doc)
  (let ([root (document-element doc)])
    (unless (eq? 'html (element-name root))
      (error 'xml->html "This is not an html document.  Expected 'html, given ~a" (element-name root)))
    (make-html (element-attributes root) (xml-contents->html (element-content root)))))


;; xml-content->html : (listof Content) -> (listof Html-element)
(define (xml-contents->html contents)
  (foldr xml-single-content->html
         null
         contents))

;; read-xhtml : [Input-port] -> Html
(define read-xhtml (compose xml->html read-xml))

;; peel-f : (Html-content -> Bool) (listof Html-content) (listof Html-content) -> (listof Html-content)
(define (peel-f toss? to-toss acc0)
  (foldr (lambda (x acc)
           (if (toss? x)
               (append (html-full-content x) acc)
               (cons x acc)))
         acc0
         to-toss))

;; repackage-html : (listof Html-content) -> Html
(define (repackage-html contents)
  (let* ([html (memf html? contents)]
         [peeled (peel-f html? contents null)]
         [body (memf body? peeled)])
    (make-html (if html
                   (html-element-attributes (car html))
                   null)
               (append (filter head? peeled)
                       (list (make-body (if body
                                            (html-element-attributes (car body))
                                            null)
                                        (filter (compose not head?) (peel-f body? peeled null))))))))

;; clean-up-pcdata : (listof Content) -> (listof Content)
;; Each pcdata inside a tag that isn't supposed to contain pcdata is either
;; a) appended to the end of the previous subelement, if that subelement may contain pcdata
;; b) prepended to the front of the next subelement, if that subelement may contain pcdata
;; c) discarded
;; unknown tags may contain pcdata
;; the top level may contain pcdata
(define clean-up-pcdata
  ;; clean-up-pcdata : (listof Content) -> (listof Content)
  (letrec ([clean-up-pcdata
            (lambda (content)
              (map (lambda (to-fix)
                     (cond
                       [(element? to-fix)
                        (recontent-xml to-fix
                                       (let ([possible (may-contain (element-name to-fix))]
                                             [content (element-content to-fix)])
                                         (if (or (not possible) (memq 'pcdata possible))
                                             (clean-up-pcdata content)
                                             (eliminate-pcdata content))))]
                       [else to-fix]))
                   content))]
           [eliminate-pcdata
            ;: (listof Content) -> (listof Content)
            (lambda (content)
              (let ([non-elements (first-non-elements content)]
                    [more (memf element? content)])
                (if more
                    (let* ([el (car more)]
                           [possible (may-contain (element-name el))])
                      (if (or (not possible) (memq 'pcdata possible))
                          (cons (recontent-xml el (append non-elements (clean-up-pcdata (element-content el)) (eliminate-pcdata (first-non-elements (cdr more)))))
                                (or (memf element? (cdr more)) null))
                          (cons (recontent-xml el (eliminate-pcdata (element-content el)))
                                (eliminate-pcdata (cdr more)))))
                    null)))])
    clean-up-pcdata))

;; first-non-elements : (listof Content) -> (listof Content)
(define (first-non-elements content)
  (cond
    [(null? content) null]
    [else (if (element? (car content))
              null
              (cons (car content) (first-non-elements (cdr content))))]))

;; recontent-xml : Element (listof Content) -> Element
(define (recontent-xml e c)
  (make-element (source-start e) (source-stop e) (element-name e) (element-attributes e) c))

;; implicit-starts : Symbol Symbol -> (U #f Symbol)
(define (implicit-starts parent child)
  (or (and (eq? child 'tr) (eq? parent 'table) 'tbody)
      (and (eq? child 'td) (memq parent '(table tbody tfoot thead)) 'tr)))

;; may-contain : Kid-lister
(define may-contain
  (gen-may-contain html-spec))

(define may-contain-anything
  (gen-may-contain null))

(define use-html-spec (make-parameter #t))

;; read-html-as-xml : [Input-port] -> (listof Content)
(define (read-html-as-xml [port (current-input-port)])
  ((if (use-html-spec) clean-up-pcdata values)
   ((gen-read-sgml (if (use-html-spec)
                       may-contain 
                       may-contain-anything)
                   implicit-starts) port)))

;; read-html : [Input-port] -> Html
(define read-html
  (compose repackage-html xml-contents->html read-html-as-xml))
