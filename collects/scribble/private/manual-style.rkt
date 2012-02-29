#lang racket/base
(require "../decode.rkt"
         "../struct.rkt"
         "../base.rkt"
         (only-in "../basic.rkt" aux-elem itemize)
         "../scheme.rkt"
         (only-in "../core.rkt" make-style plain
                  make-nested-flow box-mode box-mode*
                  [element? core:element?])
         "manual-utils.rkt"
         "on-demand.rkt"
         "manual-sprop.rkt"
         racket/list
         racket/contract/base
         racket/string)

(provide (rename-out [hyperlink link])
         (rename-out [other-doc other-manual])
         (rename-out [centered centerline])
         image
         (rename-out [image image/plain])
         itemize
         aux-elem
         code-inset)
(provide/contract [filebox (((or/c core:element? string?)) () #:rest (listof pre-flow?) . ->* . block?)])

(define styling-f/c
  (() () #:rest (listof pre-content?) . ->* . element?))
(define-syntax-rule (provide-styling id ...)
  (provide/contract [id styling-f/c] ...))
(provide-styling racketmodfont racketoutput
                 racketerror racketfont racketvalfont racketresultfont racketidfont racketvarfont
                 racketcommentfont racketparenfont racketkeywordfont racketmetafont
                 onscreen defterm filepath exec envvar Flag DFlag PFlag DPFlag math
                 procedure
                 indexed-file indexed-envvar idefterm pidefterm)
(define-syntax-rule (provide-scheme-styling [rid sid] ...)
  (provide/contract [rename rid sid styling-f/c] ...))
(provide-scheme-styling [racketmodfont schememodfont]
                        [racketoutput schemeoutput]
                        [racketerror schemeerror]
                        [racketfont schemefont]
                        [racketvalfont schemevalfont]
                        [racketresultfont schemeresultfont]
                        [racketidfont schemeidfont]
                        [racketvarfont schemevarfont]
                        [racketparenfont schemeparenfont]
                        [racketkeywordfont schemekeywordfont]
                        [racketmetafont schememetafont])

(provide void-const
         undefined-const)
(provide/contract
 [PLaneT element?]
 [hash-lang (-> element?)]
 [etc element?]
 [inset-flow (() () #:rest (listof pre-content?) . ->* . any/c)] ; XXX no docs and bad return contract
 [litchar (() () #:rest (listof string?) . ->* . element?)]
 [t (() () #:rest (listof pre-content?) . ->* . paragraph?)]
 [commandline (() () #:rest (listof pre-content?) . ->* . paragraph?)]
 [menuitem (string? string? . -> . element?)])

(define PLaneT (make-element "planetName" '("PLaneT")))

(define etc (make-element #f (list "etc" ._)))

(define (litchar . strs)
  (let ([s (string-append* (map (lambda (s) (regexp-replace* "\n" s " "))
                                strs))])
    (if (regexp-match? #rx"^ *$" s)
      (make-element input-background-color (list (hspace (string-length s))))
      (let ([^spaces (car (regexp-match-positions #rx"^ *" s))]
            [$spaces (car (regexp-match-positions #rx" *$" s))])
        (make-element
         input-background-color
         (list (hspace (cdr ^spaces))
               (make-element input-color
                             (list (substring s (cdr ^spaces) (car $spaces))))
               (hspace (- (cdr $spaces) (car $spaces)))))))))

(define (onscreen . str)
  (make-element 'sf (decode-content str)))
(define (menuitem menu item)
  (make-element 'sf (list menu "|" item)))
(define (defterm . str)
  (make-element 'italic (decode-content str)))
(define (idefterm . str)
  (let ([c (decode-content str)])
    (make-element 'italic c)))
(define (racketfont . str)
  (apply tt str))
(define (racketvalfont . str)
  (make-element value-color (decode-content str)))
(define (racketresultfont . str)
  (make-element result-color (decode-content str)))
(define (racketidfont . str)
  (make-element symbol-color (decode-content str)))
(define (racketvarfont . str)
  (make-element variable-color (decode-content str)))
(define (racketparenfont . str)
  (make-element paren-color (decode-content str)))
(define (racketmetafont . str)
  (make-element meta-color (decode-content str)))
(define (racketcommentfont . str)
  (make-element comment-color (decode-content str)))
(define (racketmodfont . str)
  (make-element module-color (decode-content str)))
(define (racketkeywordfont . str)
  (make-element keyword-color (decode-content str)))
(define (filepath . str)
  (make-element 'tt (append (list "\"") (decode-content str) (list "\""))))
(define (indexed-file . str)
  (let* ([f (apply filepath str)]
         [s (element->string f)])
    (index* (list (datum-intern-literal
                   (clean-up-index-string
                    (substring s 1 (sub1 (string-length s))))))
            (list f)
            f)))
(define (exec . str)
  (if (andmap string? str)
    (make-element 'tt str)
    (make-element #f (map (lambda (s)
                            (if (string? s)
                              (make-element 'tt (list s))
                              s))
                          str))))
(define (Flag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "-" (decode-content str))))))
(define (DFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "--" (decode-content str))))))
(define (PFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "+" (decode-content str))))))
(define (DPFlag . str)
  (make-element 'no-break
                (list (make-element 'tt (cons "++" (decode-content str))))))
(define (envvar . str)
  (make-element 'tt (decode-content str)))
(define (indexed-envvar . str)
  (let* ([f (apply envvar str)]
         [s (element->string f)])
    (index* (list s) (list f) f)))
(define (procedure . str)
  (make-element result-color `("#<procedure:" ,@(decode-content str) ">")))

(define (racketoutput . str)
  (make-element output-color (decode-content str)))
(define (racketerror . str)
  (make-element error-color (decode-content str)))

(define (t . str)
  (decode-paragraph str))

(define (inset-flow . c)
  (make-blockquote "insetpara" (flow-paragraphs (decode-flow c))))

(define code-inset-style 
  (make-style 'code-inset null))
(define (code-inset b)
  (make-blockquote code-inset-style (list b)))

(define (commandline . s)
  (make-paragraph (cons (hspace 2) (map (lambda (s)
                                          (if (string? s)
                                            (make-element 'tt (list s))
                                            s))
                                        s))))

(define (pidefterm . s)
  (let ([c (apply defterm s)])
    (index (string-append (content->string (element-content c)) "s")
           c)))

(define (hash-lang)
  (make-link-element
   module-link-color
   (list (racketmodfont "#lang"))
   `(part ,(doc-prefix '(lib "scribblings/guide/guide.scrbl") "hash-lang"))))

(define (make-v+u-link p)
  (make-link-element
   module-link-color
   p
   `(part ,(doc-prefix '(lib "scribblings/guide/guide.scrbl") "void+undefined"))))

(define-on-demand void-const
  (make-v+u-link
   (nonbreaking (racketresultfont "#<void>"))))
(define-on-demand undefined-const
  (make-v+u-link
   (nonbreaking (racketresultfont "#<undefined>"))))

(define (link url 
              #:underline? [underline? #t]
              #:style [style (if underline? #f "plainlink")]
              . str)
  (apply hyperlink url #:style (if style (make-style style null) plain) str))

(define (math . s)
  (let ([c (decode-content s)])
    (make-element
     #f
     (append-map
      (lambda (i)
        (let loop ([i i])
          (cond
            [(string? i)
             (cond
               [(regexp-match #px"^(.*)_([a-zA-Z0-9]+)(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (make-element 'subscript
                                                 (loop (caddr m))))
                             (loop (cadddr m))))]
               [(regexp-match #px"^(.*)\\^([a-zA-Z0-9]+)(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (make-element 'superscript
                                                 (loop (caddr m))))
                             (loop (cadddr m))))]
               [(regexp-match #px"^(.*)([()0-9{}\\[\\]\u03C0])(.*)$" i)
                => (lambda (m)
                     (append (loop (cadr m))
                             (list (caddr m))
                             (loop (cadddr m))))]
               [else
                (list (make-element 'italic (list i)))])]
            [(eq? i 'rsquo) (list 'prime)]
            [else (list i)])))
      c))))

(define (filebox filename . inside)
  (make-nested-flow 
   (make-style "Rfilebox" (list* 'multicommand
                                 (box-mode "RfileboxBoxT" "RfileboxBoxC" "RfileboxBoxB") 
                                 scheme-properties))
   (list
    (make-styled-paragraph 
     (list (make-element
            (make-style "Rfilename" scheme-properties)
            (if (string? filename)
                (filepath filename)
                filename)))
     (make-style "Rfiletitle" (cons (box-mode* "RfiletitleBox") scheme-properties)))
    (make-nested-flow 
     (make-style "Rfilecontent" (cons (box-mode* "RfilecontentBox") scheme-properties))
     (decode-flow inside)))))


