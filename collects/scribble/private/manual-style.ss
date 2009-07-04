#lang scheme/base
(require "../decode.ss"
         "../struct.ss"
         "../basic.ss"
         "manual-utils.ss"
         scheme/list
         scheme/string)

(provide PLaneT etc
         litchar verbatim
         image image/plain onscreen menuitem defterm emph
         schemefont schemevalfont schemeresultfont schemeidfont schemevarfont
         schemeparenfont schemekeywordfont schememetafont schememodfont
         schemeerror
         filepath exec envvar Flag DFlag PFlag DPFlag
         indexed-file indexed-envvar
         link procedure
         idefterm
         t inset-flow
         pidefterm
         hash-lang
         centerline
         commandline
         elemtag elemref
         secref seclink other-manual
         margin-note
         void-const undefined-const
         math)

(define PLaneT (make-element "planetName" '("PLaneT")))

(define etc "etc.") ; so we can fix the latex space, one day

(define (litchar . strs)
  (unless (andmap string? strs)
    (raise-type-error 'litchar "strings" strs))
  (let ([s (string-append* (map (lambda (s) (regexp-replace* "\n" s " "))
                                strs))])
    (if (regexp-match? #rx"^ *$" s)
      (make-element "schemeinputbg" (list (hspace (string-length s))))
      (let ([^spaces (car (regexp-match-positions #rx"^ *" s))]
            [$spaces (car (regexp-match-positions #rx" *$" s))])
        (make-element
         "schemeinputbg"
         (list (hspace (cdr ^spaces))
               (make-element "schemeinput"
                             (list (substring s (cdr ^spaces) (car $spaces))))
               (hspace (- (cdr $spaces) (car $spaces)))))))))

(define (verbatim #:indent [i 0] s . more)
  (define indent
    (if (zero? i)
      values
      (let ([hs (hspace i)]) (lambda (x) (cons hs x)))))
  (define strs (regexp-split #rx"\n" (string-append* s more)))
  (define (str->elts str)
    (let ([spaces (regexp-match-positions #rx"(?:^| ) +" str)])
      (if spaces
        (list* (substring str 0 (caar spaces))
               (hspace (- (cdar spaces) (caar spaces)))
               (str->elts (substring str (cdar spaces))))
        (list (make-element 'tt (list str))))))
  (define (make-line str)
    (let* ([line (indent (str->elts str))]
           [line (list (make-element 'tt line))])
      (list (make-flow (list (make-omitable-paragraph line))))))
  (make-table #f (map make-line strs)))

;; String String *-> Element
;; an in-lined image, relative to the current directory
(define (image #:scale [scale 1.0] filename-relative-to-source . alt)
  (make-element (make-image-file filename-relative-to-source scale)
                (decode-content alt)))

(define (image/plain filename-relative-to-source . alt)
  (make-element (make-image-file filename-relative-to-source 1.0)
                (decode-content alt)))

(define (onscreen . str)
  (make-element 'sf (decode-content str)))
(define (menuitem menu item)
  (make-element 'sf (list menu "|" item)))
(define (emph . str)
  (make-element 'italic (decode-content str)))
(define (defterm . str)
  (make-element 'italic (decode-content str)))
(define (idefterm . str)
  (let ([c (decode-content str)])
    (make-element 'italic c)))
(define (schemefont . str)
  (apply tt str))
(define (schemevalfont . str)
  (make-element "schemevalue" (decode-content str)))
(define (schemeresultfont . str)
  (make-element "schemeresult" (decode-content str)))
(define (schemeidfont . str)
  (make-element "schemesymbol" (decode-content str)))
(define (schemevarfont . str)
  (make-element "schemevariable" (decode-content str)))
(define (schemeparenfont . str)
  (make-element "schemeparen" (decode-content str)))
(define (schememetafont . str)
  (make-element "schememeta" (decode-content str)))
(define (schememodfont . str)
  (make-element "schememod" (decode-content str)))
(define (schemekeywordfont . str)
  (make-element "schemekeyword" (decode-content str)))
(define (filepath . str)
  (make-element 'tt (append (list "\"") (decode-content str) (list "\""))))
(define (indexed-file . str)
  (let* ([f (apply filepath str)]
         [s (element->string f)])
    (index* (list (clean-up-index-string
                   (substring s 1 (sub1 (string-length s)))))
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
  (make-element "schemeresult" `("#<procedure:" ,@(decode-content str) ">")))

(define (link url
              #:underline? [underline? #t]
              #:style [style (if underline? #f "plainlink")]
              . str)
  (make-element (make-target-url url style)
                (decode-content str)))

(define (schemeerror . str)
  (make-element "schemeerror" (decode-content str)))

(define (t . str)
  (decode-paragraph str))

(define (inset-flow . c)
  (make-blockquote "insetpara" (flow-paragraphs (decode-flow c))))



(define (centerline . s)
  (make-blockquote "SCentered" (flow-paragraphs (decode-flow s))))

(define (commandline . s)
  (make-paragraph (cons (hspace 2) (map (lambda (s)
                                          (if (string? s)
                                            (make-element 'tt (list s))
                                            s))
                                        s))))

(define (elemtag t . body)
  (make-target-element #f (decode-content body) `(elem ,t)))
(define (elemref #:underline? [u? #t] t . body)
  (make-link-element (if u? #f "plainlink") (decode-content body) `(elem ,t)))

(define (secref s #:underline? [u? #t] #:doc [doc #f] #:tag-prefixes [prefix #f])
  (make-link-element (if u? #f "plainlink") null `(part ,(doc-prefix doc prefix s))))
(define (seclink tag #:underline? [u? #t] #:doc [doc #f] #:tag-prefixes [prefix #f] . s)
  (make-link-element (if u? #f "plainlink") (decode-content s)
                     `(part ,(doc-prefix doc prefix tag))))

(define (other-manual #:underline? [u? #t] doc)
  (secref #:doc doc #:underline? u? "top"))

(define (pidefterm . s)
  (let ([c (apply defterm s)])
    (index (string-append (content->string (element-content c)) "s")
           c)))

(define (hash-lang)
  (make-link-element
   "schememodlink"
   (list (schememodfont "#lang"))
   `(part ,(doc-prefix '(lib "scribblings/guide/guide.scrbl") "hash-lang"))))

(define (margin-note . c)
  (make-blockquote
   "\\refpara"
   (list
    (make-blockquote
     "refcolumn"
     (list
      (make-blockquote
       "refcontent"
       (flow-paragraphs (decode-flow c))))))))

(define void-const
  (schemeresultfont "#<void>"))
(define undefined-const
  (schemeresultfont "#<undefined>"))

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
