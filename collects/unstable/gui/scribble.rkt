#lang racket/base

(require (prefix-in s: (combine-in scribble/manual scribble/core))
         (prefix-in slideshow: (combine-in slideshow/base slideshow/pict))
         racket/draw
         racket/class ;; only for make-object
         racket/match)

(provide codeblock->pict)

;; Converts a scribble codeblock into a slideshow pict. Most useful with at-exp
;; @codeblock{
;; #lang racket
;;
;;   (define (x z)
;;     (+ z 2))
;; }
(define (codeblock->pict block)
  (define (color-text name what)
    (define (float-part v)
      (inexact->exact (round (* 255 v))))

    (define-syntax-rule (define-color name r g b)
                        (define name (make-object color%
                                                  (float-part r)
                                                  (float-part g)
                                                  (float-part b))))
    ;; \definecolor{PaleBlue}{rgb}{0.90,0.90,1.0}
    ;; \definecolor{LightGray}{rgb}{0.90,0.90,0.90}
    ;; \definecolor{CommentColor}{rgb}{0.76,0.45,0.12}
    ;; \definecolor{ParenColor}{rgb}{0.52,0.24,0.14}
    ;; \definecolor{IdentifierColor}{rgb}{0.15,0.15,0.50}
    ;; \definecolor{ResultColor}{rgb}{0.0,0.0,0.69}
    ;; \definecolor{ValueColor}{rgb}{0.13,0.55,0.13}
    ;; \definecolor{OutputColor}{rgb}{0.59,0.00,0.59}

    (define-color value-color 0.13 0.55 0.13)
    (define-color identifier-color 0.15 0.15 0.50)
    (define-color pale-blue 0.90 0.90 1.0)
    (define-color light-gray 0.90 0.90 0.90)
    (define-color comment-color 0.76 0.45 0.12)
    (define-color paren-color 0.52 0.24 0.14)
    (define-color result-color 0.0 0.0 0.69)
    (define-color output-color 0.59 0.0 0.59)
    (define-color black 0.0 0.0 0.0)

    ;; FIXME
    (define-color blue 0 0 1)

    ;; FIXME
    (define-color red 1 0 0)

    ;; FIXME
    (define-color light-grey 0.8 0.8 0.8)

    (define (get-color name)
      (match name
        ["RktMeta" identifier-color]
        ["RktPn" paren-color]
        ["RktPlain" black]
        ["RktKw" black]
        ["RktCmt" comment-color]
        ["RktPn" paren-color]
        ["RktInBG" paren-color]
        ["RktSym" identifier-color]
        ["RktVal" value-color]
        ["RktValLink" blue]
        ["RktModLink" blue]
        ["RktRes" result-color]
        ["RktOut" output-color]
        ["RktMeta" identifier-color]
        ["RktMod" black]
        ["RktRdr" black]
        ["RktVarCol" identifier-color]

        ;; FIXME
        ;; \RktVarCol{\textsl{#1}}}
        ["RktVar" (get-color "RktVarCol")]

        ["RktErrCol" red]

        ;; FIXME:
        ;; {{\RktErrCol{\textrm{\textit{#1}}}}}
        ["RktErr" (get-color "RktErrCol")]

        ;; FIXME:
        ;; {\RktOpt}[1]{#1}
        ;;
        ["RktOpt" black]

        ;; FIXME: 
        ;;  }[1]{\incolorbox{LightGray}{\RktInBG{#1}}}
        ["RktIn" light-grey]

        ;; FIXME:
        ;;  }[1]{\colorbox{PaleBlue}{\hspace{-0.5ex}\RktInBG{#1}\hspace{-0.5ex}}}
        ["highlighted" pale-blue]
        [else (error 'color-text "unknown type type '~a'" name)]))

    (define out (slideshow:text what '(bold . modern) (slideshow:current-font-size)))
    (slideshow:colorize out (get-color name)))

  (define (append-all combiner)
    (lambda (elements)
      (apply combiner (slideshow:blank) elements)))

  (define hc-append-all (append-all slideshow:hc-append))
  (define vl-append-all (append-all slideshow:vl-append))

  (define (convert-element element)
    (match element
      [(struct s:element (style content))
       (match style
         #;
         ['hspace (tt " ")]
         ['hspace (slideshow:tt (s:content->string content))]
         #;
         ['hspace (blank 20 1)]
         [(struct s:style (name properties))
          (color-text name (s:content->string content))])]))

  (define (convert-block block)
    (match block
      [(struct s:paragraph (style content))
       (hc-append-all 
         (for/list ([element content])
           (match element
             [(? string?) (slideshow:t element)]
             [(struct s:element (style content)) (convert-element element)]
             [else (error 'convert-block "don't know what to do with ~a" element)])))]))

  (define (convert-row row)
    (hc-append-all
      (for/list ([element row])
        (convert-block element))))
    
  ;; (pretty-print block)
  (match block
    [(struct s:nested-flow (style (list blocks ...)))
     (hc-append-all
       (for/list ([block blocks])
         (match block
           [(struct s:table (style (list rows ...)))
            (vl-append-all
              (for/list ([row rows])
                (convert-row row)))])))]))
