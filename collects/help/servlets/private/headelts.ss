; elements to go in HEAD part of HTML document

(module headelts mzscheme
  (require (lib "list.ss"))
  (provide hd-css hd-links)

  ;; cascading style sheet rules for Help Desk

  ;; (listof (tag attrib+))
  ;;   where attrib is a property name, value pair
  ;;         where a value is a symbol or (listof symbol)
  (define css-rules
    '([body (background-color white) (font-family (Helvetica sans-serif))]))

  (define (css-rules->style)
    (apply
     string-append
     (map (lambda (s) (string-append s "\n"))
          (map (lambda (rule)
                 (let ([tag (car rule)]
                       [attribs (cdr rule)])
                   (string-append
                    (symbol->string tag)
                    " {"
                    (foldr
                     (lambda (s a) (if a (string-append s "; " a) s))
                     #f
                     (map
                      (lambda (attrib)
                        (let ([property (car attrib)]
                              [vals (cadr attrib)])
                          (string-append
                           (symbol->string property) ":"
                           (if (pair? vals)
                             (foldr (lambda (s a)
                                      (if a (string-append s "," a) s))
                                    #f
                                    (map symbol->string vals))
                             (symbol->string vals)))))
                      attribs))
                    "}")))
               css-rules))))
  (define hd-css
    `(style ([type "text/css"]) ,(css-rules->style)))

  ;; LINKs for showing PLT icon
  (define hd-links
    `((link ([rel "icon"] [href "/help/servlets/plticon.ico"]
             [type "image/ico"]))
      (link ([rel "SHORTCUT ICON"] [href "/help/servlets/plticon.ico"])))))
