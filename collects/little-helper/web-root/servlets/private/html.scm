(module html mzscheme
  (require (lib "kw.ss")
           "request.scm"
           "config.scm")
  
  (provide doctype-HTML-4.01-Transitional
           html-page
           title)
  
  (define doctype-HTML-4.01-Transitional
    #<<doctype
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">
doctype
    )
  
  (define (title name)
    (format "Little Helper | ~a" name))
  
  (define view-url (format "~a/servlets/view.scm/" base-url))
  
  ;; html-top : requrest -> (list xml)
  (define (html-top)
    (define last-query  (get-binding 'q ""))
    (define type-normal (get-type-normal))
    (define sensitivity (get-sensitivity))
    (define contain-all (get-contain-all))
    `((div 
       ([style "border: 1px solid black; padding: 3px; background-color: #74ca56;"])
       (table 
        ([width "98%"])
        (tr (td ([align "right"])
                (a ([href ,view-url] [class "sansa"] )
                   (img ([class "image"]
                         [src "http://www.plt-scheme.org/plt-green.jpg"]
                         [width "133"] [height "128"] [alt "[home]"] [border "0"]))))
            (td ([align "center"])
                (form ([method "GET"] [action "/servlets/search.scm"] [name "queryform"])
                      (table (tr (td ([align "center"] [class "sansa"])
                                     "Search for: "))
                             (tr (td (input ([name "q"] [type "text"]
                                                        [size "70"] [value ,last-query])))
                                 (td nbsp nbsp (button "Search")))
                             (tr (td ([align "center"])
                                     "Type: "
                                     (input ((type "radio") (name "t") (value "normal") ,@(if type-normal '((CHECKED "")) '()))) "normal" nbsp
                                     (input ((type "radio") (name "t") (value "regular")  ,@(if (not type-normal) '((CHECKED "")) '()))) "pattern match"
                                     (br) 
                                     "Case: " 
                                     (input ((type "radio") (name "s") (value "yes") ,@(if sensitivity '((CHECKED "")) '()))) "sensitive" nbsp
                                     (input ((type "radio") (name "s") (value "no")  ,@(if (not sensitivity) '((CHECKED "")) '()))) "insensitive"
                                     (br) 
                                     "Must contain: "
                                     (input ((type "radio") (name "ca") (value "yes") ,@(if contain-all '((CHECKED "")) '()))) "all" nbsp
                                     (input ((type "radio") (name "ca") (value "no")  ,@(if (not contain-all) '((CHECKED "")) '()))) "one or more"
                                     nbsp nbsp " terms")))))
            (td nbsp) (td nbsp) (td nbsp)
            (td (table (tr (td ([align "center"])
                               (a ([href ,view-url] [class "sansa"])
                                  ""))))))))
      (p " ")))
  
  
  (define/kw 
    (html-page #:key 
               (title) 
               (title-atts #f)
               (head-atts #f)
               (body)
               (body-atts #f)
               (style-sheet  external-style-sheet)
               (inline-style-sheet   #f)
               (content-type  "text/html;charset=UTF-8")
               (top? #t))
    (let ([title-atts (if title-atts (list title-atts) '())]
          [head-atts  (if head-atts  (list head-atts)  '())]
          [body-atts  (if body-atts  (list body-atts)  '())])
      `(html (head  (title ,@title-atts ,title)
                    ; external stylesheet
                    (link ((rel "stylesheet")
                           (type "text/css")
                           (href ,style-sheet)))
                    (meta ((http-equiv "Content-Type")
                           (content ,content-type)))
                    ,@head-atts
                    ,@(if inline-style-sheet
                          (list `(style ,inline-style-sheet))
                          (list))
                    )
             (body ,@(list 
                      ; auto-focus in query field [turned out to be annoying]
                      #;(cons '(onLoad "document.queryform.q.focus()") body-atts)
                      body-atts)
                   ,@(html-top)
                   ,body)))))