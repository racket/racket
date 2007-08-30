(module html mzscheme
  (provide (all-defined))

  (require (lib "servlets/private/search-util.ss" "help")
           (lib "servlet.ss" "web-server")
           (lib "etc.ss")
           (lib "kw.ss")
           (lib "port.ss")
           "../../private/options.ss"
           "util.ss"
           "url.ss")

  ;;;
  ;;; STYLESHEET
  ;;;

  ;; css : -> string
  ;;   fetch stylesheet from disk
  ;;   (convenient during development)
  (define (css)
    (define (port->string port)
      (let ([os (open-output-string)])
        (copy-port port os)
        (get-output-string os)))
    (call-with-input-file (build-path (this-expression-source-directory)
                                      "helpdesk.css")
      port->string))
  
  ;;;
  ;;; HTML FOR THE INTERNAL HELPDESK
  ;;;
  
  (define (make-green-header-text s) 
    (color-highlight `(h2 () ,s)))
  
  (define (br*)
    (if (eq? (helpdesk-platform) 'external-browser)
      '()
      '((br) (br))))

  ;;;
  ;;; GENERATE XML FOR THE ENTIRE PAGE
  ;;;

  ;; html-page : xexpr (list xml) (list xml) -> xexpr
  (define/kw (html-page #:key title (top '()) (bodies '()) body)
    (let ([bodies (if body (append bodies (list body)) bodies)])
      `(html
        (meta ([http-equiv "Content-Type"] [content "text/html;charset=UTF-8"]))
        (meta ([name "generator"] [content "PLT Scheme"]))
        ;; TODO: Ask Eli what exactly to put here in the online version
        ;; (script ([src "http://www.google-analytics.com/urchin.js"]
        ;;          [type "text/javascript"]))
        ;; (script ([type "text/javascript"])
        ;;   "_uacct=\"UA-808258-1\";_udn=\"plt-scheme.org\";urchinTracker();")
        (head
         (title ,title)
         (style ([type "text/css"]) "\n" ,(css))
         ;; TODO: Check the icons work in online version
         (link ([rel "icon"] [href "/help/servlets/plticon.ico"]
                [type "image/ico"]))
         (link ([rel "shortcut icon"] [href "/help/servlets/plticon.ico"])))
        (body ,@top ,@bodies))))

  ;; html-select : string (list string) natural -> xexpr
  (define (html-select name descriptions selected-index)
    `(select ([name ,name])
       ,@(let loop ([i 0] [ds descriptions] [xexprs '()])
           (cond [(null? ds) (reverse! xexprs)]
                 [(= i selected-index)
                  (loop (+ i 1) (cdr ds)
                        (list* (car ds) `(option ((selected "selected")))
                               xexprs))]
                 [else (loop (+ i 1) (cdr ds)
                             (list* (car ds) `(option) xexprs))]))))

  ;;;
  ;;; THE TOP SEARCH BAR
  ;;; (online version online)

  ;; html-top : requrest -> (list xml)
  (define (html-top request)
    (define bindings      (request-bindings request))
    (define search-string (get-binding bindings 'search-string ""))
    (define search-type   (get-binding bindings 'search-type search-type-default))
    (define match-type    (get-binding bindings 'match-type  match-type-default))
    `((div ([style "border: 1px solid black; padding: 3px; background-color: #74ca56;"])
        (table ([width "98%"])
          (tr (td ([align "right"])
                (img ([class "image"]
                      [src "http://www.plt-scheme.org/plt-green.jpg"]
                      [width "133"] [height "128"] [alt "[icon]"])))
              (td ([align "center"])
                (form ([method "GET"] [action ,url-helpdesk-results])
                  (table (tr (td ([align "center"] [class "sansa"])
                                 "Search the Help Desk for documentation on: "))
                         (tr (td (input ([name "search-string"] [type "text"]
                                         [size "70"] [value ,search-string])))
                             (td nbsp nbsp (button "Search")))
                         (tr (td ([align "center"])
                               ,(html-select "search-type"
                                             search-type-descriptions
                                             (search-type->index search-type))
                               nbsp nbsp nbsp nbsp
                               ,(html-select "match-type"
                                             match-type-descriptions
                                             (match-type->index match-type)))))))
              (td nbsp) (td nbsp) (td nbsp)
              (td (table (tr (td ([align "center"])
                               (a ([href ,url-helpdesk-home] [class "sansa"])
                                  "HOME")))
                         (tr (td ([align "center"])
                               (a ([href ,url-helpdesk-manuals] [class "sansa"])
                                  "MANUALS"))))))))
      (p " ")))


  ;;;
  ;;; BINDINGS
  ;;;

  (define (get-binding bindings name default-value)
    (if (exists-binding? name bindings)
      (extract-binding/single name bindings)
      default-value))

  (define (delete-binding id bindings)
    (cond [(null? bindings) '()]
          [(equal? (binding-id (car bindings)) id) (cdr bindings)]
          [else (cons (car bindings) (delete-binding id (cdr bindings)))]))

  (define (delete-bindings ids bindings)
    (if (null? ids)
      bindings
      (delete-bindings (cdr ids) (delete-binding (car ids) bindings))))

  (define (display-binding binding)
    ;; for debugging
    (printf "binding: ~a=~s\n"
            (binding-id binding)
            (binding:form-value binding)))

  ;;;
  ;;; SEARCH DESCRIPTIONS AND SHORT NAMES
  ;;;

  (define (search-type-description i)
    (cadr (list-ref search-types i)))

  (define (match-type-description i)
    (cadr (list-ref match-types i)))

  (define reversed-search-types
    (map reverse search-types))

  (define reversed-match-types
    (map reverse match-types))

  (define (search-type-description->search-type desc)
    (cond [(assoc desc reversed-search-types) => cadr]
          [else search-type-default]))

  (define (match-type-description->match-type desc)
    (cond [(assoc desc reversed-match-types) => cadr]
          [else match-type-default]))

  (define search-type->index
    (let* ([types (map car search-types)]
           [len   (length types)])
      (lambda (t)
        (cond [(member t types) => (lambda (tail) (- len (length tail)))]
              [else -1]))))

  (define match-type->index
    (let* ([types (map car match-types)]
           [len   (length types)])
      (lambda (t)
        (cond [(member t types) => (lambda (tail) (- len (length tail)))]
              [else -1]))))

  (define search-type-descriptions
    (map cadr search-types))

  (define match-type-descriptions
    (map cadr match-types))

  )
