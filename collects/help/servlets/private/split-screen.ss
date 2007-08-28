(module split-screen mzscheme
  (require (lib "match.ss")
           (only (lib "misc.ss" "swindle")  mappend)
           "html.ss"
           "url.ss"
           "../../private/options.ss")

  ;; These items are common to all split screens

  (define left-header-items
    `((VERBATIM (big (big (big (b (a ([href ,url-helpdesk-home])
                                     "PLT Scheme Help Desk"))))))))

  (define left-footer-items
    (case (helpdesk-platform)
      [(internal-browser internal-browser-simple)
       '(nbsp)]
      [else
       '(nbsp
         (VERBATIM (small (small (a ([href "http://www.plt-scheme.org/map.html"])
                                    "Site Map"))))
         (VERBATIM (hr ([noshade "1"] [size "2"] [color "#3a652b"])))
         (VERBATIM (nobr
                    (small ([class "sansa"])
                      (a ([href "http://www.plt-scheme.org/"]) "PLT")
                      nbsp "|" nbsp
                      (a ([href "http://www.plt-scheme.org/software/drscheme/"])
                         "DrScheme")
                      nbsp "|" nbsp
                      (a ([href "http://www.teach-scheme.org/"]) "TeachScheme!")
                      nbsp "|" nbsp
                      (a ([href "http://www.htdp.org/"]) "HtDP") nbsp
                      "|" nbsp
                      (a ([href "http://planet.plt-scheme.org/"]) "PLaneT")
                      nbsp)))
         ;; Google Search for PLT Documentation
         #;
         (VERBATIM
          (div ([align "center"])
            (div ([style "display: inline; margin: 0; white-space: nowrap;"])
              ;; The Google "Search Documentation" field and button
                 (form ([id "searchbox_010927490648632664335:4yu6uuqr9ia"]
                        [action "http://www.plt-scheme.org/search/"]
                        [style "display: inline; margin: 0;"])
                   (input ([type "hidden"] [name "cx"]
                           [value "010927490648632664335:4yu6uuqr9ia"]))
                   (input ([type "text"] [name "q"] [style "font-size: 75%;"]
                           [size "16"]))
                   (input ([type "hidden"] [name "hq"] [value "more:plt"]))
                   (input ([type "hidden"] [name "cxq"] [value "more:docs"]))
                   (input ([type "submit"] [name "sa"] [style "font-size: 75%;"]
                           [value "Search Documentation"]))
                   (input ([type "hidden"] [name "cof"] [value "FORID:9"]))))
            nbsp))
         )]))


  ;; the internal browser makes a "split" screen by having the left
  ;; items at the top, and the right items at the bottom
  (define (make-split-page/internal-browser title top-items left-items right-header right-items)
    (html-page
     #:title title
     #:body `(div ,(html-left-items (append ;; left-header-items
                                            left-items
                                            left-footer-items))
                  (hr)
                  ,@(html-right-items right-items))))

  ;; simple version that only shows the contents and no menu
  (define (make-simple-page/internal-browser
           title top-items left-items right-header right-items)
    (html-page
     #:title title
     #:body (if (equal? left-items "home")
              `(div (h1 "PLT Help Desk") ,(html-left-items right-items))
              `(div (h1 ,right-header)
                 ,@(html-right-items right-items)))))

  ;; an external is capable of displaying a proper split screen
  (define (make-split-page title top-items left-items right-header right-items)
    (html-page
     #:title title
     #:bodies `(,@top-items ,(make-split-screen left-items
                                                right-header
                                                right-items))))


  (define (make-split-screen left-items right-header right-items)
    `(table ([height "80%"] [width "100%"] [align "center"] [border "0"]
             [cellspacing "0"] [cellpadding "30"])
       (tr ([valign "top"])
         (td ([height "80%"] [width "50%"] [align "center"] [valign "top"]
              [bgcolor "#74ca56"])
           ;; LEFT TABLE
           (table ([align "center"] [class "sansa"] [border "0"]
                   [cellpadding "0"] [cellspacing "4"])
             ;; (tr (td ([align "center"])
             ;;       (img ([src "http://www.plt-scheme.org/plt-green.jpg"]
             ;;             [width "133"] [height "128"] [alt "[icon]"]))))
             ,(html-left-items
               (append left-header-items left-items left-footer-items))))
         (td ([height "100%"] [width "50%"] [align "left"] [valign "top"])
           ;; RIGHT TABLE
           (table ([width "80%"] [class "sansa"] [align "center"]
                   [border "0"] [cellpadding "0"] [cellspacing "0"])
             (tr (td (h1 ,right-header)))
             ;; (tr (td (small (small nbsp))))
             (tr (td (table ([border "0"] [width "100%"]
                             [cellpadding "3"] [cellspacing "0"])
                       ,@(html-right-items right-items)))))))))

  ;;;
  ;;; ITEM FORMATTING
  ;;; (ad hoc markup inherited)

  (define (html-left-items items)
    `(tr (td (table ,@(mappend html-left-item items)))))

  (define (html-left-item item)
    (match item
      ['UP               (list '(font ((size "-2")) nbsp))]
      ['--               (list '(tr ((height "4")) (td ((colspan "2")))))]
      [('VERBATIM sxml)  (list `(tr (td ((align "center")) ,sxml)))]
      [(header)          (list `(tr (td #;((colspan "2")) ,header)))]
      [(header body ...) (list `(tr (td #;((colspan "2")) ,header))
                               `(tr (td ,@body)))]
      [other             (list other)]))
  
  (define (html-right-items items)
    (mappend html-right-item items))

  (define (html-right-item item)
    (match item
      ['--              (list '(tr ((height "4")) (td ((colspan "2")))))]
      [('VERBATIM item) item]
      [(body ...)       (list body)]))


  (provide make-split-screen
           make-split-page
           make-split-page/internal-browser
           make-simple-page/internal-browser)

)
