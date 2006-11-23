(module home mzscheme
  (require "private/util.ss"
           "../private/get-help-url.ss"
           "../private/manuals.ss"
           (lib "servlet.ss" "web-server"))

  (provide interface-version timeout start)

  (define interface-version 'v1)
  (define timeout +inf.0)

  (define items
    `(("Help Desk" "How to get help" "/servlets/howtouse.ss")
      ("Software" "How to run programs" "/servlets/howtoscheme.ss"
       ,(lambda () `("Tour" ,(get-manual-index "tour")))
       ("Languages" "/servlets/scheme/what.ss")
       ("Manuals" "/servlets/manuals.ss")
       ("Release" "/servlets/releaseinfo.ss")
       ,(lambda ()
          (manual-entry "drscheme" "frequently asked questions" "FAQ")))
      ("Program Design" "Learning to program in Scheme" "/servlets/howtoprogram.ss"
       ("Teachpacks" "/servlets/teachpacks.ss")
       ("Why DrScheme?" "/servlets/research/why.ss"))
      ("External Resources" "Additional information" "/servlets/resources.ss"
       ("TeachScheme!" "/servlets/resources/teachscheme.ss")
       ("Libraries" "/servlets/resources/libext.ss")
       ("Mailing Lists" "/servlets/resources/maillist.ss"))))

  (define (item i)
    (define (item->xexpr item)
      (cond [(and (pair? item) (symbol? (car item))) item]
            [(procedure? item) (item->xexpr (item))]
            [else `(a ([href ,(cadr item)]) ,(car item))]))
    (let ([title (car i)] [subtitle (cadr i)] [url (caddr i)] [subs (cdddr i)])
      `(li (b (a ([href ,url]) ,title)) ": " ,subtitle
         ,@(if (null? subs)
             '()
             `((br) nbsp nbsp nbsp nbsp nbsp nbsp
               (font ([size "-2"])
                 ,@(apply append (map (lambda (s) `(,(item->xexpr s) ", "))
                                      subs))
                 "...")))
         (br) (br))))

  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head (title "PLT Help Desk"))
      (body
       (table ([cellspacing "0"] [cellpadding "0"])
         (tr (td (h1 "PLT Help Desk")
                 (ul ,@(map item items))
                 (p) nbsp nbsp nbsp
                 (b (a ((href "/servlets/acknowledge.ss"))
                      (font ([color "forestgreen"]) "Acknowledgements")))
                 nbsp nbsp nbsp nbsp
                 (b (a ([mzscheme
                         "((dynamic-require '(lib |bug-report.ss| |help|) 'help-desk:report-bug))"])
                      (font ([color "forestgreen"]) "Send a bug report")))
                 (p)
                 (i "Version: " ,(plt-version)))))))))
