(module releaseinfo mzscheme
  (require "private/util.ss"
           "private/headelts.ss"
           (lib "servlet.ss" "web-server"))

  (define (link-stuff url txt)
    `(li (b (a ([href ,url]) ,txt))))

  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
    `(html
      (head ,hd-css ,@hd-links (title "Release Information"))
      (body
       (h1 "Release Information")
       (p)
       (i "Version: " ,(plt-version))
       (p)
       (ul ,(link-stuff "/servlets/release/license.ss" "License")
           ,(link-stuff "/servlets/release/notes.ss" "Release Notes")
           ,(link-stuff "/servlets/release/bugs.ss" "Known Bugs")
           (li (a ([mzscheme "((dynamic-require '(lib |bug-report.ss| |help|) 'help-desk:report-bug))"])
                  (b "Submit a bug report")))
           ,(link-stuff "/servlets/release/patches.ss" "Downloadable Patches"))
       (p)
       "The PLT software is installed on this machine at" (br)
       (pre nbsp nbsp
            ,(let-values ([(base file dir?)
                           (split-path (collection-path "mzlib"))])
               (path->string base)))))))))