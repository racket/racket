(module releaseinfo mzscheme
  (require "private/util.ss")
  (require "private/headelts.ss")
  
  (define (link-stuff url txt)
    `(LI (B (A ((HREF ,url)) ,txt))))
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(HTML 
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "Release Information"))
      (BODY 
       (H1  "Release Information") 
       (P)
       (I "Version: " ,(plt-version))
       (P)
       (UL  
        ,(link-stuff "/servlets/release/license.ss" "License")
        ,(link-stuff "/servlets/release/notes.ss" "Release Notes")
        ,(link-stuff "/servlets/release/bugs.ss" "Known Bugs")
        (li (a ((mzscheme "((dynamic-require '(lib |bug-report.ss| |help|) 'help-desk:report-bug))"))
               (b "Submit a bug report")))
        ,(link-stuff "/servlets/release/patches.ss" "Downloadable Patches"))
       (P)
       "The PLT software is installed on this machine at" (BR)
       (PRE 'nbsp nbsp
            ,(let-values ([(base file dir?) (split-path (collection-path "mzlib"))])
               (path->string base)))))))