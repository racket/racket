(module home mzscheme
  (require "private/util.ss"
           "../private/manuals.ss"
	   (lib "servlet.ss" "web-server"))
  
  (provide interface-version timeout start)
  
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    (report-errors-to-browser send/finish)
    `(html
      (head (title "PLT Help Desk"))
      (BODY
       (TABLE ((CELLSPACING "0")
               (CELLPADDING "0"))
              (TR 
               (TD
                (H1 "PLT Help Desk")
                (UL
                 (LI
                  (B
                   (A ((HREF "/servlets/howtouse.ss")) "Help Desk"))
                  ":  How to get help"))
                (UL
                 (LI
                  (B
                   (A ((HREF "/servlets/howtoscheme.ss")) "Software"))
                  ": How to run programs"
                  (BR)
                  'nbsp 'nbsp 'nbsp 'nbsp 'nbsp 'nbsp
                  (FONT ((SIZE "-2"))
                        (A ((HREF "/doc/tour/")) "Tour") ", "
                        (A ((HREF "/servlets/scheme/what.ss")) "Languages") ", "
                        (A ((HREF "/servlets/manuals.ss")) "Manuals") ", "
                        (A ((HREF "/servlets/releaseinfo.ss")) "Release") ", "
                        ,(manual-entry "drscheme"
                                       "frequently asked questions"
                                       "FAQ") ", "
                                              "...")))
                (UL
                 (LI
                  (B
                   (A ((HREF "/servlets/howtoprogram.ss")) "Program Design"))
                  ": Learning to program in Scheme"             
                  (BR) 'nbsp 'nbsp 'nbsp 'nbsp 'nbsp 'nbsp 
                  (FONT ((SIZE "-2"))
                        (A ((HREF "/doc/teachpack/")) "Teachpacks") ", "
                        (A ((HREF "/servlets/research/why.ss")) "Why DrScheme?") ", "
                        "...")))
                
                (UL
                 (LI
                  (B 
                   (A ((HREF "/servlets/resources.ss")) "External Resources")) 
                  ": Additional information"
                  
                  (BR)
                  'nbsp 'nbsp 'nbsp 'nbsp 'nbsp 'nbsp 
                  (FONT ((SIZE "-2"))
                        (A ((HREF "/servlets/resources/teachscheme.ss")) "TeachScheme!") ", "
                        (A ((HREF "/servlets/resources/libext.ss")) "Libraries") ", "
                        (A ((HREF "/servlets/resources/maillist.ss")) "Mailing Lists") ", "
                        "...")))
                (P)
                'nbsp 'nbsp 'nbsp
                (B
                 (A ((HREF "/servlets/acknowledge.ss"))
                    (FONT ((COLOR "limegreen"))
                          "Acknowledgements")))
                'nbsp 'nbsp 'nbsp 'nbsp
                (B
                 (A ((mzscheme
                      "((dynamic-require '(lib |bug-report.ss| |help|) 'help-desk:report-bug))"))
                    (FONT ((COLOR "limegreen"))
                          "Send a bug report")))
                (P)
                (I "Version: " ,(plt-version)))))))))