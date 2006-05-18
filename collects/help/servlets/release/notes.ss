(module notes mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "list.ss")
           (lib "uri-codec.ss" "net")
	   (lib "dirs.ss" "setup")
           "../private/util.ss"
           "../private/headelts.ss")
  
  (define (make-entry s)
    (let* ([label (car s)]
           [dir (cadr s)]
           [filename (caddr s)]
           [file (build-path (find-doc-dir) "release-notes" dir filename)])
      (if (file-exists? file)
          `(LI (A ((HREF ,(format "/servlets/doc-anchor.ss?file=~a&name=~a&caption=~a"
                                  (uri-encode (path->string file))
                                  filename
                                  label)))
                  ,label))
          #f)))
  
  (require (lib "servlet.ss" "web-server"))
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  
  (define (start initial-request)
    
    (report-errors-to-browser send/finish)
    
    `(HTML
      (HEAD ,hd-css
            ,@hd-links
            (TITLE "PLT release notes"))
      (H1 "Release Notes for PLT Scheme version " ,(version))
      (A ((NAME "relnotes") (VALUE "Release notes")))
      "Detailed release notes:"
      (UL
       ,@(filter 
          (lambda (x) x) ; delete #f entries
          (map make-entry
               '(("DrScheme release notes"
                  "drscheme" "HISTORY")
                 ("Teachpack release notes"
                  "teachpack" "HISTORY")
                 ("MzScheme version 300 notes"
                  "mzscheme" "MzScheme_300.txt")
                 ("MzScheme release notes"
                  "mzscheme" "HISTORY")
                 ("MrEd release notes"
                  "mred" "HISTORY")
                 ("Stepper release notes"
                  "stepper" "HISTORY")
                 ("MrFlow release notes"
                  "mrflow" "HISTORY"))))))))
