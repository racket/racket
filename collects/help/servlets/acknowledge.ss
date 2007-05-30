(module acknowledge mzscheme
  (require (lib "acks.ss" "drscheme")
           (lib "servlet.ss" "web-server")
           "private/util.ss")
  (provide interface-version timeout start)
  (define interface-version 'v1)
  (define timeout +inf.0)
  (define (start initial-request)
    (with-errors-to-browser 
     send/finish
     (lambda ()
       `(html (head (title "Acknowledgements"))
              (body (a ([name "acknowledgements"] [value "acknowledgements"]))
                    (h1  "Acknowledgements")
                    (p)
                    ,(get-general-acks)
                    (p)
                    ,(get-translating-acks)))))))