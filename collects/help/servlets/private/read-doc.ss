(module read-doc mzscheme
  (require (lib "etc.ss")
           (lib "getinfo.ss" "setup")
           "util.ss"
           "read-lines.ss"
           "headelts.ss")
  (provide read-doc)

  ;; extracts help desk message
  (define (get-message coll)
    (with-handlers ([void (lambda _ #f)]) ; collection may not exist
      ((get-info (list coll)) 'help-desk-message (lambda () #f))))

  (define offset-format "file=~a&caption=~a&offset=~a#temp")

  (define (build-page file caption coll offset)
    (let ([msg (get-message coll)])
      `(html (head (title "PLT Help Desk") ,hd-css)
             ,(if msg
                `(body ,(format-collection-message msg)
                       (hr)
                       ,(read-lines file caption offset))
                `(body ,(read-lines file caption offset))))))

  (define read-doc
    (opt-lambda (file caption coll [offset #f])
      (build-page file caption coll offset))))
