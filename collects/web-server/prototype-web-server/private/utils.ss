(module utils mzscheme
  (require (lib "contract.ss")
           (lib "url.ss" "net")
           (lib "list.ss")
           (lib "serialize.ss"))
  
  (provide/contract
   [read/string (string? . -> . serializable?)]
   [write/string (serializable? . -> . string?)]
   [make-session-url (url? (listof string?) . -> . url?)])
    
  (define (read/string str)
    (read (open-input-string str)))
  (define (write/string v)
    (define str (open-output-string))
    (write v str)
    (get-output-string str))
  
  ;; make-session-url: url (listof string) -> url
  ;; produce a new url for this session:
  ;;   Minimal path to the servlet.
  ;;   No query.
  ;;   No fragment.
  (define (make-session-url uri new-path)
    (make-url
     (url-scheme uri)
     (url-user uri)
     (url-host uri)
     (url-port uri)
     #t
     (map (lambda (p) (make-path/param p empty))
          new-path)
     '()
     #f
     )))