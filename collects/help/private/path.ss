(module path mzscheme
  (require (lib "contract.ss"))
  (define (servlet-path? path)
    (if (regexp-match #rx#"^/servlets/" 
                      (path->bytes path))
        #t
        #f))
  (provide/contract
   [servlet-path? (path? . -> . boolean?)]))

