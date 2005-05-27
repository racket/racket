(module internal-structs mzscheme
  (provide current-servlet-stuff)
  (require "util.ss")
  
  ; more here - rename
  (define current-servlet-stuff (make-parameter #f (lambda (x) x)))
  
  ; servlet-instance = (make-servlet-instance Nat Channel (Hashtable Symbol -> cont))
  (provide-define-struct servlet-instance (k-counter channel cont-table))

  ; config = (make-config host-table script-table instance-table access-table)
  (provide-define-struct config (hosts scripts instances access))
  
  ; more here - rename
  ; more here - check if method is needed.  (I think it's for purge-table.)
  ; note: the url is the initial starting url without instance or continuation specific stuff at the end.
  ; servlet-stuff = (make-servlet-stuff url sym instance-table (response -> void) (instance -> doesn't) method)
  (provide-define-struct servlet-stuff (url invoke-id instances output-page resume method))
  
  ;; a connection is a structure
  ;; (make-connection custodian input-port output-port timer boolean)
  (provide-define-struct connection (i-port o-port close?))
  
  )