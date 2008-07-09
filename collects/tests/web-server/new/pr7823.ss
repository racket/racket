(module pr7823 mzscheme
  (require web-server/response-structs)
  
  (define x (make-response/full
             200 "OK" (current-seconds) #"text/html" '()
             (list "<html><body>Hello</body></html>")))
  
  (display x) (newline)
  (display (response/full-body x)) (newline)
  (display (response/basic-extras x)) (newline))
