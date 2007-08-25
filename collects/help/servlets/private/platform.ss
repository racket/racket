(module platform mzscheme
  (provide current-helpdesk-platform)
  
  ; internal browser or external browser?
  ;   (used to produce simpler html for the internal browser)
  (define current-helpdesk-platform
    (make-parameter 
     'internal-browser-simple  ; main page only
     ; 'internal-browser      ; menu + main page
     ; 'external-browser
     )))