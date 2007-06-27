(module web-server mzscheme
  (require (lib "manual.ss" "scribble")
           (lib "eval.ss" "scribble"))
  
  (define web-server "Web Server")
  
  ; XXX Format better
  (define (author x) 
    (elem (hspace 4)
          (bold x)))
  
  ; XXX Format better
  (define (warning . x)
    (apply elem "Warning:" x))
  
  ; XXX Actually display link
  (define (href-link url label)
    (elem label " (" url ")")) 

  (provide (all-from (lib "manual.ss" "scribble"))
           (all-from (lib "eval.ss" "scribble"))
           web-server
           author
           warning
           href-link))