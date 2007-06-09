(module web-server mzscheme
  (require (lib "manual.ss" "scribble")
           (lib "eval.ss" "scribble"))
  ; XXX Need signature and unit forms and class
  ; XXX @module form that checks if all exports are documented
  ; XXX formatlambda, caselambda, defproc-case
  ; XXX Copyright
  ; XXX email and href links
  ; XXX last updated
  ; XXX multiple tags
  ; XXX @require
  ; XXX editting mode drscheme or emacs
  
  ; XXX @scheme in code:comment
  
  (define web-server "Web Server")
  (define webserver "Web Server")
  
  ; XXX Make look good
  (define (author x) 
    (elem (hspace 4)
          (bold x)))

  (provide (all-from (lib "manual.ss" "scribble"))
           (all-from (lib "eval.ss" "scribble"))
           web-server
           author))