(module box-helpers mzscheme
  
  (require (lib "list.ss")
           (lib "etc.ss"))
  
  (provide last)
  
  (define (last alist)
    (with-handlers ([void (lambda (error) false)])
      (first (last-pair alist))))
  )