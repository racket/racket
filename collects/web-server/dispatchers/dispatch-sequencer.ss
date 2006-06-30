(module dispatch-sequencer mzscheme
  (require (lib "list.ss")
           (lib "contract.ss"))
  (require "dispatch.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide ; XXX contract kw
   make)
  
  (define interface-version 'v1)
  (define ((make . dispatchers) conn req)
    (let loop ([dispatchers dispatchers])
      (let ([c (first dispatchers)])
        (with-handlers ([exn:dispatcher?
                         (lambda (e) (loop (rest dispatchers)))])
          (c conn req))))))