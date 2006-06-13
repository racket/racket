(module dispatch-sequencer mzscheme
  (require (lib "list.ss"))
  (require "dispatch.ss")
  (provide interface-version
           make)
  
  (define interface-version 'v1)
  (define ((make . dispatchers) conn req)
    (let loop ([dispatchers dispatchers])
      (let ([c (first dispatchers)])
        (with-handlers ([exn:dispatcher?
                         (lambda (e) (loop (rest dispatchers)))])
          (c conn req))))))