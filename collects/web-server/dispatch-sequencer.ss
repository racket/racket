(module dispatch-sequencer mzscheme
  (require "dispatch.ss"
           (lib "list.ss"))
  (provide interface-version
           gen-dispatcher)
  
  (define interface-version 'v1)
  (define ((gen-dispatcher . dispatchers) conn req)
    (let loop ([dispatchers dispatchers])
      (let ([c (first dispatchers)])
        (with-handlers ([exn:dispatcher?
                         (lambda (e) (loop (rest dispatchers)))])
          (c conn req))))))