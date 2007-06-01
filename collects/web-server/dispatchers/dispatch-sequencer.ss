(module dispatch-sequencer mzscheme
  (require (lib "list.ss")
           (lib "contract.ss"))
  (require "dispatch.ss")
  (provide/contract
   [interface-version dispatcher-interface-version?])
  (provide make)
  
  (define interface-version 'v1)
  (define ((make . dispatchers) conn req)
    (let loop ([dispatchers dispatchers])
      (if (empty? dispatchers)
          (next-dispatcher)
          (with-handlers ([exn:dispatcher?
                           (lambda (e) (loop (rest dispatchers)))])
            ((first dispatchers) conn req))))))