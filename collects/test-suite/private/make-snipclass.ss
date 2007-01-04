(module make-snipclass mzscheme
  
  (require
   (lib "etc.ss")
   (lib "mred.ss" "mred")
   (lib "class.ss")
   (lib "contract.ss"))
  
  (define read-proc? (class? (is-a?/c editor-stream-in%) . -> . object?))
  
  (provide/contract
   (make-snipclass ((class? string?) (read-proc?) . opt-> . (is-a?/c snip-class%)))
   (send-read-from-file read-proc?))
  
  ;; Creats a snipclass and registers it with the snip class list
  (define make-snipclass
    (opt-lambda (class% classname (read-proc send-read-from-file))
      (let* ([abstract-snip-class%
              (class snip-class%
                #;((is-a?/c editor-stream-in%) . -> . (is-a?/c interactions-box%))
                ;; Produces an interaction box from the given file stream
                (define/override (read f)
                  (read-proc class% f))
                (super-new))]
             [sc (new abstract-snip-class%)])
        (send sc set-classname classname)
        (send sc set-version 2)
        (send (get-the-snip-class-list) add sc)
        sc)))
  
  ;; Returns an object of class after reading its contents from the given stream
  (define (send-read-from-file class% f)
    (let ([object (new class%)])
      (send object read-from-file f)
      object))
  )