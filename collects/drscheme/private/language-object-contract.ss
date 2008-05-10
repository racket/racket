#reader scribble/reader
#lang scheme/base
(require (for-syntax scheme/base)
         scribble/srcdoc
         scheme/class
         scheme/gui/base
         scheme/contract)
(require/doc scheme/base scribble/manual)

(define-syntax (language-object-abstraction stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([ctc
                    #'(object-contract
                       (config-panel (-> (is-a?/c area-container<%>)
                                         (case-> (-> any/c void?)
                                                 (-> any/c))))
                       (create-executable (-> any/c
                                              (or/c (is-a?/c dialog%) (is-a?/c frame%))
                                              path?
                                              void?))
                       (default-settings (-> any/c))
                       (default-settings? (-> any/c boolean?))
                       (order-manuals (-> (listof bytes?)
                                          (values (listof bytes?) boolean?)))
                       (front-end/complete-program (-> input-port?
                                                       any/c
                                                       (-> any/c)))
                       (front-end/interaction (-> input-port?
                                                  any/c
                                                  (-> any/c)))
                       (get-language-name (-> string?))
                       (get-language-numbers (-> (cons/c number? (listof number?))))
                       (get-language-position (-> (cons/c string? (listof string?))))
                       (get-language-url (-> (or/c false/c string?)))
                       (get-one-line-summary (-> string?))
                       (get-comment-character (-> (values string? char?)))
                       (get-style-delta 
                        (-> (or/c false/c
                                  (is-a?/c style-delta%)
                                  (listof 
                                   (list/c (is-a?/c style-delta%)
                                           number?
                                           number?)))))
                       (marshall-settings (-> any/c printable/c))
                       (on-execute (-> any/c (-> (-> any) any) any))
                       (render-value (-> any/c 
                                         any/c
                                         output-port?
                                         void?))
                       (render-value/format (-> any/c 
                                                any/c
                                                output-port?
                                                (or/c number? (symbols 'infinity))
                                                any))
                       (unmarshall-settings (-> printable/c any))
                       
                       
                       (capability-value (-> symbol? any))
                       
                       #;
                       (capability-value 
                        (->d ([s (and/c symbol? 
                                        drscheme:language:capability-registered?)])
                             ()
                             [res (drscheme:language:get-capability-contract s)])))])
       #'(begin
           (define id ctc)
           (provide/doc
            (thing-doc id
                       contract?
                       @{@schemeblock[ctc]}))))]))

(language-object-abstraction drscheme:language:object/c)
