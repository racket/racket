#lang racket/base
(require (for-syntax racket/base)
         scribble/srcdoc
         racket/class
         racket/gui/base
         racket/contract
         "recon.rkt")
(require/doc racket/base scribble/manual)

(require (for-meta 2 racket/base))

(provide language-object-abstraction)

(define-syntax (language-object-abstraction stx)
  (syntax-case stx ()
    [(_ id provide?)
     (let-syntax ([save-srcloc
                   (λ (s)
                     (define-struct sloc (inside loc) #:prefab)
                     (syntax-case s ()
                       [(_ arg)
                        (with-syntax ([ans
                                       (let loop ([s #'arg])
                                         (cond
                                           [(syntax? s) 
                                            (let ([loc (vector (syntax-source s)
                                                               (syntax-line s)
                                                               (syntax-column s)
                                                               (syntax-position s)
                                                               (syntax-span s))])
                                              (make-sloc (loop (syntax-e s)) loc))]
                                           [(pair? s) (cons (loop (car s)) (loop (cdr s)))]
                                           [else s]))])
                          #'ans)]))])
       (let* ([ctc
               (save-srcloc
                (object-contract
                 (config-panel (-> (is-a?/c area-container<%>)
                                   (case-> (-> any/c void?)
                                           (-> any/c))))
                 (create-executable (-> any/c
                                        (or/c (is-a?/c dialog%) (is-a?/c frame%))
                                        path?
                                        void?))
                 (default-settings (-> any/c))
                 (default-settings? (-> any/c boolean?))
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
                 (get-one-line-summary (-> (or/c #f string?)))
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
                 
                 (capability-value 
                  (->i ([s (and/c symbol? 
                                  drracket:language:capability-registered?)])
                       [res (s) (drracket:language:get-capability-contract s)]))))])
           #`(begin
               (define id (reconstitute #,ctc provide?))
               #,@(if (syntax-e #'provide?)
                      (list 
                       #`(require/doc drracket/private/recon)
                       #`(provide/doc
                          (thing-doc id
                                     contract?
                                     ((reconstitute (racketblock #,ctc) provide?)))))
                      '()))))]))
