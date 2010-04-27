#lang web-server/insta

;; A roster is a (make-roster names)
;; where names is a list of string.
(define-struct roster (names) #:mutable)

;; roster-add-name!: roster string -> void
;; Given a roster and a name, adds the name
;; to the end of the roster.
(define (roster-add-name! a-roster a-name)
  (set-roster-names! a-roster 
                    (append (roster-names a-roster)
                            (list a-name))))

(define ROSTER (make-roster '("kathi" "shriram" "dan")))

;; start: request -> html-response
(define (start request)
  (show-roster request))

;; show-roster: request -> html-response
(define (show-roster request)
  (local [(define (response-generator make-url)
            `(html (head (title "Roster"))
                   (body (h1 "Roster")
                         ,(render-as-itemized-list
                           (roster-names ROSTER))
                         (form ((action
                                 ,(make-url add-name-handler)))
                               (input ((name "a-name")))
                               (input ((type "submit")))))))
          (define (parse-name bindings)
            (extract-binding/single 'a-name bindings))
            
          (define (add-name-handler request)
            (roster-add-name!
             ROSTER (parse-name (request-bindings request)))
            (show-roster request))]
    (send/suspend/dispatch response-generator))) 

;; render-as-itemized-list: (listof html-response) -> html-response
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

;; render-as-item: html-response -> html-response
(define (render-as-item a-fragment)
  `(li ,a-fragment))
