#lang web-server/insta

;; A roster is a (roster names)
;; where names is a list of string.
(struct roster (names) #:mutable)

;; roster-add-name!: roster string -> void
;; Given a roster and a name, adds the name
;; to the end of the roster.
(define (roster-add-name! a-roster a-name)
  (set-roster-names! a-roster 
                    (append (roster-names a-roster)
                            (list a-name))))

(define ROSTER (roster '("kathi" "shriram" "dan")))

;; start: request -> doesn't return
(define (start request)
  (show-roster request))

;; show-roster: request -> doesn't return
(define (show-roster request)
  (local [(define (response-generator embed/url)
            (response/xexpr
             `(html (head (title "Roster"))
                    (body (h1 "Roster")
                          ,(render-as-itemized-list
                            (roster-names ROSTER))
                          (form ((action
                                  ,(embed/url add-name-handler)))
                                (input ((name "a-name")))
                                (input ((type "submit"))))))))
          (define (parse-name bindings)
            (extract-binding/single 'a-name bindings))
            
          (define (add-name-handler request)
            (roster-add-name!
             ROSTER (parse-name (request-bindings request)))
            (show-roster (redirect/get)))]
    (send/suspend/dispatch response-generator))) 

;; render-as-itemized-list: (listof xexpr) -> xexpr
(define (render-as-itemized-list fragments)
  `(ul ,@(map render-as-item fragments)))

;; render-as-item: xexpr -> xexpr
(define (render-as-item a-fragment)
  `(li ,a-fragment))
