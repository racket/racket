#lang scheme
(require web-server/formlets)

(define-struct date (month day))
(define (date->xml d)
  (format "~a/~a"
          (date-month d)
          (date-day d)))

(define (submit t)
  `(input ([type "submit"]) ,t))

(define date-formlet
  (formlet
   (div
    "Month:" ,{input-int . => . month}
    "Day:" ,{input-int . => . day})
   (make-date month day)))

(formlet-display date-formlet)

(define travel-formlet
  (formlet
   (div
    "Name:" ,{input-string . => . name}
    (div
     "Arrive:" ,{date-formlet . => . arrive}
     "Depart:" ,{date-formlet . => . depart})
    ,@(list "1" "2" "3")
    ,(submit "Submit"))
   (list name arrive depart)))

(formlet-display travel-formlet)

(define display-itinernary
  (match-lambda
    [(list name arrive depart)
     `(html
       (head (title "Itinerary"))
       (body
        "Itinerary for: " ,name
        "Arriving:" ,(date->xml arrive)
        "Departing:" ,(date->xml depart)))]))

(require net/url
         web-server/servlet)
(formlet-process travel-formlet
                 (make-request 'get (string->url "http://test.com")
                               empty
                               (list (make-binding:form #"input_0" #"Jay")
                                     (make-binding:form #"input_1" #"10")
                                     (make-binding:form #"input_2" #"6")
                                     (make-binding:form #"input_3" #"10")
                                     (make-binding:form #"input_4" #"8"))
                               #f "127.0.0.1" 80 "127.0.0.1"))

(define (start request)
  (display-itinernary
   (send/formlet
    travel-formlet)))

