#lang racket/base
(require web-server/servlet)

(define (extract-number req)
  (string->number
   (extract-binding/single
    'number
    (request-bindings req))))

; build-request-page : str str -> response
(define (build-request-page which-number k-url)
  (response/xexpr
   `(html (head (title "Enter a Number to Add"))
          (body ([bgcolor "white"])
                (form ([action ,k-url] [method "post"])
                      "Enter the " ,which-number " number to add: "
                      (input ([type "text"] [name "number"] [value ""]))
                      (input ([type "submit"] [name "enter"] [value "Enter"])))))))

(define (get-first req)
  (build-request-page "First" (add-url get-second)))

(define (get-second req)
  (define fst (extract-number req))
  (build-request-page "Second" (add-url display-sum fst)))

(define (display-sum req fst)
  (define snd (extract-number req))
  (response/xexpr
   `(html (head (title "Sum"))
          (body ([bgcolor "white"])
                (p "The answer is "
                   ,(number->string (+ fst snd)))))))

(define-values (start add-url)
  (dispatch-rules
   [("get-second") get-second]
   [("display-sum" (integer-arg)) display-sum]
   [else get-first]))

(serve/dispatch start)
