(require (lib "gui.ss" "frtime"))

(define kinds (list "New York" "Chicago" "California" "Hawaii"))
(define sizes (list "small" "medium" "large" "Texas"))

(define customer
  (make-text "Customer name:"))

(define kind
  (make-choice "Kind:" kinds))

(define size
  (make-choice "Size:" sizes))

(define button-event
  (make-button "Confirm"))

(make-message
 (hold (button-event
        . -=> .
        (snapshot (customer kind size)
          (string-append customer " ordered a "
                         (list-ref sizes size) " "
                         (list-ref kinds kind) " pizza.")))))
