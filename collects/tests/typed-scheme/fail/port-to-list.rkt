#lang typed/racket
(car (car
      (parameterize ((current-input-port (open-input-string "2")))
              ((inst port->list (List Number))))))

