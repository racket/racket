(require (lib "servlet-sig.ss" "web-server")
         (lib "servlet-helpers.ss" "web-server")
         (lib "unitsig.ss")
         (lib "etc.ss")
         "helper-sig.ss")

(define multiply@
  (unit/sig ()
    (import servlet^ my-servlet-helpers^)
    
    ; matrix = (listof (listof num))
    
    ; matrix-multiply : matrix matrix -> matrix
    (define (matrix-multiply a b)
      (map (lambda (a-row)
             (side-map (lambda (b-column)
                         (apply + (map * a-row b-column)))
                       b))
           a))
    
    ; side-map : ((listof a) -> b) (listof (listof a)) -> (listof b)
    (define (side-map f m)
      (cond
        [(null? (car m)) null]
        [else (cons (f (map car m))
                    (side-map f (map cdr m)))]))
    
    ; ---
    
    ; get-dimentions : -> nat nat
    ; to ask for and return the number of rows and columns
    (define (get-dimentions)
      (values
       (get-number "the number of rows in the first matrix")
       (get-number "the number of rows in the second matrix")))
    
    ; get-matrix : nat nat -> matrix
    (define (get-matrix rows columns)
      (let ([b (get-matrix-bindings rows columns)])
        (build-list
         rows
         (lambda (r)
           (build-list
            columns
            (lambda (c)
              (string->number (extract-binding/single (string->symbol (field-name r c)) b))))))))
    
    ; get-matrix-bindings : nat nat -> (listof (cons sym str))
    (define (get-matrix-bindings rows columns)
      (request-bindings
       (send/suspend
        (build-suspender
         (list "Enter a " (number->string rows) " by "
               (number->string columns) " Matrix")
         `((table
            . ,(build-list
                rows
                (lambda (r)
                  `(tr . ,(build-list
                           columns
                           (lambda (c)
                             `(td (input ([type "text"] [name ,(field-name r c)])))))))))
           (input ([type "submit"] [name "submit"] [value "Okay"])))))))
    
    ; field-name : nat nat -> str
    (define (field-name row column)
      (format "x-~a-~a" row column))
    
    ; ---
    
    ; render-matrix : matrix -> html
    (define (render-matrix m)
      `(table
        ([border "1"])
        . ,(map (lambda (row)
                  `(tr . ,(map (lambda (n)
                                 `(td ,(number->string n)))
                               row)))
                m)))
    
    ; main
    `(html (head (title "Matrix Product"))
           (body
            (p "The matrix product is"
               ,(render-matrix
                 (let-values ([(r c) (get-dimentions)])
                   (matrix-multiply (get-matrix r c)
                                    (get-matrix c r)))))))))

(compound-unit/sig
 (import (S : servlet^))
 (link
  [H : my-servlet-helpers^ ((load-relative "helper.ss") S)]
  [M : () (multiply@ S H)])
 (export (open M)))