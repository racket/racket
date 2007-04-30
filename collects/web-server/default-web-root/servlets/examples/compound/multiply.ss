(module multiply mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "etc.ss")
           "helper.ss")
  (provide (all-defined))
  (define interface-version 'v1)
  (define timeout +inf.0)
  
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
  (define (start initial-request)
    `(html (head (title "Matrix Product"))
           (body
            (p "The matrix product is"
               ,(render-matrix
                 (let-values ([(r c) (get-dimentions)])
                   (matrix-multiply (get-matrix r c)
                                    (get-matrix c r)))))))))