
(module maze mzscheme
  (require (lib "class.ss")
           "private/utils.ss")
  (require-for-syntax "private/utils.ss")

  (provide maze)

  (define (check <%> v)
    (unless (v . is-a? . <%>)
      (error 'maze
             "not an instance of ~a: ~e"
             <%>
             v))
    v)

  (define (connect-all! connect-key layout)
    (define-member-name connect connect-key)
    (let loop ([layout layout]
               [j (sub1 (quotient (length layout) 2))])
      (unless (null? (cdr layout))
        (let loop ([walls (car layout)]
                   [rooms (cadr layout)]
                   [next-walls (caddr layout)]
                   [i 0])
          (unless (null? (cdr rooms))
            (let ([n (car walls)]
                  [s (car next-walls)]
                  [e (caddr rooms)]
                  [w (car rooms)]
                  [r (cadr rooms)])
              (send r connect i j n s e w))
            (loop (cdr walls)
                  (cddr rooms)
                  (cdr next-walls)
                  (add1 i))))
        (loop (cddr layout) (sub1 j)))))

  (define-syntax maze
    (lambda (stx)
      (syntax-case stx ()
        [(maze connect wall<%> room<%> (items ...) ...)
         (let ([itemss (syntax->list #'((items ...) ...))])
           (unless (odd? (length itemss))
             (raise-syntax-error
              #f
              "need an odd number of rows"
              stx))
           (let-values ([(wallss roomss) (alternates itemss)])
             (when (null? roomss)
               (raise-syntax-error
                #f
                "no rooms supplied"
                stx))
             (let ([first-walls-len
                    (length (syntax->list (car wallss)))])
               (for-each (lambda (walls)
                           (let ([len (length (syntax->list walls))])
                             (unless (= len first-walls-len)
                               (raise-syntax-error
                                #f
                                "N/S walls sequence length doesn't match first walls sequence"
                                stx
                                walls))))
                         wallss)
               (for-each (lambda (rooms)
                           (let ([len (length (syntax->list rooms))])
                             (unless (= len (add1 (* 2 first-walls-len)))
                               (raise-syntax-error
                                #f
                                "rooms with E/W walls sequence length doesn't match first walls sequence"
                                stx
                                rooms))))
                         roomss))
             (with-syntax ([((items ...) ...)
                            (interleave
                             (map (lambda (walls)
                                    (map (lambda (wall)
                                           (quasisyntax/loc wall 
                                             (instance wall<%> #,wall)))
                                         (syntax->list walls)))
                                  wallss)
                             (map (lambda (rooms)
                                    (let-values ([(walls rooms) 
                                                  (alternates (syntax->list rooms))])
                                      (interleave
                                       (map (lambda (wall)
                                              (quasisyntax/loc wall 
                                                (instance wall<%> #,wall)))
                                            walls)
                                       (map (lambda (room)
                                              (quasisyntax/loc room
                                                (instance room<%> #,room)))
                                            rooms))))
                                  roomss))])
               (syntax/loc stx
                 (connect-all! (member-name-key connect) (list (list items ...) ...))))))])))
  
  (define-syntax instance
    (syntax-rules (unquote)
      [(instance <%> (unquote v))
       (check <%> v)]
      [(instance <%> %)
       (check <%> (new %))])))

