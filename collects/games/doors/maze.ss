
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
        (let loop ([doors (car layout)]
                   [rooms (cadr layout)]
                   [next-doors (caddr layout)]
                   [i 0])
          (unless (null? (cdr rooms))
            (let ([n (car doors)]
                  [s (car next-doors)]
                  [e (caddr rooms)]
                  [w (car rooms)]
                  [r (cadr rooms)])
              (send r connect i j n s e w))
            (loop (cdr doors)
                  (cddr rooms)
                  (cdr next-doors)
                  (add1 i))))
        (loop (cddr layout) (sub1 j)))))

  (define-syntax maze
    (lambda (stx)
      (syntax-case stx ()
        [(maze connect door<%> room<%> (items ...) ...)
         (let ([itemss (syntax->list #'((items ...) ...))])
           (unless (odd? (length itemss))
             (raise-syntax-error
              #f
              "need an odd number of rows"
              stx))
           (let-values ([(doorss roomss) (alternates itemss)])
             (when (null? roomss)
               (raise-syntax-error
                #f
                "no rooms supplied"
                stx))
             (let ([first-doors-len
                    (length (syntax->list (car doorss)))])
               (for-each (lambda (doors)
                           (let ([len (length (syntax->list doors))])
                             (unless (= len first-doors-len)
                               (raise-syntax-error
                                #f
                                "N/S doors sequence length doesn't match first doors sequence"
                                stx
                                doors))))
                         doorss)
               (for-each (lambda (rooms)
                           (let ([len (length (syntax->list rooms))])
                             (unless (= len (add1 (* 2 first-doors-len)))
                               (raise-syntax-error
                                #f
                                "rooms with E/W doors sequence length doesn't match first doors sequence"
                                stx
                                rooms))))
                         roomss))
             (with-syntax ([((items ...) ...)
                            (interleave
                             (map (lambda (doors)
                                    (map (lambda (door)
                                           (quasisyntax/loc door 
                                             (instance door<%> #,door)))
                                         (syntax->list doors)))
                                  doorss)
                             (map (lambda (rooms)
                                    (let-values ([(doors rooms) 
                                                  (alternates (syntax->list rooms))])
                                      (interleave
                                       (map (lambda (door)
                                              (quasisyntax/loc door 
                                                (instance door<%> #,door)))
                                            doors)
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

