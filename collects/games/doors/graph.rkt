(module graph mzscheme
  (require mzlib/class
           "private/utils.rkt")
  (require-for-syntax "private/utils.rkt")

  (provide node% edge% 
           grid-graph)

  (define node%
    (class object%
      (define edges null)
      (define loc #f)
      (define/public (set-location l)
        (set! loc l))
      (define/public (get-location)
        loc)
      (define/public (connect e)
        (set! edges (cons e edges)))
      (define/public (get-edges)
        edges)
      (define/public (edge-to n)
        (ormap (lambda (e) 
                 (and (memq n (send e get-nodes))
                      e))
               edges))
      (super-new)))
      
  (define edge%
    (class object%
      (define nodes null)
      (define loc #f)
      (define/public (set-location l)
        (set! loc l))
      (define/public (get-location)
        loc)
      (define/public (connect n)
        (set! nodes (cons n nodes)))
      (define/public (get-nodes)
        nodes)
      (super-new)))

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
        (let loop ([edges (car layout)]
                   [nodes (cadr layout)]
                   [next-edges (caddr layout)]
                   [i 0])
          (unless (null? (cdr nodes))
            (let ([n (car edges)]
                  [s (car next-edges)]
                  [e (caddr nodes)]
                  [w (car nodes)]
                  [r (cadr nodes)])
              (send r set-location (list i j))
              (send n set-location (list i (+ j 0.5)))
              (send s set-location (list i (- j 0.5)))
              (send e set-location (list (+ i 0.5) j))
              (send w set-location (list (- i 0.5) j))
              (send r connect n)
              (send r connect s)
              (send r connect e)
              (send r connect w)
              (send n connect r)
              (send s connect r)
              (send e connect r)
              (send w connect r))
            (loop (cdr edges)
                  (cddr nodes)
                  (cdr next-edges)
                  (add1 i))))
        (loop (cddr layout) (sub1 j)))))

  (define-syntax grid-graph
    (lambda (stx)
      (syntax-case stx ()
        [(maze edge<%> node<%> (items ...) ...)
         (let ([itemss (syntax->list #'((items ...) ...))])
           (unless (odd? (length itemss))
             (raise-syntax-error
              #f
              "need an odd number of rows"
              stx))
           (let-values ([(edgess nodess) (alternates itemss)])
             (when (null? nodess)
               (raise-syntax-error
                #f
                "no nodes supplied"
                stx))
             (let ([first-edges-len
                    (length (syntax->list (car edgess)))])
               (for-each (lambda (edges)
                           (let ([len (length (syntax->list edges))])
                             (unless (= len first-edges-len)
                               (raise-syntax-error
                                #f
                                "N/S edges sequence length doesn't match first edges sequence"
                                stx
                                edges))))
                         edgess)
               (for-each (lambda (nodes)
                           (let ([len (length (syntax->list nodes))])
                             (unless (= len (add1 (* 2 first-edges-len)))
                               (raise-syntax-error
                                #f
                                "nodes with E/W edges sequence length doesn't match first edges sequence"
                                stx
                                nodes))))
                         nodess))
             (with-syntax ([((items ...) ...)
                            (interleave
                             (map (lambda (edges)
                                    (map (lambda (edge)
                                           (quasisyntax/loc edge 
                                             (instance edge<%> #,edge)))
                                         (syntax->list edges)))
                                  edgess)
                             (map (lambda (nodes)
                                    (let-values ([(edges nodes) 
                                                  (alternates (syntax->list nodes))])
                                      (interleave
                                       (map (lambda (edge)
                                              (quasisyntax/loc edge 
                                                (instance edge<%> #,edge)))
                                            edges)
                                       (map (lambda (node)
                                              (quasisyntax/loc node
                                                (instance node<%> #,node)))
                                            nodes))))
                                  nodess))])
               (syntax/loc stx
                 (connect-all! (member-name-key connect) (list (list items ...) ...))))))])))
  
  (define-syntax instance
    (syntax-rules ()
      [(instance <%> v)
       (check <%> v)])))
