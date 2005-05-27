(module mrflow mzscheme
  (require (lib "pretty.ss")
           (lib "contract.ss")
           (lib "mred.ss" "mred"))

  (provide (all-from mzscheme)
           ;(all-from-except mzscheme vector-ref)
           ;(rename dbg-vector-ref vector-ref)
           
           (all-from-except (lib "contract.ss") provide/contract define/contract)
           ; one or the other
           provide/contract define/contract
           ;(rename dbg-provide/contract provide/contract)(rename dbg-define-contract define/contract)
           
           non-negative-exact-integer?
           text%?
           style-delta%?
           )
  
  (define-syntax (dbg-provide/contract stx)
    (syntax-case stx (struct)
      [(_) #'(provide)]
      [(_ (id contract) other ...)
       #'(begin (provide id) (dbg-provide/contract other ...))]
      [(_ (struct id ((field contract) ...)) other ...)
       #'(begin (provide (struct id (field ...))) (dbg-provide/contract other ...))])
    )
  
  (define-syntax (dbg-define/contract stx)
    (syntax-case stx ()
      [(_ name contract body) #'(define name body)]))
  
  (define-syntax dbg-vector-ref
    (lambda (stx)
      (syntax-case stx ()
        [(_ args ...)
         #`(begin
             (printf "~a ~a ~a ~a~n"
                     #,(syntax-source stx)
                     #,(syntax-line stx)
                     #,(syntax-column stx)
                     #,(syntax-original? stx))
             (#,#'vector-ref args ...))])))

  (define non-negative-exact-integer? (and/c integer? exact? (>=/c 0)))  
  (define text%? (is-a?/c text%))
  (define style-delta%? (is-a?/c style-delta%))
  
  )
