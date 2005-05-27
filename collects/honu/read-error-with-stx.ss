(module read-error-with-stx mzscheme
  
  (require (lib "readerr.ss" "syntax"))
  
  (provide raise-read-error-with-stx)
  ;; Yes, this is misleading for now.  I'll rename it later.
  ;; (define (raise-read-error-with-stx str stx)
  ;;   (raise-syntax-error #f str stx))

  ;; Forget it, this gives you less extra crap with your error message.
  ;; I'd need to decide what to put in those places instead, so at the
  ;; moment we'll keep it a read error.
  (define (raise-read-error-with-stx str stx)
    (raise-read-error str
                      (syntax-source stx)
                      (syntax-line stx)
                      (syntax-column stx)
                      (syntax-position stx)
                      (syntax-span stx)))
  )
