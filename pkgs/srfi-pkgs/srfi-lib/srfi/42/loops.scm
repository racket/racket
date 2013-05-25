;;;
;;; LOOPS
;;;

(module loops mzscheme
  (provide loop loop-stx
           loop->syntax 
           (rename checked-make-loop make-loop)
           loop? )
  
  ; The structure of a loop is:
  
  #;(let (<ob>*)                      ; outer bindings
      <oc>*                           ; outer commands
      (let loop (<lb>*)               ; loop bindings
        (if <ne1?>                    ; not end (stop-before)
            (let (<ib>*)              ; inner bindings
              <ic>*                   ; inner commands
              <payload>               ; payload (from the comprehension)
              (if <ne2?>              ; not end (stop-after)
                  (loop <ls>*))))))   ; loop steppers
  
  ; A binding is a list consisting of two syntax-objects,
  ; the first represents the variable, the other the expression.
  
  ; Actually for the inner and outer bindings we are using let-values instead of let.
  ; The form (:do _) supports both (let _) and (let-values _) syntax for these bindings.
  ;         (let ((v e) ...) c ...) 
  ;     ==  (let-values (((v) e) ...) c ...)
  
  (define-struct loop (stx))
  ; stx is a syntax-object representing:
  ;   (ob* oc* lb* ne1 ib* ic* ne2 ls*)
  
  (define (checked-make-loop stx)
    (define (check-values-bindings stx)
      (syntax-case stx ()
        [(((name ...) expr) ...)
         (begin
           (unless (andmap identifier? (syntax->list #'(name ... ...)))
             (raise-syntax-error 
              'make-loop "expected list of bindings, got: " stx)))]))
    (define (check-bindings stx)
      (syntax-case stx ()
        [((name expr) ...)
         (begin
           (unless (andmap identifier? (syntax->list #'(name ...)))
             (raise-syntax-error 
              'make-loop "expected list of bindings, got: " stx)))]))
    (define (check-list-of stx what)
      (syntax-case stx ()
        [(x ...) 'ok]
        [_ (raise-syntax-error 
            'make-loop (format "expected list of ~a, got: " what) stx)]))
    ; checks
    (syntax-case stx ()
      [(ob* oc* lb* ne1 ib* ic* ne2 ls*)
       (begin
         (check-values-bindings #'ob*)
         (check-values-bindings #'ib*)
         ; (check-bindings #'lb*)
         (check-list-of #'oc* "outer commands")
         (check-list-of #'ic* "inner commands")
         (check-list-of #'ls* "loop steppers"))]
      [_else 
       (raise-syntax-error 
        'make-loop 
        "expected (ob* oc* lb* ne1 ib* ic* ne2 ls*), got: " stx)])
    ; everything's ok
    (make-loop stx))
  
  
  ; A simple loop has the structure:
  
  #;(let loop (<lb>*)
      (if <ne1?>
          (loop <ls>*)))
  
  (require-for-template mzscheme)
  (require-for-template "simplifier.scm")
  
  ; make-simple-loop : stx stx stx -> loop
  ;  build a loop from the simple pieces
  ;(define (make-simple-loop lb* ne1 ls*)
  ;  (with-syntax ([lb* lb*] [ne1 ne1] [ls* ls*])
  ;    (make-loop #'(() () lb* ne1 () () #t ls*))))
  
  ; loop->syntax : src-stx loop stx -> stx
  ;   Turn the loop structure l into a
  ;   syntax-object containing a loop.
  ;   Use payload as the body of the load.
  ;   The src-location info is taken from src-stx.
  (define (loop->syntax src-stx l payload)
    (syntax-case (loop-stx l) ()
      [((ob ...) (oc ...) (lb ...) ne1 (ib ...) (ic ...) ne2 (ls ...))
       (with-syntax ([payload payload])
         (syntax/loc src-stx
           (let-values (ob ...)
             oc ...
             (let loop (lb ...)
               (ec-simplify
                (if ne1                    
                    (let-values (ib ...)
                      ic ...
                      (ec-simplify payload)
                      (ec-simplify
                       (if ne2
                           (loop ls ...))))))))))]))
  
  )
