;; Include this file in a module where `chez:define` is
;; the host Scheme `define`

;; Allocation of place registers to built-in subsystems, where the
;; first index is reserved for Rumble:

(meta chez:define thread-register-start 1)
(meta chez:define thread-register-count 31)

(meta chez:define io-register-start (+ thread-register-start thread-register-count))
(meta chez:define io-register-count 32)

(meta chez:define regexp-register-start (+ io-register-start io-register-count))
(meta chez:define regexp-register-count 32)

(meta chez:define expander-register-start (+ regexp-register-start regexp-register-count))
(meta chez:define expander-register-count 32)

;; ----------------------------------------

(meta chez:define place-registers (make-eq-hashtable))

(meta chez:define add-place-register!
      (lambda (id start count)
        (let ([n (hashtable-size place-registers)])
          (when (= n count)
            (#%error 'add-place-register! "too many place registers"))
          (let ([i (+ n start)])
            (hashtable-set! place-registers (#%syntax->datum id) i)
            i))))

(define-syntax (define-place-register-define stx)
  (syntax-case stx ()
    [(_ new-define start count)
     #'(define-syntax (new-define stx)
         (syntax-case stx (unsafe-make-place-local)
           [(_ id (unsafe-make-place-local v))
            (with-syntax ([i (#%datum->syntax #'here (add-place-register! #'id start count))])
              #`(begin
                  ;; The `id` shoiuld be used only with
                  ;; `unsafe-place-local-{ref,set!}`:
                  (define-syntax id (syntax-rules ()))
                  ;; Initialize the place value:
                  (define dummy (place-local-register-init! i v))))]
           [(_ . rest) #'(chez:define . rest)]))]))

(define-syntax (unsafe-place-local-ref stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax ([i (#%datum->syntax #'here (or (hashtable-ref place-registers (#%syntax->datum #'id) #f)
                                                  (#%error 'unsafe-place-register-ref (#%format "unknown register ~s" #'id))))])
       #'(place-local-register-ref i))]
    [_ #'rumble:unsafe-place-local-ref]))

(define-syntax (unsafe-place-local-set! stx)
  (syntax-case stx ()
    [(_ id v)
     (with-syntax ([i (#%datum->syntax #'here (or (hashtable-ref place-registers (#%syntax->datum #'id) #f)
                                                  (#%error 'unsafe-place-register-ref (#%format "unknown register ~s" #'id))))])
       #'(place-local-register-set! i v))]
    [_ #'rumble:unsafe-place-local-set!]))
