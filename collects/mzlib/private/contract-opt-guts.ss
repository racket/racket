(module contract-opt-guts mzscheme
  (require "contract.ss"
           "contract-guts.ss"
           "contract-arrow.ss")
  
  (provide make-known known? known-flag known-sexp
           get-opter reg-opter! opter
           make-lifted interleave-lifted)
  
  (define-struct known (flag sexp))
  
  ;; State information for opters
  (define opters-table
    (make-hash-table 'equal))
  
  ;; get-opter : syntax -> opter
  (define (get-opter ctc)
    (hash-table-get opters-table ctc #f))
  
  ;; opter : syntax or symbol -> opter
  (define (opter ctc)
    (if (or (identifier? ctc) (symbol? ctc))
        (let ((key (if (syntax? ctc) (syntax-e ctc) ctc)))
          (get-opter key))
        (error 'opter "the argument must either be an identifier or a syntax object of an identifier, got ~e" ctc)))
  
  ;; reg-opter! : symbol opter ->
  (define (reg-opter! ctc opter)
    (hash-table-put! opters-table ctc opter))
  
  ;; make-lifted : list -> syntax
  ;; converts a list of lifted-var lifted-expr pairs into a syntax object
  ;; suitable for use in a let.
  (define (make-lifted lst)
    (map (λ (x) (with-syntax ((var (car x))
                              (e (cdr x)))
                  (syntax (var e)))) lst))
  
  ;; interleave-lifted : list list -> list
  ;; interleaves a list of variables names and a list of sexps into a list of
  ;; (var sexp) pairs
  (define (interleave-lifted vars sexps)
    (if (= (length vars) (length sexps))
        (if (null? vars) null
            (cons (cons (car vars) (car sexps))
                  (interleave-lifted (cdr vars) (cdr sexps))))
        (error 'interleave-lifted "expected lists of equal length, got ~e and ~e" vars sexps))))
