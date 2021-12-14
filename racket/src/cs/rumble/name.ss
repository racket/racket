;; Used to name procedures internally and to encode an 'inferred-name
;; property as a Scheme expression
(define-syntax (|#%name| stx)
  (syntax-case stx ()
    [(_ name val) #`(let ([name val]) name)]))
