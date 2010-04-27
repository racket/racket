(module lex-sre mzscheme
  (require parser-tools/lex)
    
  (provide (rename sre-* *)
           (rename sre-+ +)
           ?
           (rename sre-= =)
           (rename sre->= >=)
           **
           (rename sre-or or)
           :
           seq
           &
           ~
           (rename sre-- -)
           (rename sre-/ /)
           /-only-chars)
           
  (define-lex-trans sre-*
    (syntax-rules ()
      ((_ re ...)
       (repetition 0 +inf.0 (union re ...)))))

  (define-lex-trans sre-+
    (syntax-rules ()
      ((_ re ...)
       (repetition 1 +inf.0 (union re ...)))))

  (define-lex-trans ?
    (syntax-rules ()
      ((_ re ...)
       (repetition 0 1 (union re ...)))))
  
  (define-lex-trans sre-=
    (syntax-rules ()
      ((_ n re ...)
       (repetition n n (union re ...)))))
  
  (define-lex-trans sre->=
    (syntax-rules ()
      ((_ n re ...)
       (repetition n +inf.0 (union re ...)))))

  (define-lex-trans **
    (syntax-rules ()
      ((_ low #f re ...)
       (** low +inf.0 re ...))
      ((_ low high re ...)
       (repetition low high (union re ...)))))
  
  (define-lex-trans sre-or
    (syntax-rules ()
      ((_ re ...)
       (union re ...))))
  
  (define-lex-trans :
    (syntax-rules ()
      ((_ re ...)
       (concatenation re ...))))

  (define-lex-trans seq
    (syntax-rules ()
      ((_ re ...)
       (concatenation re ...))))

  (define-lex-trans &
    (syntax-rules ()
      ((_ re ...)
       (intersection re ...))))

  (define-lex-trans ~
    (syntax-rules ()
      ((_ re ...)
       (char-complement (union re ...)))))
  
  ;; set difference
  (define-lex-trans (sre-- stx)
    (syntax-case stx ()
      ((_)
       (raise-syntax-error #f
                           "must have at least one argument"
                           stx))
      ((_ big-re re ...)
       (syntax (& big-re (complement (union re ...)))))))
  
  (define-lex-trans (sre-/ stx)
    (syntax-case stx ()
      ((_ range ...)
       (let ((chars
              (apply append (map (lambda (r)
                                   (let ((x (syntax-e r)))
                                     (cond
                                       ((char? x) (list x))
                                       ((string? x) (string->list x))
                                       (else
                                        (raise-syntax-error 
                                         #f
                                         "not a char or string"
                                         stx
                                         r)))))
                                 (syntax->list (syntax (range ...)))))))
         (unless (even? (length chars))
           (raise-syntax-error
            #f
            "not given an even number of characters"
            stx))
         #`(/-only-chars #,@chars)))))
  
  (define-lex-trans /-only-chars
    (syntax-rules ()
      ((_ c1 c2)
       (char-range c1 c2))
      ((_ c1 c2 c ...)
       (union (char-range c1 c2)
              (/-only-chars c ...)))))
  
  )
                           
   
