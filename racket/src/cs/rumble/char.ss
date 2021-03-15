
(define/who (char-blank? x)
  (check who char? x)
  (or (char=? x #\tab)
      (eq? (#%char-general-category x) 'Zs)))

(define/who (char-iso-control? x)
  (check who char? x)
  (or (char<=? #\nul x #\x1F)
      (char<=? #\delete x #\x9F)))
  
(define/who (char-punctuation? x)
  (check who char? x)
  (and (#%memq (#%char-general-category x) '(Pc Pd Ps Pe Pi Pf Po)) #t))

(define/who (char-graphic? x)
  (check who char? x)
  (or (char-numeric? x)
      (char-alphabetic? x)
      (and (#%memq (#%char-general-category x) '(Ll Lm Lo Lt Lu Nd Nl No Mn Mc Me
                                                    ;; char-symbolic?:
                                                    Sm Sc Sk So
                                                    ;; char-punctuation?:
                                                    Pc Pd Ps Pe Pi Pf Po))
           #t)))

(define/who (char-symbolic? x)
  (check who char? x)
  (and (#%memq (#%char-general-category x) '(Sm Sc Sk So)) #t))

(define (interned-char? v)
  (char? v))

(define (char-general-category ch)
  (or (with-global-lock* (getprop (#%char-general-category ch) 'downcase #f))
      (let* ([s (#%char-general-category ch)]
             [ds (string->symbol (string-downcase (#%symbol->string s)))])
        (with-global-lock* (putprop s 'downcase ds))
        ds)))
