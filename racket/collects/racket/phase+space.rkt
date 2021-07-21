(module phase+space '#%kernel
  (#%require "private/qq-and-or.rkt")
  
  (#%provide phase?
             space?
             phase+space
             phase+space?
             phase+space-shift?
             phase+space-phase
             phase+space-space
             phase+space+
             phase+space-shift+)

  (define-values (phase?)
    (lambda (v)
      (or (not v)
          (exact-integer? v))))

  (define-values (interned-symbol?)
    (lambda (v)
      (and (symbol? v)
           (symbol-interned? v))))

  (define-values (space?)
    (lambda (v)
      (or (not v)
          (interned-symbol? v))))

  (define-values (phase+space?)
    (lambda (v)
      (or (phase? v)
          (and (pair? v)
               (interned-symbol? (cdr v))
               (phase? (car v))))))

  (define-values (phase+space)
    (lambda (phase space)
      (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (begin
            (if (phase? phase)
                (void)
                (raise-argument-error 'phase+space "phase?" phase))
            (if (space? space)
                (void)
                (raise-argument-error 'phase+space "space?" space))))
      (if space
          (cons phase space)
          phase)))

  (define-values (phase+space-shift?)
    (lambda (v)
      (or (phase? v)
          (and (pair? v)
               (or (not (cdr v))
                   (interned-symbol? (cdr v)))
               (phase? (car v))))))

  (define-values (phase+space-phase)
    (lambda (p+s)
      (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (if (phase+space? p+s)
              (void)
              (raise-argument-error 'phase+space-phase "phase+space?" p+s)))
      (if (pair? p+s) (car p+s) p+s)))

  (define-values (phase+space-space)
    (lambda (p+s)
      (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (if (phase+space? p+s)
              (void)
              (raise-argument-error 'phase+space-space "phase+space?" p+s)))
      (and (pair? p+s) (cdr p+s))))

  (define-values (phase+space+)
    (lambda (p+s s)
      (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (begin
            (if (phase+space? p+s)
                (void)
                (raise-argument-error 'phase+space+ "phase+space?" p+s))
            (if (phase+space-shift? s)
                (void)
                (raise-argument-error 'phase+space+ "phase+space-shift?" s))))
      (let ([p1 (if (pair? p+s) (car p+s) p+s)]
            [p2 (if (pair? s) (car s) s)]
            [sp1 (and (pair? p+s) (cdr p+s))])
        (let ([p (and p1 p2 (+ p1 p2))]
              [sp (if (pair? s) (cdr s) sp1)])
          (if sp
              (cons p sp)
              p)))))

  (define-values (phase+space-shift+)
    (lambda (s1 s2)
      (if (variable-reference-from-unsafe? (#%variable-reference))
          (void)
          (begin
            (if (phase+space-shift? s1)
                (void)
                (raise-argument-error 'phase+space+ "phase+space-shift?" s1))
            (if (phase+space-shift? s2)
                (void)
                (raise-argument-error 'phase+space+ "phase+space-shift?" s2))))
      (let ([p1 (if (pair? s1) (car s1) s1)]
            [p2 (if (pair? s2) (car s2) s2)])
        (let ([p (and p1 p2 (+ p1 p2))])
          (if (pair? s2)
              (cons p (cdr s2))
              (if (pair? s1)
                  (cons p (cdr s1))
                  p)))))))
