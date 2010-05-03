
(load-relative "loadtest.rktl")

(Section 'kw)

(require mzlib/kw
         (prefix-in mz: mzscheme))

(let-syntax ([#%datum (syntax-rules () [(_ . xs) (mz:#%datum . xs)])]
             [#%app   (syntax-rules () [(_ . xs) (mz:#%app   . xs)])])
  (define-syntax t
    (syntax-rules (=> <= :rt-err: :st-err:)
      [(t E => :rt-err:) (err/rt-test E)]
      [(t E => :st-err:) (syntax-test #'E)]
      [(t (f x ...) => res) (test res f x ...)]
      [(t E => R more ...) (begin (t E => R) (t more ...))]
      [(t R <= E more ...) (t E => R more ...)]))

  ;; make sure that lambda/kw behaves as lambda
  (t ((lambda/kw () 1)) => 1
     ((lambda/kw (x) 1) 0) => 1
     ((lambda/kw x x)) => '()
     ((lambda/kw x x) 1 2) => '(1 2)
     ((lambda/kw (x . xs) xs) 0 1 2) => '(1 2))
  ;; even with keywords
  (t ((lambda/kw () #:x)) => #:x
     ((lambda/kw (x) #:x) #:y) => #:x
     ((lambda/kw x x) #:x #:y) => '(#:x #:y)
     ((lambda/kw (x . xs) xs) #:z #:x #:y) => '(#:x #:y))

  ;; just using #:rest is the same as a dot
  (let ([f (lambda/kw (#:rest r) r)])
    (t (f)     => '()
       (f 1)   => '(1)
       (f 1 2) => '(1 2)))
  (let ([f (lambda/kw (x #:rest r) r)])
    (t (f 0)     => '()
       (f 0 1)   => '(1)
       (f 0 1 2) => '(1 2)))

  ;; using only optionals
  (t (procedure-arity (lambda/kw (#:optional) 0))       => 0
     (procedure-arity (lambda/kw (x #:optional y z) 0)) => '(1 2 3))
  (let ([f (lambda/kw (x #:optional y) (list x y))])
    (t (f 0)   => '(0 #f)
       (f 0 1) => '(0 1)))
  (let ([f (lambda/kw (x #:optional [y 0]) (list x y))])
    (t (f 0)   => '(0 0)
       (f 0 1) => '(0 1)))
  (let ([f (lambda/kw (x #:optional [y x]) (list x y))])
    (t (f 0)   => '(0 0)
       (f 0 1) => '(0 1)))
  (let ([f (lambda/kw (x #:optional [y x] [z x]) (list x y z))])
    (t (f 0)     => '(0 0 0)
       (f 0 1)   => '(0 1 0)
       (f 0 1 2) => '(0 1 2)))
  (let ([f (lambda/kw (x #:optional [y x] [z y]) (list x y z))])
    (t (f 0)     => '(0 0 0)
       (f 0 1)   => '(0 1 1)
       (f 0 1 2) => '(0 1 2)))

  ;; keywords: basic stuff
  (let ([f (lambda/kw (#:key x [y 1] [z #:zz #:z] #:allow-duplicate-keys)
             (list x y z))])
    (t (f) => '(#f 1 #:z)
       (f #:zz #:zzz #:zz 123 #:x #:zz) => '(#:zz 1 #:zzz)))
  ;; keywords: default-expr scope
  (let ([f (lambda/kw (#:key x y #:allow-duplicate-keys) (list x y))])
    (t '(#f #f) <= (f)
       '(1  #f) <= (f #:x 1)
       '(#f 2 ) <= (f #:y 2)
       '(1  2 ) <= (f #:x 1 #:y 2)
       '(1  2 ) <= (f #:x 1 #:y 2 #:y 3 #:x 4)))
  (let ([f (lambda/kw (#:key x [y x]) (list x y))])
    (t '(1  1 ) <= (f #:x 1)
       '(#f 2 ) <= (f #:y 2)
       '(1  2 ) <= (f #:x 1 #:y 2)))
  (let ([f (lambda/kw (#:key x [y x] [z x]) (list x y z))])
    (t '(1  1  1 ) <= (f #:x 1)
       '(#f 1  #f) <= (f #:y 1)
       '(#f #f 1 ) <= (f #:z 1)))
  (let ([f (lambda/kw (#:key x [y x] [z y]) (list x y z))])
    (t '(1  1  1 ) <= (f #:x 1)
       '(#f 1  1 ) <= (f #:y 1)
       '(#f #f 1 ) <= (f #:z 1)))
  (t
   ((let ([y 1]) (lambda/kw (#:key [x y] [y (add1 x)]) (list x y)))) => '(1 2)
   ((let ([x 1]) (lambda/kw (#:key [x x] [y (add1 x)]) (list x y)))) => '(1 2))
  ;; keywords: default-expr evaluation
  (t ((lambda/kw (#:key [x 1]) x))   => 1
     ((lambda/kw (#:key [x "1"]) x)) => "1"
     ((lambda/kw (#:key [x '1]) x))  => 1
     ((lambda/kw (#:key [x ''1]) x)) => ''1
     ((lambda/kw (#:key [x '(add1 1)]) x)) => '(add1 1)
     ((lambda/kw (#:key [x +]) x))   => +)
  (let ([f (lambda ()
             (let ([y 1]) (lambda/kw (#:key [x (begin (set! y 3) 2)]) y)))])
    (t ((f))       => 3
       ((f) #:x 1) => 1))
  (let ([f (lambda ()
             (let ([y 1])
               (let-syntax ([z (syntax-id-rules () [_ (begin (set! y 3) 2)])])
                 (lambda/kw (#:key [x z]) y))))])
    (t ((f))       => 3
       ((f) #:x 1) => 1))
  ;; keywords: make sure that getarg stops at end of keyword part
  (let ([f (lambda/kw (#:key x y #:body b) (list x y b))])
    (t (f)           => '(#f #f ())
       (f 2)         => '(#f #f (2))
       (f 2 #:x 1)   => '(#f #f (2 #:x 1))
       (f 2 3 #:x 1) => '(#f #f (2 3 #:x 1))))

  ;; exotic extras
  (let ([f (lambda/kw (#:key a b #:rest r #:allow-duplicate-keys) r)])
    (t (f 1 2 3)                   => '(1 2 3)
       (f #:a 1 1 2 3)             => '(#:a 1 1 2 3)
       (f #:a 1 #:a 2 1 2 3)       => '(#:a 1 #:a 2 1 2 3)
       (f #:b 2 1 2 3)             => '(#:b 2 1 2 3)
       (f #:a 1 #:b 2 1 2 3)       => '(#:a 1 #:b 2 1 2 3)
       (f #:a 1 #:b 2 #:c 3 1 2 3) => '(#:a 1 #:b 2 #:c 3 1 2 3)))
  (let ([f (lambda/kw (#:key a b #:body r) r)])
    (t (f 1 2 3)             => '(1 2 3)
       (f #:a 1 1 2 3)       => '(1 2 3)
       (f #:a 1 #:a 2 1 2 3) => :rt-err:
       (f #:b 2 1 2 3)       => '(1 2 3)
       (f #:a 1 #:b 2 1 2 3) => '(1 2 3)))
  (let ([f (lambda/kw (#:key a b #:other-keys r) r)])
    (t (f) => '()
       (f #:a 1 #:b 2) => '()
       (f #:a 1 #:b 2 #:c 3) => '(#:c 3)
       (f #:d 4 #:a 1 #:b 2 #:c 3) => '(#:d 4 #:c 3)
       ;; #:c is not a specified key, so it is allowed to repeat
       (f #:d 4 #:a 1 #:b 2 #:c 3 #:c 33) => '(#:d 4 #:c 3 #:c 33)
       (f #:d 4 #:a 1 #:c 3 #:b 2 #:c 33) => '(#:d 4 #:c 3 #:c 33)
       ))
  (let ([f (lambda/kw (#:key a b #:other-keys r #:allow-duplicate-keys) r)])
    (t (f)                                => '()
       (f #:a 1 #:b 2)                    => '()
       (f #:b 1 #:a 2)                    => '()
       (f #:a 1 #:b 2 #:c 3)              => '(#:c 3)
       (f #:a 1 #:c 2 #:b 3)              => '(#:c 2)
       (f #:c 1 #:a 2 #:b 3)              => '(#:c 1)
       (f #:a 1 #:a 2 #:b 3)              => '(#:a 2)
       (f #:a 1 #:b 2 #:c 3)              => '(#:c 3)
       (f #:a 1 #:a 2 #:b 3 #:a 4)        => '(#:a 2 #:a 4)
       (f #:a 1 #:a 2 #:b 3 #:b 4 #:a 5)  => '(#:a 2 #:b 4 #:a 5)
       (f #:d 4 #:a 1 #:b 2 #:c 3)        => '(#:d 4 #:c 3)
       (f #:d 4 #:a 1 #:b 2 #:c 3 #:c 33) => '(#:d 4 #:c 3 #:c 33)
       (f #:d 4 #:a 1 #:b 2 #:a 3 #:c 33) => '(#:d 4 #:a 3 #:c 33)
       (f #:d 4 #:a 1 #:c 3 #:b 2 #:c 33) => '(#:d 4 #:c 3 #:c 33)
       (f #:d 4 #:a 1 #:c 3 #:a 2 #:c 33) => '(#:d 4 #:c 3 #:a 2 #:c 33)))
  (let ([f (lambda/kw (#:key a b #:other-keys+body r) r)])
    (t (f)                                    => '()
       (f 1 2)                                => '(1 2)
       (f #:a 1 #:b 2)                        => '()
       (f #:a 1 #:b 2 1 2)                    => '(1 2)
       (f #:a 1 #:b 2 #:c 3)                  => '(#:c 3)
       (f #:a 1 #:b 2 #:c 3 1 2)              => '(#:c 3 1 2)
       (f #:d 4 #:a 1 #:b 2 #:c 3)            => '(#:d 4 #:c 3)
       (f #:d 4 #:a 1 #:b 2 #:c 3 1 2)        => '(#:d 4 #:c 3 1 2)
       (f #:d 4 #:a 1 #:b 2 #:c 3 #:c 33)     => '(#:d 4 #:c 3 #:c 33)
       (f #:d 4 #:a 1 #:b 2 #:c 3 #:c 33 1 2) => '(#:d 4 #:c 3 #:c 33 1 2)
       (f #:d 4 #:a 1 #:c 3 #:b 2 #:c 33)     => '(#:d 4 #:c 3 #:c 33)
       (f #:d 4 #:a 1 #:c 3 #:b 2 #:c 33 1 2) => '(#:d 4 #:c 3 #:c 33 1 2)))
  (let ([f (lambda/kw (#:key a b #:other-keys+body r #:allow-duplicate-keys)
             r)])
    (t (f)                                    => '()
       (f 1 2)                                => '(1 2)
       (f #:a 1 #:b 2)                        => '()
       (f #:a 1 #:b 2 1 2)                    => '(1 2)
       (f #:a 1 #:a 2 #:b 3)                  => '(#:a 2)
       (f #:a 1 #:a 2 #:b 3 1 2)              => '(#:a 2 1 2)
       (f #:a 1 #:b 2 #:c 3)                  => '(#:c 3)
       (f #:a 1 #:b 2 #:c 3 1 2)              => '(#:c 3 1 2)
       (f #:d 4 #:a 1 #:b 2 #:c 3)            => '(#:d 4 #:c 3)
       (f #:d 4 #:a 1 #:b 2 #:c 3 1 2)        => '(#:d 4 #:c 3 1 2)
       (f #:d 4 #:a 1 #:b 2 #:c 3 #:c 33)     => '(#:d 4 #:c 3 #:c 33)
       (f #:d 4 #:a 1 #:b 2 #:c 3 #:c 33 1 2) => '(#:d 4 #:c 3 #:c 33 1 2)
       (f #:d 4 #:a 1 #:c 3 #:b 2 #:c 33)     => '(#:d 4 #:c 3 #:c 33)
       (f #:d 4 #:a 1 #:c 3 #:b 2 #:c 33 1 2) => '(#:d 4 #:c 3 #:c 33 1 2)))
  (let ([f (lambda/kw (x #:key a b #:all-keys r) r)])
    (t (f 1)                                => '()
       (f 1 #:a 1 #:b 2)                    => '(#:a 1 #:b 2)
       (f 1 #:a 1 #:a 2 #:b 3)              => '(#:a 1 #:a 2 #:b 3)
       (f 1 #:a 1 #:b 2 #:c 3)              => '(#:a 1 #:b 2 #:c 3)
       (f 1 #:d 4 #:a 1 #:b 2 #:c 3)        => '(#:d 4 #:a 1 #:b 2 #:c 3)
       (f 1 #:d 4 #:a 1 #:b 2 #:c 3 #:c 33) => '(#:d 4 #:a 1 #:b 2 #:c 3 #:c 33)
       (f 1 #:d 4 #:a 1 #:c 3 #:b 2 #:c 33) => '(#:d 4 #:a 1 #:c 3 #:b 2 #:c 33)
       (f 1 #:a 2 3)                        => :rt-err:
       (f 1 #:a 2 3 4)                      => :rt-err:))
  ;; check when other keys are allowed
  (t :rt-err: <= ((lambda/kw (#:key a #:body r) r) #:a 1 #:b 2)
     :rt-err: <= ((lambda/kw (#:key a) a) #:a 1 #:b 2)
     1 <= ((lambda/kw (#:key a #:rest r) a) #:a 1 #:b 2)
     1 <= ((lambda/kw (#:key a #:other-keys+body r) a) #:a 1 #:b 2)
     1 <= ((lambda/kw (#:key a #:allow-other-keys) a) #:a 1 #:b 2)
     :rt-err: <= ((lambda/kw (#:key a #:rest r #:forbid-other-keys) a)
                  #:a 1 #:b 2))
  ;; check when duplicate keys are allowed
  (t :rt-err: <= ((lambda/kw (#:key a #:body r) r) #:a 1 #:a 2)
     :rt-err: <= ((lambda/kw (#:key a) a) #:a 1 #:a 2)
     1        <= ((lambda/kw (#:key a #:rest r) a) #:a 1 #:a 2)
     :rt-err: <= ((lambda/kw (#:key a #:other-keys+body r) a) #:a 1 #:a 2)
     1        <= ((lambda/kw (#:key a #:allow-duplicate-keys) a) #:a 1 #:a 2)
     :rt-err: <= ((lambda/kw (#:key a #:rest r #:forbid-duplicate-keys) a)
                  #:a 1 #:a 2))
  ;; check when body is allowed
  (t :rt-err: <= ((lambda/kw (#:key a #:all-keys r) r) #:a 1 #:b 2 3)
     :rt-err: <= ((lambda/kw (#:key a #:all-keys r) r) #:a 1 #:b 2 3 4)
     :rt-err: <= ((lambda/kw (#:key a #:other-keys r) r) #:a 1 #:b 2 3)
     :rt-err: <= ((lambda/kw (#:key a #:other-keys r) r) #:a 1 #:b 2 3 4)
     '(#:a 1 #:b 2 3)   <= ((lambda/kw (#:key a #:rest r) r) #:a 1 #:b 2 3)
     '(#:a 1 #:b 2 3 4) <= ((lambda/kw (#:key a #:rest r) r) #:a 1 #:b 2 3 4))
  (let ([f (lambda/kw (#:key a #:body r) r)])
    (t '(3)   <= (f #:a 1 3)
       '(3 4) <= (f #:a 1 3 4)
       :rt-err: <= (f #:a 1 #:a 2 3)
       :rt-err: <= (f #:a 1 #:a 2 3 4)))
  (let ([f (lambda/kw (#:key a #:body r #:allow-duplicate-keys) r)])
    (t '(3)   <= (f #:a 1 3)
       '(3 4) <= (f #:a 1 3 4)
       '(3)   <= (f #:a 1 #:a 2 3)
       '(3 4) <= (f #:a 1 #:a 2 3 4)))
  (t '(#:a 1 #:b 2) <= ((lambda/kw (#:key a #:all-keys r #:allow-body) r)
                        #:a 1 #:b 2 3)
     :rt-err: <= ((lambda/kw (#:key x y) (list x y)) #:x)
     :rt-err: <= ((lambda/kw (#:key x y) (list x y)) #:x 1 #:x)
     :rt-err: <= ((lambda/kw (#:key x y) (list x y)) #:x #:x #:x))

  ;; optionals and keys
  (let ([f (lambda/kw (#:optional a b #:key c d) (list a b c d))])
    ;; the parts that are commented out are relying on the old (CL-like)
    ;; behavior of always treating the first arguments as optionals.  Now a
    ;; keyword marks the end of the optionals.
    (t '(#f #f #f #f)    <= (f)
       '(1 #f #f #f)     <= (f 1)
       '(1 2 #f #f)      <= (f 1 2)
       ;; '(#:c #:d #f #f)  <= (f #:c #:d)
       ;; '(#:c 1 #f #f)    <= (f #:c 1)
       '(1 2 #:d #f)     <= (f 1 2 #:c #:d)
       ;; '(#:c #:d #:d #f) <= (f #:c #:d #:c #:d)
       ;; '(#:c 1 #:d #f)   <= (f #:c 1 #:c #:d)
       ;; Test new behavior on the commented expressions that are valid
       '(#f #f #:d #f)   <= (f #:c #:d)
       '(#f #f 1 #f)     <= (f #:c 1)
       ;; Now test the new behavior
       '(#f #f #f 2)     <= (f #:d 2)
       '(1 #f #f 2)      <= (f 1 #:d 2)
       '(1 2 #f 2)       <= (f 1 2 #:d 2)
       '(1 #f #f 2)      <= (f 1 #f #:d 2)))
  (let ([f (lambda/kw (x #:optional a b #:key c d) (list x a b c d))])
    ;; also test that the required argument is still working fine
    (t '(0 #f #f #f 2)   <= (f 0 #:d 2)
       '(0 1 #f #f 2)    <= (f 0 1 #:d 2)
       '(0 1 2 #f 2)     <= (f 0 1 2 #:d 2)
       '(0 1 #f #f 2)    <= (f 0 1 #f #:d 2)
       '(#:x 1 #f #f 2)  <= (f #:x 1 #f #:d 2)
       ;; and test errors
       :rt-err:          <= (f 0 #:c 2 #:c 3)))

  ;; multi-level arg lists with #:body specs
  (let ([f (lambda/kw (#:key x y #:body (z)) (list x y z))])
    (t (f 3) => '(#f #f 3)
       (f #:y 2 3) => '(#f 2 3)
       (f #:y 2) => :rt-err:
       (f #:y 2 3 4) => :rt-err:))
  (let ([f (lambda/kw (#:key x y #:body (z . r)) (list x y z r))])
    (t (f 3)         => '(#f #f 3 ())
       (f #:y 2 3)   => '(#f 2 3 ())
       (f #:y 2)     => :rt-err:
       (f #:y 2 3 4) => '(#f 2 3 (4))))
  (let ([f (lambda/kw (#:key x y #:body (a #:key (xx #:x #f) (yy #:y #f)
                                           #:allow-duplicate-keys)
                                 #:allow-duplicate-keys)
             (list x y a xx yy))])
    (t '(1 #f 2 3 #f)   <= (f #:x 1 2 #:x 3)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x        2 #:x 3 #:y #:x)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x #:x 11 2 #:x 3 #:y #:x)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x        2 #:x 3 #:y #:x #:x 33)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x #:x 11 2 #:x 3 #:y #:x #:x 33)))
  (let ([f (lambda/kw (#:key x y #:body (a #:key (xx #:x #f) (yy #:y #f)
                                           #:allow-duplicate-keys))
             (list x y a xx yy))])
    (t '(1 #f 2 3 #f)   <= (f #:x 1 2 #:x 3)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x        2 #:x 3 #:y #:x)
       :rt-err:         <= (f #:x 1 #:y #:x #:x 11 2 #:x 3 #:y #:x)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x        2 #:x 3 #:y #:x #:x 33)
       :rt-err:         <= (f #:x 1 #:y #:x #:x 11 2 #:x 3 #:y #:x #:x 33)))
  (let ([f (lambda/kw (#:key x y #:body (a #:key (xx #:x #f) (yy #:y #f))
                                 #:allow-duplicate-keys)
             (list x y a xx yy))])
    (t '(1 #f 2 3 #f)   <= (f #:x 1 2 #:x 3)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x        2 #:x 3 #:y #:x)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x #:x 11 2 #:x 3 #:y #:x)
       :rt-err:         <= (f #:x 1 #:y #:x        2 #:x 3 #:y #:x #:x 33)
       :rt-err:         <= (f #:x 1 #:y #:x #:x 11 2 #:x 3 #:y #:x #:x 33)))
  (let ([f (lambda/kw (#:key x y #:body (a #:key (xx #:x #f) (yy #:y #f)))
             (list x y a xx yy))])
    (t '(1 #f 2 3 #f)   <= (f #:x 1 2 #:x 3)
       '(1 #:x 2 3 #:x) <= (f #:x 1 #:y #:x        2 #:x 3 #:y #:x)
       :rt-err:         <= (f #:x 1 #:y #:x #:x 11 2 #:x 3 #:y #:x)
       :rt-err:         <= (f #:x 1 #:y #:x        2 #:x 3 #:y #:x #:x 33)
       :rt-err:         <= (f #:x 1 #:y #:x #:x 11 2 #:x 3 #:y #:x #:x 33)))

  ;; #:allow-anything does not check for imbalanced keyword-values
  (let ([f (lambda/kw (#:key x #:allow-anything) x)])
    (t (f #:x 1)           => 1
       (f #:x 1 2)         => 1
       (f #:x 1 #:y)       => 1
       (f #:x 1 #:x)       => 1
       (f #:x 1 #:y 1)     => 1
       (f #:x 1 #:x 2)     => 1
       (f #:x 1 #:x 2 #:y) => 1))
  (t '(#:x 1 #:z) <= ((lambda/kw (#:key x #:allow-anything #:rest r) r)
                      #:x 1 #:z))
  (t '(#:z) <= ((lambda/kw (#:key x #:allow-anything #:body r) r) #:x 1 #:z))

  ;; #:body without #:keys forbids all keys
  (let ([f1 (lambda/kw (#:body b) b)]
        [f2 (lambda/kw (x #:body b) (cons x b))])
    (t (f1 1 2) => '(1 2)
       (f2 1 2) => '(1 2)
       (f1 #:foo 1 1 2) => :rt-err:
       (f2 1 #:foo 1 2) => :rt-err:))

  ;; make sure that internal definitions work
  (let ([f (lambda/kw (#:key x) (define xx x) xx)])
    (t #f <= (f)
       1  <= (f #:x 1)))

  ;; test syntax errors
  (t :st-err: <= (lambda/kw (x #:blah y) 1)
     :st-err: <= (lambda/kw (x #:rest) 1)
     :st-err: <= (lambda/kw (x #:key k . r) 1)
     :st-err: <= (lambda/kw (x #:key k #:key o) 1)
     :st-err: <= (lambda/kw (x #:key k #:optional o) 1)
     :st-err: <= (lambda/kw (x #:optional k #:optional o) 1)
     :st-err: <= (lambda/kw (x #:rest r #:optional o) 1)
     :st-err: <=
       (lambda/kw (x #:rest r #:forbid-other-keys #:allow-other-keys) 1)
     :st-err: <=
       (lambda/kw (x #:rest r #:allow-other-keys #:forbid-other-keys) 1)
     :st-err: <=
       (lambda/kw (x #:rest r #:forbid-duplicate-keys #:allow-duplicate-keys) 1)
     :st-err: <=
       (lambda/kw (x #:rest r #:allow-duplicate-keys #:forbid-duplicate-keys) 1)
     :st-err: <= (lambda/kw (x #:rest r #:forbid-body #:allow-body) 1)
     :st-err: <= (lambda/kw (x #:rest r #:allow-body #:forbid-body) 1)
     :st-err: <= (lambda/kw (x #:rest r #:forbid-anything #:allow-anything) 1)
     :st-err: <= (lambda/kw (x #:rest r #:allow-anything #:forbid-anything) 1)
     :st-err: <= (lambda/kw (#:key a #:forbid-other-keys #:allow-anything) 1)
     :st-err: <=
       (lambda/kw (#:key a #:forbid-duplicate-keys #:allow-anything) 1)
     :st-err: <= (lambda/kw (#:key a #:forbid-other-keys #:allow-anything) 1)
     :st-err: <=
       (lambda/kw (#:key a #:forbid-duplicate-keys #:allow-anything) 1)
     :st-err: <=
       (lambda/kw (#:key a #:forbid-body #:allow-anything) 1)
     :st-err: <= (lambda/kw (x #:rest r1 #:rest r2) 1)
     :st-err: <= (lambda/kw (x #:rest) 1)
     :st-err: <= (lambda/kw (x #:rest r1 r2) 1)
     ;; :st-err: <= (lambda/kw (x #:body b) 1) ; valid!
     :st-err: <= (lambda/kw (x x) 1)
     :st-err: <= (lambda/kw (x #:optional [x 1]) 1)
     :st-err: <= (lambda/kw (x #:key [x 1]) 1)
     :st-err: <= (lambda/kw (x #:rest x) 1)
     :st-err: <= (lambda/kw (x #:body x) 1)
     :st-err: <= (lambda/kw (x #:optional 3) 1)
     :st-err: <= (lambda/kw (x #:optional "3") 1)
     :st-err: <= (lambda/kw (x #:optional [(x) 3]) 1)
     :st-err: <= (lambda/kw (x #:key 3) 1)
     :st-err: <= (lambda/kw (x #:key "3") 1)
     :st-err: <= (lambda/kw (x #:key [(y) 3]) 1)
     :st-err: <= (lambda/kw (x #:key [x]) 1)
     :st-err: <= (lambda/kw (x #:key [y 1 2]) 1)
     :st-err: <= (lambda/kw (x #:key [y #:y 1 2]) 1)
     :st-err: <= (lambda/kw (x #:rest 3) 1)
     :st-err: <= (lambda/kw (x #:rest "3") 1)
     :st-err: <= (lambda/kw (x #:rest (x)) 1)
     :st-err: <= (lambda/kw (x #:body 3) 1)
     :st-err: <= (lambda/kw (x #:key y #:body 3) 1)
     :st-err: <= (lambda/kw (x #:body "3") 1)
     :st-err: <= (lambda/kw (x #:key y #:body "3") 1)
     :st-err: <= (lambda/kw (x #:body (x)) 1)
     :st-err: <= (lambda/kw (x #:body x #:allow-other-keys) 1)
     :st-err: <= (lambda/kw (x #:optional ()) 1)
     :st-err: <= (lambda/kw (x #:optional (x y z)) 1)
     ;; :st-err: <= (lambda/kw (x #:other-keys z) 1)      <-- these are all
     ;; :st-err: <= (lambda/kw (x #:other-keys+body z) 1) <-- fine!
     ;; :st-err: <= (lambda/kw (x #:all-keys z) 1)        <-- (see below)
     :st-err: <= (lambda/kw (x #:other-keys z #:forbid-other-keys) 1)
     :st-err: <= (lambda/kw (x #:all-keys   z #:forbid-other-keys) 1)
     :st-err: <= (lambda/kw (x #:key y #:allow-other-keys z) 1)
     :st-err: <= (lambda/kw (x #:key y #:forbid-body z) 1)
     :st-err: <= (lambda/kw (x #:key y #:allow-body #:rest r #:forbid-body) 1)
     :st-err: <=
       (lambda/kw (x #:key y #:forbid-other-keys #:rest r #:allow-other-keys) 1)
     :st-err: <= (lambda/kw (x #:key y z #:body (x)) x)
     :st-err: <= (lambda/kw (#:key a #:body r #:forbid-body) r)
     :st-err: <= (lambda/kw (#:key a #:other-keys r #:forbid-other-keys) r))

  ;; it is ok to have no keys, and still specify all-keys etc
  (let ([f (lambda/kw (x #:all-keys ak) (list x ak))])
    (t (f 1)             => '(1 ())
       (f 1 #:a 2)       => '(1 (#:a 2))
       (f 1 #:a 2 #:b 3) => '(1 (#:a 2 #:b 3))
       (f 1 #:a 2 #:a 3) => '(1 (#:a 2 #:a 3))
       (f 1 #:a 2 #:a 3) => '(1 (#:a 2 #:a 3))))

  )

(report-errs)
