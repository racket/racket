
(load-relative "loadtest.rktl")

(require racket/class
         racket/trait)

(Section 'trait)

;; ----------------------------------------
;; fields

(test 'yes 't
      (send (new ((trait->mixin (trait
                                 (field [ok? 'yes])
                                 (define/public (check) ok?)))
                  object%))
            check))

(test 'no 't
      (send (new ((trait->mixin 
                   (trait-sum
                    (trait-exclude-field (trait
                                          (field [ok? 'yes])
                                          (define/public (check) ok?))
                                         ok?)
                    (trait (field [ok? 'no]))))
                  object%))
            check))

(err/rt-test (trait-sum
              (trait (field [x 'x]))
              (trait (field [x 'y]))))

(test #t trait? (trait
                 (inherit-field f)))

;; ----------------------------------------
;; internal and external names

(test 'hi 't
      (send (new ((trait->mixin (trait
                                 (public hello)
                                 (define (hello) 'hi)))
                  object%))
            hello))

(test 'hi 't
      (send (new ((trait->mixin 
                   (trait
                    (public [nihao hello])
                    (define (nihao) 'hi)))
                  object%))
            hello))

(test 'hey 't
      (send (new ((trait->mixin 
                   (trait-sum
                    (trait
                     (public [nihao howdy])
                     (define (nihao) 'hey))
                    (trait
                     (public hello)
                     (inherit [hola howdy])
                     (define (hello) (hola)))))
                  object%))
            hello))

;; ----------------------------------------

(test '(zoo (100))
      't
      (send (new (class ((trait->mixin (trait (define/augment (y) (list (inner #t y)))))
                         (class object% 
                           (define/pubment (y) (list 'zoo (inner #f y)))
                           (super-new)))
                   (define/augment (y) 100)
                   (super-new)))
            y))

(test '(too (200))
      't
      (send (new (class ((trait->mixin (trait (define/augment (y) (list (inner #t y)))
                                              (define/augment (x) (list (inner #f x)))))
                         (class object% 
                           (define/pubment (y) (list 'zoo (inner #f y)))
                           (define/pubment (x) (list 'too (inner #f x)))
                           (super-new)))
                   (define/augment (y) 100)
                   (define/augment (x) 200)
                   (super-new)))
            x))

(test '(8 (12 (#t))) ; OR '(8 (12 100)) !!!!!!!!!!!!!!!
      't
      (send (new (class ((trait->mixin (trait (define/augment (x) (list 12 (inner #f y)))
                                              (define/augment (y) (list (inner #t x)))))
                         (class object% 
                           (define/pubment (x) (list 8 (inner 90 x)))
                           (define/pubment (y) (list 'zoo (inner #f y)))
                           (super-new)))
                   (define/augment (y) 100)
                   (super-new)))
            x))

(test '(8 (12 (#t))) ;; OR '(8 (12 #f)) !!!!!!!!!!!!!!!!!!!!!!!!!
      't
      (send (new (class ((trait->mixin (trait (define/augment (x) (list 12 (inner #f y)))
                                              (define/augment (y) (list (inner #t x)))))
                         (class object% 
                           (define/pubment (x) (list 8 (inner 90 x)))
                           (define/pubment (y) (list 'zoo (inner #f y)))
                           (super-new)))
                   (super-new)))
            x))

(test '(zoo (#t))
      't
      (send (new (class ((trait->mixin (trait (define/augment (x) (list 12 (inner #f y)))
                                              (define/augment (y) (list (inner #t x)))))
                         (class object% 
                           (define/pubment (x) (list 8 (inner 90 x)))
                           (define/pubment (y) (list 'zoo (inner #f y)))
                           (super-new)))
                   (define/augment (y) 100)
                   (super-new)))
            y))

(test '(zoo (100))
      't
      (send (new (class ((trait->mixin (trait (define/augment (x) (list 12 (inner #f y)))
                                              (define/augment (y) (list (inner #t x)))))
                         (class object% 
                           (define/pubment (x) (list 8 (inner 90 x)))
                           (define/pubment (y) (list 'zoo (inner #f y)))
                           (super-new)))
                   (define/augment (x) 100)
                   (super-new)))
            y))

(test '(12 100)
      't
      (send (new (class ((trait->mixin (trait (define/overment (x) (list 12 (inner #f x)))))
                         (class object% 
                           (define/public (x) 'zoo)
                           (super-new)))
                   (define/augment (x) 100)
                   (super-new)))
            x))

;;----------------------------------------

(report-errs)
