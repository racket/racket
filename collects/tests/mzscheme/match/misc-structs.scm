;; structure tests

(define-struct shape (color))
(define-struct (ovoid shape) (x-diam y-diam))
(define-struct (circle ovoid) (radius))
(define-struct (rectangle shape) (height width))
(define-struct (square rectangle) (width))
(define c (make-circle 5 4 3 2))


(define-struct test-struct (a b c d))
  (define inst-struct-name29 (make-test-struct `#\b `2 `(+ 4 5) `symbols))
  (define inst-struct-name33
    (make-test-struct
      `(,inst-struct-name29 (+ 1 2) #\c (+ 4 5))
      `()
      `(these #\d #t)
      `16))
  (define inst-struct-name46 (make-test-struct `#\c `#\d `16 `4))
  (define inst-struct-name54 (make-test-struct `#\c `4 `#\c `#\b))
  (define inst-struct-name72 (make-test-struct `#f `4 `4 `#\c))
  (define inst-struct-name75
    (make-test-struct
      `(+ 4 (+ 1 1))
      `,inst-struct-name72
      `#3(9 #\b 9)
      `(+ 4 5)))
  (define inst-struct-name77
    (make-test-struct `#\c `(#\d symbols these) `,inst-struct-name75 `these))
  (define inst-struct-name84 (make-test-struct `#\b `4 `#\c `#\d))
  (define inst-struct-name90 (make-test-struct `symbols `#\a `#\c `symbols))
  (define inst-struct-name116
    (make-test-struct `symbols `(+ 4 (+ 1 1)) `4 `#\b))
  (define inst-struct-name145 (make-test-struct `(+ 1 2) `4 `symbols `these))
  (define inst-struct-name147
    (make-test-struct
      `#f
      `((+ 4 (+ 1 1)) these 6)
      `#4(are () ,inst-struct-name145 (+ 4 (+ 1 1)))
      `()))
  (define inst-struct-name158
    (make-test-struct `18 `(#\b 2 #t) `() `(+ 4 (+ 1 1))))
  (define inst-struct-name164
    (make-test-struct
      `()
      `,inst-struct-name158
      `#\d
      `(#3(these these 1) #\b (#\c #f (+ 4 (+ 1 1))) . #f)))
  (define inst-struct-name173 (make-test-struct `#\b `#\b `#\b `are))
  (define inst-struct-name175
    (make-test-struct `() `#&((+ 1 2) #t (+ 4 5)) `,inst-struct-name173 `#t))
  (define inst-struct-name190
    (make-test-struct
      `,inst-struct-name175
      `()
      `#f
      `((are #\b 5) these #4((+ 1 2) 8 these (+ 1 2)) . #f)))
  (define inst-struct-name198 (make-test-struct `these `#t `7 `(+ 4 5)))
  (define inst-struct-name218
    (make-test-struct `(+ 4 (+ 1 1)) `#4((+ 4 5) symbols 2 4) `#\a `#\d))
  (define inst-struct-name224 (make-test-struct `#\b `(+ 1 2) `4 `these))
  (define inst-struct-name250 (make-test-struct `7 `symbols `are `these))
  (define inst-struct-name260
    (make-test-struct
      `(symbols #\b symbols)
      `()
      `(symbols (+ 1 2) 5 #\d)
      `(#\a #\c #\c . 1)))
  (define inst-struct-name272
    (make-test-struct `#&(#\b #\a #f) `#3((+ 4 5) #\b #t) `#\b `(+ 4 (+ 1 1))))
  (define inst-struct-name281 (make-test-struct `#\b `5 `(+ 1 2) `4))
  (define inst-struct-name286 (make-test-struct `#\b `(+ 4 5) `0 `1))
  (define inst-struct-name288
    (make-test-struct
      `#&(#\b #\b #\b)
      `(these (+ 1 2) #\c)
      `,inst-struct-name281
      `,inst-struct-name286))
  (define inst-struct-name304 (make-test-struct `#\d `are `#f `7))
  (define inst-struct-name305
    (make-test-struct `#\d `(7 #\c #\c) `2 `,inst-struct-name304))
  (define inst-struct-name317 (make-test-struct `9 `#\c `symbols `#\b))
  (define inst-struct-name318
    (make-test-struct `() `,inst-struct-name317 `symbols `0))
  (define inst-struct-name334 (make-test-struct `#\d `#f `(+ 4 5) `7))
  (define inst-struct-name350
    (make-test-struct
      `()
      `(+ 4 (+ 1 1))
      `#&(#\d 2 #f)
      `#4(#\a these #t (+ 1 2))))
  (define inst-struct-name359
    (make-test-struct `(+ 4 (+ 1 1)) `(+ 4 5) `#\b `#\a))
  (define inst-struct-name372 (make-test-struct `symbols `6 `4 `#\c))
  (define inst-struct-name385 (make-test-struct `#\d `8 `#\a `#\a))
  (define inst-struct-name399
    (make-test-struct `4 `() `#\a `#3(#f (+ 4 5) #f)))
  (define inst-struct-name408
    (make-test-struct `these `(+ 4 (+ 1 1)) `((+ 4 (+ 1 1)) (+ 4 5) are) `#\a))
  (define inst-struct-name417
    (make-test-struct `6 `#\b `(+ 4 (+ 1 1)) `(+ 4 5)))
  (define inst-struct-name424
    (make-test-struct
      `(#4((+ 4 (+ 1 1)) #\b 7 #\a)
        ,inst-struct-name417
        ((+ 4 5) are 9 . these)
        .
        #f)
      `(#t #\a #f)
      `()
      `(are #\b 4)))
  (define inst-struct-name431
    (make-test-struct
      `these
      `3
      `#4((+ 4 (+ 1 1)) symbols 1 are)
      `(8 #t (+ 4 (+ 1 1)))))
  (define inst-struct-name433
    (make-test-struct `() `1 `,inst-struct-name431 `are))
  (define inst-struct-name477
    (make-test-struct `#t `() `these `#4((+ 4 5) #t #t #\c)))
  (define inst-struct-name487
    (make-test-struct `((7 these #\b) (+ 4 (+ 1 1)) #t . 0) `() `(+ 4 5) `7))
  (define inst-struct-name514
    (make-test-struct `(+ 4 5) `#\b `(+ 4 (+ 1 1)) `#t))
  (define inst-struct-name522
    (make-test-struct
      `,inst-struct-name514
      `(#\b #t #\d . #\a)
      `(7 5 #f . #\b)
      `#\c))
  (define inst-struct-name530 (make-test-struct `8 `5 `#\c `7))
  (define inst-struct-name541 (make-test-struct `(+ 1 2) `4 `symbols `(+ 1 2)))
  (define inst-struct-name545
    (make-test-struct `,inst-struct-name541 `8 `() `()))
  (define inst-struct-name547
    (make-test-struct
      `are
      `(9 (#f symbols #\d) ,inst-struct-name530 symbols)
      `(4 #\b #t)
      `,inst-struct-name545))
  (define inst-struct-name554
    (make-test-struct `#3(#\d #f #\d) `are `1 `(+ 4 (+ 1 1))))
  (define inst-struct-name597 (make-test-struct `#\d `(+ 4 (+ 1 1)) `2 `#\a))
  (define inst-struct-name600
    (make-test-struct
      `#f
      `(these #\b 7)
      `(18 #3(#f (+ 4 5) #t) ,inst-struct-name597 (#t 8 these))
      `(+ 4 (+ 1 1))))
  (define inst-struct-name618
    (make-test-struct `#&((+ 4 5) #\d #\c) `are `() `()))
  (define inst-struct-name642
    (make-test-struct `#\d `(+ 4 5) `(+ 4 5) `(+ 1 2)))
  (define inst-struct-name661 (make-test-struct `#\a `#\c `#f `#\c))
  (define inst-struct-name669
    (make-test-struct
      `,inst-struct-name661
      `(#\b #\b #f . #\a)
      `(#\d #t (+ 1 2) . #\b)
      `#t))

 (define inst-struct-name3134 (make-test-struct `6 `#t `(+ 4 (+ 1 1)) `#\a))
  (define inst-struct-name3144 (make-test-struct `#\a `3 `(+ 4 5) `7))
  (define inst-struct-name3157
    (make-test-struct
      `#&((#f (+ 1 2) #\d #\b) (#\d 1 symbols) ,inst-struct-name3134)
      `((5 7 symbols #t)
        ,inst-struct-name3144
        #((+ 4 (+ 1 1)) #f #\a #\d)
        .
        #\d)
      `(#\a (+ 4 (+ 1 1)) these)
      `(#\b 3 4 #&((+ 4 (+ 1 1)) #f 2))))
  (define inst-struct-name3197
    (make-test-struct `(8 #\b (+ 1 2)) `(+ 1 2) `#\a `((+ 1 2) #f 5)))
  (define inst-struct-name3200
    (make-test-struct
      `#(#\b symbols (+ 1 2))
      `symbols
      `,inst-struct-name3197
      `5))
  (define inst-struct-name3228 (make-test-struct `these `3 `#\d `#t))
  (define inst-struct-name3234 (make-test-struct `(+ 4 5) `#\a `#\a `#f))
  (define inst-struct-name3239
    (make-test-struct `() `#(#\d #\b #\b) `#\c `#\b))
  (define inst-struct-name3252
    (make-test-struct
      `(#t #\c #\c . are)
      `#((+ 1 2) (+ 4 5) #\b 9)
      `16
      `(+ 4 5)))
  (define inst-struct-name3287
    (make-test-struct `#\d `#((+ 4 5) #t (+ 4 5) #t) `#t `(+ 4 (+ 1 1))))
  (define inst-struct-name3297
    (make-test-struct `these `#f `#(are symbols #\c) `#(these symbols #t #\d)))
  (define inst-struct-name3307 (make-test-struct `#t `(+ 4 (+ 1 1)) `#\d `#f))
  (define inst-struct-name3312
    (make-test-struct
      `#&(symbols #\d #\c)
      `,inst-struct-name3307
      `(these are (+ 1 2) (+ 4 5))
      `#f))
  (define inst-struct-name3322
    (make-test-struct
      `(#t 6 #\c)
      `(#t #\d 2 . #\a)
      `#(#f 6 #\b)
      `#&(#f 7 symbols)))
  (define inst-struct-name3355 (make-test-struct `(+ 1 2) `these `#t `#t))
  (define inst-struct-name3367 (make-test-struct `#\d `#\a `6 `(+ 1 2)))
  (define inst-struct-name3372 (make-test-struct `symbols `#\a `3 `#\c))
  (define inst-struct-name3396 (make-test-struct `0 `#\d `#\a `#\b))
  (define inst-struct-name3419 (make-test-struct `#\c `#\c `symbols `9))
  (define inst-struct-name3444
    (make-test-struct
      `(#\c (+ 4 (+ 1 1)) (+ 4 5))
      `()
      `((6 #\a 2) 0 #(these #\c these 1) . 5)
      `#(#f #\a #\d)))
  (define inst-struct-name3455
    (make-test-struct
      `()
      `(#\d #t symbols . #\d)
      `#&(#\d (+ 4 (+ 1 1)) these)
      `2))
  (define inst-struct-name3474
    (make-test-struct `(#t #\c #f . #\d) `#\a `() `(#t #\a #\a)))
  (define inst-struct-name3482 (make-test-struct `symbols `#\b `#f `#f))
  (define inst-struct-name3493
    (make-test-struct
      `16
      `(,inst-struct-name3482 #(#f #\c (+ 4 5) #f) #&((+ 4 5) #\d are) (+ 4 5))
      `#(#t #f 4)
      `#\d))
  (define inst-struct-name3505 (make-test-struct `4 `(+ 4 (+ 1 1)) `#f `these))
  (define inst-struct-name3512
    (make-test-struct
      `#&(symbols #f 2)
      `()
      `#t
      `(are (+ 4 (+ 1 1)) (+ 4 (+ 1 1)))))
  (define inst-struct-name3518 (make-test-struct `8 `#\b `#\c `symbols))
  (define inst-struct-name3527 (make-test-struct `#\a `#t `4 `#t))
  (define inst-struct-name3550
    (make-test-struct `(+ 4 (+ 1 1)) `(#t 8 symbols) `(#\a #\c #t) `0))
  (define inst-struct-name3570 (make-test-struct `#\b `#\c `#\d `#\c))
  (define inst-struct-name3577 (make-test-struct `#\b `(+ 1 2) `#\b `#\b))
  (define inst-struct-name3578
    (make-test-struct `,inst-struct-name3570 `16 `,inst-struct-name3577 `9))
  (define inst-struct-name3595
    (make-test-struct
      `(0 these are)
      `(#\d #f (+ 4 (+ 1 1)) . #\c)
      `#(#\b #\a these symbols)
      `#\b))
  (define inst-struct-name3600 (make-test-struct `#\a `0 `2 `2))
  (define inst-struct-name3605 (make-test-struct `are `1 `3 `#\d))
  (define inst-struct-name3625
    (make-test-struct
      `()
      `#\a
      `#(#\a #\c (+ 1 2) #t)
      `#((+ 4 (+ 1 1)) #\b 5 these)))
  (define inst-struct-name3633 (make-test-struct `(+ 4 (+ 1 1)) `are `#f `#\c))
  (define inst-struct-name3639
    (make-test-struct `#\a `,inst-struct-name3633 `#&(are symbols 4) `#\c))
  (define inst-struct-name3677
    (make-test-struct `#(#\b are #\b) `#\a `#\c `(#\b #\c #\d . #\d)))
  (define inst-struct-name3701
    (make-test-struct `#\b `9 `#&(#\c (#f #\d (+ 1 2)) are) `#\d))
  (define inst-struct-name3712
    (make-test-struct `#\d `#\d `(+ 4 (+ 1 1)) `are))
  (define inst-struct-name3744
    (make-test-struct `symbols `(+ 1 2) `symbols `#\b))
  (define inst-struct-name3752 (make-test-struct `#\b `#t `(+ 4 5) `#\b))
  (define inst-struct-name3753
    (make-test-struct
      `,inst-struct-name3744
      `#(#\c #\d #\d)
      `#\b
      `,inst-struct-name3752))
  (define inst-struct-name3755
    (make-test-struct `(these 0 3) `(+ 4 5) `2 `,inst-struct-name3753))
  (define inst-struct-name3762 (make-test-struct `these `(+ 1 2) `2 `#\d))
  (define inst-struct-name3776 (make-test-struct `#\b `#\b `symbols `these))
  (define inst-struct-name3777
    (make-test-struct
      `#&(symbols (+ 4 (+ 1 1)) (+ 1 2))
      `#(#\c 6 #\c #t)
      `#\b
      `,inst-struct-name3776))
  (define inst-struct-name3805
    (make-test-struct `#\d `#&((+ 1 2) #\d 1) `(+ 4 5) `#\b))
  (define inst-struct-name3817 (make-test-struct `1 `(+ 1 2) `#\d `(+ 1 2)))
  (define inst-struct-name3820
    (make-test-struct
      `#\d
      `(2 (+ 4 (+ 1 1)) symbols)
      `,inst-struct-name3817
      `#((+ 1 2) 8 (+ 4 5))))
  (define inst-struct-name3822
    (make-test-struct
      `,inst-struct-name3805
      `#&(#&(#f (+ 4 (+ 1 1)) #\d) #\c (#t #\c #f))
      `(+ 4 5)
      `,inst-struct-name3820))
  (define inst-struct-name3851 (make-test-struct `#f `(+ 1 2) `#\a `#f))
  (define inst-struct-name3854
    (make-test-struct `#\b `#\b `,inst-struct-name3851 `18))
  (define inst-struct-name3856
    (make-test-struct
      `#&(#t #\a symbols)
      `(((+ 4 (+ 1 1)) (+ 4 5) #\b) #t 12 #(these #\c #f))
      `(#(#\d these #\d) #t 7 #f)
      `,inst-struct-name3854))
  (define inst-struct-name3890
    (make-test-struct `#f `(#t #f are symbols) `#\d `#t))
  (define inst-struct-name3900 (make-test-struct `(+ 4 (+ 1 1)) `0 `#\d `0))
  (define inst-struct-name3902
    (make-test-struct
      `,inst-struct-name3890
      `(+ 4 5)
      `()
      `#((#\a 7 #\b (+ 1 2)) ,inst-struct-name3900 #\b ())))
  (define inst-struct-name3928
    (make-test-struct `8 `#\c `#\c `#(8 #\d #f #\a)))
  (define inst-struct-name3939 (make-test-struct `6 `#\d `(+ 4 (+ 1 1)) `7))
  (define inst-struct-name3944
    (make-test-struct
      `#\d
      `,inst-struct-name3928
      `(,inst-struct-name3939 () #&(#\b #f #\b) #t)
      `(4 (+ 1 2) 3)))
  (define inst-struct-name3952
    (make-test-struct `() `7 `(+ 1 2) `(#\d #\d #\c)))
  (define inst-struct-name3957
    (make-test-struct `#t `(#\a #\b symbols) `#\d `10))
  (define inst-struct-name3962 (make-test-struct `4 `(+ 4 (+ 1 1)) `#\c `6))
  (define inst-struct-name3973 (make-test-struct `#t `1 `2 `#t))
  (define inst-struct-name3974
    (make-test-struct `(+ 4 (+ 1 1)) `9 `#f `,inst-struct-name3973))
  (define inst-struct-name3986 (make-test-struct `these `#\d `#f `#t))
  (define inst-struct-name3998 (make-test-struct `(+ 4 5) `#f `0 `#\a))
  (define inst-struct-name4008 (make-test-struct `#\c `#t `#\c `#t))
  (define inst-struct-name4019
    (make-test-struct
      `((+ 4 5) #\b these . 1)
      `,inst-struct-name4008
      `(2 #f #t #\d)
      `#(9 #\b 2)))
  (define inst-struct-name4024
    (make-test-struct `#\a `(+ 4 (+ 1 1)) `(+ 4 (+ 1 1)) `#t))
  (define inst-struct-name4026
    (make-test-struct `(+ 1 2) `,inst-struct-name4024 `() `#\d))
  (define inst-struct-name4028
    (make-test-struct
      `(#\a 8 #f)
      `,inst-struct-name4019
      `()
      `,inst-struct-name4026))
  (define inst-struct-name4042
    (make-test-struct
      `#\b
      `(5 (+ 4 5) (+ 4 (+ 1 1)) #\d)
      `(are #\b these . 2)
      `#\b))
  (define inst-struct-name4060 (make-test-struct `0 `symbols `#f `#\a))
  (define inst-struct-name4063
    (make-test-struct `8 `#&(5 #\c #f) `,inst-struct-name4060 `10))
  (define inst-struct-name4064
    (make-test-struct `4 `#f `0 `,inst-struct-name4063))
  (define inst-struct-name4088
    (make-test-struct
      `#\a
      `(#\d symbols these)
      `(#\a are 2 #(#\b 0 symbols #\d))
      `#f))
  (define inst-struct-name4109 (make-test-struct `4 `symbols `(+ 4 5) `#\b))
  (define inst-struct-name4137
    (make-test-struct `(+ 4 5) `(+ 4 5) `are `these))
  (define inst-struct-name4145
    (make-test-struct
      `,inst-struct-name4137
      `#\d
      `#(2 #\c (+ 1 2) #\c)
      `(#\c 6 9 . 7)))
  (define inst-struct-name4146
    (make-test-struct `these `(+ 4 (+ 1 1)) `(+ 1 2) `,inst-struct-name4145))
  (define inst-struct-name4160 (make-test-struct `4 `#\d `2 `#\a))
  (define inst-struct-name4174
    (make-test-struct
      `these
      `#((#t (+ 1 2) #\c are) #(#\d (+ 4 5) #\a #\d) ,inst-struct-name4160 #t)
      `(#\b #\b 9)
      `#(#\c #(#\c #\c #\c #f) these ())))
  (define inst-struct-name4181 (make-test-struct `#\c `#(#f #\c #t) `#f `#t))
  (define inst-struct-name4194 (make-test-struct `#\a `#t `6 `symbols))
  (define inst-struct-name4208
    (make-test-struct
      `(#f 7 #t)
      `,inst-struct-name4181
      `#&(#(#f symbols (+ 1 2) #\a)
          ((+ 1 2) symbols #\b #\c)
          ,inst-struct-name4194)
      `#t))
  (define inst-struct-name4214
    (make-test-struct `(+ 4 5) `symbols `#\d `these))
  (define inst-struct-name4239
    (make-test-struct `(#\c 6 (+ 4 5) . #f) `() `14 `(#\a (+ 4 5) #\a . #\c)))
  (define inst-struct-name4241
    (make-test-struct
      `()
      `#(these (#\b #\a these #t) (#\a #\c are) (these 4 #\d))
      `,inst-struct-name4239
      `()))
  (define inst-struct-name4252 (make-test-struct `#t `5 `symbols `(+ 1 2)))
  (define inst-struct-name4260
    (make-test-struct `() `symbols `#t `(#\d (+ 4 5) #\b 6)))
  (define inst-struct-name4266
    (make-test-struct `symbols `symbols `(+ 4 (+ 1 1)) `(+ 1 2)))
  (define inst-struct-name4280 (make-test-struct `#\c `8 `#\d `#t))
  (define inst-struct-name4287
    (make-test-struct `,inst-struct-name4280 `4 `#\b `(#\b #t #f . #t)))
  (define inst-struct-name4293
    (make-test-struct `#\c `#\a `symbols `(#\d #t (+ 4 (+ 1 1)))))
  (define inst-struct-name4314
    (make-test-struct
      `,inst-struct-name4293
      `8
      `(#(#\c #\a #t 5) #&((+ 1 2) 6 these) (+ 1 2) (#\c #\c #t))
      `#&(#(#\c (+ 4 (+ 1 1)) these #t) these (#\a symbols (+ 1 2)))))
  (define inst-struct-name4325
    (make-test-struct
      `#(symbols are (are (+ 4 5) (+ 4 (+ 1 1)) . #\b) 3)
      `(#\a these (+ 4 5))
      `()
      `(#\b #(#\a these #\b) 12 . #t)))
  (define inst-struct-name4343
    (make-test-struct `(+ 4 5) `#\a `#t `(+ 4 (+ 1 1))))
  (define inst-struct-name4348 (make-test-struct `(+ 1 2) `#\a `9 `#\a))
  (define inst-struct-name4351
    (make-test-struct
      `#f
      `#\c
      `(+ 1 2)
      `(,inst-struct-name4343
        ,inst-struct-name4348
        #(#\a (+ 4 5) (+ 4 5))
        #\c)))
  (define inst-struct-name4363 (make-test-struct `(+ 4 5) `are `#f `#t))
  (define inst-struct-name4369
    (make-test-struct
      `#\b
      `are
      `,inst-struct-name4363
      `((+ 4 (+ 1 1)) (+ 1 2) these #f)))
  (define inst-struct-name4376 (make-test-struct `#f `#\d `#t `are))
  (define inst-struct-name4391 (make-test-struct `#t `#\d `6 `#\d))
  (define inst-struct-name4405 (make-test-struct `(+ 4 5) `symbols `#f `#\b))
  (define inst-struct-name4409
    (make-test-struct
      `#(6 (+ 1 2) #\a)
      `,inst-struct-name4405
      `((+ 1 2) 8 8)
      `((+ 1 2) #\b #f)))
  (define inst-struct-name4429
    (make-test-struct
      `(#\d (+ 4 5) 1 . #\a)
      `#\a
      `#(are these are are)
      `(are #f #\a)))
  (define inst-struct-name4430
    (make-test-struct
      `,inst-struct-name4409
      `#&(5 #&(symbols #\b #\a) (4 6 #\a #\c))
      `,inst-struct-name4429
      `()))
  (define inst-struct-name4444
    (make-test-struct
      `(#\b (+ 1 2) are . 9)
      `(+ 1 2)
      `(+ 1 2)
      `(#\c (+ 1 2) #\d)))
  (define inst-struct-name4460
    (make-test-struct
      `#&(#t (+ 4 5) #\c)
      `(1 #\c 1)
      `(#f (+ 4 5) #\a . symbols)
      `(+ 4 5)))
  (define inst-struct-name4470
    (make-test-struct
      `,inst-struct-name4460
      `10
      `#t
      `(() #\d (#f #\d #\c are) . #\b)))
  (define inst-struct-name4501 (make-test-struct `#\b `8 `are `#\a))
  (define inst-struct-name4516
    (make-test-struct
      `#&(,inst-struct-name4501 () #(#\b #\b #t #t))
      `((#\b #\c #t) 0 ((+ 4 5) #t these) . these)
      `(#\a #\b 0)
      `()))
  (define inst-struct-name4532 (make-test-struct `(+ 4 (+ 1 1)) `#\c `#f `#\c))
  (define inst-struct-name4539
    (make-test-struct
      `,inst-struct-name4532
      `(#f 9 symbols)
      `#\d
      `#(are #f are)))
  (define inst-struct-name4554 (make-test-struct `#f `#\b `(+ 4 5) `these))
  (define inst-struct-name4555
    (make-test-struct
      `#\b
      `#(#\c #\b #f (+ 4 5))
      `#(#f 5 #\a)
      `,inst-struct-name4554))
  (define inst-struct-name4578
    (make-test-struct
      `5
      `#&(#\c #((+ 4 (+ 1 1)) 9 these (+ 1 2)) are)
      `#((#\a 1 #t #\c) #f (#f #\c #\a) (+ 4 (+ 1 1)))
      `#(#\a #t #t)))
  (define inst-struct-name4593 (make-test-struct `#t `(+ 4 5) `#\d `#t))
  (define inst-struct-name4594
    (make-test-struct
      `these
      `#(these #\d symbols #\c)
      `#((+ 4 5) 0 #f #t)
      `,inst-struct-name4593))
  (define inst-struct-name4629 (make-test-struct `(+ 4 5) `2 `are `#\d))
  (define inst-struct-name4635 (make-test-struct `#f `#\d `1 `2))
  (define inst-struct-name4644
    (make-test-struct
      `,inst-struct-name4629
      `,inst-struct-name4635
      `#(7 #\c #\b (+ 1 2))
      `#(#\c (+ 4 5) #t these)))
  (define inst-struct-name4656
    (make-test-struct `symbols `these `#f `(+ 4 (+ 1 1))))
  (define inst-struct-name4663 (make-test-struct `#\c `#\b `#\a `6))
  (define inst-struct-name4668
    (make-test-struct
      `,inst-struct-name4656
      `18
      `,inst-struct-name4663
      `#&(#\b #\c #\a)))
  (define inst-struct-name4676 (make-test-struct `#\a `#\a `5 `are))
  (define inst-struct-name4710
    (make-test-struct
      `#(#\b #f #f)
      `#t
      `18
      `#(9 (#\a #\a #t) #&(#t #\d #t) (+ 1 2))))
  (define inst-struct-name4724 (make-test-struct `#\b `#t `#\a `7))
  (define inst-struct-name4742
    (make-test-struct `#\a `#\a `(+ 4 (+ 1 1)) `#\c))
  (define inst-struct-name4748 (make-test-struct `#\c `#\c `#t `symbols))
  (define inst-struct-name4767
    (make-test-struct `8 `#&((+ 1 2) these #\b) `8 `10))
  (define inst-struct-name4787
    (make-test-struct
      `#f
      `(are #f 8 #f)
      `(+ 4 (+ 1 1))
      `(#\d #\b (+ 1 2) . 7)))
  (define inst-struct-name4798
    (make-test-struct
      `#\b
      `#f
      `#&(3 ((+ 4 (+ 1 1)) #\d (+ 4 (+ 1 1))) ((+ 1 2) #\a 8))
      `#\d))
  (define inst-struct-name4804
    (make-test-struct `5 `(+ 4 (+ 1 1)) `(+ 4 5) `#(symbols 8 #\a)))
  (define inst-struct-name4835
    (make-test-struct `#&(#f (+ 1 2) #f) `(+ 1 2) `#\d `(5 #f #\d are)))
  (define inst-struct-name4844 (make-test-struct `7 `#t `(+ 1 2) `symbols))
  (define inst-struct-name4850 (make-test-struct `these `symbols `4 `are))
  (define inst-struct-name4851
    (make-test-struct
      `(#f #\d #\a)
      `7
      `,inst-struct-name4844
      `,inst-struct-name4850))
  (define inst-struct-name4871 (make-test-struct `#f `symbols `#\a `#t))
  (define inst-struct-name4879
    (make-test-struct
      `(#f #\c #\d)
      `(,inst-struct-name4871 are #(are symbols #\b) (8 #\b (+ 1 2)))
      `14
      `#((+ 4 5) (+ 1 2) (+ 4 5))))
  (define inst-struct-name4891 (make-test-struct `5 `(+ 4 5) `5 `#\a))
  (define inst-struct-name4893
    (make-test-struct
      `(#\a 9 #\a (+ 1 2))
      `,inst-struct-name4891
      `(+ 4 (+ 1 1))
      `#\d))
  (define inst-struct-name4902 (make-test-struct `#\a `#\d `7 `5))
  (define inst-struct-name4934 (make-test-struct `are `#\d `(+ 4 5) `symbols))
  (define inst-struct-name4940 (make-test-struct `#\a `1 `#t `#f))
  (define inst-struct-name4946 (make-test-struct `are `3 `4 `these))
  (define inst-struct-name4980 (make-test-struct `#\d `#t `2 `1))
  (define inst-struct-name4997 (make-test-struct `(+ 1 2) `#\d `these `#\d))
  (define inst-struct-name4999
    (make-test-struct
      `((+ 1 2) #t (+ 4 (+ 1 1)) . 4)
      `,inst-struct-name4997
      `9
      `#t))
  (define inst-struct-name5031 (make-test-struct `#f `#\b `#\c `are))
  (define inst-struct-name5037
    (make-test-struct
      `#\d
      `,inst-struct-name5031
      `(7 2 (+ 1 2))
      `((+ 1 2) (+ 4 (+ 1 1)) are these)))
  (define inst-struct-name5040
    (make-test-struct `(+ 1 2) `symbols `(7 0 #t) `are))
  (define inst-struct-name5059 (make-test-struct `5 `#f `#\c `(+ 4 (+ 1 1))))
  (define inst-struct-name5060
    (make-test-struct
      `#f
      `(are 6 #\a)
      `#t
      `((+ 4 (+ 1 1)) ,inst-struct-name5059 #t . 5)))
  (define inst-struct-name5078 (make-test-struct `#\d `#t `#t `#\a))
  (define inst-struct-name5086
    (make-test-struct `,inst-struct-name5078 `#\a `() `#\a))
  (define inst-struct-name5094 (make-test-struct `0 `#(#\d #\a 1 #t) `() `#\b))
  (define inst-struct-name5097
    (make-test-struct `() `(#\b #f are) `symbols `#t))
  (define inst-struct-name5105 (make-test-struct `#\a `these `#\d `2))
  (define inst-struct-name5110
    (make-test-struct
      `(+ 4 5)
      `#t
      `#\b
      `#(,inst-struct-name5105 #\b #(8 #f #f (+ 4 (+ 1 1))) #\b)))
  (define inst-struct-name5126
    (make-test-struct `(3 these these) `(#\c 3 #\b) `#t `(#\b #f are)))
  (define inst-struct-name5137
    (make-test-struct `#f `7 `(+ 4 (+ 1 1)) `(+ 4 (+ 1 1))))
  (define inst-struct-name5138
    (make-test-struct
      `#\d
      `#((+ 4 (+ 1 1)) (+ 4 5) 2 #t)
      `(+ 4 5)
      `,inst-struct-name5137))
  (define inst-struct-name5140
    (make-test-struct
      `#(() #\a #f #\a)
      `(10 ((+ 4 (+ 1 1)) #t 9 . #\b) (#\c #\a #\a #\a) #t)
      `,inst-struct-name5126
      `,inst-struct-name5138))
  (define inst-struct-name5172
    (make-test-struct
      `(#\c are #t #\d)
      `(+ 4 5)
      `(#f #t (+ 4 (+ 1 1)) . these)
      `16))
  (define inst-struct-name5175
    (make-test-struct
      `,inst-struct-name5172
      `#\a
      `(are #\a 7)
      `#(#f (+ 1 2) (+ 4 5))))
  (define inst-struct-name5182
    (make-test-struct `#\c `(are (+ 4 5) (+ 4 5)) `4 `(7 (+ 4 (+ 1 1)) 4)))
  (define inst-struct-name5195
    (make-test-struct `0 `these `() `((+ 1 2) symbols #f)))
  (define inst-struct-name5207
    (make-test-struct `3 `(+ 1 2) `(+ 4 (+ 1 1)) `4))
  (define inst-struct-name5210
    (make-test-struct
      `()
      `#\a
      `(((+ 4 5) (+ 1 2) (+ 4 5))
        (#\b (+ 4 (+ 1 1)) are . symbols)
        ,inst-struct-name5207
        .
        #t)
      `4))
  (define inst-struct-name5226 (make-test-struct `#\b `#\a `(+ 4 5) `#\b))
  (define inst-struct-name5230
    (make-test-struct
      `(#((+ 4 (+ 1 1)) #\b #\a symbols) #\b ,inst-struct-name5226 #\c)
      `(#\d #\b #\b)
      `#(5 8 #\d)
      `#t))
  (define inst-struct-name5238 (make-test-struct `0 `6 `7 `#f))
  (define inst-struct-name5244 (make-test-struct `these `(+ 1 2) `#f `#t))
  (define inst-struct-name5247
    (make-test-struct
      `#(#\b #\a (+ 4 5))
      `,inst-struct-name5244
      `(0 #\b these)
      `#t))
  (define inst-struct-name5249
    (make-test-struct
      `((+ 1 2) (5 #\b #f) #\d ,inst-struct-name5238)
      `,inst-struct-name5247
      `#\c
      `#f))
  (define inst-struct-name5259 (make-test-struct `6 `#\b `(+ 1 2) `8))
  (define inst-struct-name5267
    (make-test-struct `,inst-struct-name5259 `() `8 `(#\c #\d symbols)))
  (define inst-struct-name5299
    (make-test-struct `4 `#\a `(these #(7 are #t) 8 . #t) `(+ 4 (+ 1 1))))
  (define inst-struct-name5336
    (make-test-struct
      `()
      `((#f are #\a) #(#t are symbols #t) (#\d #\b #\b) . #\a)
      `#&(#t #(are (+ 4 5) 1 are) #\d)
      `symbols))
  (define inst-struct-name5350
    (make-test-struct
      `(#\d #f (+ 4 5) (+ 1 2))
      `#t
      `these
      `(#\c #\b #\b . symbols)))
  (define inst-struct-name5363 (make-test-struct `#\c `#t `#f `(+ 1 2)))
  (define inst-struct-name5381
    (make-test-struct
      `these
      `#(7 #t #\c (+ 4 (+ 1 1)))
      `(symbols 2 (+ 1 2) #\a)
      `#((+ 4 5) (+ 4 (+ 1 1)) these 5)))
  (define inst-struct-name5395
    (make-test-struct `6 `(+ 4 (+ 1 1)) `symbols `#\c))
  (define inst-struct-name5402 (make-test-struct `#\b `#t `#\d `#\b))
  (define inst-struct-name5413 (make-test-struct `#\a `#\d `(+ 4 (+ 1 1)) `5))
  (define inst-struct-name5417
    (make-test-struct
      `()
      `()
      `()
      `#&(,inst-struct-name5413 #\a (#t (+ 1 2) #\d))))
  (define inst-struct-name5423 (make-test-struct `8 `4 `1 `#\d))
  (define inst-struct-name5430 (make-test-struct `(+ 4 5) `2 `#\b `7))
  (define inst-struct-name5431
    (make-test-struct `#\a `5 `#\d `,inst-struct-name5430))
  (define inst-struct-name5445 (make-test-struct `#\a `#\b `#\c `these))
  (define inst-struct-name5449
    (make-test-struct
      `(#f (0 9 are (+ 1 2)) (#\d (+ 1 2) 2 . #f) #f)
      `()
      `(,inst-struct-name5445 (9 #\c (+ 1 2)) #(are symbols #\a) #\c)
      `#(#\c #\a symbols)))
  (define inst-struct-name5454 (make-test-struct `2 `#\a `3 `are))
  (define inst-struct-name5459
    (make-test-struct `(+ 4 (+ 1 1)) `#\c `(+ 1 2) `#f))
  (define inst-struct-name5472 (make-test-struct `4 `8 `#t `#\a))
  (define inst-struct-name5482
    (make-test-struct
      `these
      `(#f #\b #t . these)
      `((+ 4 (+ 1 1)) #\d #t . #f)
      `#\a))
  (define inst-struct-name5484
    (make-test-struct
      `#f
      `#&((7 #\a #\d) #(#\b (+ 4 (+ 1 1)) 3) ,inst-struct-name5472)
      `,inst-struct-name5482
      `#\d))
(define inst-struct-name782 (make-test-struct `#\d `9 `6 `#\c))
  (define inst-struct-name788 (make-test-struct `(+ 4 5) `#\d `0 `are))
  (define inst-struct-name793 (make-test-struct `(+ 4 (+ 1 1)) `#\b `#\c `3))
  (define inst-struct-name810 (make-test-struct `#f `8 `#\d `(+ 4 (+ 1 1))))
  (define inst-struct-name816 (make-test-struct `6 `4 `#f `#\b))
  (define inst-struct-name818
    (make-test-struct `symbols `() `,inst-struct-name816 `()))
  (define inst-struct-name830 (make-test-struct `#\d `#\b `#t `6))
  (define inst-struct-name836
    (make-test-struct `are `,inst-struct-name830 `(9 are 6 2) `#\d))
  (define inst-struct-name841 (make-test-struct `#f `are `symbols `7))
  (define inst-struct-name862 (make-test-struct `#t `#t `9 `(+ 4 (+ 1 1))))
  (define inst-struct-name876 (make-test-struct `3 `6 `#f `#\d))
  (define inst-struct-name882
    (make-test-struct
      `(#\c (+ 4 (+ 1 1)) (+ 4 (+ 1 1)))
      `(6 #f 4 . #\c)
      `,inst-struct-name876
      `(#\c #\a 9 #\a)))
  (define inst-struct-name888 (make-test-struct `symbols `symbols `#\c `#f))
  (define inst-struct-name891
    (make-test-struct `(+ 1 2) `,inst-struct-name888 `#\d `#f))
  (define inst-struct-name903
    (make-test-struct `#\c `(#f #f symbols) `14 `#3(are #\b #f)))
  (define inst-struct-name925
    (make-test-struct `are `(+ 4 (+ 1 1)) `0 `(#t (+ 4 (+ 1 1)) #t)))
  (define inst-struct-name928
    (make-test-struct
      `(((+ 1 2) (+ 4 (+ 1 1)) are) these these (#t #\b #\d))
      `,inst-struct-name925
      `#f
      `#\c))
  (define inst-struct-name941 (make-test-struct `these `(+ 1 2) `6 `#\d))
  (define inst-struct-name952 (make-test-struct `#f `#t `8 `(+ 4 (+ 1 1))))
  (define inst-struct-name958 (make-test-struct `#f `#\a `#\b `#\a))
  (define inst-struct-name981
    (make-test-struct `symbols `#\a `(+ 1 2) `(+ 1 2)))
  (define inst-struct-name991 (make-test-struct `#\b `#\c `(+ 4 5) `#\d))
  (define inst-struct-name1004
    (make-test-struct `#t `#\b `#\a `((+ 4 (+ 1 1)) #t #\d)))
  (define inst-struct-name1022
    (make-test-struct `(#\b #f #t + 4 (+ 1 1)) `() `(#t #\b #\b) `12))
  (define inst-struct-name1030
    (make-test-struct `2 `these `#4(#\d symbols #\a #\c) `3))
  (define inst-struct-name1032
    (make-test-struct `#\b `6 `,inst-struct-name1022 `,inst-struct-name1030))
  (define inst-struct-name1043 (make-test-struct `#\d `6 `8 `(+ 1 2)))
  (define inst-struct-name1048
    (make-test-struct `#\b `(+ 1 2) `#\b `(+ 4 (+ 1 1))))
  (define inst-struct-name1050
    (make-test-struct
      `(+ 4 5)
      `#f
      `,inst-struct-name1043
      `,inst-struct-name1048))
  (define inst-struct-name1069 (make-test-struct `#\b `(+ 1 2) `8 `symbols))
  (define inst-struct-name1071
    (make-test-struct `are `symbols `#t `,inst-struct-name1069))
  (define inst-struct-name1080
    (make-test-struct `#&(#\d 6 (+ 4 (+ 1 1))) `#t `0 `(#\d #\d (+ 4 5))))
  (define inst-struct-name1087 (make-test-struct `symbols `#t `#\d `(+ 1 2)))
  (define inst-struct-name1091
    (make-test-struct
      `,inst-struct-name1071
      `,inst-struct-name1080
      `#4(6 ,inst-struct-name1087 #\c #f)
      `0))
  (define inst-struct-name1099
    (make-test-struct `8 `() `#3(#\d (+ 1 2) 9) `()))
  (define inst-struct-name1106
    (make-test-struct
      `(#\c 5 #\a . 8)
      `(+ 4 (+ 1 1))
      `(+ 4 (+ 1 1))
      `(+ 4 (+ 1 1))))
  (define inst-struct-name1118
    (make-test-struct
      `((+ 4 (+ 1 1)) these #\d #\a)
      `,inst-struct-name1106
      `#\a
      `#&((are #\c #t (+ 4 5)) (#\b 0 #\c) (1 are (+ 1 2) (+ 4 (+ 1 1))))))
  (define inst-struct-name1125 (make-test-struct `#\b `(+ 4 5) `#\c `these))
  (define inst-struct-name1139
    (make-test-struct
      `#t
      `(#t #\c these . 9)
      `#\a
      `#4(9 (+ 4 (+ 1 1)) #t (+ 1 2))))
  (define inst-struct-name1141
    (make-test-struct
      `(() these ,inst-struct-name1125 #4(6 1 5 #\c))
      `,inst-struct-name1139
      `()
      `5))
  (define inst-struct-name1156
    (make-test-struct
      `((#t 5 9 5) #3(symbols symbols these) 14 (are these are))
      `2
      `(((+ 4 (+ 1 1)) these #f) #3((+ 4 (+ 1 1)) #t #\b) symbols . 0)
      `are))
  (define inst-struct-name1161 (make-test-struct `#\d `#\c `these `1))
  (define inst-struct-name1174 (make-test-struct `#f `(+ 4 5) `#\d `#f))
  (define inst-struct-name1187
    (make-test-struct `,inst-struct-name1174 `#\c `#\c `#&(symbols 2 #\a)))
  (define inst-struct-name1198
    (make-test-struct
      `(#\d #\b #t)
      `#4(#f (+ 4 5) symbols (+ 1 2))
      `#f
      `#&(#\d 8 3)))
  (define inst-struct-name1221
    (make-test-struct `#\c `4 `() `(#\d #\c #\c . #\d)))
  (define inst-struct-name1229
    (make-test-struct `#\c `(+ 4 (+ 1 1)) `these `#f))
  (define inst-struct-name1230
    (make-test-struct `() `#3(#\b 0 (+ 4 5)) `#\b `,inst-struct-name1229))
  (define inst-struct-name1232
    (make-test-struct
      `#3((+ 1 2) #\a 2)
      `,inst-struct-name1221
      `#&(() (+ 1 2) ())
      `,inst-struct-name1230))
  (define inst-struct-name1238
    (make-test-struct `(#f #\c #\c) `#\b `() `(#\d these these)))
  (define inst-struct-name1254
    (make-test-struct `#\b `(+ 4 (+ 1 1)) `#f `(+ 4 5)))
  (define inst-struct-name1260
    (make-test-struct
      `#&(are #\c #t)
      `(9 these #\d)
      `,inst-struct-name1254
      `(are #\b (+ 1 2) 3)))
  (define inst-struct-name1266 (make-test-struct `#t `#\b `#t `#\b))
  (define inst-struct-name1279 (make-test-struct `#\d `#\c `#\a `#t))
  (define inst-struct-name1312
    (make-test-struct `these `#f `#4(#\b #\c 2 #f) `1))
  (define inst-struct-name1316
    (make-test-struct `,inst-struct-name1312 `#t `#\d `(+ 1 2)))
  (define inst-struct-name1325 (make-test-struct `these `#\a `#t `these))
  (define inst-struct-name1340
    (make-test-struct `(+ 4 5) `#\c `symbols `(+ 1 2)))
  (define inst-struct-name1342
    (make-test-struct
      `#t
      `#4(,inst-struct-name1325
          (4 (+ 4 5) #\a)
          (#\d symbols #\b)
          #4(#\c #\c #\b #t))
      `((#\c symbols (+ 4 (+ 1 1)) . #\a) #f 3 ,inst-struct-name1340)
      `#3(8 9 1)))
  (define inst-struct-name1351
    (make-test-struct
      `(these #\a #t)
      `4
      `(#\c (0 #t #\b) #\c #t)
      `(#\a these (+ 1 2))))
  (define inst-struct-name1364
    (make-test-struct `#f `symbols `(+ 1 2) `symbols))
  (define inst-struct-name1367
    (make-test-struct
      `(#\b symbols #\b . 5)
      `(#\b 7 #\a)
      `,inst-struct-name1364
      `these))
  (define inst-struct-name1383 (make-test-struct `#t `#\d `#\a `#f))
  (define inst-struct-name1403 (make-test-struct `(+ 1 2) `#\c `#\b `#\d))
  (define inst-struct-name1409
    (make-test-struct
      `(#\a (+ 4 5) #\b)
      `1
      `,inst-struct-name1403
      `(#\a 1 #\c . are)))
  (define inst-struct-name1439
    (make-test-struct `(#t 8 #f) `#3(#\a are 3) `#\d `are))
  (define inst-struct-name1460 (make-test-struct `#f `#\d `5 `#\b))
  (define inst-struct-name1466
    (make-test-struct
      `(#\a 4 #t)
      `((#\a (+ 1 2) 7 . #\d) () 0 . #t)
      `((#f #\a #\b . #\a)
        are
        (2 symbols #f + 4 (+ 1 1))
        ,inst-struct-name1460)
      `#&(2 #\b #\b)))
  (define inst-struct-name1502
    (make-test-struct `#\c `() `#&(2 are 8) `#4(these #\c #\a)))
  (define inst-struct-name1508
    (make-test-struct `#\c `#\d `(#t (+ 4 (+ 1 1)) #\c . symbols) `()))
  (define inst-struct-name1519 (make-test-struct `#t `#\b `#\d `6))
  (define inst-struct-name1534 (make-test-struct `these `#\b `#\c `are))
  (define inst-struct-name1536
    (make-test-struct
      `(#\d #f 0 . #\d)
      `(are are #\c . #\b)
      `(+ 1 2)
      `,inst-struct-name1534))
  (define inst-struct-name1538
    (make-test-struct
      `,inst-struct-name1519
      `2
      `,inst-struct-name1536
      `(+ 4 5)))
  (define inst-struct-name1543 (make-test-struct `2 `3 `#\d `(+ 4 5)))
  (define inst-struct-name1561 (make-test-struct `#f `symbols `#\b `#t))
  (define inst-struct-name1563
    (make-test-struct `(+ 4 (+ 1 1)) `,inst-struct-name1561 `(+ 4 5) `3))
  (define inst-struct-name1572 (make-test-struct `(+ 1 2) `#\a `#\b `#\d))
  (define inst-struct-name1577 (make-test-struct `#\b `#\b `#\b `4))
  (define inst-struct-name1594 (make-test-struct `#f `are `#\b `#f))
  (define inst-struct-name1609
    (make-test-struct
      `(symbols (+ 1 2) #\d)
      `(#\c symbols are)
      `(are are #\c (+ 1 2))
      `(symbols #\d (+ 1 2) . #\b)))
  (define inst-struct-name1619 (make-test-struct `#t `(+ 4 (+ 1 1)) `#\c `#t))
  (define inst-struct-name1635
    (make-test-struct `#\b `#\d `(+ 4 (+ 1 1)) `#\c))
  (define inst-struct-name1663 (make-test-struct `#f `#\a `#f `(+ 1 2)))
  (define inst-struct-name1666
    (make-test-struct
      `(#\c these symbols . #f)
      `(4 5 #f)
      `,inst-struct-name1663
      `(#f (+ 4 5) (+ 4 5))))
  (define inst-struct-name1687 (make-test-struct `symbols `are `1 `are))
  (define inst-struct-name1701
    (make-test-struct
      `#4(#4(these #\d these symbols) (are 6 these) #\c #\d)
      `#3(#f 8 (+ 4 5))
      `(#\b are symbols)
      `#\b))
  (define inst-struct-name1708
    (make-test-struct
      `(+ 1 2)
      `#4(() #&((+ 4 5) #\a #f) () (+ 4 (+ 1 1)))
      `#\b
      `#\a))
  (define inst-struct-name1719
    (make-test-struct `#&(#\d #t #t) `(+ 4 (+ 1 1)) `0 `(+ 4 (+ 1 1))))
  (define inst-struct-name1741 (make-test-struct `these `#t `7 `#\b))
  (define inst-struct-name1775
    (make-test-struct `(8 #f #f (+ 4 (+ 1 1))) `(9 #\c #t) `#f `#t))
  (define inst-struct-name1776
    (make-test-struct
      `symbols
      `#3(#\d #\c (+ 4 (+ 1 1)))
      `#3(symbols #\a #\d)
      `,inst-struct-name1775))
  (define inst-struct-name1786 (make-test-struct `0 `#\a `0 `#t))
  (define inst-struct-name1796
    (make-test-struct `3 `#3(#\b #\c 0) `#t `(+ 4 5)))
  (define inst-struct-name1797
    (make-test-struct
      `((these #t (+ 4 (+ 1 1)) . #\a)
        ,inst-struct-name1786
        (#\a #t 7)
        #&(9 #\b symbols))
      `(+ 4 5)
      `,inst-struct-name1796
      `(+ 4 (+ 1 1))))
  (define inst-struct-name1813
    (make-test-struct
      `(symbols #\d #\b are)
      `#4(#\d (+ 4 (+ 1 1)) #f #\b)
      `are
      `()))
  (define inst-struct-name1818
    (make-test-struct
      `#&(#\a (9 #\c symbols) #f)
      `,inst-struct-name1813
      `#&(8 () #\d)
      `#3(#\a these symbols)))
  (define inst-struct-name1832 (make-test-struct `are `1 `() `(12 #\c #t . 4)))
  (define inst-struct-name1873 (make-test-struct `#t `7 `#\b `(+ 4 5)))
  (define inst-struct-name1876
    (make-test-struct
      `0
      `#&(#f 0 these)
      `,inst-struct-name1873
      `(+ 4 (+ 1 1))))
  (define inst-struct-name1896
    (make-test-struct
      `#3(5 (+ 1 2) these)
      `(+ 4 (+ 1 1))
      `0
      `#3(#\a #\d these)))
  (define inst-struct-name1900
    (make-test-struct `0 `these `#3(6 are symbols) `#3((+ 4 5) #t 8)))
  (define inst-struct-name1919
    (make-test-struct `(+ 1 2) `#\b `#&((+ 1 2) #\a #t) `#\b))
  (define inst-struct-name1935
    (make-test-struct `#&(2 #\c #\c) `(#f 0 6 #f) `#&(1 #\a symbols) `#\b))
  (define inst-struct-name1941 (make-test-struct `#\b `(+ 4 5) `#f `are))
  (define inst-struct-name1947 (make-test-struct `(+ 4 (+ 1 1)) `#\d `5 `#\d))
  (define inst-struct-name1948
    (make-test-struct
      `(+ 4 5)
      `,inst-struct-name1941
      `(1 #\b (+ 4 5))
      `,inst-struct-name1947))
  (define inst-struct-name1955 (make-test-struct `2 `5 `#\c `()))
  (define inst-struct-name1962
    (make-test-struct `#4(#t are #\b #f) `#3(#f #f (+ 4 5)) `#t `()))
  (define inst-struct-name1975 (make-test-struct `(+ 1 2) `(+ 1 2) `#\c `#f))
  (define inst-struct-name1977
    (make-test-struct
      `symbols
      `symbols
      `(+ 4 (+ 1 1))
      `(#3(2 (+ 4 5) #\b) are ,inst-struct-name1975 . 5)))
  (define inst-struct-name1995 (make-test-struct `#\a `(+ 4 5) `8 `8))
  (define inst-struct-name1996
    (make-test-struct
      `(#\c (+ 4 (+ 1 1)) 2 . #\a)
      `(#\b #\c (+ 4 (+ 1 1)) . are)
      `#\b
      `,inst-struct-name1995))
  (define inst-struct-name2013
    (make-test-struct `18 `() `,inst-struct-name1996 `(are #\c symbols)))
  (define inst-struct-name2041
    (make-test-struct `(+ 1 2) `7 `#&(#\c #\d (+ 4 5)) `#t))
  (define inst-struct-name2085
    (make-test-struct `#4(#t (+ 4 5) (+ 4 5) #t) `0 `#\c `(these these 7)))
  (define inst-struct-name2093
    (make-test-struct `(#\b 9 #\b) `16 `#\a `#4((+ 1 2) 9 () 2)))
  (define inst-struct-name2104
    (make-test-struct `#\b `(symbols 1 (+ 4 5)) `12 `0))
  (define inst-struct-name2110 (make-test-struct `#\a `#\a `these `symbols))
  (define inst-struct-name2117
    (make-test-struct
      `,inst-struct-name2110
      `#3(#t 4 7)
      `(+ 1 2)
      `#4(2 symbols (+ 4 (+ 1 1)) 5)))
  (define inst-struct-name2125 (make-test-struct `#\c `(+ 4 (+ 1 1)) `#\c `2))
  (define inst-struct-name2127
    (make-test-struct
      `(+ 4 5)
      `()
      `,inst-struct-name2117
      `#4(#&(are 6 #\c) ,inst-struct-name2125 9 ())))
  (define inst-struct-name2133
    (make-test-struct
      `symbols
      `#3(#\b #\a #\d)
      `((+ 4 5) #\d #\b . symbols)
      `these))
  (define inst-struct-name2138
    (make-test-struct `0 `,inst-struct-name2133 `10 `#\d))
  (define inst-struct-name2146 (make-test-struct `9 `#t `#t `(+ 4 5)))
  (define inst-struct-name2153
    (make-test-struct `#\c `symbols `(+ 4 (+ 1 1)) `#\a))
  (define inst-struct-name2160
    (make-test-struct
      `(,inst-struct-name2146 #f 3 . #t)
      `2
      `#\c
      `(,inst-struct-name2153 #f #&(7 0 2) . #\b)))
  (define inst-struct-name2170 (make-test-struct `6 `8 `#f `(+ 4 (+ 1 1))))
  (define inst-struct-name2188
    (make-test-struct `#t `symbols `(+ 4 (+ 1 1)) `#t))
  (define inst-struct-name2190
    (make-test-struct
      `#4(7 #\a (these 9 (+ 1 2)) ())
      `2
      `#\b
      `#4(#3(are #\a #\c) #\a ,inst-struct-name2188 #\d)))
  (define inst-struct-name2202 (make-test-struct `are `#f `#t `are))
  (define inst-struct-name2204
    (make-test-struct `(+ 1 2) `6 `,inst-struct-name2202 `#\c))
  (define inst-struct-name2211
    (make-test-struct `#t `(+ 4 (+ 1 1)) `symbols `these))
  (define inst-struct-name2215
    (make-test-struct
      `(+ 4 (+ 1 1))
      `,inst-struct-name2211
      `(#f 6 #\c)
      `(#\b #\b 1)))
  (define inst-struct-name2245
    (make-test-struct `#\c `#\a `symbols `(+ 4 (+ 1 1))))
  (define inst-struct-name2252 (make-test-struct `2 `5 `#t `2))
  (define inst-struct-name2263
    (make-test-struct
      `#\a
      `#3(#\d symbols (+ 4 5))
      `()
      `((+ 1 2) (are #\b #\d) #\b . these)))
  (define inst-struct-name2269
    (make-test-struct `(symbols #\a #\a) `are `#f `()))
  (define inst-struct-name2279
    (make-test-struct
      `#3(#\a #\b symbols)
      `#3((+ 4 (+ 1 1)) #\d 2)
      `#3(#f #\b (+ 1 2))
      `(these #\b are)))
  (define inst-struct-name2305 (make-test-struct `#\c `() `#t `(#\c 0 9)))
  (define inst-struct-name2310 (make-test-struct `#\b `() `symbols `18))
  (define inst-struct-name2327 (make-test-struct `#\d `#f `these `3))
  (define inst-struct-name2335 (make-test-struct `#f `#\c `#\d `2))
  (define inst-struct-name2336
    (make-test-struct `#\b `,inst-struct-name2335 `#\c `()))
  (define inst-struct-name2354 (make-test-struct `(+ 1 2) `#\b `0 `#\d))
  (define inst-struct-name2359 (make-test-struct `#\d `#f `(+ 4 (+ 1 1)) `#f))
  (define inst-struct-name2370
    (make-test-struct `#\d `#\c `(these 6 #\a (+ 1 2)) `()))
  (define inst-struct-name2376 (make-test-struct `(+ 1 2) `1 `3 `#\a))
  (define inst-struct-name2382
    (make-test-struct
      `,inst-struct-name2376
      `(#\b (+ 4 5) (+ 1 2))
      `18
      `#3(8 #\a (+ 4 5))))
  (define inst-struct-name2388 (make-test-struct `#f `4 `3 `#t))
  (define inst-struct-name2395
    (make-test-struct
      `,inst-struct-name2370
      `,inst-struct-name2382
      `#&((#\c #\c (+ 1 2)) ,inst-struct-name2388 (+ 4 5))
      `14))
  (define inst-struct-name2408
    (make-test-struct
      `#4(#\d #f these symbols)
      `10
      `these
      `((+ 4 (+ 1 1)) symbols (+ 4 5) . symbols)))
  (define inst-struct-name2413
    (make-test-struct `(0 #\b #\b) `#\a `symbols `2))
  (define inst-struct-name2438
    (make-test-struct
      `#&((+ 4 (+ 1 1)) (+ 1 2) (+ 4 5))
      `#f
      `#4((+ 4 (+ 1 1)) 9 #t #\c)
      `6))
  (define inst-struct-name2454
    (make-test-struct `#3(#\a symbols 8) `#\d `#f `#3(1 #\c 4)))
  (define inst-struct-name2465
    (make-test-struct `() `#\b `symbols `#4(#t (+ 4 5) #t #\d)))
  (define inst-struct-name2469
    (make-test-struct
      `#\b
      `,inst-struct-name2465
      `#\d
      `#&(#\b #3(4 are 1) (+ 1 2))))
  (define inst-struct-name2480
    (make-test-struct
      `#4((+ 1 2) 7 are #\b)
      `(#\b symbols #\d)
      `(#\d symbols symbols)
      `(+ 1 2)))
