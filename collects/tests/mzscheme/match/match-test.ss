(load-relative "../loadtest.ss")
(SECTION 'MATCH)
(require  (lib "match.ss"))
;(require  "../match.ss")

(define-syntax test-mac
  (syntax-rules ()
    ((id to-test eval-to)
     (with-handlers ([(lambda exn #t)
                      (lambda (exn) (failed-test (exn-message exn) 
                                                  (quote to-test)
                                                 '() eval-to))])
                    (let ((res to-test))
                      (if (equal? res eval-to)
                          #t
                          (failed-test '() (quote to-test) res eval-to)))))))

(define (failed-test exn-msg test-code result should-have-been)
  `((Test-Failure)
    (Code ,test-code)
    (Expected-Result ,should-have-been)
    ,(if (null? exn-msg) 
         `(Actual-Result ,result)
         `(Exception ,exn-msg))))

; (define-syntax mytest
;   (lambda (stx)
;     (syntax-case stx ()
;       ((_ t result)
;        #`(test result #,(syntax/loc stx (lambda () t))))))) 


(define-syntax mytest
  (lambda (stx)
    (syntax-case stx ()
      ((_ t result)
       #`(test #t #,(syntax/loc stx (lambda () (test-mac t result)))))))) 


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
      `#(9 #\b 9)
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
      `#(are () ,inst-struct-name145 (+ 4 (+ 1 1)))
      `()))
  (define inst-struct-name158
    (make-test-struct `18 `(#\b 2 #t) `() `(+ 4 (+ 1 1))))
  (define inst-struct-name164
    (make-test-struct
      `()
      `,inst-struct-name158
      `#\d
      `(#(these these 1) #\b (#\c #f (+ 4 (+ 1 1))) . #f)))
  (define inst-struct-name173 (make-test-struct `#\b `#\b `#\b `are))
  (define inst-struct-name175
    (make-test-struct `() `#&((+ 1 2) #t (+ 4 5)) `,inst-struct-name173 `#t))
  (define inst-struct-name190
    (make-test-struct
      `,inst-struct-name175
      `()
      `#f
      `((are #\b 5) these #((+ 1 2) 8 these (+ 1 2)) . #f)))
  (define inst-struct-name198 (make-test-struct `these `#t `7 `(+ 4 5)))
  (define inst-struct-name218
    (make-test-struct `(+ 4 (+ 1 1)) `#((+ 4 5) symbols 2 4) `#\a `#\d))
  (define inst-struct-name224 (make-test-struct `#\b `(+ 1 2) `4 `these))
  (define inst-struct-name250 (make-test-struct `7 `symbols `are `these))
  (define inst-struct-name260
    (make-test-struct
      `(symbols #\b symbols)
      `()
      `(symbols (+ 1 2) 5 #\d)
      `(#\a #\c #\c . 1)))
  (define inst-struct-name272
    (make-test-struct `#&(#\b #\a #f) `#((+ 4 5) #\b #t) `#\b `(+ 4 (+ 1 1))))
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
      `#(#\a these #t (+ 1 2))))
  (define inst-struct-name359
    (make-test-struct `(+ 4 (+ 1 1)) `(+ 4 5) `#\b `#\a))
  (define inst-struct-name372 (make-test-struct `symbols `6 `4 `#\c))
  (define inst-struct-name385 (make-test-struct `#\d `8 `#\a `#\a))
  (define inst-struct-name399 (make-test-struct `4 `() `#\a `#(#f (+ 4 5) #f)))
  (define inst-struct-name408
    (make-test-struct `these `(+ 4 (+ 1 1)) `((+ 4 (+ 1 1)) (+ 4 5) are) `#\a))
  (define inst-struct-name417
    (make-test-struct `6 `#\b `(+ 4 (+ 1 1)) `(+ 4 5)))
  (define inst-struct-name424
    (make-test-struct
      `(#((+ 4 (+ 1 1)) #\b 7 #\a)
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
      `#((+ 4 (+ 1 1)) symbols 1 are)
      `(8 #t (+ 4 (+ 1 1)))))
  (define inst-struct-name433
    (make-test-struct `() `1 `,inst-struct-name431 `are))
  (define inst-struct-name477
    (make-test-struct `#t `() `these `#((+ 4 5) #t #t #\c)))
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
    (make-test-struct `#(#\d #f #\d) `are `1 `(+ 4 (+ 1 1))))
  (define inst-struct-name597 (make-test-struct `#\d `(+ 4 (+ 1 1)) `2 `#\a))
  (define inst-struct-name600
    (make-test-struct
      `#f
      `(these #\b 7)
      `(18 #(#f (+ 4 5) #t) ,inst-struct-name597 (#t 8 these))
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
  (define (let-tests)
    (list
     (mytest
      (match-letrec
        (((#&((and (? number? tlp327) (? even? tlp328))
              #(tlp329 ...)
              (tlp330 ..3))
           ()
           tlp331
           (or #(tlp332 ...) #(tlp332 ...)))
          `(#&(2 #(6 (+ 1 2) #t) (#\c #t symbols)) () #\b #(#\d #f #\b))))
        (list tlp327 tlp329 tlp330 tlp331 tlp332))
      '(2 (6 (+ 1 2) #t) (#\c #t symbols) #\b (#\d #f #\b)))
     (mytest
      (match-let*
        (((#(tlp333 ...)
           ((and (? test-struct? tlp335)
                 (= test-struct-a (and _ _))
                 (= test-struct-b (not (not #f)))
                 (= test-struct-c '(+ 4 5))
                 (= test-struct-d 7))
            (and (? number? tlp336) (? even? tlp337))
            tlp338
            quote
            these)
           (tlp339
             (not (not 'these))
             ($
              test-struct
              ()
              '(+ 4 (+ 1 1))
              (and #&(tlp340 tlp341 tlp342) #&(tlp343 tlp344 tlp345))
              #(tlp346 tlp347 tlp348 tlp349))
             .
             #\a)
           #(tlp351 ...))
          `(#(#\a #\a 4)
            (,inst-struct-name334 12 (+ 1 2) . these)
            ((+ 4 (+ 1 1)) these ,inst-struct-name350 . #\a)
            #(#\b #\d 4))))
        (list
         tlp333
         tlp336
         tlp338
         tlp339
         tlp340
         tlp341
         tlp342
         tlp346
         tlp347
         tlp348
         tlp349
         tlp351))
      '((#\a #\a 4)
        12
        (+ 1 2)
        (+ 4 (+ 1 1))
        #\d
        2
        #f
        #\a
        these
        #t
        (+ 1 2)
        (#\b #\d 4)))))
  (define (multi-let-tests)
    (list
     (mytest
      (match-let
        (((tlp352
            #(#&((or _ _)
                 (and (? number? tlp353) (? even? tlp354))
                 ($ test-struct tlp355 tlp356 tlp357 tlp358))
              (_
               (tlp360 ..3)
               (and (? number? tlp361) (? even? tlp362))
               (or (tlp363 ..3) (tlp363 ..3)))
              ((not (not 'symbols))
               (and (and tlp364 tlp365) (and tlp366 tlp367))
               ($ test-struct tlp368 tlp369 tlp370 tlp371)
               _)
              tlp373)
            ((tlp374 ...) (tlp375 ...) _ . #\c)
            (and (? number? tlp376) (? even? tlp377)))
          `((+ 1 2)
            #(#&(2 16 ,inst-struct-name359)
              (#\a (are #\c #t) 8 (#\b #\a #\d))
              (symbols #\b ,inst-struct-name372 4)
              #\b)
            ((2 6 #\c) (#\d #t #t) 1 . #\c)
            16))
         ((() #&(() tlp378 (not (not #\c))) #\b tlp379)
          `(() #&(() #\c #\c) #\b #\d)))
        (list
         tlp352
         tlp353
         tlp355
         tlp356
         tlp357
         tlp358
         tlp360
         tlp361
         tlp363
         tlp364
         tlp368
         tlp369
         tlp370
         tlp371
         tlp373
         tlp374
         tlp375
         tlp376
         tlp378
         tlp379))
      '((+ 1 2)
        16
        (+ 4 (+ 1 1))
        (+ 4 5)
        #\b
        #\a
        (are #\c #t)
        8
        (#\b #\a #\d)
        #\b
        symbols
        6
        4
        #\c
        #\b
        (2 6 #\c)
        (#\d #t #t)
        16
        #\c
        #\d))
     (mytest
      (match-letrec
        (((#&(((not (not 8))
               #(tlp380 ...)
               (and (? test-struct? tlp386)
                    (= test-struct-a tlp381)
                    (= test-struct-b tlp382)
                    (= test-struct-c tlp383)
                    (= test-struct-d tlp384))
               .
               #\a)
              #&(#t #(tlp387 ___) (tlp388 ..3))
              (and tlp389 tlp390))
           (not (not 6))
           ()
           '(+ 4 (+ 1 1)))
          `(#&((8 #(#\c #\a are) ,inst-struct-name385 . #\a)
               #&(#t #(#f #\c #t) (these #\b (+ 1 2)))
               (+ 4 5))
            6
            ()
            (+ 4 (+ 1 1))))
         (((tlp391 __3) (not (not 3)) tlp392 (tlp393 ...))
          `(((+ 1 2) #f (+ 4 (+ 1 1))) 3 are (symbols #\a #\d))))
        (list
         tlp380
         tlp381
         tlp382
         tlp383
         tlp384
         tlp387
         tlp388
         tlp389
         tlp391
         tlp392
         tlp393))
      '((#\c #\a are)
        #\d
        8
        #\a
        #\a
        (#f #\c #t)
        (these #\b (+ 1 2))
        (+ 4 5)
        ((+ 1 2) #f (+ 4 (+ 1 1)))
        are
        (symbols #\a #\d)))))
  (define (define-tests)
    (list
     (mytest
      (let ()
        (match-define
          (tlp394
            ($
             test-struct
             (and (? number? tlp395) (? even? tlp396))
             ()
             tlp397
             #(tlp398 ___))
            _
            #(tlp400 ...))
          `(symbols ,inst-struct-name399 #t #(these #f 6)))
        (list tlp394 tlp395 tlp397 tlp398 tlp400))
      '(symbols 4 #\a (#f (+ 4 5) #f) (these #f 6)))
     (mytest
      (let ()
        (match-define
          (tlp401 tlp402 (tlp403 ..3) #(tlp404 ...))
          `(#\a #t ((+ 4 5) #\d #t) #((+ 1 2) #t 6)))
        (list tlp401 tlp402 tlp403 tlp404))
      '(#\a #t ((+ 4 5) #\d #t) ((+ 1 2) #t 6)))))




  (define (sing-match-tests)
    (list
     (let ((tlp435
             (lambda (tlp405)
               (match
                tlp405
                ((($ test-struct tlp406 '(+ 4 (+ 1 1)) (tlp407 __3) _)
                  _
                  ($
                   test-struct
                   (#(tlp409 tlp410 tlp411 tlp412)
                    (and (? test-struct? tlp418)
                         (= test-struct-a tlp413)
                         (= test-struct-b tlp414)
                         (= test-struct-c tlp415)
                         (= test-struct-d tlp416))
                    (tlp419 tlp420 tlp421 quote these)
                    .
                    #f)
                   (tlp422 ...)
                   ()
                   (tlp423 ...))
                  (and (? test-struct? tlp434)
                       (= test-struct-a ())
                       (= test-struct-b tlp425)
                       (=
                        test-struct-c
                        (and (? test-struct? tlp432)
                             (= test-struct-a 'these)
                             (=
                              test-struct-b
                              (and (not (not 3)) (not (not 3))))
                             (= test-struct-c #(tlp426 tlp427 tlp428 tlp429))
                             (= test-struct-d (tlp430 ___))))
                       (= test-struct-d 'are)))
                 (list
                  tlp406
                  tlp407
                  tlp409
                  tlp410
                  tlp411
                  tlp412
                  tlp413
                  tlp414
                  tlp415
                  tlp416
                  tlp419
                  tlp420
                  tlp421
                  tlp422
                  tlp423
                  tlp425
                  tlp426
                  tlp427
                  tlp428
                  tlp429
                  tlp430))))))
       (list
        (mytest
         (tlp435
           `(,inst-struct-name408
             #\d
             ,inst-struct-name424
             ,inst-struct-name433))
         '(these
           ((+ 4 (+ 1 1)) (+ 4 5) are)
           (+ 4 (+ 1 1))
           #\b
           7
           #\a
           6
           #\b
           (+ 4 (+ 1 1))
           (+ 4 5)
           (+ 4 5)
           are
           9
           (#t #\a #f)
           (are #\b 4)
           1
           (+ 4 (+ 1 1))
           symbols
           1
           are
           (8 #t (+ 4 (+ 1 1)))))))
     (let ((tlp441
             (lambda (tlp436)
               (match
                tlp436
                (((tlp437 ..3) _ (and (tlp438 ..3) (tlp439 ..3)) tlp440)
                 (list tlp437 tlp438 tlp440))))))
       (list
        (mytest
         (tlp441 `((#f #t #f) #t (these #\c #\b) 1))
         '((#f #t #f) (these #\c #\b) 1))))))
  (define (mult-match-tests)
    (list
     (let ((tlp480
             (lambda (tlp442)
               (match
                tlp442
                (((and (tlp443 ___) (tlp444 ___))
                  #&(#&(#f tlp445 tlp446)
                     #((tlp447 __3) () 5 (or #(tlp448 ___) #(tlp448 ___)))
                     #&((or (not (not '(+ 4 (+ 1 1))))
                            (not (not '(+ 4 (+ 1 1)))))
                        ()
                        (tlp449 tlp450 tlp451 . #f)))
                  #(#\d
                    _
                    #&(#(tlp452 ___)
                       (and () ())
                       (and #(tlp453 ___) #(tlp454 ___)))
                    #((tlp455 ...)
                      (and (? number? tlp456) (? even? tlp457))
                      #(tlp458 tlp459 tlp460 tlp461)
                      #\b))
                  ())
                 (list
                  tlp443
                  tlp445
                  tlp446
                  tlp447
                  tlp448
                  tlp449
                  tlp450
                  tlp451
                  tlp452
                  tlp453
                  tlp455
                  tlp456
                  tlp458
                  tlp459
                  tlp460
                  tlp461))
                (((tlp462 ..3)
                  tlp463
                  ((#\d
                    (tlp464 tlp465 tlp466 tlp467)
                    (tlp468 tlp469 tlp470 quote these)
                    .
                    #\b)
                   'symbols
                   (and (? test-struct? tlp478)
                        (= test-struct-a tlp471)
                        (= test-struct-b ())
                        (= test-struct-c tlp472)
                        (= test-struct-d #(tlp473 tlp474 tlp475 tlp476)))
                   .
                   7)
                  tlp479)
                 (list
                  tlp462
                  tlp463
                  tlp464
                  tlp465
                  tlp466
                  tlp467
                  tlp468
                  tlp469
                  tlp470
                  tlp471
                  tlp472
                  tlp473
                  tlp474
                  tlp475
                  tlp476
                  tlp479))))))
       (list
        (mytest
         (tlp480
           `((#t #\d symbols)
             #&(#&(#f 3 #\d)
                #((are #\b #\c) () 5 #(#\b #f #f))
                #&((+ 4 (+ 1 1)) () (symbols #\c #\a . #f)))
             #(#\d
               2
               #&(#(#t 9 #f) () #((+ 4 5) #f 6))
               #((#\c 8 symbols) 2 #(7 (+ 1 2) #f #\d) #\b))
             ()))
         '((#t #\d symbols)
           3
           #\d
           (are #\b #\c)
           (#\b #f #f)
           symbols
           #\c
           #\a
           (#t 9 #f)
           ((+ 4 5) #f 6)
           (#\c 8 symbols)
           2
           7
           (+ 1 2)
           #f
           #\d))
        (mytest
         (tlp480
           `((are 0 are)
             #\b
             ((#\d ((+ 1 2) #\c symbols #\b) (#\a (+ 4 5) 3 . these) . #\b)
              symbols
              ,inst-struct-name477
              .
              7)
             #\d))
         '((are 0 are)
           #\b
           (+ 1 2)
           #\c
           symbols
           #\b
           #\a
           (+ 4 5)
           3
           #t
           these
           (+ 4 5)
           #t
           #t
           #\c
           #\d))))
     (let ((tlp508
             (lambda (tlp481)
               (match
                tlp481
                (((tlp482 tlp483 () . #\a)
                  ($
                   test-struct
                   ((tlp484 ___) tlp485 (not (not #t)) . 0)
                   ()
                   tlp486
                   (not (not 7)))
                  (and (? number? tlp488) (? even? tlp489))
                  #\a)
                 (list tlp482 tlp483 tlp484 tlp485 tlp486 tlp488))
                (((tlp490 ..3)
                  #&((and (? number? tlp491) (? even? tlp492))
                     (and #(#(tlp493 ...)
                            tlp494
                            #(tlp495 ...)
                            #&(tlp496 tlp497 tlp498))
                          #(#(tlp499 ...)
                            tlp500
                            #(tlp501 ...)
                            #&(tlp502 tlp503 tlp504)))
                     (and (? number? tlp505) (? even? tlp506)))
                  tlp507
                  #\c)
                 (list
                  tlp490
                  tlp491
                  tlp493
                  tlp494
                  tlp495
                  tlp496
                  tlp497
                  tlp498
                  tlp505
                  tlp507))))))
       (list
        (mytest
         (tlp508 `((symbols 4 () . #\a) ,inst-struct-name487 18 #\a))
         '(symbols 4 (7 these #\b) (+ 4 (+ 1 1)) (+ 4 5) 18))
        (mytest
         (tlp508
           `((7 #t #f)
             #&(14
                #(#(#\c are 7)
                  (+ 4 (+ 1 1))
                  #(#f 9 symbols)
                  #&(symbols #\a are))
                18)
             these
             #\c))
         '((7 #t #f)
           14
           (#\c are 7)
           (+ 4 (+ 1 1))
           (#f 9 symbols)
           symbols
           #\a
           are
           18
           these))))))
  (define (sing-match-lambda-tests)
    (list
     (let ((tlp550
             (match-lambda
               ((#&(#\d
                    #&(tlp509 'are _)
                    ($
                     test-struct
                     ($ test-struct tlp510 tlp511 tlp512 tlp513)
                     (tlp515 tlp516 tlp517 . #\a)
                     (tlp518 tlp519 tlp520 . #\b)
                     tlp521))
                 #(tlp523 ___)
                 (and (? test-struct? tlp548)
                      (= test-struct-a 'are)
                      (=
                       test-struct-b
                       (tlp524
                         (tlp525 ...)
                         (and ($ test-struct tlp526 tlp527 tlp528 tlp529)
                              ($ test-struct tlp531 tlp532 tlp533 tlp534))
                         tlp535))
                      (= test-struct-c (tlp536 ...))
                      (=
                       test-struct-d
                       (and (? test-struct? tlp546)
                            (=
                             test-struct-a
                             (and (? test-struct? tlp542)
                                  (= test-struct-a tlp537)
                                  (= test-struct-b tlp538)
                                  (= test-struct-c tlp539)
                                  (= test-struct-d tlp540)))
                            (=
                             test-struct-b
                             (and (? number? tlp543) (? even? tlp544)))
                            (= test-struct-c ())
                            (= test-struct-d ()))))
                 tlp549)
                (list
                 tlp509
                 tlp510
                 tlp511
                 tlp512
                 tlp513
                 tlp515
                 tlp516
                 tlp517
                 tlp518
                 tlp519
                 tlp520
                 tlp521
                 tlp523
                 tlp524
                 tlp525
                 tlp526
                 tlp527
                 tlp528
                 tlp529
                 tlp535
                 tlp536
                 tlp537
                 tlp538
                 tlp539
                 tlp540
                 tlp543
                 tlp549)))))
       (list
        (mytest
         (tlp550
           `(#&(#\d #&(#\a are are) ,inst-struct-name522)
             #(#\c #\b #f)
             ,inst-struct-name547
             7))
         '(#\a
           (+ 4 5)
           #\b
           (+ 4 (+ 1 1))
           #t
           #\b
           #t
           #\d
           7
           5
           #f
           #\c
           (#\c #\b #f)
           9
           (#f symbols #\d)
           8
           5
           #\c
           7
           symbols
           (4 #\b #t)
           (+ 1 2)
           4
           symbols
           (+ 1 2)
           8
           7))))
     (let ((tlp555
             (match-lambda
               ((tlp551 ($ test-struct #(tlp552 ...) 'are _ tlp553) () ())
                (list tlp551 tlp552 tlp553)))))
       (list
        (mytest
         (tlp555 `(#\b ,inst-struct-name554 () ()))
         '(#\b (#\d #f #\d) (+ 4 (+ 1 1))))))))
  (define (mult-match-lambda-tests)
    (list
     (let ((tlp580
             (match-lambda
               (((not (not #f))
                 #&(tlp556
                     (and (tlp557 ___) (tlp558 ___))
                     (and (? number? tlp559) (? even? tlp560)))
                 tlp561
                 ())
                (list tlp556 tlp557 tlp559 tlp561))
               ((_
                 (tlp562 ..3)
                 '(+ 4 (+ 1 1))
                 #((#(tlp563 tlp564 tlp565 tlp566)
                    tlp567
                    #&(tlp568 tlp569 tlp570)
                    tlp571)
                   (and tlp572 tlp573)
                   #&(#&(tlp574 tlp575 tlp576)
                      (not (not #f))
                      (tlp577 tlp578 tlp579 . #\b))
                   #f))
                (list
                 tlp562
                 tlp563
                 tlp564
                 tlp565
                 tlp566
                 tlp567
                 tlp568
                 tlp569
                 tlp570
                 tlp571
                 tlp572
                 tlp574
                 tlp575
                 tlp576
                 tlp577
                 tlp578
                 tlp579)))))
       (list
        (mytest
         (tlp580 `(#f #&(9 (#\c (+ 1 2) symbols) 16) #\c ()))
         '(9 (#\c (+ 1 2) symbols) 16 #\c))
        (mytest
         (tlp580
           `(#f
             (#\a #t 5)
             (+ 4 (+ 1 1))
             #((#(9 #\c (+ 4 (+ 1 1)) symbols) 7 #&(#\a #t (+ 4 5)) #t)
               #\b
               #&(#&(5 #\d #t) #f ((+ 1 2) #t #\b . #\b))
               #f)))
         '((#\a #t 5)
           9
           #\c
           (+ 4 (+ 1 1))
           symbols
           7
           #\a
           #t
           (+ 4 5)
           #t
           #\b
           5
           #\d
           #t
           (+ 1 2)
           #t
           #\b))))
     (let ((tlp604
             (match-lambda
               (((and (? number? tlp581) (? even? tlp582))
                 tlp583
                 #&('(+ 4 5)
                    #&(tlp584
                        (and (? number? tlp585) (? even? tlp586))
                        (or (or tlp587 tlp587) (or tlp587 tlp587)))
                    '(+ 4 5))
                 #(tlp588 ...))
                (list tlp581 tlp583 tlp584 tlp585 tlp587 tlp588))
               (((and (? test-struct? tlp601)
                      (= test-struct-a #f)
                      (= test-struct-b (tlp589 ...))
                      (=
                       test-struct-c
                       ((and (? number? tlp590) (? even? tlp591))
                        #(tlp592 ...)
                        (and (? test-struct? tlp598)
                             (= test-struct-a tlp593)
                             (= test-struct-b tlp594)
                             (= test-struct-c tlp595)
                             (= test-struct-d tlp596))
                        (tlp599 __3)))
                      (= test-struct-d '(+ 4 (+ 1 1))))
                 #f
                 #(tlp602 ___)
                 (tlp603 ___))
                (list
                 tlp589
                 tlp590
                 tlp592
                 tlp593
                 tlp594
                 tlp595
                 tlp596
                 tlp599
                 tlp602
                 tlp603)))))
       (list
        (mytest
         (tlp604
           `(18 #\a #&((+ 4 5) #&(these 0 #\d) (+ 4 5)) #(1 (+ 1 2) symbols)))
         '(18 #\a these 0 #\d (1 (+ 1 2) symbols)))
        (mytest
         (tlp604 `(,inst-struct-name600 #f #(4 0 #\d) (are 7 #\c)))
         '((these #\b 7)
           18
           (#f (+ 4 5) #t)
           #\d
           (+ 4 (+ 1 1))
           2
           #\a
           (#t 8 these)
           (4 0 #\d)
           (are 7 #\c)))))))
  (define (sing-match-lambda*-tests)
    (list
     (let ((tlp610
             (match-lambda*
               ((tlp606 tlp607 (tlp608 ___) tlp609)
                (list tlp606 tlp607 tlp608 tlp609)))))
       (list
        (mytest
         (tlp610 `#f `#\c `((+ 4 (+ 1 1)) 7 #t) `3)
         '(#f #\c ((+ 4 (+ 1 1)) 7 #t) 3))))
     (let ((tlp620
             (match-lambda*
               ((('(+ 4 5)
                  #(tlp612 ___)
                  (tlp613 __3)
                  ($ test-struct #&(tlp614 tlp615 tlp616) tlp617 () ()))
                 tlp619
                 #&(7 '(+ 4 (+ 1 1)) ())
                 _)
                (list tlp612 tlp613 tlp614 tlp615 tlp616 tlp617 tlp619)))))
       (list
        (mytest
         (tlp620
           `((+ 4 5) #(1 9 (+ 1 2)) (3 these these) ,inst-struct-name618)
           `#\c
           `#&(7 (+ 4 (+ 1 1)) ())
           `(+ 1 2))
         '((1 9 (+ 1 2)) (3 these these) (+ 4 5) #\d #\c are #\c))))))
  (define (mult-match-lambda*-tests)
    (list
     (let ((tlp645
             (match-lambda*
               ((#&((and (? number? tlp622) (? even? tlp623))
                    ((tlp624 ___)
                     (and (? number? tlp625) (? even? tlp626))
                     #\c
                     .
                     #\a)
                    tlp627)
                 tlp628
                 (tlp629 ___)
                 #\c)
                (list tlp622 tlp624 tlp625 tlp627 tlp628 tlp629))
               ((tlp630
                  (or #(_
                        #(tlp631 ...)
                        #((tlp632 tlp633 tlp634 . tlp635)
                          (and tlp636 tlp637)
                          #\b
                          ($ test-struct tlp638 tlp639 tlp640 tlp641))
                        ())
                      #(_
                        #(tlp631 ...)
                        #((tlp632 tlp633 tlp634 . tlp635)
                          (and tlp636 tlp637)
                          #\b
                          ($ test-struct tlp638 tlp639 tlp640 tlp641))
                        ()))
                  #f
                  (and (? number? tlp643) (? even? tlp644)))
                (list
                 tlp630
                 tlp631
                 tlp632
                 tlp633
                 tlp634
                 tlp635
                 tlp636
                 tlp638
                 tlp639
                 tlp640
                 tlp641
                 tlp643)))))
       (list
        (mytest
         (tlp645 `#&(4 ((1 these 7) 2 #\c . #\a) #t) `#\a `(7 #f #t) `#\c)
         '(4 (1 these 7) 2 #t #\a (7 #f #t)))
        (mytest
         (tlp645
           `#\c
           `#(are
              #(#\a (+ 1 2) #\d)
              #((#\c #\b #t . #\b) #f #\b ,inst-struct-name642)
              ())
           `#f
           `14)
         '(#\c
           (#\a (+ 1 2) #\d)
           #\c
           #\b
           #t
           #\b
           #f
           #\d
           (+ 4 5)
           (+ 4 5)
           (+ 1 2)
           14))))
     (let ((tlp672
             (match-lambda*
               ((#(#t
                   ()
                   (tlp647
                     (or #&(tlp648 tlp649 tlp650) #&(tlp648 tlp649 tlp650))
                     (tlp651 ...)
                     tlp652)
                   #f)
                 'these
                 tlp653
                 tlp654)
                (list tlp647 tlp648 tlp649 tlp650 tlp651 tlp652 tlp653 tlp654))
               ((((tlp655 __3)
                  (tlp656 ___)
                  ($
                   test-struct
                   (and (? test-struct? tlp662)
                        (= test-struct-a tlp657)
                        (= test-struct-b tlp658)
                        (= test-struct-c tlp659)
                        (= test-struct-d tlp660))
                   (tlp663 tlp664 tlp665 . #\a)
                   (tlp666 tlp667 tlp668 . #\b)
                   _)
                  quote
                  are)
                 (not (not 'symbols))
                 (and tlp670 tlp671)
                 _)
                (list
                 tlp655
                 tlp656
                 tlp657
                 tlp658
                 tlp659
                 tlp660
                 tlp663
                 tlp664
                 tlp665
                 tlp666
                 tlp667
                 tlp668
                 tlp670)))))
       (list
        (mytest
         (tlp672
           `#(#t () (#\c #&(#\a #t symbols) (symbols 2 #\a) (+ 4 5)) #f)
           `these
           `#\d
           `3)
         '(#\c #\a #t symbols (symbols 2 #\a) (+ 4 5) #\d 3))
        (mytest
         (tlp672
           `((#\d symbols #t) (symbols #t #\c) ,inst-struct-name669 . are)
           `symbols
           `#\c
           `#f)
         '((#\d symbols #t)
           (symbols #t #\c)
           #\a
           #\c
           #f
           #\c
           #\b
           #\b
           #f
           #\d
           #t
           (+ 1 2)
           #\c))))))
  

  (define (test-all)
    (list
     (let-tests)
     (define-tests)
     (sing-match-tests)
     (mult-match-tests)
     (sing-match-lambda-tests)
     (mult-match-lambda-tests)
     (sing-match-lambda*-tests)
     (mult-match-lambda*-tests)))

(test-all)


(mytest (letrec ((z 
                (lambda (x) 
                  (match x
                         ((a b c) 
                          (if (= a 10)
                              (list a b c)
                              (begin (cons a (z (list (add1 a) 2 3)))))))))) 
        (z '(1 2 3)))
      '(1 2 3 4 5 6 7 8 9 10 2 3))

; this is the same test for match-lambda

(mytest (letrec ((z 
                (match-lambda ((a b c) 
                               (if (= a 10)
                                   (list a b c)
                                   (cons a (z (list (add1 a) 2 3)))))))) 
        (z '(1 2 3)))
      '(1 2 3 4 5 6 7 8 9 10 2 3))

(mytest (letrec ((z 
                (match-lambda* ((a b c) 
                                (if (= a 10)
                                    (list a b c)
                                    (cons a (z (add1 a) 2 3))))))) 
        (z 1 2 3))
      '(1 2 3 4 5 6 7 8 9 10 2 3))
; matchlet tests

(mytest (match-let (((a b c) '(1 2 3))
                  ((d e f) '(4 5 6)))
                 (list a b c d e f))
      '(1 2 3 4 5 6))


; match: syntax error in (match (hey (((a b c) (d e f)) (list a b c d e f))))
(mytest (match-let hey (((a b c) '(1 2 3))
                      ((d e f) '(4 5 6)))
                 (list a b c d e f))
      '(1 2 3 4 5 6))

(mytest (match-let hey (((a b c) '(1 2 3))
                ((d e f) '(4 5 6)))
           (if (= a 10)
               '()
               (cons a  (hey (list (add1 a) b c) '(d e f)))))
      '(1 2 3 4 5 6 7 8 9))

 (mytest (let ((f 7))
         (match-let ([(a b c) (list 1 2 f)] [(d e) '(5 6)]) (list a d c f)))
       '(1 5 7 7))

; match-let*

(mytest (match-let* (((a b c) '(1 2 3))
                  ((d e f) '(4 5 6)))
                 (list a b c d e f))
      '(1 2 3 4 5 6))

(mytest (match-let* ([(a b c) '(1 2 3)]
                   [(d e f) (list a b c)])
                  (list d e f)) ; should be (1 2 3)
      '(1 2 3))


(mytest (let ((f 7))
        (match-let* ([(a b c) (list 1 2 f)] [(d e) '(5 6)]) (list a d c f)))
      '(1 5 7 7))
; match-letrec

;; let rec does not work this well on chez or plt
;(match-letrec ([(a b) (list (lambda (x) (if (zero? x) '() (cons x (a (sub1 x)))))
;                            (lambda (x) (if (= x 10) '() (cons x (b (add1 x))))))]
;               [(c d) (list (a 10) (b 0))])
;              (list c d))

(mytest (match-letrec (((a b c) '(1 2 3))
                  ((d e f) '(4 5 6)))
                 (list a b c d e f))
      '(1 2 3 4 5 6))

(mytest (match-letrec ([(a b) (list (lambda (x) (if (zero? x) '() (cons x (a (sub1 x)))))
                            (lambda (x) (if (= x 10) '() (cons x (b (add1 x))))))])
              (a 10)) 
      '(10 9 8 7 6 5 4 3 2 1))

(mytest (match-letrec ([(a b) (list (lambda (x) (if (zero? x) '() (cons (b x) (a (sub1 x)))))
                            (lambda (x) (if (= x 10) '() (cons  x (b (add1 x))))))])
              (a 10)) 
      '(() (9) (8 9) (7 8 9) (6 7 8 9) (5 6 7 8 9) (4 5 6 7 8 9) 
        (3 4 5 6 7 8 9) (2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9)))
                                                       

(mytest (let ((f 7))
        (match-letrec ([(a b c) (list 1 2 f)] [(d e) '(5 6)]) (list a d c f)))
      '(1 5 7 7))


; match-lambda


(mytest (let ((f 7))
        ((match-lambda ((a b) (list a b f))) '(4 5)))
      '(4 5 7))

(mytest (let ((f 7))
        ((match-lambda* ((a b) (list a b f))) 4 5))
      '(4 5 7))

; match-define

(mytest (let ((f 7))
        (match-define (a b c) (list 1 2 f))
        (list a b c f))
      '(1 2 7 7))


(let () (match-define (a b) (list (lambda (x) (if (zero? x) '() (cons (b x) (a (sub1 x)))))
                                (lambda (x) (if (= x 10) '() (cons  x (b (add1 x)))))))
        (mytest (a 10) 
              '(() (9) (8 9) (7 8 9) (6 7 8 9) (5 6 7 8 9) (4 5 6 7 8 9) 
                (3 4 5 6 7 8 9) (2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9))))


; this is some thing that I missed before

(mytest (match '((1) (2) (3))
       (((_) ...) 'hey))
      'hey)

; failure tests

(mytest (match '(1 2 3)
             ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
             ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
             ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
             ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
             ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
             ((a b c) (=> fail) (if (= a 1) (fail) 'bad))
             ((a b c)  (list a b c)))
      '(1 2 3))

;(mytest (match '(1 2 3)
;             ((a b c) (=> fail) (if (= a 1) (fail) 'bad)))
;      '()) ; this should through a different exception
      



; set! tests

; set! for lists
(mytest (let ((x '(1 2 (3 4))))
        (match x
               ((_ _ ((set! set-it) _)) (set-it 17)))
        x)
      '(1 2 (17 4)))

(mytest (let ((x '(1 2 (3 4))))
        (match x
               ((_ _ (_ (set! set-it))) (set-it 17)))
        x)
      '(1 2 (3 17)))

(mytest (let ((x '(1 2 (3 4))))
        (match x
               (((set! set-it) _ (_ _)) (set-it 17)))
        x)
      '(17 2 (3 4)))

(mytest (let ((x '(1 2 (3 4))))
        (match x
               ((_ (set! set-it) (_ _)) (set-it 17)))
        x)
      '(1 17 (3 4)))

;set! for improper lists

(mytest (let ((x '(1 2 (3 . 4) . 5)))
        (match x
               (((set! set-it) _ (_ . _) . _) (set-it 17)))
        x)
      '(17 2 (3 . 4) . 5))

(mytest (let ((x '(1 2 (3 . 4) . 5)))
        (match x
               ((_ (set! set-it) (_ . _) . _) (set-it 17)))
        x)
      '(1 17 (3 . 4) . 5))

(mytest (let ((x '(1 2 (3 . 4) . 5)))
        (match x
               ((_ _ ((set! set-it) . _) . _) (set-it 17)))
        x)
      '(1 2 (17 . 4) . 5))

(mytest (let ((x '(1 2 (3 . 4) . 5)))
        (match x
               ((_ _ (_ . (set! set-it)) . _) (set-it 17)))
        x)
      '(1 2 (3 . 17) . 5))

(mytest (let ((x '(1 2 (3 . 4) . 5)))
        (match x
               ((_ _ (_ . _) . (set! set-it)) (set-it 17)))
        x)
      '(1 2 (3 . 4) . 17))

;; set! for vectors

(mytest (let ((x #(1 2)))
        (match x
               (#(_ (set! set-it)) (set-it 17)))
        x)
      #(1 17))

(mytest (let ((x #(1 2)))
        (match x
               (#((set! set-it) _) (set-it 17)))
        x)
      #(17 2))

;; set! for boxes

(mytest (let ((x #&1))
        (match x
               (#&(set! set-it) (set-it 17)))
        x)
      #&17)

(mytest (let ((x #&(1 2)))
        (match x
               (#&(_ (set! set-it)) (set-it 17)))
        x)
      #&(1 17))

(mytest (let ((x #&#(1 2)))
        (match x
               (#&#(_ (set! set-it)) (set-it 17)))
        x)
      #&#(1 17))


; get! tests

; get! for lists

(mytest (let* ((x '(1 2 (3 4)))
             (f
        (match x
               ((_ _ ((get! get-it) _)) get-it))))
        (match x
               ((_ _ ((set! set-it) _)) (set-it 17)))
        (f)) 17)

(mytest (let* ((x '(1 2 (3 4)))
             (f
        (match x
               ((_ _ (_ (get! get-it))) get-it))))
        (match x
               ((_ _ (_ (set! set-it))) (set-it 17))) 
        (f)) 17)

(mytest (let* ((x '(1 2 (3 4)))
             (f
        (match x
               (((get! get-it) _ (_ _)) get-it))))
        (match x
               (((set! set-it) _ (_ _)) (set-it 17)))
        (f)) 17)


(mytest (let* ((x '(1 2 (3 4)))
             (f
        (match x
               ((_ (get! get-it) (_ _)) get-it))))
        (match x
               ((_ (set! set-it) (_ _)) (set-it 17)))
        (f)) 17)


;get! for improper lists

(mytest (let* ((x '(1 2 (3 . 4) . 5))
             (f
        (match x
               (((get! get-it) _ (_ . _) . _) get-it))))
        (match x
               (((set! set-it) _ (_ . _) . _) (set-it 17)))
        (f)) 17)


(mytest (let* ((x '(1 2 (3 . 4) . 5))
             (f
        (match x
               ((_ (get! get-it) (_ . _) . _) get-it))))
        (match x
               ((_ (set! set-it) (_ . _) . _) (set-it 17)))
        (f)) 17)


(mytest (let* ((x '(1 2 (3 . 4) . 5))
             (f
        (match x
               ((_ _ ((get! get-it) . _) . _) get-it))))
        (match x
               ((_ _ ((set! set-it) . _) . _) (set-it 17)))
        (f)) 17)


(mytest (let* ((x '(1 2 (3 . 4) . 5))
             (f
        (match x
               ((_ _ (_ . (get! get-it)) . _) get-it))))
        (match x
               ((_ _ (_ . (set! set-it)) . _) (set-it 17)))
        (f)) 17)


(mytest (let* ((x '(1 2 (3 . 4) . 5))
             (f
        (match x
               ((_ _ (_ . _) . (get! get-it)) get-it))))
        (match x
               ((_ _ (_ . _) . (set! set-it)) (set-it 17)))
        (f)) 17)


;; get! for vectors

(mytest (let* ((x #(1 2))
             (f
        (match x
               (#(_ (get! get-it)) get-it))))
        (match x
               (#(_ (set! set-it)) (set-it 17)))
        (f)) 17)


(mytest (let* ((x #(1 2))
             (f
        (match x
               (#((get! get-it) _) get-it))))
        (match x
               (#((set! set-it) _) (set-it 17)))
        (f)) 17)


;; get! for boxes

(mytest (let* ((x #&1)
             (f
        (match x
               (#&(get! get-it) get-it))))
        (match x
               (#&(set! set-it) (set-it 17)))
        (f)) 17)


(mytest (let* ((x #&(1 2))
             (f
        (match x
               (#&(_ (get! get-it)) get-it))))
        (match x
               (#&(_ (set! set-it)) (set-it 17)))
        (f)) 17)


(mytest (let* ((x #&#(1 2))
             (f
        (match x
               (#&#(_ (get! get-it)) get-it))))
        (match x
               (#&#(_ (set! set-it)) (set-it 17)))
        (f)) 17)



;; quasi quote tests


(mytest (match '(1 2 3 4 . b)
             (`(,b 2 ,@(3 4) . b) b))
      1)

(mytest (match '(1 2 3 4 . 5)
             (`(1 2 ,@(3 4) . ,b) b))
      5)
  
(mytest (match '(a ()) (`(a ()) #t))
        #t)

(mytest (match '(1 2 3)
             (`(,a ,b ,c) (list a b c)))
      '(1 2 3))

(mytest (match '(c a b 1 2 3 r f i) 
             (`(c a b ,@(a b c) r f i) (list a b c)))
      '(1 2 3))

(mytest (match '(3 4 #\c a b 1 (2 (c d)))
             (`(3 4 #\c a b ,a ,(b `(c e))) 'not-good)
             (`(3 4 #\c a b ,a ,(b `(c d))) (list a b)))
      '(1 2))

(mytest (match #(x 2 x)
       (`#(x ,x x) (list x)))
      '(2))

(mytest (match #(x 2 x)        ;remember that the x's are symbols here
       (`#(x ,x x) (list x)))
      '(2))

(mytest (match #(c a b 1 2 3 r f i) 
             (`#(c a b ,@(a b c) r f i) (list a b c)))
      '(1 2 3))

(mytest (match #&(c a b 1 2 3 r f i) 
             (`#&(c a b ,@(a b c) r f i) (list a b c)))
      '(1 2 3))

(mytest (match (list 
        "hi" 
        1 
        'there 
        #\c 
        #t 
        #f 
         '(a b c) 
         '(a b . c) 
         '(a b c c c c) 
         #(a b c) 
         #(a b c c c c) 
         #&(a b c) 
         '(1 2 3) 
         '(4 5 . 6) 
         '(7 8 9) 
         #(10 11 12)
         #&(13 14 15 16) 
         1 
         2 
        3 
         4 
         17
        )
 (`(
    "hi" 
    1 
    there 
    #\c 
    #t 
    #f 
   (a b c) 
    (a b . c) 
     (a b c ..2) 
     #(a b c) 
     #(a b c ..2) 
     #&(a b c) 
     ,(a b c) 
     ,(c1 d . e) 
     ,(f g h ...) 
     ,#(i j k)
     ,#&(l m n o) 
     ,@(1 2 3 4 p)
    )
  (list 
    a b c 
    c1 d e 
    f g h 
    i j k 
    l m n o 
    p
   )))
'(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))

(mytest (match (vector 
              "hi" 
              1 
              'there 
              #\c 
              #t 
              #f 
              '(a b c) 
              '(a b . c) 
              '(a b c c c c) 
              #(a b c) 
              #(a b c c c c) 
              #&(a b c) 
              '(1 2 3) 
              '(4 5 . 6) 
              '(7 8 9) 
              #(10 11 12)
              #&(13 14 15 16) 
              1 
              2 
              3 
              4 
              17
              )
             (`#(
                 "hi" 
                 1 
                 there 
                 #\c 
                 #t 
                 #f 
                 (a b c) 
                 (a b . c) 
                 (a b c ..2) 
                 #(a b c) 
                 #(a b c ..2) 
                 #&(a b c) 
                 ,(a b c) 
                 ,(c1 d . e) 
                 ,(f g h ...) 
                 ,#(i j k)
                 ,#&(l m n o) 
                 ,@(1 2 3 4 p)
                 )
              (list 
               a b c 
               c1 d e 
               f g h 
               i j k 
               l m n o 
               p
               )))
'(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))

(mytest (match (box (list 
              "hi" 
              1 
              'there 
              #\c 
              #t 
              #f 
              '(a b c) 
              '(a b . c) 
              '(a b c c c c) 
              #(a b c) 
              #(a b c c c c) 
              #&(a b c) 
              '(1 2 3) 
              '(4 5 . 6) 
              '(7 8 9) 
              #(10 11 12)
              #&(13 14 15 16) 
              1 
              2 
              3 
              4 
              17
              ))
             (`#&(
                 "hi" 
                 1 
                 there 
                 #\c 
                 #t 
                 #f 
                 (a b c) 
                 (a b . c) 
                 (a b c ..2) 
                 #(a b c) 
                 #(a b c ..2) 
                 #&(a b c) 
                 ,(a b c) 
                 ,(c1 d . e) 
                 ,(f g h ...) 
                 ,#(i j k)
                 ,#&(l m n o) 
                 ,@(1 2 3 4 p)
                 )
              (list 
               a b c 
               c1 d e 
               f g h 
               i j k 
               l m n o 
               p
               )))
'(1 2 3 4 5 6 7 8 (9) 10 11 12 13 14 15 16 17))

(mytest (match '(1 2 3 4)
            (`(,@`(,x ,y) ,@`(,a ,b)) (list x y a b)))
      '(1 2 3 4))


;; deep nesting 

(mytest (match #(#(#(1 2 3) #(1 2 3) #(2 3 4)) #(#(1 2 3) #(1 2 3) #(2 3 4)))
             (#(#(#(a ...) ...) ...) a))
      '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4))))

(mytest (match '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4)))
             ((((a ...) ...) ...) a))
      '(((1 2 3) (1 2 3) (2 3 4)) ((1 2 3) (1 2 3) (2 3 4))))

(mytest (match '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
             (((((((a ...) ...) ...) ...) ...) ...) a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))



(mytest (match #(#(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8)))))
               #(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
             (#(#(#(#(#(#(a ...) ...) ...) ...) ...) ...) a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))

(mytest (match '(#((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                 (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8)))))
               #((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                 (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))))
             ((#((#((#(a ...) ...) ...) ...) ...) ...) a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))

(mytest (match '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
             (((((((a ..2) ..2) ..2) ..2) ..2) ..2) a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))



(mytest (match #(#(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8)))))
               #(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
             (#(#(#(#(#(#(a ..2) ..2) ..2) ..2) ..2) ..2) a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))

(mytest (match '(#((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                 (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8)))))
               #((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                 (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))))
             ((#((#((#(a ..2) ..2) ..2) ..2) ..2) ..2) a))
      '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))))


(mytest (match '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
             (((((((_ ...) ...) ...) ...) ...) ...) #t)
             (_ #f))
      #t)

(mytest (match #(#(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8)))))
               #(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
             (#(#(#(#(#(#(_ ...) ...) ...) ...) ...) ...) #t)
             (_ #f))
      #t)

(mytest (match '(#((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                 (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8)))))
               #((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                 (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))))
             ((#((#((#(_ ...) ...) ...) ...) ...) ...) #t)
             (_ #f))
      #t)

(mytest (match '((((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8)))))
               (((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))
                ((((1 2) (3 4)) ((5 6) (7 8))) (((1 2) (3 4)) ((5 6) (7 8))))))
             (((((((a b) ...) ...) ...) ...) ...) (list a b)))
       '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))) ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
 (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))) ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8))))))) 


(mytest (match #(#(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8)))))
               #(#(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))
                 #(#(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))) #(#(#(1 2) #(3 4)) #(#(5 6) #(7 8))))))
             (#(#(#(#(#(#(a b) ...) ...) ...) ...) ...) (list a b)))
      '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))) ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
 (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))) ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))))))

(mytest (match '(#((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                 (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8)))))
               #((#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))
                 (#((#(1 2) #(3 4)) (#(5 6) #(7 8))) #((#(1 2) #(3 4)) (#(5 6) #(7 8))))))
             ((#((#((#(a b) ...) ...) ...) ...) ...) (list a b)))
      '((((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))) ((((1 3) (5 7)) ((1 3) (5 7))) (((1 3) (5 7)) ((1 3) (5 7)))))
 (((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))) ((((2 4) (6 8)) ((2 4) (6 8))) (((2 4) (6 8)) ((2 4) (6 8)))))))


;the new var pattern
; this allows one to use 
; var, $, =, and, or, not, ?, set!, or get!
; as pattern variables
; (mytest (match '(1 2 3)
;             (((var $) b c) (list $ b c)))
;       '(1 2 3))

; (mytest (match '(1 2 3)
;             (((var var) b c) (list var b c)))
;       '(1 2 3))

; (mytest (match '(1 2 3)
;             (((var =) b c) (list = b c)))
;       '(1 2 3))

; (mytest (match '(1 2 3)
;             (((var and) b c) (list and b c)))
;       '(1 2 3))

; (mytest (match '(1 2 3)
;             (((var or) b c) (list or b c)))
;       '(1 2 3))

; (mytest (match '(1 2 3)
;             (((var not) b c) (list not b c)))
;       '(1 2 3))

; (mytest (match '(1 2 3)
;             (((var ?) b c) (list ? b c)))
;       '(1 2 3))

; (mytest (match '(1 2 3)
;             (((var set!) b c) (list set! b c)))
;       '(1 2 3))

; (mytest (match '(1 2 3)
;             (((var get!) b c) (list get! b c)))
;       '(1 2 3))


(mytest (match '((1 1 2 2) (1 1 2 2) 5 5 5)
             (((1 ... a ...) ... 7 ...) #f)
             (((1 ... a ...) ... 6 ...) #f)
             (((1 ... a ...) ... 5 ...) a))
      '((2 2) (2 2)))

(mytest (match '(1 1 1 1 1 2 2 2 2)
             ((1 ... 2 2 2 2) #t))
      #t)
(mytest (match '(1 1 1 1 1 2 2 2 2)
             ((1 ... 2 ...) #t))
      #t)

(mytest (match '(1 1 1 1 1 2 2 2 2)
             (((and (not 2) a) ... 2 ...) a))
      '(1 1 1 1 1))

(mytest (match '(1 1 1 1 1 2 2 2 2)
             ((a ... 2 ...) a))
      '(1 1 1 1 1 2 2 2 2))

(mytest (match '(1 1 1 1 1 2 2 2 2)
             ((_ ... 2 ...) #t))
      #t)

(mytest (match '(pattern matching in scheme is very cool)
             (((and (not 'in) a) ... (and (not 'is) b) ... c ...) (list a c b)))
      '((pattern matching) (is very cool) (in scheme)))

(mytest (match '((1 1 2 2) (1 1 2 2) 5 5 5)
       (((1 ... 2 ...) ... 5 ...) #t))
      #t)

(mytest (match #(1 3 1 9 8 4 2 2 4 7 a b c) (#((and (? odd?) a) ... 8 (and (? even?) b) ... 7 r ...) (list a b r)))
      '((1 3 1 9) (4 2 2 4) (a b c)))

(mytest (match #(#(1 1 2 2) #(1 1 2 2) 5 5 5)
             (#(#(1 ... 2 ...) ... 5 ...) #t))
      #t)


(mytest (match #(#(1 1 2 2) #(1 1 2 2) 5 5 5)
             (#(#(1 ... a ...) ... 7 ...) #f)
             (#(#(1 ... a ...) ... 6 ...) #f)
             (#(#(1 ... a ...) ... 5 ...) a))
      '((2 2) (2 2)))

(mytest (match #(#(1 2) #(1 2) #(1 2) 5 6) (#(#(_ _) ..3 a ...) a))
      '(5 6))
; should not work
; (match x ((... ...) #t))


; should not work
; (match x ((pat ... ... pat) #t))

(mytest (match #(1 2 3 4 5) (#(a b (and c (not 5)) ... d) (list a b c d)))
      '(1 2 (3 4) 5))


;; list-no-order

;(mytest (match '(1 2 3 4) ((list-no-order a b 1 c) (list a b c)))
;      '(2 3 4))



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



(define (let-tests)
    (list
     (mytest
      (match-let*
        ((((tlp3121 __3)
           ()
           (tlp3122 __3)
           (and (? number? tlp3123) (? even? tlp3124)))
          `((#\a symbols #\a) () (#\b #f 0) 6)))
        (list tlp3121 tlp3122 tlp3123))
      '((#\a symbols #\a) (#\b #f 0) 6))
     (mytest
      (match-let*
        ((((and (and (? test-struct? tlp3158)
                     (=
                      test-struct-a
                      #&((tlp3125 tlp3126 tlp3127 tlp3128)
                         (tlp3129 ___)
                         (and (? test-struct? tlp3135)
                              (= test-struct-a tlp3130)
                              (= test-struct-b tlp3131)
                              (= test-struct-c tlp3132)
                              (= test-struct-d tlp3133))))
                     (=
                      test-struct-b
                      ((tlp3136 tlp3137 tlp3138 tlp3139)
                       (and (? test-struct? tlp3145)
                            (= test-struct-a tlp3140)
                            (= test-struct-b tlp3141)
                            (= test-struct-c tlp3142)
                            (= test-struct-d tlp3143))
                       #(tlp3146 tlp3147 tlp3148 tlp3149)
                       .
                       #\d))
                     (= test-struct-c (tlp3150 __3))
                     (=
                      test-struct-d
                      (#\b
                       tlp3151
                       (and (? number? tlp3152) (? even? tlp3153))
                       #&(tlp3154 tlp3155 tlp3156))))
                (and (? test-struct? tlp3159)
                     (=
                      test-struct-a
                      #&((tlp3160 tlp3161 tlp3162 tlp3163)
                         (tlp3164 ___)
                         (and (? test-struct? tlp3165)
                              (= test-struct-a tlp3166)
                              (= test-struct-b tlp3167)
                              (= test-struct-c tlp3168)
                              (= test-struct-d tlp3169))))
                     (=
                      test-struct-b
                      ((tlp3170 tlp3171 tlp3172 tlp3173)
                       (and (? test-struct? tlp3174)
                            (= test-struct-a tlp3175)
                            (= test-struct-b tlp3176)
                            (= test-struct-c tlp3177)
                            (= test-struct-d tlp3178))
                       #(tlp3179 tlp3180 tlp3181 tlp3182)
                       .
                       #\d))
                     (= test-struct-c (tlp3183 __3))
                     (=
                      test-struct-d
                      (#\b
                       tlp3184
                       (and (? number? tlp3185) (? even? tlp3186))
                       #&(tlp3187 tlp3188 tlp3189)))))
           (not (not #\a))
           ($
            test-struct
            #(tlp3190 ...)
            tlp3191
            (and (? test-struct? tlp3198)
                 (= test-struct-a (tlp3192 __3))
                 (= test-struct-b tlp3193)
                 (= test-struct-c (and tlp3194 tlp3195))
                 (= test-struct-d (tlp3196 ...)))
            tlp3199)
           (tlp3201 __3))
          `(,inst-struct-name3157 #\a ,inst-struct-name3200 (#t #\b symbols))))
        (list
         tlp3125
         tlp3126
         tlp3127
         tlp3128
         tlp3129
         tlp3130
         tlp3131
         tlp3132
         tlp3133
         tlp3136
         tlp3137
         tlp3138
         tlp3139
         tlp3140
         tlp3141
         tlp3142
         tlp3143
         tlp3146
         tlp3147
         tlp3148
         tlp3149
         tlp3150
         tlp3151
         tlp3152
         tlp3154
         tlp3155
         tlp3156
         tlp3190
         tlp3191
         tlp3192
         tlp3193
         tlp3194
         tlp3196
         tlp3199
         tlp3201))
      '(#f
        (+ 1 2)
        #\d
        #\b
        (#\d 1 symbols)
        6
        #t
        (+ 4 (+ 1 1))
        #\a
        5
        7
        symbols
        #t
        #\a
        3
        (+ 4 5)
        7
        (+ 4 (+ 1 1))
        #f
        #\a
        #\d
        (#\a (+ 4 (+ 1 1)) these)
        3
        4
        (+ 4 (+ 1 1))
        #f
        2
        (#\b symbols (+ 1 2))
        symbols
        (8 #\b (+ 1 2))
        (+ 1 2)
        #\a
        ((+ 1 2) #f 5)
        5
        (#t #\b symbols)))
     (mytest
      (match-let
        (((((and tlp3202 tlp3203) (tlp3204 ___) tlp3205 tlp3206)
           tlp3207
           tlp3208
           tlp3209)
          `((these ((+ 4 5) #\b 6) #\a #\c) #\b (+ 4 5) 0)))
        (list tlp3202 tlp3204 tlp3205 tlp3206 tlp3207 tlp3208 tlp3209))
      '(these ((+ 4 5) #\b 6) #\a #\c #\b (+ 4 5) 0))
     (mytest
      (match-let*
        ((((and () ()) tlp3210 tlp3211 _) `(() #t (+ 4 (+ 1 1)) #\a)))
        (list tlp3210 tlp3211))
      '(#t (+ 4 (+ 1 1))))
     (mytest
      (match-let
        ((((and (? number? tlp3212) (? even? tlp3213))
           (and (? number? tlp3214) (? even? tlp3215))
           (and (tlp3216 ___) (tlp3217 ___))
           tlp3218)
          `(2 16 (#\c symbols (+ 1 2)) 7)))
        (list tlp3212 tlp3214 tlp3216 tlp3218))
      '(2 16 (#\c symbols (+ 1 2)) 7))))

(let-tests)

(define (multi-let-tests)
    (list
     (mytest
      (match-let
        (((#(tlp3219 ...) #(tlp3220 ...) #(tlp3221 ___) ())
          `(#(#\d #f #f) #(#t (+ 1 2) (+ 4 5)) #(#t are #t) ()))
         (('are tlp3222 #\a 9) `(are 3 #\a 9))
         ((tlp3223
            ((#\b
              ($ test-struct tlp3224 tlp3225 tlp3226 tlp3227)
              #f
              quote
              these)
             ()
             (#(tlp3229 ...)
              (or (and (? test-struct? tlp3235)
                       (= test-struct-a tlp3230)
                       (= test-struct-b tlp3231)
                       (= test-struct-c tlp3232)
                       (= test-struct-d tlp3233))
                  (and (? test-struct? tlp3235)
                       (= test-struct-a tlp3230)
                       (= test-struct-b tlp3231)
                       (= test-struct-c tlp3232)
                       (= test-struct-d tlp3233)))
              #(tlp3236 ___)
              .
              5)
             ())
            (tlp3237 ..3)
            (and (? test-struct? tlp3240)
                 (= test-struct-a (or () ()))
                 (= test-struct-b #(tlp3238 ...))
                 (= test-struct-c (not (not #\c)))
                 (= test-struct-d #\b)))
          `(#f
            ((#\b ,inst-struct-name3228 #f . these)
             ()
             (#((+ 1 2) #\c 2) ,inst-struct-name3234 #(#t #\b are) . 5)
             ())
            (#\d are (+ 4 5))
            ,inst-struct-name3239))
         (((tlp3241 ___)
           ()
           ((and (and (? test-struct? tlp3253)
                      (= test-struct-a (tlp3242 tlp3243 tlp3244 . tlp3245))
                      (= test-struct-b #(tlp3246 tlp3247 tlp3248 tlp3249))
                      (=
                       test-struct-c
                       (and (? number? tlp3250) (? even? tlp3251)))
                      (= test-struct-d _))
                 (and (? test-struct? tlp3254)
                      (= test-struct-a (tlp3255 tlp3256 tlp3257 . tlp3258))
                      (= test-struct-b #(tlp3259 tlp3260 tlp3261 tlp3262))
                      (=
                       test-struct-c
                       (and (? number? tlp3263) (? even? tlp3264)))
                      (= test-struct-d _)))
            #(#\a
              (or 'symbols 'symbols)
              #(tlp3265 tlp3266 tlp3267 tlp3268)
              #&(tlp3269 tlp3270 tlp3271))
            (and (? number? tlp3272) (? even? tlp3273))
            (tlp3274 ..3))
           tlp3275)
          `((#t #\b symbols)
            ()
            (,inst-struct-name3252
             #(#\a symbols #(these #\a #f #\a) #&(#t (+ 4 (+ 1 1)) #\b))
             4
             (#t #\d #\c))
            are))
         (((tlp3276
             ($
              test-struct
              tlp3277
              (and #(tlp3278 tlp3279 tlp3280 tlp3281)
                   #(tlp3282 tlp3283 tlp3284 tlp3285))
              (and (not (not #t)) (not (not #t)))
              tlp3286)
             #&((tlp3288 ___) (tlp3289 __3) tlp3290)
             .
             #\b)
           ((or () ())
            (and (? test-struct? tlp3298)
                 (= test-struct-a _)
                 (= test-struct-b tlp3291)
                 (= test-struct-c #(tlp3292 ...))
                 (= test-struct-d #(tlp3293 tlp3294 tlp3295 tlp3296)))
            tlp3299
            (and (? test-struct? tlp3313)
                 (= test-struct-a #&(tlp3300 tlp3301 tlp3302))
                 (=
                  test-struct-b
                  ($ test-struct tlp3303 tlp3304 tlp3305 tlp3306))
                 (=
                  test-struct-c
                  (or (tlp3308 tlp3309 tlp3310 tlp3311)
                      (tlp3308 tlp3309 tlp3310 tlp3311)))
                 (= test-struct-d _)))
           #&((and (? test-struct? tlp3323)
                   (= test-struct-a (tlp3314 ...))
                   (= test-struct-b (tlp3315 tlp3316 tlp3317 . #\a))
                   (= test-struct-c #(tlp3318 ___))
                   (= test-struct-d #&(tlp3319 tlp3320 tlp3321)))
              #&((tlp3324 __3) #(tlp3325 ___) (tlp3326 ___))
              ((tlp3327 ...) tlp3328 (tlp3329 ...) (tlp3330 __3)))
           (tlp3331 ___))
          `((these
             ,inst-struct-name3287
             #&((5 #\a 4) (#\b #\a #f) these)
             .
             #\b)
            (() ,inst-struct-name3297 (+ 4 5) ,inst-struct-name3312)
            #&(,inst-struct-name3322
               #&((are 6 2) #((+ 4 5) these #\a) ((+ 1 2) 0 #\b))
               ((#\b these 2) 0 (1 (+ 4 (+ 1 1)) #t) (#\a #t #\a)))
            (these #\d #\b))))
        (list
         tlp3219
         tlp3220
         tlp3221
         tlp3222
         tlp3223
         tlp3224
         tlp3225
         tlp3226
         tlp3227
         tlp3229
         tlp3230
         tlp3231
         tlp3232
         tlp3233
         tlp3236
         tlp3237
         tlp3238
         tlp3241
         tlp3242
         tlp3243
         tlp3244
         tlp3245
         tlp3246
         tlp3247
         tlp3248
         tlp3249
         tlp3250
         tlp3265
         tlp3266
         tlp3267
         tlp3268
         tlp3269
         tlp3270
         tlp3271
         tlp3272
         tlp3274
         tlp3275
         tlp3276
         tlp3277
         tlp3278
         tlp3279
         tlp3280
         tlp3281
         tlp3286
         tlp3288
         tlp3289
         tlp3290
         tlp3291
         tlp3292
         tlp3293
         tlp3294
         tlp3295
         tlp3296
         tlp3299
         tlp3300
         tlp3301
         tlp3302
         tlp3303
         tlp3304
         tlp3305
         tlp3306
         tlp3308
         tlp3309
         tlp3310
         tlp3311
         tlp3314
         tlp3315
         tlp3316
         tlp3317
         tlp3318
         tlp3319
         tlp3320
         tlp3321
         tlp3324
         tlp3325
         tlp3326
         tlp3327
         tlp3328
         tlp3329
         tlp3330
         tlp3331))
      '((#\d #f #f)
        (#t (+ 1 2) (+ 4 5))
        (#t are #t)
        3
        #f
        these
        3
        #\d
        #t
        ((+ 1 2) #\c 2)
        (+ 4 5)
        #\a
        #\a
        #f
        (#t #\b are)
        (#\d are (+ 4 5))
        (#\d #\b #\b)
        (#t #\b symbols)
        #t
        #\c
        #\c
        are
        (+ 1 2)
        (+ 4 5)
        #\b
        9
        16
        these
        #\a
        #f
        #\a
        #t
        (+ 4 (+ 1 1))
        #\b
        4
        (#t #\d #\c)
        are
        these
        #\d
        (+ 4 5)
        #t
        (+ 4 5)
        #t
        (+ 4 (+ 1 1))
        (5 #\a 4)
        (#\b #\a #f)
        these
        #f
        (are symbols #\c)
        these
        symbols
        #t
        #\d
        (+ 4 5)
        symbols
        #\d
        #\c
        #t
        (+ 4 (+ 1 1))
        #\d
        #f
        these
        are
        (+ 1 2)
        (+ 4 5)
        (#t 6 #\c)
        #t
        #\d
        2
        (#f 6 #\b)
        #f
        7
        symbols
        (are 6 2)
        ((+ 4 5) these #\a)
        ((+ 1 2) 0 #\b)
        (#\b these 2)
        0
        (1 (+ 4 (+ 1 1)) #t)
        (#\a #t #\a)
        (these #\d #\b)))
     (mytest
      (match-let*
        (((#\d
           ((not (not '(+ 4 5))) tlp3332 (not (not #f)) #t)
           #&(tlp3333
               (not (not 7))
               (#(tlp3334 tlp3335 tlp3336 tlp3337)
                #&(tlp3338 tlp3339 tlp3340)
                tlp3341
                .
                9))
           (or (tlp3342 ___) (tlp3342 ___)))
          `(#\d
            ((+ 4 5) 9 #f #t)
            #&(#f 7 (#(4 symbols are are) #&((+ 4 (+ 1 1)) #\d 3) these . 9))
            (#\d these 7)))
         ((((and (tlp3343 ...) (tlp3344 ...)) () tlp3345 . #\a)
           ((not (not #t)) _ _ . #f)
           #\a
           #(_
             (or (_
                  tlp3346
                  (tlp3347 tlp3348 tlp3349 tlp3350)
                  (and (? test-struct? tlp3356)
                       (= test-struct-a tlp3351)
                       (= test-struct-b tlp3352)
                       (= test-struct-c tlp3353)
                       (= test-struct-d tlp3354)))
                 (_
                  tlp3346
                  (tlp3347 tlp3348 tlp3349 tlp3350)
                  (and (? test-struct? tlp3356)
                       (= test-struct-a tlp3351)
                       (= test-struct-b tlp3352)
                       (= test-struct-c tlp3353)
                       (= test-struct-d tlp3354))))
             #&(#(tlp3357 tlp3358 tlp3359 tlp3360)
                #\d
                (and (? number? tlp3361) (? even? tlp3362)))
             #(($ test-struct tlp3363 tlp3364 tlp3365 tlp3366)
               ($ test-struct tlp3368 tlp3369 tlp3370 tlp3371)
               (and (? number? tlp3373) (? even? tlp3374))
               tlp3375)))
          `(((#\a #\b #f) () #f . #\a)
            (#t #f #t . #f)
            #\a
            #((+ 1 2)
              (3 #f (#\d are #\c these) ,inst-struct-name3355)
              #&(#(#t (+ 1 2) #\a #\b) #\d 0)
              #(,inst-struct-name3367 ,inst-struct-name3372 4 (+ 4 5)))))
         (((and tlp3376 tlp3377) #\a () #(tlp3378 ...)) `(3 #\a () #(1 #t 1)))
         ((_
           #((()
              (and #(tlp3379 tlp3380 tlp3381 tlp3382)
                   #(tlp3383 tlp3384 tlp3385 tlp3386))
              (tlp3387 ..3)
              .
              3)
             ((tlp3388 tlp3389 tlp3390 . #\a)
              (or (tlp3391 ..3) (tlp3391 ..3))
              _
              ($ test-struct tlp3392 tlp3393 tlp3394 tlp3395))
             (and (? number? tlp3397) (? even? tlp3398))
             tlp3399)
           tlp3400
           ())
          `(#\d
            #((() #(these 7 #t #\a) (#\d #f #t) . 3)
              (((+ 1 2) (+ 4 5) are . #\a)
               (#\d #t these)
               (+ 4 5)
               ,inst-struct-name3396)
              6
              (+ 1 2))
            #\a
            ()))
         ((#(tlp3401 ___)
           '(+ 1 2)
           #(_
             '(+ 1 2)
             (and #&((tlp3402 tlp3403 tlp3404 tlp3405) tlp3406 '(+ 1 2))
                  #&((tlp3407 tlp3408 tlp3409 tlp3410) tlp3411 '(+ 1 2)))
             (tlp3412 ..3))
           _)
          `(#(#\c #\b these)
            (+ 1 2)
            #(these (+ 1 2) #&((are 4 #f #\d) symbols (+ 1 2)) (#f #\a 6))
            these)))
        (list
         tlp3332
         tlp3333
         tlp3334
         tlp3335
         tlp3336
         tlp3337
         tlp3338
         tlp3339
         tlp3340
         tlp3341
         tlp3342
         tlp3343
         tlp3345
         tlp3346
         tlp3347
         tlp3348
         tlp3349
         tlp3350
         tlp3351
         tlp3352
         tlp3353
         tlp3354
         tlp3357
         tlp3358
         tlp3359
         tlp3360
         tlp3361
         tlp3363
         tlp3364
         tlp3365
         tlp3366
         tlp3368
         tlp3369
         tlp3370
         tlp3371
         tlp3373
         tlp3375
         tlp3376
         tlp3378
         tlp3379
         tlp3380
         tlp3381
         tlp3382
         tlp3387
         tlp3388
         tlp3389
         tlp3390
         tlp3391
         tlp3392
         tlp3393
         tlp3394
         tlp3395
         tlp3397
         tlp3399
         tlp3400
         tlp3401
         tlp3402
         tlp3403
         tlp3404
         tlp3405
         tlp3406
         tlp3412))
      '(9
        #f
        4
        symbols
        are
        are
        (+ 4 (+ 1 1))
        #\d
        3
        these
        (#\d these 7)
        (#\a #\b #f)
        #f
        #f
        #\d
        are
        #\c
        these
        (+ 1 2)
        these
        #t
        #t
        #t
        (+ 1 2)
        #\a
        #\b
        0
        #\d
        #\a
        6
        (+ 1 2)
        symbols
        #\a
        3
        #\c
        4
        (+ 4 5)
        3
        (1 #t 1)
        these
        7
        #t
        #\a
        (#\d #f #t)
        (+ 1 2)
        (+ 4 5)
        are
        (#\d #t these)
        0
        #\d
        #\a
        #\b
        6
        (+ 1 2)
        #\a
        (#\c #\b these)
        are
        4
        #f
        #\d
        symbols
        (#f #\a 6)))
     (mytest
      (match-let
        (((_
           (tlp3413 ...)
           (#&(#(tlp3414 ___)
               ($ test-struct tlp3415 tlp3416 tlp3417 tlp3418)
               (and (? number? tlp3420) (? even? tlp3421)))
            ((not (not #\b))
             (tlp3422 tlp3423 tlp3424 . #\a)
             (tlp3425 tlp3426 tlp3427 tlp3428)
             .
             #\c)
            (tlp3429 ...)
            tlp3430)
           tlp3431)
          `((+ 4 5)
            (these #f #\b)
            (#&(#(7 4 2) ,inst-struct-name3419 8)
             (#\b (#\c 2 #t . #\a) (#f (+ 4 (+ 1 1)) #t #\b) . #\c)
             (#\b 0 2)
             #f)
            #\b))
         ((tlp3432 tlp3433 (tlp3434 ...) ()) `((+ 4 5) #\c (7 #t #\c) ()))
         (((tlp3435 ...)
           (and (? test-struct? tlp3445)
                (= test-struct-a (tlp3436 ..3))
                (= test-struct-b ())
                (=
                 test-struct-c
                 ((tlp3437 ___)
                  tlp3438
                  #(tlp3439 tlp3440 tlp3441 tlp3442)
                  .
                  5))
                (= test-struct-d #(tlp3443 ___)))
           #((and (and (? test-struct? tlp3456)
                       (= test-struct-a ())
                       (= test-struct-b (tlp3446 tlp3447 tlp3448 . tlp3449))
                       (= test-struct-c #&(tlp3450 tlp3451 tlp3452))
                       (= test-struct-d (and tlp3453 tlp3454)))
                  (and (? test-struct? tlp3457)
                       (= test-struct-a ())
                       (= test-struct-b (tlp3458 tlp3459 tlp3460 . tlp3461))
                       (= test-struct-c #&(tlp3462 tlp3463 tlp3464))
                       (= test-struct-d (and tlp3465 tlp3466))))
             tlp3467
             #t
             #&((not (not 2)) #&(tlp3468 tlp3469 tlp3470) _))
           (and (? test-struct? tlp3475)
                (= test-struct-a (_ (or tlp3471 tlp3471) (not (not #f)) . #\d))
                (= test-struct-b tlp3472)
                (= test-struct-c ())
                (= test-struct-d (tlp3473 ...))))
          `((these symbols 9)
            ,inst-struct-name3444
            #(,inst-struct-name3455 #\c #t #&(2 #&(are #f (+ 1 2)) #\b))
            ,inst-struct-name3474))
         ((($
            test-struct
            (and (? number? tlp3476) (? even? tlp3477))
            ((or (and (? test-struct? tlp3483)
                      (= test-struct-a tlp3478)
                      (= test-struct-b tlp3479)
                      (= test-struct-c tlp3480)
                      (= test-struct-d tlp3481))
                 (and (? test-struct? tlp3483)
                      (= test-struct-a tlp3478)
                      (= test-struct-b tlp3479)
                      (= test-struct-c tlp3480)
                      (= test-struct-d tlp3481)))
             #(tlp3484 tlp3485 tlp3486 tlp3487)
             #&(tlp3488 tlp3489 tlp3490)
             '(+ 4 5))
            #(tlp3491 ___)
            tlp3492)
           (tlp3494 ..3)
           (tlp3495 ..3)
           tlp3496)
          `(,inst-struct-name3493
            (#t #\a (+ 4 (+ 1 1)))
            (are (+ 1 2) these)
            (+ 4 (+ 1 1))))
         (((#&((tlp3497 tlp3498 tlp3499 tlp3500)
               (and _ _)
               (and (? test-struct? tlp3506)
                    (= test-struct-a tlp3501)
                    (= test-struct-b tlp3502)
                    (= test-struct-c tlp3503)
                    (= test-struct-d tlp3504)))
            (tlp3507 ...)
            (and (? test-struct? tlp3513)
                 (= test-struct-a #&(tlp3508 tlp3509 tlp3510))
                 (= test-struct-b ())
                 (= test-struct-c _)
                 (= test-struct-d (tlp3511 ___)))
            (or (and ($ test-struct tlp3514 tlp3515 tlp3516 tlp3517)
                     ($ test-struct tlp3519 tlp3520 tlp3521 tlp3522))
                (and ($ test-struct tlp3514 tlp3515 tlp3516 tlp3517)
                     ($ test-struct tlp3519 tlp3520 tlp3521 tlp3522))))
           ((not (not 9))
            #f
            '(+ 4 5)
            #&((and (? test-struct? tlp3528)
                    (= test-struct-a tlp3523)
                    (= test-struct-b tlp3524)
                    (= test-struct-c tlp3525)
                    (= test-struct-d tlp3526))
               (tlp3529 __3)
               #(tlp3530 tlp3531 tlp3532 tlp3533)))
           (not (not #\d))
           9)
          `((#&((#f #\c (+ 4 5) #\d) #t ,inst-struct-name3505)
             (4 #\d #\c)
             ,inst-struct-name3512
             ,inst-struct-name3518)
            (9
             #f
             (+ 4 5)
             #&(,inst-struct-name3527 (#\c #\d #\c) #(symbols #\a #f #\c)))
            #\d
            9)))
        (list
         tlp3413
         tlp3414
         tlp3415
         tlp3416
         tlp3417
         tlp3418
         tlp3420
         tlp3422
         tlp3423
         tlp3424
         tlp3425
         tlp3426
         tlp3427
         tlp3428
         tlp3429
         tlp3430
         tlp3431
         tlp3432
         tlp3433
         tlp3434
         tlp3435
         tlp3436
         tlp3437
         tlp3438
         tlp3439
         tlp3440
         tlp3441
         tlp3442
         tlp3443
         tlp3446
         tlp3447
         tlp3448
         tlp3449
         tlp3450
         tlp3451
         tlp3452
         tlp3453
         tlp3467
         tlp3468
         tlp3469
         tlp3470
         tlp3471
         tlp3472
         tlp3473
         tlp3476
         tlp3478
         tlp3479
         tlp3480
         tlp3481
         tlp3484
         tlp3485
         tlp3486
         tlp3487
         tlp3488
         tlp3489
         tlp3490
         tlp3491
         tlp3492
         tlp3494
         tlp3495
         tlp3496
         tlp3497
         tlp3498
         tlp3499
         tlp3500
         tlp3501
         tlp3502
         tlp3503
         tlp3504
         tlp3507
         tlp3508
         tlp3509
         tlp3510
         tlp3511
         tlp3514
         tlp3515
         tlp3516
         tlp3517
         tlp3523
         tlp3524
         tlp3525
         tlp3526
         tlp3529
         tlp3530
         tlp3531
         tlp3532
         tlp3533))
      '((these #f #\b)
        (7 4 2)
        #\c
        #\c
        symbols
        9
        8
        #\c
        2
        #t
        #f
        (+ 4 (+ 1 1))
        #t
        #\b
        (#\b 0 2)
        #f
        #\b
        (+ 4 5)
        #\c
        (7 #t #\c)
        (these symbols 9)
        (#\c (+ 4 (+ 1 1)) (+ 4 5))
        (6 #\a 2)
        0
        these
        #\c
        these
        1
        (#f #\a #\d)
        #\d
        #t
        symbols
        #\d
        #\d
        (+ 4 (+ 1 1))
        these
        2
        #\c
        are
        #f
        (+ 1 2)
        #\c
        #\a
        (#t #\a #\a)
        16
        symbols
        #\b
        #f
        #f
        #f
        #\c
        (+ 4 5)
        #f
        (+ 4 5)
        #\d
        are
        (#t #f 4)
        #\d
        (#t #\a (+ 4 (+ 1 1)))
        (are (+ 1 2) these)
        (+ 4 (+ 1 1))
        #f
        #\c
        (+ 4 5)
        #\d
        4
        (+ 4 (+ 1 1))
        #f
        these
        (4 #\d #\c)
        symbols
        #f
        2
        (are (+ 4 (+ 1 1)) (+ 4 (+ 1 1)))
        8
        #\b
        #\c
        symbols
        #\a
        #t
        4
        #t
        (#\c #\d #\c)
        symbols
        #\a
        #f
        #\c))
     (mytest
      (match-let
        (((tlp3534
            tlp3535
            (tlp3536 _ tlp3537 quote symbols)
            (or (or tlp3538 tlp3538) (or tlp3538 tlp3538)))
          `(symbols #\b (3 (+ 1 2) 2 . symbols) (+ 4 (+ 1 1))))
         (((tlp3539 __3)
           tlp3540
           (and (? number? tlp3541) (? even? tlp3542))
           (and (? number? tlp3543) (? even? tlp3544)))
          `((#\c #f (+ 1 2)) #t 2 2))
         (((or tlp3545 tlp3545)
           (or #f #f)
           ($ test-struct tlp3546 (tlp3547 ..3) (tlp3548 ___) tlp3549)
           (#(tlp3551 ...) 7 #f . #\b))
          `(#\b #f ,inst-struct-name3550 (#(#f #\b symbols) 7 #f . #\b)))
         ((#&(#&((or tlp3552 tlp3552)
                 tlp3553
                 (and (? number? tlp3554) (? even? tlp3555)))
              tlp3556
              (tlp3557 ___))
           (((tlp3558 tlp3559 tlp3560 . #\b)
             (or #(tlp3561 ___) #(tlp3561 ___))
             (tlp3562 tlp3563 tlp3564 . #\d)
             tlp3565)
            (and (? test-struct? tlp3579)
                 (=
                  test-struct-a
                  ($ test-struct tlp3566 tlp3567 tlp3568 tlp3569))
                 (=
                  test-struct-b
                  (or (and (? number? tlp3571) (? even? tlp3572))
                      (and (? number? tlp3571) (? even? tlp3572))))
                 (=
                  test-struct-c
                  ($ test-struct tlp3573 tlp3574 tlp3575 tlp3576))
                 (= test-struct-d (not (not 9))))
            (and tlp3580 tlp3581)
            (and (? number? tlp3582) (? even? tlp3583)))
           #(tlp3584 ___)
           (or tlp3585 tlp3585))
          `(#&(#&(9 #\a 4) symbols ((+ 1 2) 0 (+ 4 5)))
            (((#\b (+ 1 2) #f . #\b)
              #((+ 4 5) #\c #\c)
              ((+ 4 5) (+ 4 5) #f . #\d)
              7)
             ,inst-struct-name3578
             symbols
             18)
            #(are #\b #t)
            #\a))
         ((#(tlp3586 ...)
           #f
           ((and (? test-struct? tlp3596)
                 (= test-struct-a (tlp3587 ...))
                 (= test-struct-b (tlp3588 tlp3589 tlp3590 . #\c))
                 (= test-struct-c #(tlp3591 tlp3592 tlp3593 tlp3594))
                 (= test-struct-d (not (not #\b))))
            ($
             test-struct
             #\a
             (and (? number? tlp3597) (? even? tlp3598))
             _
             tlp3599)
            ((and (? test-struct? tlp3606)
                  (= test-struct-a tlp3601)
                  (= test-struct-b tlp3602)
                  (= test-struct-c tlp3603)
                  (= test-struct-d tlp3604))
             (tlp3607 __3)
             (and #(tlp3608 ...) #(tlp3609 ...))
             .
             6)
            '(+ 1 2))
           #\b)
          `(#(#\a 0 7)
            #f
            (,inst-struct-name3595
             ,inst-struct-name3600
             (,inst-struct-name3605 (#\d 6 #\c) #(2 symbols 1) . 6)
             (+ 1 2))
            #\b)))
        (list
         tlp3534
         tlp3535
         tlp3536
         tlp3537
         tlp3538
         tlp3539
         tlp3540
         tlp3541
         tlp3543
         tlp3545
         tlp3546
         tlp3547
         tlp3548
         tlp3549
         tlp3551
         tlp3552
         tlp3553
         tlp3554
         tlp3556
         tlp3557
         tlp3558
         tlp3559
         tlp3560
         tlp3561
         tlp3562
         tlp3563
         tlp3564
         tlp3565
         tlp3566
         tlp3567
         tlp3568
         tlp3569
         tlp3571
         tlp3573
         tlp3574
         tlp3575
         tlp3576
         tlp3580
         tlp3582
         tlp3584
         tlp3585
         tlp3586
         tlp3587
         tlp3588
         tlp3589
         tlp3590
         tlp3591
         tlp3592
         tlp3593
         tlp3594
         tlp3597
         tlp3599
         tlp3601
         tlp3602
         tlp3603
         tlp3604
         tlp3607
         tlp3608))
      '(symbols
         #\b
         3
         2
         (+ 4 (+ 1 1))
         (#\c #f (+ 1 2))
         #t
         2
         2
         #\b
         (+ 4 (+ 1 1))
         (#t 8 symbols)
         (#\a #\c #t)
         0
         (#f #\b symbols)
         9
         #\a
         4
         symbols
         ((+ 1 2) 0 (+ 4 5))
         #\b
         (+ 1 2)
         #f
         ((+ 4 5) #\c #\c)
         (+ 4 5)
         (+ 4 5)
         #f
         7
         #\b
         #\c
         #\d
         #\c
         16
         #\b
         (+ 1 2)
         #\b
         #\b
         symbols
         18
         (are #\b #t)
         #\a
         (#\a 0 7)
         (0 these are)
         #\d
         #f
         (+ 4 (+ 1 1))
         #\b
         #\a
         these
         symbols
         0
         2
         are
         1
         3
         #\d
         (#\d 6 #\c)
         (2 symbols 1)))
     (mytest
      (match-let
        (((_ (not (not #f)) (not (not #f)) #(tlp3610 ___))
          `(#t #f #f #(#\b #\d these)))
         (((tlp3611 ...)
           #((tlp3612 ...)
             (and tlp3613 tlp3614)
             (tlp3615 ...)
             (and (? test-struct? tlp3626)
                  (= test-struct-a (and () ()))
                  (= test-struct-b tlp3616)
                  (= test-struct-c #(tlp3617 tlp3618 tlp3619 tlp3620))
                  (= test-struct-d #(tlp3621 tlp3622 tlp3623 tlp3624))))
           (and #(tlp3627
                  #(tlp3628 ...)
                  (and (? test-struct? tlp3640)
                       (= test-struct-a _)
                       (=
                        test-struct-b
                        ($ test-struct tlp3629 tlp3630 tlp3631 tlp3632))
                       (= test-struct-c #&(tlp3634 tlp3635 tlp3636))
                       (= test-struct-d (and tlp3637 tlp3638)))
                  tlp3641)
                #(tlp3642
                  #(tlp3643 ...)
                  (and (? test-struct? tlp3644)
                       (= test-struct-a _)
                       (=
                        test-struct-b
                        ($ test-struct tlp3645 tlp3646 tlp3647 tlp3648))
                       (= test-struct-c #&(tlp3649 tlp3650 tlp3651))
                       (= test-struct-d (and tlp3652 tlp3653)))
                  tlp3654))
           tlp3655)
          `((4 are #\c)
            #((#\b #t symbols) these ((+ 4 5) 6 #\b) ,inst-struct-name3625)
            #(#f #(#\c #\c 6) ,inst-struct-name3639 4)
            #\a))
         ((tlp3656 #\c tlp3657 ((tlp3658 __3) #\d (tlp3659 ___) . #\c))
          `(#t #\c (+ 1 2) ((5 #\b 1) #\d (0 symbols #\d) . #\c)))
         ((tlp3660 _ () _) `(#\c #\a () #f))
         (('these tlp3661 () tlp3662) `(these #\a () #f)))
        (list
         tlp3610
         tlp3611
         tlp3612
         tlp3613
         tlp3615
         tlp3616
         tlp3617
         tlp3618
         tlp3619
         tlp3620
         tlp3621
         tlp3622
         tlp3623
         tlp3624
         tlp3627
         tlp3628
         tlp3629
         tlp3630
         tlp3631
         tlp3632
         tlp3634
         tlp3635
         tlp3636
         tlp3637
         tlp3641
         tlp3655
         tlp3656
         tlp3657
         tlp3658
         tlp3659
         tlp3660
         tlp3661
         tlp3662))
      '((#\b #\d these)
        (4 are #\c)
        (#\b #t symbols)
        these
        ((+ 4 5) 6 #\b)
        #\a
        #\a
        #\c
        (+ 1 2)
        #t
        (+ 4 (+ 1 1))
        #\b
        5
        these
        #f
        (#\c #\c 6)
        (+ 4 (+ 1 1))
        are
        #f
        #\c
        are
        symbols
        4
        #\c
        4
        #\a
        #t
        (+ 1 2)
        (5 #\b 1)
        (0 symbols #\d)
        #\c
        #\a
        #f))))

(multi-let-tests)

(define (define-tests)
    (list
     (mytest
      (let ()
        (match-define
          (#(#(tlp3663 ___)
             (tlp3664 ___)
             (and (? number? tlp3665) (? even? tlp3666))
             #&(#(tlp3667 ...) (tlp3668 ..3) _))
           tlp3669
           (tlp3670 __3)
           tlp3671)
          `(#(#(symbols #t (+ 1 2))
              ((+ 4 (+ 1 1)) symbols (+ 1 2))
              14
              #&(#(1 #t #\d) (are 6 #\c) (+ 1 2)))
            #\b
            (symbols #f #\a)
            #\a))
        (list tlp3663 tlp3664 tlp3665 tlp3667 tlp3668 tlp3669 tlp3670 tlp3671))
      '((symbols #t (+ 1 2))
        ((+ 4 (+ 1 1)) symbols (+ 1 2))
        14
        (1 #t #\d)
        (are 6 #\c)
        #\b
        (symbols #f #\a)
        #\a))
     (mytest
      (let ()
        (match-define
          (#&(($
               test-struct
               #(tlp3672 ...)
               tlp3673
               (and (not (not #\c)) (not (not #\c)))
               (tlp3674 tlp3675 tlp3676 . #\d))
              tlp3678
              (and (tlp3679 ...) (tlp3680 ...)))
           _
           tlp3681
           tlp3682)
          `(#&(,inst-struct-name3677 #\c (are (+ 1 2) (+ 1 2))) 1 3 #f))
        (list
         tlp3672
         tlp3673
         tlp3674
         tlp3675
         tlp3676
         tlp3678
         tlp3679
         tlp3681
         tlp3682))
      '((#\b are #\b) #\a #\b #\c #\d #\c (are (+ 1 2) (+ 1 2)) 3 #f))
     (mytest
      (let ()
        (match-define
          (tlp3683
            ((and #\d #\d) (or (tlp3684 __3) (tlp3684 __3)) () . #\d)
            (and #(tlp3685 ___) #(tlp3686 ___))
            (tlp3687 ___))
          `((+ 1 2)
            (#\d (symbols #\d #f) () . #\d)
            #((+ 4 5) these #f)
            ((+ 1 2) these symbols)))
        (list tlp3683 tlp3684 tlp3685 tlp3687))
      '((+ 1 2) (symbols #\d #f) ((+ 4 5) these #f) ((+ 1 2) these symbols)))
     (mytest
      (let ()
        (match-define
          (tlp3688
            tlp3689
            (#&((tlp3690 ___)
                (and (? number? tlp3691) (? even? tlp3692))
                (not (not #\c)))
             (and (tlp3693 ...) (tlp3694 ...))
             tlp3695
             (not (not #f)))
            #(tlp3696 ...))
          `(#\b
            (+ 4 (+ 1 1))
            (#&((#f #\d 9) 10 #\c) (#\d #t #f) these #f)
            #(#t #\c #f)))
        (list tlp3688 tlp3689 tlp3690 tlp3691 tlp3693 tlp3695 tlp3696))
      '(#\b (+ 4 (+ 1 1)) (#f #\d 9) 10 (#\d #t #f) these (#t #\c #f)))
     (mytest
      (let ()
        (match-define
          (($
            test-struct
            (not (not #\b))
            tlp3697
            #&(tlp3698 (tlp3699 ..3) tlp3700)
            (or _ _))
           (() #(tlp3702 ...) (and 'are 'are) . #\d)
           #&(tlp3703
               #((tlp3704 ___) (tlp3705 ...) (tlp3706 ___) #(tlp3707 ...))
               (($ test-struct tlp3708 tlp3709 tlp3710 tlp3711)
                #t
                ()
                (tlp3713 __3)))
           #(tlp3714 ___))
          `(,inst-struct-name3701
            (() #(0 #\a (+ 4 5)) are . #\d)
            #&((+ 1 2)
               #((2 (+ 4 (+ 1 1)) #\d)
                 (#\b 0 (+ 4 (+ 1 1)))
                 ((+ 4 5) symbols #\d)
                 #(4 #\a are))
               (,inst-struct-name3712 #t () (#\c 5 0)))
            #(#f #\d #\a)))
        (list
         tlp3697
         tlp3698
         tlp3699
         tlp3700
         tlp3702
         tlp3703
         tlp3704
         tlp3705
         tlp3706
         tlp3707
         tlp3708
         tlp3709
         tlp3710
         tlp3711
         tlp3713
         tlp3714))
      '(9
        #\c
        (#f #\d (+ 1 2))
        are
        (0 #\a (+ 4 5))
        (+ 1 2)
        (2 (+ 4 (+ 1 1)) #\d)
        (#\b 0 (+ 4 (+ 1 1)))
        ((+ 4 5) symbols #\d)
        (4 #\a are)
        #\d
        #\d
        (+ 4 (+ 1 1))
        are
        (#\c 5 0)
        (#f #\d #\a)))))
(define-tests)

(define (sing-match-tests)
    (list
     (let ((tlp3719
             (lambda (tlp3715)
               (match
                tlp3715
                ((3
                  (and (? number? tlp3716) (? even? tlp3717))
                  (and #f #f)
                  (or #(tlp3718 ___) #(tlp3718 ___)))
                 (list tlp3716 tlp3718))))))
       (list
        (mytest (tlp3719 `(3 2 #f #((+ 1 2) are #\a))) '(2 ((+ 1 2) are #\a)))))
     (let ((tlp3733
             (lambda (tlp3720)
               (match
                tlp3720
                ((tlp3721
                   tlp3722
                   (or ((and (? number? tlp3723) (? even? tlp3724))
                        tlp3725
                        ()
                        #(tlp3726
                          (tlp3727 tlp3728 tlp3729 tlp3730)
                          ()
                          tlp3731))
                       ((and (? number? tlp3723) (? even? tlp3724))
                        tlp3725
                        ()
                        #(tlp3726
                          (tlp3727 tlp3728 tlp3729 tlp3730)
                          ()
                          tlp3731)))
                   (or tlp3732 tlp3732))
                 (list
                  tlp3721
                  tlp3722
                  tlp3723
                  tlp3725
                  tlp3726
                  tlp3727
                  tlp3728
                  tlp3729
                  tlp3730
                  tlp3731
                  tlp3732))))))
       (list
        (mytest
         (tlp3733
           `(these
             (+ 4 5)
             (16
              (+ 4 5)
              ()
              #(#f ((+ 4 (+ 1 1)) (+ 1 2) (+ 1 2) (+ 4 (+ 1 1))) () are))
             #\c))
         '(these
           (+ 4 5)
           16
           (+ 4 5)
           #f
           (+ 4 (+ 1 1))
           (+ 1 2)
           (+ 1 2)
           (+ 4 (+ 1 1))
           are
           #\c))))
     (let ((tlp3824
             (lambda (tlp3734)
               (match
                tlp3734
                ((($
                   test-struct
                   (and (tlp3735 __3) (tlp3736 __3))
                   tlp3737
                   (and (? number? tlp3738) (? even? tlp3739))
                   (and (? test-struct? tlp3754)
                        (=
                         test-struct-a
                         (and (? test-struct? tlp3745)
                              (= test-struct-a tlp3740)
                              (= test-struct-b tlp3741)
                              (= test-struct-c tlp3742)
                              (= test-struct-d tlp3743)))
                        (= test-struct-b (and #(tlp3746 ___) #(tlp3747 ___)))
                        (= test-struct-c _)
                        (=
                         test-struct-d
                         ($ test-struct tlp3748 tlp3749 tlp3750 tlp3751))))
                  #t
                  (and #((_
                          (and tlp3756 tlp3757)
                          (and (? test-struct? tlp3763)
                               (= test-struct-a tlp3758)
                               (= test-struct-b tlp3759)
                               (= test-struct-c tlp3760)
                               (= test-struct-d tlp3761))
                          (not (not 'are)))
                         _
                         ($
                          test-struct
                          #&(tlp3764 tlp3765 tlp3766)
                          #(tlp3767 tlp3768 tlp3769 tlp3770)
                          tlp3771
                          ($ test-struct tlp3772 tlp3773 tlp3774 tlp3775))
                         _)
                       #((_
                          (and tlp3778 tlp3779)
                          (and (? test-struct? tlp3780)
                               (= test-struct-a tlp3781)
                               (= test-struct-b tlp3782)
                               (= test-struct-c tlp3783)
                               (= test-struct-d tlp3784))
                          (not (not 'are)))
                         _
                         ($
                          test-struct
                          #&(tlp3785 tlp3786 tlp3787)
                          #(tlp3788 tlp3789 tlp3790 tlp3791)
                          tlp3792
                          ($ test-struct tlp3793 tlp3794 tlp3795 tlp3796))
                         _))
                  (and (? test-struct? tlp3823)
                       (=
                        test-struct-a
                        (and (? test-struct? tlp3806)
                             (=
                              test-struct-a
                              (and (and tlp3797 tlp3798)
                                   (and tlp3799 tlp3800)))
                             (= test-struct-b #&(tlp3801 tlp3802 tlp3803))
                             (= test-struct-c (or '(+ 4 5) '(+ 4 5)))
                             (= test-struct-d tlp3804)))
                       (=
                        test-struct-b
                        #&(#&(tlp3807 tlp3808 tlp3809) #\c (tlp3810 ..3)))
                       (= test-struct-c _)
                       (=
                        test-struct-d
                        (and (? test-struct? tlp3821)
                             (= test-struct-a tlp3811)
                             (= test-struct-b (tlp3812 ...))
                             (=
                              test-struct-c
                              (and (? test-struct? tlp3818)
                                   (= test-struct-a tlp3813)
                                   (= test-struct-b tlp3814)
                                   (= test-struct-c tlp3815)
                                   (= test-struct-d tlp3816)))
                             (= test-struct-d #(tlp3819 ___))))))
                 (list
                  tlp3735
                  tlp3737
                  tlp3738
                  tlp3740
                  tlp3741
                  tlp3742
                  tlp3743
                  tlp3746
                  tlp3748
                  tlp3749
                  tlp3750
                  tlp3751
                  tlp3756
                  tlp3758
                  tlp3759
                  tlp3760
                  tlp3761
                  tlp3764
                  tlp3765
                  tlp3766
                  tlp3767
                  tlp3768
                  tlp3769
                  tlp3770
                  tlp3771
                  tlp3772
                  tlp3773
                  tlp3774
                  tlp3775
                  tlp3797
                  tlp3801
                  tlp3802
                  tlp3803
                  tlp3804
                  tlp3807
                  tlp3808
                  tlp3809
                  tlp3810
                  tlp3811
                  tlp3812
                  tlp3813
                  tlp3814
                  tlp3815
                  tlp3816
                  tlp3819))))))
       (list
        (mytest
         (tlp3824
           `(,inst-struct-name3755
             #t
             #((#f 8 ,inst-struct-name3762 are)
               symbols
               ,inst-struct-name3777
               these)
             ,inst-struct-name3822))
         '((these 0 3)
           (+ 4 5)
           2
           symbols
           (+ 1 2)
           symbols
           #\b
           (#\c #\d #\d)
           #\b
           #t
           (+ 4 5)
           #\b
           8
           these
           (+ 1 2)
           2
           #\d
           symbols
           (+ 4 (+ 1 1))
           (+ 1 2)
           #\c
           6
           #\c
           #t
           #\b
           #\b
           #\b
           symbols
           these
           #\d
           (+ 1 2)
           #\d
           1
           #\b
           #f
           (+ 4 (+ 1 1))
           #\d
           (#t #\c #f)
           #\d
           (2 (+ 4 (+ 1 1)) symbols)
           1
           (+ 1 2)
           #\d
           (+ 1 2)
           ((+ 1 2) 8 (+ 4 5))))))
     (let ((tlp3883
             (lambda (tlp3825)
               (match
                tlp3825
                ((#&(tlp3826
                      tlp3827
                      ((tlp3828 tlp3829 tlp3830 . #t)
                       (tlp3831 __3)
                       (tlp3832 tlp3833 tlp3834 quote these)
                       .
                       #\b))
                  ()
                  (and (? test-struct? tlp3857)
                       (= test-struct-a #&(tlp3835 #\a tlp3836))
                       (=
                        test-struct-b
                        (and ((tlp3837 __3)
                              #t
                              (and (? number? tlp3838) (? even? tlp3839))
                              #(tlp3840 ...))
                             ((tlp3841 __3)
                              #t
                              (and (? number? tlp3842) (? even? tlp3843))
                              #(tlp3844 ...))))
                       (= test-struct-c (#(tlp3845 ...) #t tlp3846 (or #f #f)))
                       (=
                        test-struct-d
                        (or (and (? test-struct? tlp3855)
                                 (= test-struct-a #\b)
                                 (= test-struct-b _)
                                 (=
                                  test-struct-c
                                  ($
                                   test-struct
                                   tlp3847
                                   tlp3848
                                   tlp3849
                                   tlp3850))
                                 (=
                                  test-struct-d
                                  (and (? number? tlp3852) (? even? tlp3853))))
                            (and (? test-struct? tlp3855)
                                 (= test-struct-a #\b)
                                 (= test-struct-b _)
                                 (=
                                  test-struct-c
                                  ($
                                   test-struct
                                   tlp3847
                                   tlp3848
                                   tlp3849
                                   tlp3850))
                                 (=
                                  test-struct-d
                                  (and (? number? tlp3852)
                                       (? even? tlp3853)))))))
                  (((tlp3858 tlp3859 tlp3860 quote these)
                    #&(tlp3861 tlp3862 tlp3863)
                    (not (not '(+ 4 (+ 1 1))))
                    .
                    #\a)
                   (and (#(tlp3864 tlp3865 tlp3866 tlp3867)
                         #(tlp3868 ...)
                         '(+ 4 5)
                         .
                         #\d)
                        (#(tlp3869 tlp3870 tlp3871 tlp3872)
                         #(tlp3873 ...)
                         '(+ 4 5)
                         .
                         #\d))
                   (not (not #\b))
                   #(tlp3874
                     (not (not '(+ 1 2)))
                     (tlp3875 tlp3876 tlp3877 tlp3878)
                     (tlp3879 tlp3880 tlp3881 tlp3882))))
                 (list
                  tlp3826
                  tlp3827
                  tlp3828
                  tlp3829
                  tlp3830
                  tlp3831
                  tlp3832
                  tlp3833
                  tlp3834
                  tlp3835
                  tlp3836
                  tlp3837
                  tlp3838
                  tlp3840
                  tlp3845
                  tlp3846
                  tlp3847
                  tlp3848
                  tlp3849
                  tlp3850
                  tlp3852
                  tlp3858
                  tlp3859
                  tlp3860
                  tlp3861
                  tlp3862
                  tlp3863
                  tlp3864
                  tlp3865
                  tlp3866
                  tlp3867
                  tlp3868
                  tlp3874
                  tlp3875
                  tlp3876
                  tlp3877
                  tlp3878
                  tlp3879
                  tlp3880
                  tlp3881
                  tlp3882))))))
       (list
        (mytest
         (tlp3883
           `(#&(2
                #\d
                ((#t (+ 4 5) are . #t) (#t 8 #\a) (#\c 2 these . these) . #\b))
             ()
             ,inst-struct-name3856
             (((#\a #\b (+ 1 2) . these) #&(8 are #\b) (+ 4 (+ 1 1)) . #\a)
              (#((+ 1 2) these #\b #\c) #(these are #\d) (+ 4 5) . #\d)
              #\b
              #(#\b (+ 1 2) ((+ 4 (+ 1 1)) #\d #\d #\c) (2 are #\a #t)))))
         '(2
           #\d
           #t
           (+ 4 5)
           are
           (#t 8 #\a)
           #\c
           2
           these
           #t
           symbols
           ((+ 4 (+ 1 1)) (+ 4 5) #\b)
           12
           (these #\c #f)
           (#\d these #\d)
           7
           #f
           (+ 1 2)
           #\a
           #f
           18
           #\a
           #\b
           (+ 1 2)
           8
           are
           #\b
           (+ 1 2)
           these
           #\b
           #\c
           (these are #\d)
           #\b
           (+ 4 (+ 1 1))
           #\d
           #\d
           #\c
           2
           are
           #\a
           #t))))
     (let ((tlp3917
             (lambda (tlp3884)
               (match
                tlp3884
                (((and (? test-struct? tlp3903)
                       (=
                        test-struct-a
                        (and (? test-struct? tlp3891)
                             (= test-struct-a tlp3885)
                             (=
                              test-struct-b
                              (tlp3886 tlp3887 tlp3888 tlp3889))
                             (= test-struct-c (not (not #\d)))
                             (= test-struct-d _)))
                       (= test-struct-b '(+ 4 5))
                       (= test-struct-c ())
                       (=
                        test-struct-d
                        #((tlp3892 tlp3893 tlp3894 tlp3895)
                          (and (? test-struct? tlp3901)
                               (= test-struct-a tlp3896)
                               (= test-struct-b tlp3897)
                               (= test-struct-c tlp3898)
                               (= test-struct-d tlp3899))
                          _
                          (or () ()))))
                  (tlp3904 ..3)
                  (tlp3905 ...)
                  #((tlp3906 ...)
                    ((tlp3907 ..3) (tlp3908 tlp3909 tlp3910 . #\d) 5 quote are)
                    (tlp3911
                      (and (? number? tlp3912) (? even? tlp3913))
                      (tlp3914 ...)
                      .
                      #\d)
                    (tlp3915 #\b #(tlp3916 ...) ())))
                 (list
                  tlp3885
                  tlp3886
                  tlp3887
                  tlp3888
                  tlp3889
                  tlp3892
                  tlp3893
                  tlp3894
                  tlp3895
                  tlp3896
                  tlp3897
                  tlp3898
                  tlp3899
                  tlp3904
                  tlp3905
                  tlp3906
                  tlp3907
                  tlp3908
                  tlp3909
                  tlp3910
                  tlp3911
                  tlp3912
                  tlp3914
                  tlp3915
                  tlp3916))))))
       (list
        (mytest
         (tlp3917
           `(,inst-struct-name3902
             (these (+ 4 (+ 1 1)) symbols)
             (#\b #\c 7)
             #((these #\a #\d)
               ((#t #\d 9) ((+ 4 5) (+ 4 5) 6 . #\d) 5 . are)
               (#\b 4 (#\a symbols #\c) . #\d)
               ((+ 4 (+ 1 1)) #\b #(#t #\d 8) ()))))
         '(#f
           #t
           #f
           are
           symbols
           #\a
           7
           #\b
           (+ 1 2)
           (+ 4 (+ 1 1))
           0
           #\d
           0
           (these (+ 4 (+ 1 1)) symbols)
           (#\b #\c 7)
           (these #\a #\d)
           (#t #\d 9)
           (+ 4 5)
           (+ 4 5)
           6
           #\b
           4
           (#\a symbols #\c)
           (+ 4 (+ 1 1))
           (#t #\d 8)))))))
(sing-match-tests)

(define (sing-match-lambda*-tests)
    (list
     (let ((tlp4986
             (match-lambda*
               ((tlp4984 'these (not (not #\b)) tlp4985)
                (list tlp4984 tlp4985)))))
       (list (mytest (tlp4986 `3 `these `#\b `symbols) '(3 symbols))))
     (let ((tlp5001
             (match-lambda*
               ((_
                 #(tlp4988 ...)
                 tlp4989
                 #&((not (not '(+ 4 (+ 1 1))))
                    ($
                     test-struct
                     (tlp4990 tlp4991 tlp4992 . 4)
                     ($ test-struct tlp4993 tlp4994 tlp4995 tlp4996)
                     tlp4998
                     #t)
                    tlp5000))
                (list
                 tlp4988
                 tlp4989
                 tlp4990
                 tlp4991
                 tlp4992
                 tlp4993
                 tlp4994
                 tlp4995
                 tlp4996
                 tlp4998
                 tlp5000)))))
       (list
        (mytest
         (tlp5001
           `(+ 1 2)
           `#(#t #\b #\d)
           `#\d
           `#&((+ 4 (+ 1 1)) ,inst-struct-name4999 1))
         '((#t #\b #\d)
           #\d
           (+ 1 2)
           #t
           (+ 4 (+ 1 1))
           (+ 1 2)
           #\d
           these
           #\d
           9
           1))))
     (let ((tlp5004
             (match-lambda*
               ((#\b (and () ()) '(+ 4 (+ 1 1)) tlp5003) (list tlp5003)))))
       (list (mytest (tlp5004 `#\b `() `(+ 4 (+ 1 1)) `(+ 4 5)) '((+ 4 5)))))
     (let ((tlp5014
             (match-lambda*
               ((#(tlp5006 ...)
                 _
                 #((tlp5007 ..3)
                   #&((tlp5008 ___) _ (not (not #t)))
                   tlp5009
                   (tlp5010 #(tlp5011 ...) () ()))
                 (and (? number? tlp5012) (? even? tlp5013)))
                (list
                 tlp5006
                 tlp5007
                 tlp5008
                 tlp5009
                 tlp5010
                 tlp5011
                 tlp5012)))))
       (list
        (mytest
         (tlp5014
           `#(#\b (+ 4 (+ 1 1)) #f)
           `(+ 4 5)
           `#((#\c #\d #\c)
              #&((#\c #\c #\a) are #t)
              #\c
              (#\c #(these 3 #\c) () ()))
           `6)
         '((#\b (+ 4 (+ 1 1)) #f)
           (#\c #\d #\c)
           (#\c #\c #\a)
           #\c
           #\c
           (these 3 #\c)
           6))))
     (let ((tlp5045
             (match-lambda*
               (((and (((and (? number? tlp5016) (? even? tlp5017)) () #\a ())
                       (and (? number? tlp5018) (? even? tlp5019))
                       ()
                       tlp5020)
                      (((and (? number? tlp5021) (? even? tlp5022)) () #\a ())
                       (and (? number? tlp5023) (? even? tlp5024))
                       ()
                       tlp5025))
                 (or (not (not 'are)) (not (not 'are)))
                 tlp5026
                 (($
                   test-struct
                   _
                   (or ($ test-struct tlp5027 tlp5028 tlp5029 tlp5030)
                       ($ test-struct tlp5027 tlp5028 tlp5029 tlp5030))
                   (or (tlp5032 __3) (tlp5032 __3))
                   (tlp5033 tlp5034 tlp5035 tlp5036))
                  (and (? test-struct? tlp5041)
                       (= test-struct-a '(+ 1 2))
                       (= test-struct-b tlp5038)
                       (= test-struct-c (tlp5039 ___))
                       (= test-struct-d 'are))
                  tlp5042
                  (or (and (? number? tlp5043) (? even? tlp5044))
                      (and (? number? tlp5043) (? even? tlp5044)))))
                (list
                 tlp5016
                 tlp5018
                 tlp5020
                 tlp5026
                 tlp5027
                 tlp5028
                 tlp5029
                 tlp5030
                 tlp5032
                 tlp5033
                 tlp5034
                 tlp5035
                 tlp5036
                 tlp5038
                 tlp5039
                 tlp5042
                 tlp5043)))))
       (list
        (mytest
         (tlp5045
           `((10 () #\a ()) 0 () #f)
           `are
           `2
           `(,inst-struct-name5037 ,inst-struct-name5040 are 16))
         '(10
           0
           #f
           2
           #f
           #\b
           #\c
           are
           (7 2 (+ 1 2))
           (+ 1 2)
           (+ 4 (+ 1 1))
           are
           these
           symbols
           (7 0 #t)
           are
           16))))))
(sing-match-lambda*-tests)

(define (sing-match-lambda-tests)
    (list
     (let ((tlp4450
             (match-lambda
               (((not (not 'are))
                 (tlp4432
                   (tlp4433 (tlp4434 ...) tlp4435 quote symbols)
                   (not (not 2))
                   quote
                   symbols)
                 (not (not #\a))
                 ((and (? test-struct? tlp4445)
                       (= test-struct-a (tlp4436 tlp4437 tlp4438 . 9))
                       (=
                        test-struct-b
                        (and (and tlp4439 tlp4440) (and tlp4441 tlp4442)))
                       (= test-struct-c '(+ 1 2))
                       (= test-struct-d (tlp4443 ___)))
                  tlp4446
                  ('these tlp4447 (tlp4448 ..3) quote these)
                  (tlp4449 ___)))
                (list
                 tlp4432
                 tlp4433
                 tlp4434
                 tlp4435
                 tlp4436
                 tlp4437
                 tlp4438
                 tlp4439
                 tlp4443
                 tlp4446
                 tlp4447
                 tlp4448
                 tlp4449)))))
       (list
        (mytest
         (tlp4450
           `(are
             (symbols ((+ 1 2) (#f (+ 4 5) (+ 4 5)) #t . symbols) 2 . symbols)
             #\a
             (,inst-struct-name4444
              7
              (these (+ 4 5) (9 (+ 4 5) #\d) . these)
              (#t #f #\b))))
         '(symbols
            (+ 1 2)
            (#f (+ 4 5) (+ 4 5))
            #t
            #\b
            (+ 1 2)
            are
            (+ 1 2)
            (#\c (+ 1 2) #\d)
            7
            (+ 4 5)
            (9 (+ 4 5) #\d)
            (#t #f #\b)))))
     (let ((tlp4473
             (match-lambda
               ((tlp4451
                  tlp4452
                  (and (? test-struct? tlp4471)
                       (=
                        test-struct-a
                        (and (? test-struct? tlp4461)
                             (= test-struct-a #&(tlp4453 tlp4454 tlp4455))
                             (= test-struct-b (tlp4456 ...))
                             (=
                              test-struct-c
                              (tlp4457 tlp4458 tlp4459 quote symbols))
                             (= test-struct-d '(+ 4 5))))
                       (=
                        test-struct-b
                        (and (? number? tlp4462) (? even? tlp4463)))
                       (= test-struct-c tlp4464)
                       (=
                        test-struct-d
                        (or (()
                             tlp4465
                             (tlp4466 tlp4467 tlp4468 tlp4469)
                             .
                             #\b)
                            (()
                             tlp4465
                             (tlp4466 tlp4467 tlp4468 tlp4469)
                             .
                             #\b))))
                  tlp4472)
                (list
                 tlp4451
                 tlp4452
                 tlp4453
                 tlp4454
                 tlp4455
                 tlp4456
                 tlp4457
                 tlp4458
                 tlp4459
                 tlp4462
                 tlp4464
                 tlp4465
                 tlp4466
                 tlp4467
                 tlp4468
                 tlp4469
                 tlp4465
                 tlp4466
                 tlp4467
                 tlp4468
                 tlp4469
                 tlp4472)))))
       (list
        (mytest
         (tlp4473 `(#f #\a ,inst-struct-name4470 5))
         '(#f
           #\a
           #t
           (+ 4 5)
           #\c
           (1 #\c 1)
           #f
           (+ 4 5)
           #\a
           10
           #t
           #\d
           #f
           #\d
           #\c
           are
           #\d
           #f
           #\d
           #\c
           are
           5))))
     (let ((tlp4520
             (match-lambda
               ((((and (#(tlp4474 tlp4475 tlp4476 tlp4477)
                        #(tlp4478 tlp4479 tlp4480 tlp4481)
                        ()
                        (or tlp4482 tlp4482))
                       (#(tlp4483 tlp4484 tlp4485 tlp4486)
                        #(tlp4487 tlp4488 tlp4489 tlp4490)
                        ()
                        (or tlp4491 tlp4491)))
                  (and (? number? tlp4492) (? even? tlp4493))
                  ('(+ 1 2) (tlp4494 ...) tlp4495 tlp4496)
                  .
                  #f)
                 (and (? test-struct? tlp4517)
                      (=
                       test-struct-a
                       (or #&((and (? test-struct? tlp4502)
                                   (= test-struct-a tlp4497)
                                   (= test-struct-b tlp4498)
                                   (= test-struct-c tlp4499)
                                   (= test-struct-d tlp4500))
                              ()
                              #(tlp4503 tlp4504 tlp4505 tlp4506))
                           #&((and (? test-struct? tlp4502)
                                   (= test-struct-a tlp4497)
                                   (= test-struct-b tlp4498)
                                   (= test-struct-c tlp4499)
                                   (= test-struct-d tlp4500))
                              ()
                              #(tlp4503 tlp4504 tlp4505 tlp4506))))
                      (=
                       test-struct-b
                       (and ((tlp4507 ___)
                             (and (? number? tlp4508) (? even? tlp4509))
                             (tlp4510 __3)
                             quote
                             these)
                            ((tlp4511 ___)
                             (and (? number? tlp4512) (? even? tlp4513))
                             (tlp4514 __3)
                             quote
                             these)))
                      (= test-struct-c (or (tlp4515 ..3) (tlp4515 ..3)))
                      (= test-struct-d ()))
                 tlp4518
                 tlp4519)
                (list
                 tlp4474
                 tlp4475
                 tlp4476
                 tlp4477
                 tlp4478
                 tlp4479
                 tlp4480
                 tlp4481
                 tlp4482
                 tlp4492
                 tlp4494
                 tlp4495
                 tlp4496
                 tlp4497
                 tlp4498
                 tlp4499
                 tlp4500
                 tlp4503
                 tlp4504
                 tlp4505
                 tlp4506
                 tlp4507
                 tlp4508
                 tlp4510
                 tlp4515
                 tlp4518
                 tlp4519)))))
       (list
        (mytest
         (tlp4520
           `(((#(#\c #\c 0 (+ 4 (+ 1 1))) #(#f 9 8 #\a) () #t)
              10
              ((+ 1 2) ((+ 4 5) symbols (+ 4 (+ 1 1))) #f 3)
              .
              #f)
             ,inst-struct-name4516
             (+ 1 2)
             5))
         '(#\c
           #\c
           0
           (+ 4 (+ 1 1))
           #f
           9
           8
           #\a
           #t
           10
           ((+ 4 5) symbols (+ 4 (+ 1 1)))
           #f
           3
           #\b
           8
           are
           #\a
           #\b
           #\b
           #t
           #t
           (#\b #\c #t)
           0
           ((+ 4 5) #t these)
           (#\a #\b 0)
           (+ 1 2)
           5))))
     (let ((tlp4526
             (match-lambda
               (((and (? number? tlp4521) (? even? tlp4522))
                 (or tlp4523 tlp4523)
                 tlp4524
                 (tlp4525 ...))
                (list tlp4521 tlp4523 tlp4524 tlp4525)))))
       (list
        (mytest
         (tlp4526 `(4 #\a (+ 4 (+ 1 1)) (are symbols #\c)))
         '(4 #\a (+ 4 (+ 1 1)) (are symbols #\c)))))
     (let ((tlp4561
             (match-lambda
               (((tlp4527 __3)
                 ((and (? test-struct? tlp4540)
                       (=
                        test-struct-a
                        (and ($ test-struct tlp4528 tlp4529 tlp4530 tlp4531)
                             ($ test-struct tlp4533 tlp4534 tlp4535 tlp4536)))
                       (= test-struct-b (tlp4537 ..3))
                       (= test-struct-c #\d)
                       (= test-struct-d #(tlp4538 ...)))
                  (and (? test-struct? tlp4556)
                       (= test-struct-a (not (not #\b)))
                       (=
                        test-struct-b
                        (and #(tlp4541 tlp4542 tlp4543 tlp4544)
                             #(tlp4545 tlp4546 tlp4547 tlp4548)))
                       (= test-struct-c #(tlp4549 ...))
                       (=
                        test-struct-d
                        ($ test-struct tlp4550 tlp4551 tlp4552 tlp4553)))
                  #&((and (tlp4557 __3) (tlp4558 __3)) tlp4559 (not (not 4)))
                  tlp4560)
                 ()
                 (or 'these 'these))
                (list
                 tlp4527
                 tlp4528
                 tlp4529
                 tlp4530
                 tlp4531
                 tlp4537
                 tlp4538
                 tlp4541
                 tlp4542
                 tlp4543
                 tlp4544
                 tlp4549
                 tlp4550
                 tlp4551
                 tlp4552
                 tlp4553
                 tlp4557
                 tlp4559
                 tlp4560)))))
       (list
        (mytest
         (tlp4561
           `((4 2 #f)
             (,inst-struct-name4539
              ,inst-struct-name4555
              #&((symbols are these) 6 4)
              2)
             ()
             these))
         '((4 2 #f)
           (+ 4 (+ 1 1))
           #\c
           #f
           #\c
           (#f 9 symbols)
           (are #f are)
           #\c
           #\b
           #f
           (+ 4 5)
           (#f 5 #\a)
           #f
           #\b
           (+ 4 5)
           these
           (symbols are these)
           6
           2))))))
(sing-match-lambda-tests)

  (define-struct test-struct (a b c d))
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
    (make-test-struct `#\c `(#f #f symbols) `14 `#(are #\b #f)))
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
    (make-test-struct `2 `these `#(#\d symbols #\a #\c) `3))
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
      `#(6 ,inst-struct-name1087 #\c #f)
      `0))
  (define inst-struct-name1099 (make-test-struct `8 `() `#(#\d (+ 1 2) 9) `()))
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
      `#(9 (+ 4 (+ 1 1)) #t (+ 1 2))))
  (define inst-struct-name1141
    (make-test-struct
      `(() these ,inst-struct-name1125 #(6 1 5 #\c))
      `,inst-struct-name1139
      `()
      `5))
  (define inst-struct-name1156
    (make-test-struct
      `((#t 5 9 5) #(symbols symbols these) 14 (are these are))
      `2
      `(((+ 4 (+ 1 1)) these #f) #((+ 4 (+ 1 1)) #t #\b) symbols . 0)
      `are))
  (define inst-struct-name1161 (make-test-struct `#\d `#\c `these `1))
  (define inst-struct-name1174 (make-test-struct `#f `(+ 4 5) `#\d `#f))
  (define inst-struct-name1187
    (make-test-struct `,inst-struct-name1174 `#\c `#\c `#&(symbols 2 #\a)))
  (define inst-struct-name1198
    (make-test-struct
      `(#\d #\b #t)
      `#(#f (+ 4 5) symbols (+ 1 2))
      `#f
      `#&(#\d 8 3)))
  (define inst-struct-name1221
    (make-test-struct `#\c `4 `() `(#\d #\c #\c . #\d)))
  (define inst-struct-name1229
    (make-test-struct `#\c `(+ 4 (+ 1 1)) `these `#f))
  (define inst-struct-name1230
    (make-test-struct `() `#(#\b 0 (+ 4 5)) `#\b `,inst-struct-name1229))
  (define inst-struct-name1232
    (make-test-struct
      `#((+ 1 2) #\a 2)
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
    (make-test-struct `these `#f `#(#\b #\c 2 #f) `1))
  (define inst-struct-name1316
    (make-test-struct `,inst-struct-name1312 `#t `#\d `(+ 1 2)))
  (define inst-struct-name1325 (make-test-struct `these `#\a `#t `these))
  (define inst-struct-name1340
    (make-test-struct `(+ 4 5) `#\c `symbols `(+ 1 2)))
  (define inst-struct-name1342
    (make-test-struct
      `#t
      `#(,inst-struct-name1325
         (4 (+ 4 5) #\a)
         (#\d symbols #\b)
         #(#\c #\c #\b #t))
      `((#\c symbols (+ 4 (+ 1 1)) . #\a) #f 3 ,inst-struct-name1340)
      `#(8 9 1)))
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
    (make-test-struct `(#t 8 #f) `#(#\a are 3) `#\d `are))
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
    (make-test-struct `#\c `() `#&(2 are 8) `#(these #\c #\a #\a)))
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
      `#(#(these #\d these symbols) (are 6 these) #\c #\d)
      `#(#f 8 (+ 4 5))
      `(#\b are symbols)
      `#\b))
  (define inst-struct-name1708
    (make-test-struct
      `(+ 1 2)
      `#(() #&((+ 4 5) #\a #f) () (+ 4 (+ 1 1)))
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
      `#(#\d #\c (+ 4 (+ 1 1)))
      `#(symbols #\a #\d)
      `,inst-struct-name1775))
  (define inst-struct-name1786 (make-test-struct `0 `#\a `0 `#t))
  (define inst-struct-name1796
    (make-test-struct `3 `#(#\b #\c 0) `#t `(+ 4 5)))
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
      `#(#\d (+ 4 (+ 1 1)) #f #\b)
      `are
      `()))
  (define inst-struct-name1818
    (make-test-struct
      `#&(#\a (9 #\c symbols) #f)
      `,inst-struct-name1813
      `#&(8 () #\d)
      `#(#\a these symbols)))
  (define inst-struct-name1832 (make-test-struct `are `1 `() `(12 #\c #t . 4)))
  (define inst-struct-name1873 (make-test-struct `#t `7 `#\b `(+ 4 5)))
  (define inst-struct-name1876
    (make-test-struct
      `0
      `#&(#f 0 these)
      `,inst-struct-name1873
      `(+ 4 (+ 1 1))))
  (define inst-struct-name1896
    (make-test-struct `#(5 (+ 1 2) these) `(+ 4 (+ 1 1)) `0 `#(#\a #\d these)))
  (define inst-struct-name1900
    (make-test-struct `0 `these `#(6 are symbols) `#((+ 4 5) #t 8)))
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
    (make-test-struct `#(#t are #\b #f) `#(#f #f (+ 4 5)) `#t `()))
  (define inst-struct-name1975 (make-test-struct `(+ 1 2) `(+ 1 2) `#\c `#f))
  (define inst-struct-name1977
    (make-test-struct
      `symbols
      `symbols
      `(+ 4 (+ 1 1))
      `(#(2 (+ 4 5) #\b) are ,inst-struct-name1975 . 5)))
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
    (make-test-struct `#(#t (+ 4 5) (+ 4 5) #t) `0 `#\c `(these these 7)))
  (define inst-struct-name2093
    (make-test-struct `(#\b 9 #\b) `16 `#\a `#((+ 1 2) 9 () 2)))
  (define inst-struct-name2104
    (make-test-struct `#\b `(symbols 1 (+ 4 5)) `12 `0))
  (define inst-struct-name2110 (make-test-struct `#\a `#\a `these `symbols))
  (define inst-struct-name2117
    (make-test-struct
      `,inst-struct-name2110
      `#(#t 4 7)
      `(+ 1 2)
      `#(2 symbols (+ 4 (+ 1 1)) 5)))
  (define inst-struct-name2125 (make-test-struct `#\c `(+ 4 (+ 1 1)) `#\c `2))
  (define inst-struct-name2127
    (make-test-struct
      `(+ 4 5)
      `()
      `,inst-struct-name2117
      `#(#&(are 6 #\c) ,inst-struct-name2125 9 ())))
  (define inst-struct-name2133
    (make-test-struct
      `symbols
      `#(#\b #\a #\d)
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
      `#(7 #\a (these 9 (+ 1 2)) ())
      `2
      `#\b
      `#(#(are #\a #\c) #\a ,inst-struct-name2188 #\d)))
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
      `#(#\d symbols (+ 4 5))
      `()
      `((+ 1 2) (are #\b #\d) #\b . these)))
  (define inst-struct-name2269
    (make-test-struct `(symbols #\a #\a) `are `#f `()))
  (define inst-struct-name2279
    (make-test-struct
      `#(#\a #\b symbols)
      `#((+ 4 (+ 1 1)) #\d 2)
      `#(#f #\b (+ 1 2))
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
      `#(8 #\a (+ 4 5))))
  (define inst-struct-name2388 (make-test-struct `#f `4 `3 `#t))
  (define inst-struct-name2395
    (make-test-struct
      `,inst-struct-name2370
      `,inst-struct-name2382
      `#&((#\c #\c (+ 1 2)) ,inst-struct-name2388 (+ 4 5))
      `14))
  (define inst-struct-name2408
    (make-test-struct
      `#(#\d #f these symbols)
      `10
      `these
      `((+ 4 (+ 1 1)) symbols (+ 4 5) . symbols)))
  (define inst-struct-name2413
    (make-test-struct `(0 #\b #\b) `#\a `symbols `2))
  (define inst-struct-name2438
    (make-test-struct
      `#&((+ 4 (+ 1 1)) (+ 1 2) (+ 4 5))
      `#f
      `#((+ 4 (+ 1 1)) 9 #t #\c)
      `6))
  (define inst-struct-name2454
    (make-test-struct `#(#\a symbols 8) `#\d `#f `#(1 #\c 4)))
  (define inst-struct-name2465
    (make-test-struct `() `#\b `symbols `#(#t (+ 4 5) #t #\d)))
  (define inst-struct-name2469
    (make-test-struct
      `#\b
      `,inst-struct-name2465
      `#\d
      `#&(#\b #(4 are 1) (+ 1 2))))
  (define inst-struct-name2480
    (make-test-struct
      `#((+ 1 2) 7 are #\b)
      `(#\b symbols #\d)
      `(#\d symbols symbols)
      `(+ 1 2)))

  (define (multi-let-tests)
    (list
     (mytest
      (match-let*
        ((((or (and #\c #\c) (and #\c #\c))
           (tlp963 __3)
           (not (not #f))
           ((and (or (tlp964 tlp965 tlp966 quote are)
                     (tlp964 tlp965 tlp966 quote are))
                 (or (tlp967 tlp968 tlp969 quote are)
                     (tlp967 tlp968 tlp969 quote are)))
            (tlp970 __3)
            tlp971
            tlp972))
          `(#\c (#\d #f #\c) #f ((6 8 2 . are) ((+ 1 2) #t #\c) are #f)))
         ((#&(((tlp973 tlp974 tlp975 quote symbols)
               (not (not 'these))
               tlp976
               (and (? test-struct? tlp982)
                    (= test-struct-a tlp977)
                    (= test-struct-b tlp978)
                    (= test-struct-c tlp979)
                    (= test-struct-d tlp980)))
              ((tlp983 tlp984 tlp985 tlp986)
               (and (? test-struct? tlp992)
                    (= test-struct-a tlp987)
                    (= test-struct-b tlp988)
                    (= test-struct-c tlp989)
                    (= test-struct-d tlp990))
               #(tlp993 tlp994 tlp995 tlp996)
               #&(tlp997 tlp998 tlp999))
              (or tlp1000 tlp1000))
           (and (? test-struct? tlp1005)
                (= test-struct-a #t)
                (= test-struct-b (or tlp1001 tlp1001))
                (= test-struct-c tlp1002)
                (= test-struct-d (tlp1003 ...)))
           (not (not 'these))
           (tlp1006 ___))
          `(#&(((symbols are these . symbols) these #\c ,inst-struct-name981)
               ((#t #\a #\c (+ 1 2))
                ,inst-struct-name991
                #(#t are #\a #\d)
                #&(#\a (+ 1 2) (+ 4 (+ 1 1))))
               #\d)
            ,inst-struct-name1004
            these
            (#\d symbols #t)))
         ((()
           (and (? test-struct? tlp1033)
                (= test-struct-a tlp1007)
                (= test-struct-b (and (? number? tlp1008) (? even? tlp1009)))
                (=
                 test-struct-c
                 (and (? test-struct? tlp1023)
                      (=
                       test-struct-a
                       (and (tlp1010 tlp1011 tlp1012 . tlp1013)
                            (tlp1014 tlp1015 tlp1016 . tlp1017)))
                      (= test-struct-b ())
                      (= test-struct-c (and (tlp1018 ___) (tlp1019 ___)))
                      (=
                       test-struct-d
                       (and (? number? tlp1020) (? even? tlp1021)))))
                (=
                 test-struct-d
                 (and (? test-struct? tlp1031)
                      (=
                       test-struct-a
                       (or (and (? number? tlp1024) (? even? tlp1025))
                           (and (? number? tlp1024) (? even? tlp1025))))
                      (= test-struct-b _)
                      (= test-struct-c #(tlp1026 tlp1027 tlp1028 tlp1029))
                      (= test-struct-d _))))
           9
           (($
             test-struct
             (and (and tlp1034 tlp1035) (and tlp1036 tlp1037))
             tlp1038
             ($ test-struct tlp1039 tlp1040 tlp1041 tlp1042)
             (and (? test-struct? tlp1049)
                  (= test-struct-a tlp1044)
                  (= test-struct-b tlp1045)
                  (= test-struct-c tlp1046)
                  (= test-struct-d tlp1047)))
            tlp1051
            1
            _))
          `(() ,inst-struct-name1032 9 (,inst-struct-name1050 #f 1 these))))
        (list
         tlp963
         tlp964
         tlp965
         tlp966
         tlp964
         tlp965
         tlp966
         tlp970
         tlp971
         tlp972
         tlp973
         tlp974
         tlp975
         tlp976
         tlp977
         tlp978
         tlp979
         tlp980
         tlp983
         tlp984
         tlp985
         tlp986
         tlp987
         tlp988
         tlp989
         tlp990
         tlp993
         tlp994
         tlp995
         tlp996
         tlp997
         tlp998
         tlp999
         tlp1000
         tlp1001
         tlp1002
         tlp1003
         tlp1006
         tlp1007
         tlp1008
         tlp1010
         tlp1011
         tlp1012
         tlp1013
         tlp1018
         tlp1020
         tlp1024
         tlp1026
         tlp1027
         tlp1028
         tlp1029
         tlp1034
         tlp1038
         tlp1039
         tlp1040
         tlp1041
         tlp1042
         tlp1044
         tlp1045
         tlp1046
         tlp1047
         tlp1051))
      '((#\d #f #\c)
        6
        8
        2
        6
        8
        2
        ((+ 1 2) #t #\c)
        are
        #f
        symbols
        are
        these
        #\c
        symbols
        #\a
        (+ 1 2)
        (+ 1 2)
        #t
        #\a
        #\c
        (+ 1 2)
        #\b
        #\c
        (+ 4 5)
        #\d
        #t
        are
        #\a
        #\d
        #\a
        (+ 1 2)
        (+ 4 (+ 1 1))
        #\d
        #\b
        #\a
        ((+ 4 (+ 1 1)) #t #\d)
        (#\d symbols #t)
        #\b
        6
        #\b
        #f
        #t
        (+ 4 (+ 1 1))
        (#t #\b #\b)
        12
        2
        #\d
        symbols
        #\a
        #\c
        (+ 4 5)
        #f
        #\d
        6
        8
        (+ 1 2)
        #\b
        (+ 1 2)
        #\b
        (+ 4 (+ 1 1))
        #f))
     (mytest
      (match-let*
        (((()
           (tlp1052 ...)
           ((tlp1053 ..3)
            #(tlp1054 ...)
            #(tlp1055 #(tlp1056 ___) (tlp1057 ...) #&(tlp1058 tlp1059 tlp1060))
            .
            #\d)
           (and (? number? tlp1061) (? even? tlp1062)))
          `(()
            (6 #f #f)
            ((1 symbols (+ 4 (+ 1 1)))
             #(#\c are #f)
             #(3 #(#\b #\a #\a) (#\a #t (+ 1 2)) #&(3 #\d #\b))
             .
             #\d)
            14))
         (((and (? test-struct? tlp1092)
                (=
                 test-struct-a
                 ($
                  test-struct
                  (not (not 'are))
                  tlp1063
                  tlp1064
                  (and (? test-struct? tlp1070)
                       (= test-struct-a tlp1065)
                       (= test-struct-b tlp1066)
                       (= test-struct-c tlp1067)
                       (= test-struct-d tlp1068))))
                (=
                 test-struct-b
                 ($
                  test-struct
                  #&(tlp1072 tlp1073 tlp1074)
                  (and tlp1075 tlp1076)
                  tlp1077
                  (and (tlp1078 ...) (tlp1079 ...))))
                (=
                 test-struct-c
                 (or #((and tlp1081 tlp1082)
                       (and (? test-struct? tlp1088)
                            (= test-struct-a tlp1083)
                            (= test-struct-b tlp1084)
                            (= test-struct-c tlp1085)
                            (= test-struct-d tlp1086))
                       #\c
                       #f)
                     #((and tlp1081 tlp1082)
                       (and (? test-struct? tlp1088)
                            (= test-struct-a tlp1083)
                            (= test-struct-b tlp1084)
                            (= test-struct-c tlp1085)
                            (= test-struct-d tlp1086))
                       #\c
                       #f)))
                (= test-struct-d (and (? number? tlp1089) (? even? tlp1090))))
           #t
           (tlp1093 ..3)
           _)
          `(,inst-struct-name1091 #t ((+ 4 5) #t these) (+ 4 5)))
         (((tlp1094 __3) tlp1095 (tlp1096 ..3) ())
          `((are symbols #t) #\c (these are (+ 4 (+ 1 1))) ())))
        (list
         tlp1052
         tlp1053
         tlp1054
         tlp1055
         tlp1056
         tlp1057
         tlp1058
         tlp1059
         tlp1060
         tlp1061
         tlp1063
         tlp1064
         tlp1065
         tlp1066
         tlp1067
         tlp1068
         tlp1072
         tlp1073
         tlp1074
         tlp1075
         tlp1077
         tlp1078
         tlp1081
         tlp1083
         tlp1084
         tlp1085
         tlp1086
         tlp1089
         tlp1093
         tlp1094
         tlp1095
         tlp1096))
      '((6 #f #f)
        (1 symbols (+ 4 (+ 1 1)))
        (#\c are #f)
        3
        (#\b #\a #\a)
        (#\a #t (+ 1 2))
        3
        #\d
        #\b
        14
        symbols
        #t
        #\b
        (+ 1 2)
        8
        symbols
        #\d
        6
        (+ 4 (+ 1 1))
        #t
        0
        (#\d #\d (+ 4 5))
        6
        symbols
        #t
        #\d
        (+ 1 2)
        0
        ((+ 4 5) #t these)
        (are symbols #t)
        #\c
        (these are (+ 4 (+ 1 1)))))
     (mytest
      (match-letrec
        (((($ test-struct tlp1097 () #(tlp1098 ___) ())
           (and (? test-struct? tlp1119)
                (= test-struct-a (tlp1100 tlp1101 tlp1102 #\a))
                (=
                 test-struct-b
                 (and (? test-struct? tlp1107)
                      (= test-struct-a (tlp1103 tlp1104 tlp1105 . 8))
                      (= test-struct-b _)
                      (= test-struct-c (or '(+ 4 (+ 1 1)) '(+ 4 (+ 1 1))))
                      (= test-struct-d _)))
                (= test-struct-c tlp1108)
                (=
                 test-struct-d
                 #&((tlp1109 tlp1110 tlp1111 tlp1112)
                    (tlp1113 ..3)
                    (tlp1114 tlp1115 tlp1116 tlp1117))))
           (and (? test-struct? tlp1142)
                (=
                 test-struct-a
                 (()
                  tlp1120
                  ($ test-struct tlp1121 tlp1122 tlp1123 tlp1124)
                  #(tlp1126 tlp1127 tlp1128 tlp1129)))
                (=
                 test-struct-b
                 (and (? test-struct? tlp1140)
                      (= test-struct-a _)
                      (=
                       test-struct-b
                       (or (tlp1130 tlp1131 tlp1132 . tlp1133)
                           (tlp1130 tlp1131 tlp1132 . tlp1133)))
                      (= test-struct-c tlp1134)
                      (= test-struct-d #(tlp1135 tlp1136 tlp1137 tlp1138))))
                (= test-struct-c ())
                (= test-struct-d (not (not 5))))
           (and (? number? tlp1143) (? even? tlp1144)))
          `(,inst-struct-name1099
            ,inst-struct-name1118
            ,inst-struct-name1141
            12))
         (('(+ 1 2)
           'symbols
           _
           ($
            test-struct
            ((tlp1145 tlp1146 tlp1147 tlp1148)
             #(tlp1149 ___)
             (and (? number? tlp1150) (? even? tlp1151))
             (tlp1152 __3))
            2
            ((tlp1153 ___) #(tlp1154 ___) 'symbols . 0)
            tlp1155))
          `((+ 1 2) symbols 5 ,inst-struct-name1156))
         ((((#\c
             ($ test-struct tlp1157 tlp1158 tlp1159 tlp1160)
             (and (tlp1162 tlp1163 tlp1164 . tlp1165)
                  (tlp1166 tlp1167 tlp1168 . tlp1169))
             .
             #\b)
            ($
             test-struct
             (and (and (? test-struct? tlp1175)
                       (= test-struct-a tlp1170)
                       (= test-struct-b tlp1171)
                       (= test-struct-c tlp1172)
                       (= test-struct-d tlp1173))
                  (and (? test-struct? tlp1176)
                       (= test-struct-a tlp1177)
                       (= test-struct-b tlp1178)
                       (= test-struct-c tlp1179)
                       (= test-struct-d tlp1180)))
             tlp1181
             (and tlp1182 tlp1183)
             #&(tlp1184 tlp1185 tlp1186))
            (or _ _)
            .
            #\a)
           (_
            (and (? number? tlp1188) (? even? tlp1189))
            ()
            (and (? test-struct? tlp1199)
                 (= test-struct-a (tlp1190 ..3))
                 (= test-struct-b #(tlp1191 tlp1192 tlp1193 tlp1194))
                 (= test-struct-c #f)
                 (= test-struct-d #&(tlp1195 tlp1196 tlp1197))))
           (and (? number? tlp1200) (? even? tlp1201))
           '(+ 4 5))
          `(((#\c ,inst-struct-name1161 (#\a are 4 . #t) . #\b)
             ,inst-struct-name1187
             (+ 1 2)
             .
             #\a)
            (#\b 6 () ,inst-struct-name1198)
            0
            (+ 4 5))))
        (list
         tlp1097
         tlp1098
         tlp1100
         tlp1101
         tlp1102
         tlp1103
         tlp1104
         tlp1105
         tlp1108
         tlp1109
         tlp1110
         tlp1111
         tlp1112
         tlp1113
         tlp1114
         tlp1115
         tlp1116
         tlp1117
         tlp1120
         tlp1121
         tlp1122
         tlp1123
         tlp1124
         tlp1126
         tlp1127
         tlp1128
         tlp1129
         tlp1130
         tlp1131
         tlp1132
         tlp1133
         tlp1130
         tlp1131
         tlp1132
         tlp1133
         tlp1134
         tlp1135
         tlp1136
         tlp1137
         tlp1138
         tlp1143
         tlp1145
         tlp1146
         tlp1147
         tlp1148
         tlp1149
         tlp1150
         tlp1152
         tlp1153
         tlp1154
         tlp1155
         tlp1157
         tlp1158
         tlp1159
         tlp1160
         tlp1162
         tlp1163
         tlp1164
         tlp1165
         tlp1170
         tlp1171
         tlp1172
         tlp1173
         tlp1181
         tlp1182
         tlp1184
         tlp1185
         tlp1186
         tlp1188
         tlp1190
         tlp1191
         tlp1192
         tlp1193
         tlp1194
         tlp1195
         tlp1196
         tlp1197
         tlp1200))
      '(8
        (#\d (+ 1 2) 9)
        (+ 4 (+ 1 1))
        these
        #\d
        #\c
        5
        #\a
        #\a
        are
        #\c
        #t
        (+ 4 5)
        (#\b 0 #\c)
        1
        are
        (+ 1 2)
        (+ 4 (+ 1 1))
        these
        #\b
        (+ 4 5)
        #\c
        these
        6
        1
        5
        #\c
        #t
        #\c
        these
        9
        #t
        #\c
        these
        9
        #\a
        9
        (+ 4 (+ 1 1))
        #t
        (+ 1 2)
        12
        #t
        5
        9
        5
        (symbols symbols these)
        14
        (are these are)
        ((+ 4 (+ 1 1)) these #f)
        ((+ 4 (+ 1 1)) #t #\b)
        are
        #\d
        #\c
        these
        1
        #\a
        are
        4
        #t
        #f
        (+ 4 5)
        #\d
        #f
        #\c
        #\c
        symbols
        2
        #\a
        6
        (#\d #\b #t)
        #f
        (+ 4 5)
        symbols
        (+ 1 2)
        #\d
        8
        3
        0))
     (mytest
      (match-let
        (((#f
           (tlp1202 ..3)
           #(tlp1203 ___)
           (#&(_ #&(tlp1204 tlp1205 tlp1206) (tlp1207 ___))
            (and (? number? tlp1208) (? even? tlp1209))
            ()
            (and (tlp1210 ...) (tlp1211 ...))))
          `(#f
            (these are #\a)
            #(#\b (+ 4 5) #\c)
            (#&(9 #&(#t #\b #\a) ((+ 4 (+ 1 1)) #\b are)) 18 () (#t #\b #t))))
         (('symbols (and (? number? tlp1212) (? even? tlp1213)) #f 'are)
          `(symbols 2 #f are))
         (((and tlp1214 tlp1215)
           (and (? test-struct? tlp1233)
                (= test-struct-a #(tlp1216 ___))
                (=
                 test-struct-b
                 (and (? test-struct? tlp1222)
                      (= test-struct-a tlp1217)
                      (= test-struct-b (not (not 4)))
                      (= test-struct-c ())
                      (= test-struct-d (tlp1218 tlp1219 tlp1220 . #\d))))
                (= test-struct-c #&(() (not (not '(+ 1 2))) ()))
                (=
                 test-struct-d
                 (and (? test-struct? tlp1231)
                      (= test-struct-a ())
                      (= test-struct-b #(tlp1223 ...))
                      (= test-struct-c tlp1224)
                      (=
                       test-struct-d
                       ($ test-struct tlp1225 tlp1226 tlp1227 tlp1228)))))
           (tlp1234 ___)
           _)
          `((+ 4 5) ,inst-struct-name1232 ((+ 4 5) #t #\c) #\d)))
        (list
         tlp1202
         tlp1203
         tlp1204
         tlp1205
         tlp1206
         tlp1207
         tlp1208
         tlp1210
         tlp1212
         tlp1214
         tlp1216
         tlp1217
         tlp1218
         tlp1219
         tlp1220
         tlp1223
         tlp1224
         tlp1225
         tlp1226
         tlp1227
         tlp1228
         tlp1234))
      '((these are #\a)
        (#\b (+ 4 5) #\c)
        #t
        #\b
        #\a
        ((+ 4 (+ 1 1)) #\b are)
        18
        (#t #\b #t)
        2
        (+ 4 5)
        ((+ 1 2) #\a 2)
        #\c
        #\d
        #\c
        #\c
        (#\b 0 (+ 4 5))
        #\b
        #\c
        (+ 4 (+ 1 1))
        these
        #f
        ((+ 4 5) #t #\c)))
     (mytest
      (match-let*
        ((((and (? test-struct? tlp1239)
                (= test-struct-a (tlp1235 __3))
                (= test-struct-b tlp1236)
                (= test-struct-c ())
                (= test-struct-d (tlp1237 __3)))
           tlp1240
           (#(tlp1241 ___)
            (or (or (tlp1242 tlp1243 tlp1244 tlp1245)
                    (tlp1242 tlp1243 tlp1244 tlp1245))
                (or (tlp1242 tlp1243 tlp1244 tlp1245)
                    (tlp1242 tlp1243 tlp1244 tlp1245)))
            (or ($
                 test-struct
                 #&(tlp1246 tlp1247 tlp1248)
                 (tlp1249 ___)
                 (and (? test-struct? tlp1255)
                      (= test-struct-a tlp1250)
                      (= test-struct-b tlp1251)
                      (= test-struct-c tlp1252)
                      (= test-struct-d tlp1253))
                 (tlp1256 tlp1257 tlp1258 tlp1259))
                ($
                 test-struct
                 #&(tlp1246 tlp1247 tlp1248)
                 (tlp1249 ___)
                 (and (? test-struct? tlp1255)
                      (= test-struct-a tlp1250)
                      (= test-struct-b tlp1251)
                      (= test-struct-c tlp1252)
                      (= test-struct-d tlp1253))
                 (tlp1256 tlp1257 tlp1258 tlp1259)))
            quote
            these)
           (tlp1261
             (not (not #t))
             #&((and (? test-struct? tlp1267)
                     (= test-struct-a tlp1262)
                     (= test-struct-b tlp1263)
                     (= test-struct-c tlp1264)
                     (= test-struct-d tlp1265))
                #\d
                (tlp1268 tlp1269 tlp1270 tlp1271))
             (()
              (tlp1272 tlp1273 tlp1274 . #\c)
              (and (? test-struct? tlp1280)
                   (= test-struct-a tlp1275)
                   (= test-struct-b tlp1276)
                   (= test-struct-c tlp1277)
                   (= test-struct-d tlp1278))
              .
              #f)))
          `(,inst-struct-name1238
            1
            (#(#\c #\c (+ 1 2))
             (#\c #\b (+ 4 (+ 1 1)) #\d)
             ,inst-struct-name1260
             .
             these)
            ((+ 4 (+ 1 1))
             #t
             #&(,inst-struct-name1266 #\d (symbols #\a #t #\d))
             (() (#t #f #\c . #\c) ,inst-struct-name1279 . #f))))
         ((#(tlp1281 ___)
           tlp1282
           (not (not #\b))
           (or (tlp1283 ___) (tlp1283 ___)))
          `(#((+ 4 5) are #\c) are #\b (#\a #\a symbols)))
         ((((tlp1284 ..3)
            ()
            tlp1285
            #&((and (? number? tlp1286) (? even? tlp1287))
               (or #&(tlp1288 tlp1289 tlp1290) #&(tlp1288 tlp1289 tlp1290))
               #&(tlp1291 tlp1292 tlp1293)))
           (tlp1294 __3)
           2
           #(tlp1295 ___))
          `(((are #\c (+ 4 (+ 1 1)))
             ()
             #\d
             #&(2 #&(8 (+ 4 5) #\b) #&(are 3 #\a)))
            (3 1 these)
            2
            #(3 (+ 4 5) #\b))))
        (list
         tlp1235
         tlp1236
         tlp1237
         tlp1240
         tlp1241
         tlp1242
         tlp1243
         tlp1244
         tlp1245
         tlp1246
         tlp1247
         tlp1248
         tlp1249
         tlp1250
         tlp1251
         tlp1252
         tlp1253
         tlp1256
         tlp1257
         tlp1258
         tlp1259
         tlp1261
         tlp1262
         tlp1263
         tlp1264
         tlp1265
         tlp1268
         tlp1269
         tlp1270
         tlp1271
         tlp1272
         tlp1273
         tlp1274
         tlp1275
         tlp1276
         tlp1277
         tlp1278
         tlp1281
         tlp1282
         tlp1283
         tlp1284
         tlp1285
         tlp1286
         tlp1288
         tlp1289
         tlp1290
         tlp1291
         tlp1292
         tlp1293
         tlp1294
         tlp1295))
      '((#f #\c #\c)
        #\b
        (#\d these these)
        1
        (#\c #\c (+ 1 2))
        #\c
        #\b
        (+ 4 (+ 1 1))
        #\d
        are
        #\c
        #t
        (9 these #\d)
        #\b
        (+ 4 (+ 1 1))
        #f
        (+ 4 5)
        are
        #\b
        (+ 1 2)
        3
        (+ 4 (+ 1 1))
        #t
        #\b
        #t
        #\b
        symbols
        #\a
        #t
        #\d
        #t
        #f
        #\c
        #\d
        #\c
        #\a
        #t
        ((+ 4 5) are #\c)
        are
        (#\a #\a symbols)
        (are #\c (+ 4 (+ 1 1)))
        #\d
        2
        8
        (+ 4 5)
        #\b
        are
        3
        #\a
        (3 1 these)
        (3 (+ 4 5) #\b)))))

  (define (mult-match-tests)
    (list
     (let ((tlp1555
             (lambda (tlp1436)
               (match
                tlp1436
                ((($ test-struct (tlp1437 ...) #(tlp1438 ___) #\d 'are)
                  (and ($
                        test-struct
                        (tlp1440 ___)
                        ((tlp1441 tlp1442 tlp1443 . tlp1444)
                         ()
                         (and (? number? tlp1445) (? even? tlp1446))
                         .
                         #t)
                        ((tlp1447 tlp1448 tlp1449 . tlp1450)
                         tlp1451
                         (tlp1452 tlp1453 tlp1454 . tlp1455)
                         (and (? test-struct? tlp1461)
                              (= test-struct-a tlp1456)
                              (= test-struct-b tlp1457)
                              (= test-struct-c tlp1458)
                              (= test-struct-d tlp1459)))
                        #&((and (? number? tlp1462) (? even? tlp1463))
                           tlp1464
                           (or tlp1465 tlp1465)))
                       ($
                        test-struct
                        (tlp1467 ___)
                        ((tlp1468 tlp1469 tlp1470 . tlp1471)
                         ()
                         (and (? number? tlp1472) (? even? tlp1473))
                         .
                         #t)
                        ((tlp1474 tlp1475 tlp1476 . tlp1477)
                         tlp1478
                         (tlp1479 tlp1480 tlp1481 . tlp1482)
                         (and (? test-struct? tlp1483)
                              (= test-struct-a tlp1484)
                              (= test-struct-b tlp1485)
                              (= test-struct-c tlp1486)
                              (= test-struct-d tlp1487)))
                        #&((and (? number? tlp1488) (? even? tlp1489))
                           tlp1490
                           (or tlp1491 tlp1491))))
                  #(tlp1492 ...)
                  #(#&(_ #(tlp1493 ___) tlp1494)
                    (not (not #\b))
                    (and (? test-struct? tlp1503)
                         (= test-struct-a (and _ _))
                         (= test-struct-b (and () ()))
                         (= test-struct-c #&(tlp1495 tlp1496 tlp1497))
                         (= test-struct-d #(tlp1498 tlp1499 tlp1500 tlp1501)))
                    _))
                 (list
                  tlp1437
                  tlp1438
                  tlp1440
                  tlp1441
                  tlp1442
                  tlp1443
                  tlp1444
                  tlp1445
                  tlp1447
                  tlp1448
                  tlp1449
                  tlp1450
                  tlp1451
                  tlp1452
                  tlp1453
                  tlp1454
                  tlp1455
                  tlp1456
                  tlp1457
                  tlp1458
                  tlp1459
                  tlp1462
                  tlp1464
                  tlp1465
                  tlp1492
                  tlp1493
                  tlp1494
                  tlp1495
                  tlp1496
                  tlp1497
                  tlp1498
                  tlp1499
                  tlp1500
                  tlp1501))
                (((and #((and (? test-struct? tlp1509)
                              (= test-struct-a #\c)
                              (= test-struct-b _)
                              (=
                               test-struct-c
                               (tlp1504 tlp1505 tlp1506 . tlp1507))
                              (= test-struct-d ()))
                         tlp1510
                         (tlp1511 __3)
                         #\a)
                       #((and (? test-struct? tlp1512)
                              (= test-struct-a #\c)
                              (= test-struct-b _)
                              (=
                               test-struct-c
                               (tlp1513 tlp1514 tlp1515 . tlp1516))
                              (= test-struct-d ()))
                         tlp1517
                         (tlp1518 __3)
                         #\a))
                  _
                  ($
                   test-struct
                   ($ test-struct #t #\b #\d (or 6 6))
                   (and (? number? tlp1520) (? even? tlp1521))
                   (or ($
                        test-struct
                        (tlp1522 tlp1523 tlp1524 . tlp1525)
                        (tlp1526 tlp1527 tlp1528 . tlp1529)
                        (not (not '(+ 1 2)))
                        (and (? test-struct? tlp1535)
                             (= test-struct-a tlp1530)
                             (= test-struct-b tlp1531)
                             (= test-struct-c tlp1532)
                             (= test-struct-d tlp1533)))
                       ($
                        test-struct
                        (tlp1522 tlp1523 tlp1524 . tlp1525)
                        (tlp1526 tlp1527 tlp1528 . tlp1529)
                        (not (not '(+ 1 2)))
                        (and (? test-struct? tlp1535)
                             (= test-struct-a tlp1530)
                             (= test-struct-b tlp1531)
                             (= test-struct-c tlp1532)
                             (= test-struct-d tlp1533))))
                   tlp1537)
                  (and (#(tlp1539 ...)
                        #\b
                        ($ test-struct tlp1540 3 tlp1541 tlp1542)
                        (tlp1544 ..3))
                       (#(tlp1545 ...)
                        #\b
                        ($ test-struct tlp1546 3 tlp1547 tlp1548)
                        (tlp1549 ..3))))
                 (list
                  tlp1504
                  tlp1505
                  tlp1506
                  tlp1507
                  tlp1510
                  tlp1511
                  tlp1520
                  tlp1522
                  tlp1523
                  tlp1524
                  tlp1525
                  tlp1526
                  tlp1527
                  tlp1528
                  tlp1529
                  tlp1530
                  tlp1531
                  tlp1532
                  tlp1533
                  tlp1537
                  tlp1539
                  tlp1540
                  tlp1541
                  tlp1542
                  tlp1544))
                (((and tlp1550 tlp1551)
                  (tlp1552 __3)
                  (tlp1553 ___)
                  (tlp1554 ...))
                 (list tlp1550 tlp1552 tlp1553 tlp1554))))))
       (list
        (mytest
         (tlp1555
           `(,inst-struct-name1439
             ,inst-struct-name1466
             #(#\a #\b (+ 4 (+ 1 1)))
             #(#&(#t #((+ 1 2) #f symbols) 2)
               #\b
               ,inst-struct-name1502
               (+ 4 (+ 1 1)))))
         '((#t 8 #f)
           (#\a are 3)
           (#\a 4 #t)
           #\a
           (+ 1 2)
           7
           #\d
           0
           #f
           #\a
           #\b
           #\a
           are
           2
           symbols
           #f
           (+ 4 (+ 1 1))
           #f
           #\d
           5
           #\b
           2
           #\b
           #\b
           (#\a #\b (+ 4 (+ 1 1)))
           ((+ 1 2) #f symbols)
           2
           2
           are
           8
           these
           #\c
           #\a
           #\a))
        (mytest
         (tlp1555
           `(#(,inst-struct-name1508 #f (#\c symbols 9) #\a)
             #f
             ,inst-struct-name1538
             (#(#\b are (+ 4 (+ 1 1)))
              #\b
              ,inst-struct-name1543
              (#f #\c #\d))))
         '(#t
           (+ 4 (+ 1 1))
           #\c
           symbols
           #f
           (#\c symbols 9)
           2
           #\d
           #f
           0
           #\d
           are
           are
           #\c
           #\b
           these
           #\b
           #\c
           are
           (+ 4 5)
           (#\b are (+ 4 (+ 1 1)))
           2
           #\d
           (+ 4 5)
           (#f #\c #\d)))
        (mytest
         (tlp1555 `(#t (#\c 8 #\a) (#\d #\b #\d) (#\d symbols 8)))
         '(#t (#\c 8 #\a) (#\d #\b #\d) (#\d symbols 8)))))
     (let ((tlp1668
             (lambda (tlp1556)
               (match
                tlp1556
                ((#((and (? test-struct? tlp1564)
                         (= test-struct-a '(+ 4 (+ 1 1)))
                         (=
                          test-struct-b
                          (and (? test-struct? tlp1562)
                               (= test-struct-a tlp1557)
                               (= test-struct-b tlp1558)
                               (= test-struct-c tlp1559)
                               (= test-struct-d tlp1560)))
                         (= test-struct-c '(+ 4 5))
                         (= test-struct-d (not (not 3))))
                    (and #&(#&(tlp1565 tlp1566 tlp1567)
                            ($ test-struct tlp1568 tlp1569 tlp1570 tlp1571)
                            ($ test-struct tlp1573 tlp1574 tlp1575 tlp1576))
                         #&(#&(tlp1578 tlp1579 tlp1580)
                            ($ test-struct tlp1581 tlp1582 tlp1583 tlp1584)
                            ($ test-struct tlp1585 tlp1586 tlp1587 tlp1588)))
                    (or ((or tlp1589 tlp1589)
                         (and (? test-struct? tlp1595)
                              (= test-struct-a tlp1590)
                              (= test-struct-b tlp1591)
                              (= test-struct-c tlp1592)
                              (= test-struct-d tlp1593))
                         #(tlp1596 tlp1597 tlp1598 tlp1599)
                         .
                         #t)
                        ((or tlp1589 tlp1589)
                         (and (? test-struct? tlp1595)
                              (= test-struct-a tlp1590)
                              (= test-struct-b tlp1591)
                              (= test-struct-c tlp1592)
                              (= test-struct-d tlp1593))
                         #(tlp1596 tlp1597 tlp1598 tlp1599)
                         .
                         #t))
                    (and (? test-struct? tlp1610)
                         (= test-struct-a (tlp1600 ...))
                         (= test-struct-b (tlp1601 ...))
                         (= test-struct-c (tlp1602 tlp1603 tlp1604 tlp1605))
                         (= test-struct-d (tlp1606 tlp1607 tlp1608 . #\b))))
                  ((and (? number? tlp1611) (? even? tlp1612))
                   (or (and (? number? tlp1613) (? even? tlp1614))
                       (and (? number? tlp1613) (? even? tlp1614)))
                   #&(($ test-struct tlp1615 tlp1616 tlp1617 tlp1618)
                      (and (? number? tlp1620) (? even? tlp1621))
                      #f)
                   (and (? number? tlp1622) (? even? tlp1623)))
                  _
                  ())
                 (list
                  tlp1557
                  tlp1558
                  tlp1559
                  tlp1560
                  tlp1565
                  tlp1566
                  tlp1567
                  tlp1568
                  tlp1569
                  tlp1570
                  tlp1571
                  tlp1573
                  tlp1574
                  tlp1575
                  tlp1576
                  tlp1589
                  tlp1590
                  tlp1591
                  tlp1592
                  tlp1593
                  tlp1596
                  tlp1597
                  tlp1598
                  tlp1599
                  tlp1589
                  tlp1590
                  tlp1591
                  tlp1592
                  tlp1593
                  tlp1596
                  tlp1597
                  tlp1598
                  tlp1599
                  tlp1600
                  tlp1601
                  tlp1602
                  tlp1603
                  tlp1604
                  tlp1605
                  tlp1606
                  tlp1607
                  tlp1608
                  tlp1611
                  tlp1613
                  tlp1615
                  tlp1616
                  tlp1617
                  tlp1618
                  tlp1620
                  tlp1622))
                ((#&((#(tlp1624 tlp1625 tlp1626 tlp1627) tlp1628 _ . #t)
                     (tlp1629 ___)
                     (#(tlp1630 ___)
                      (and (? test-struct? tlp1636)
                           (= test-struct-a tlp1631)
                           (= test-struct-b tlp1632)
                           (= test-struct-c tlp1633)
                           (= test-struct-d tlp1634))
                      tlp1637
                      tlp1638))
                  (tlp1639 ..3)
                  (_ (tlp1640 __3) _ . #t)
                  #&(tlp1641
                      #t
                      #((and (? number? tlp1642) (? even? tlp1643))
                        ()
                        #\a
                        #&(tlp1644 tlp1645 tlp1646))))
                 (list
                  tlp1624
                  tlp1625
                  tlp1626
                  tlp1627
                  tlp1628
                  tlp1629
                  tlp1630
                  tlp1631
                  tlp1632
                  tlp1633
                  tlp1634
                  tlp1637
                  tlp1638
                  tlp1639
                  tlp1640
                  tlp1641
                  tlp1642
                  tlp1644
                  tlp1645
                  tlp1646))
                ((((and ((not (not 'symbols))
                         _
                         (tlp1647 tlp1648 tlp1649 . tlp1650)
                         .
                         0)
                        ((not (not 'symbols))
                         _
                         (tlp1651 tlp1652 tlp1653 . tlp1654)
                         .
                         0))
                   _
                   ($
                    test-struct
                    (tlp1655 tlp1656 tlp1657 . #f)
                    (tlp1658 ...)
                    (and (? test-struct? tlp1664)
                         (= test-struct-a tlp1659)
                         (= test-struct-b tlp1660)
                         (= test-struct-c tlp1661)
                         (= test-struct-d tlp1662))
                    (tlp1665 ___))
                   #(_ (and _ _) (not (not 6)) _))
                  (tlp1667 __3)
                  (or 9 9)
                  '(+ 4 5))
                 (list
                  tlp1647
                  tlp1648
                  tlp1649
                  tlp1650
                  tlp1655
                  tlp1656
                  tlp1657
                  tlp1658
                  tlp1659
                  tlp1660
                  tlp1661
                  tlp1662
                  tlp1665
                  tlp1667))))))
       (list
        (mytest
         (tlp1668
           `(#(,inst-struct-name1563
               #&(#&(#\c (+ 4 (+ 1 1)) these)
                  ,inst-struct-name1572
                  ,inst-struct-name1577)
               (4 ,inst-struct-name1594 #(#t #\d these #\c) . #t)
               ,inst-struct-name1609)
             (0 10 #&(,inst-struct-name1619 2 #f) 2)
             are
             ()))
         '(#f
           symbols
           #\b
           #t
           #\c
           (+ 4 (+ 1 1))
           these
           (+ 1 2)
           #\a
           #\b
           #\d
           #\b
           #\b
           #\b
           4
           4
           #f
           are
           #\b
           #f
           #t
           #\d
           these
           #\c
           4
           #f
           are
           #\b
           #f
           #t
           #\d
           these
           #\c
           (symbols (+ 1 2) #\d)
           (#\c symbols are)
           are
           are
           #\c
           (+ 1 2)
           symbols
           #\d
           (+ 1 2)
           0
           10
           #t
           (+ 4 (+ 1 1))
           #\c
           #t
           2
           2))
        (mytest
         (tlp1668
           `(#&((#(#\c symbols #\a 8) #\b #f . #t)
                ((+ 4 (+ 1 1)) (+ 4 5) #\c)
                (#(#\d (+ 4 5) 8) ,inst-struct-name1635 7 #t))
             (3 7 #\d)
             (#\b (#\b (+ 1 2) #f) #f . #t)
             #&(#\a #t #(10 () #\a #&(symbols symbols 7)))))
         '(#\c
           symbols
           #\a
           8
           #\b
           ((+ 4 (+ 1 1)) (+ 4 5) #\c)
           (#\d (+ 4 5) 8)
           #\b
           #\d
           (+ 4 (+ 1 1))
           #\c
           7
           #t
           (3 7 #\d)
           (#\b (+ 1 2) #f)
           #\a
           10
           symbols
           symbols
           7))
        (mytest
         (tlp1668
           `(((symbols #\d (symbols #\d are . 8) . 0)
              #\b
              ,inst-struct-name1666
              #(#f #\d 6 #\c))
             (#\c are (+ 1 2))
             9
             (+ 4 5)))
         '(symbols
            #\d
            are
            8
            #\c
            these
            symbols
            (4 5 #f)
            #f
            #\a
            #f
            (+ 1 2)
            (#f (+ 4 5) (+ 4 5))
            (#\c are (+ 1 2))))))
     (let ((tlp1713
             (lambda (tlp1669)
               (match
                tlp1669
                ((#&(() () (tlp1670 __3))
                  (not (not '(+ 1 2)))
                  #&('symbols
                     #((or (tlp1671 tlp1672 tlp1673 tlp1674)
                           (tlp1671 tlp1672 tlp1673 tlp1674))
                       tlp1675
                       #(tlp1676 ...)
                       (tlp1677 tlp1678 tlp1679 tlp1680))
                     tlp1681)
                  #&(#(tlp1682 #f #\a _)
                     #((and (? test-struct? tlp1688)
                            (= test-struct-a tlp1683)
                            (= test-struct-b tlp1684)
                            (= test-struct-c tlp1685)
                            (= test-struct-d tlp1686))
                       _
                       (tlp1689 __3)
                       #&(tlp1690 tlp1691 tlp1692))
                     ()))
                 (list
                  tlp1670
                  tlp1671
                  tlp1672
                  tlp1673
                  tlp1674
                  tlp1675
                  tlp1676
                  tlp1677
                  tlp1678
                  tlp1679
                  tlp1680
                  tlp1681
                  tlp1682
                  tlp1683
                  tlp1684
                  tlp1685
                  tlp1686
                  tlp1689
                  tlp1690
                  tlp1691
                  tlp1692))
                ((($
                   test-struct
                   #(#(tlp1693 tlp1694 tlp1695 tlp1696)
                     (tlp1697 __3)
                     (not (not #\c))
                     tlp1698)
                   #(tlp1699 ___)
                   (tlp1700 ..3)
                   #\b)
                  ($
                   test-struct
                   tlp1702
                   #(() #&(tlp1703 tlp1704 tlp1705) (or () ()) tlp1706)
                   _
                   tlp1707)
                  #t
                  tlp1709)
                 (list
                  tlp1693
                  tlp1694
                  tlp1695
                  tlp1696
                  tlp1697
                  tlp1698
                  tlp1699
                  tlp1700
                  tlp1702
                  tlp1703
                  tlp1704
                  tlp1705
                  tlp1706
                  tlp1707
                  tlp1709))
                ((tlp1710 #(tlp1711 ___) _ tlp1712)
                 (list tlp1710 tlp1711 tlp1712))))))
       (list
        (mytest
         (tlp1713
           `(#&(() () (#f are #\d))
             (+ 1 2)
             #&(symbols
                 #((#\a 3 (+ 1 2) 3)
                   (+ 4 5)
                   #(#f #\c 2)
                   (#t (+ 4 5) (+ 4 (+ 1 1)) symbols))
                 these)
             #&(#(#t #f #\a #f)
                #(,inst-struct-name1687 8 (3 5 #\d) #&(#\d 8 #\a))
                ())))
         '((#f are #\d)
           #\a
           3
           (+ 1 2)
           3
           (+ 4 5)
           (#f #\c 2)
           #t
           (+ 4 5)
           (+ 4 (+ 1 1))
           symbols
           these
           #t
           symbols
           are
           1
           are
           (3 5 #\d)
           #\d
           8
           #\a))
        (mytest
         (tlp1713 `(,inst-struct-name1701 ,inst-struct-name1708 #t #\c))
         '(these
           #\d
           these
           symbols
           (are 6 these)
           #\d
           (#f 8 (+ 4 5))
           (#\b are symbols)
           (+ 1 2)
           (+ 4 5)
           #\a
           #f
           (+ 4 (+ 1 1))
           #\a
           #\c))
        (mytest (tlp1713 `(#f #(9 (+ 4 5) 9) #t #\d)) '(#f (9 (+ 4 5) 9) #\d))))
     (let ((tlp1746
             (lambda (tlp1714)
               (match
                tlp1714
                (((()
                   (and (? test-struct? tlp1720)
                        (= test-struct-a #&(tlp1715 tlp1716 tlp1717))
                        (= test-struct-b _)
                        (= test-struct-c 0)
                        (= test-struct-d tlp1718))
                   (not (not '(+ 4 5)))
                   .
                   8)
                  '(+ 4 (+ 1 1))
                  (and (? number? tlp1721) (? even? tlp1722))
                  #((and (? number? tlp1723) (? even? tlp1724))
                    _
                    #&((not (not '(+ 4 (+ 1 1)))) (not (not #t)) (tlp1725 ___))
                    (not (not '(+ 1 2)))))
                 (list
                  tlp1715
                  tlp1716
                  tlp1717
                  tlp1718
                  tlp1721
                  tlp1723
                  tlp1725))
                ((_ #&((tlp1726 ...) tlp1727 ()) tlp1728 '(+ 4 5))
                 (list tlp1726 tlp1727 tlp1728))
                ((tlp1729
                   #\a
                   #((and (? number? tlp1730) (? even? tlp1731))
                     ('(+ 1 2)
                      (tlp1732 tlp1733 tlp1734 tlp1735)
                      (tlp1736 __3)
                      .
                      #\b)
                     ((and (? test-struct? tlp1742)
                           (= test-struct-a tlp1737)
                           (= test-struct-b tlp1738)
                           (= test-struct-c tlp1739)
                           (= test-struct-d tlp1740))
                      ()
                      #&(tlp1743 tlp1744 tlp1745)
                      quote
                      these)
                     ())
                   #f)
                 (list
                  tlp1729
                  tlp1730
                  tlp1732
                  tlp1733
                  tlp1734
                  tlp1735
                  tlp1736
                  tlp1737
                  tlp1738
                  tlp1739
                  tlp1740
                  tlp1743
                  tlp1744
                  tlp1745))))))
       (list
        (mytest
         (tlp1746
           `((() ,inst-struct-name1719 (+ 4 5) . 8)
             (+ 4 (+ 1 1))
             2
             #(6 2 #&((+ 4 (+ 1 1)) #t (2 (+ 4 (+ 1 1)) are)) (+ 1 2))))
         '(#\d #t #t (+ 4 (+ 1 1)) 2 6 (2 (+ 4 (+ 1 1)) are)))
        (mytest
         (tlp1746 `(are #&(((+ 4 5) are these) (+ 1 2) ()) are (+ 4 5)))
         '(((+ 4 5) are these) (+ 1 2) are))
        (mytest
         (tlp1746
           `(#\d
             #\a
             #(4
               ((+ 1 2) (4 #t #\b #f) (0 (+ 4 5) these) . #\b)
               (,inst-struct-name1741 () #&(#\d #\c #\a) . these)
               ())
             #f))
         '(#\d 4 4 #t #\b #f (0 (+ 4 5) these) these #t 7 #\b #\d #\c #\a))))
     (let ((tlp1820
             (lambda (tlp1747)
               (match
                tlp1747
                ((#(tlp1748 ...) _ (or _ _) (tlp1749 __3))
                 (list tlp1748 tlp1749))
                (((tlp1750 ..3)
                  (and tlp1751 tlp1752)
                  tlp1753
                  (tlp1754
                    (() tlp1755 () #&(tlp1756 tlp1757 tlp1758))
                    ((tlp1759 ___)
                     (and (? number? tlp1760) (? even? tlp1761))
                     #(tlp1762 tlp1763 tlp1764 tlp1765)
                     .
                     #t)
                    .
                    #t))
                 (list
                  tlp1750
                  tlp1751
                  tlp1753
                  tlp1754
                  tlp1755
                  tlp1756
                  tlp1757
                  tlp1758
                  tlp1759
                  tlp1760
                  tlp1762
                  tlp1763
                  tlp1764
                  tlp1765))
                (((and (? test-struct? tlp1777)
                       (= test-struct-a tlp1766)
                       (= test-struct-b #(tlp1767 ...))
                       (= test-struct-c #(tlp1768 ___))
                       (=
                        test-struct-d
                        ($
                         test-struct
                         (tlp1769 tlp1770 tlp1771 tlp1772)
                         (tlp1773 ___)
                         tlp1774
                         #t)))
                  tlp1778
                  (and (? test-struct? tlp1798)
                       (=
                        test-struct-a
                        ((tlp1779 tlp1780 tlp1781 . #\a)
                         (and (? test-struct? tlp1787)
                              (= test-struct-a tlp1782)
                              (= test-struct-b tlp1783)
                              (= test-struct-c tlp1784)
                              (= test-struct-d tlp1785))
                         (tlp1788 ___)
                         #&(tlp1789 tlp1790 tlp1791)))
                       (= test-struct-b (or '(+ 4 5) '(+ 4 5)))
                       (=
                        test-struct-c
                        ($ test-struct tlp1792 #(tlp1793 ...) tlp1794 tlp1795))
                       (= test-struct-d _))
                  (and (? test-struct? tlp1819)
                       (=
                        test-struct-a
                        #&(tlp1799 (tlp1800 ..3) (and tlp1801 tlp1802)))
                       (=
                        test-struct-b
                        (and (? test-struct? tlp1814)
                             (=
                              test-struct-a
                              (tlp1803 tlp1804 tlp1805 tlp1806))
                             (=
                              test-struct-b
                              #(tlp1807 tlp1808 tlp1809 tlp1810))
                             (=
                              test-struct-c
                              (and (or tlp1811 tlp1811) (or tlp1812 tlp1812)))
                             (= test-struct-d (or () ()))))
                       (= test-struct-c #&(tlp1815 () tlp1816))
                       (= test-struct-d #(tlp1817 ...))))
                 (list
                  tlp1766
                  tlp1767
                  tlp1768
                  tlp1769
                  tlp1770
                  tlp1771
                  tlp1772
                  tlp1773
                  tlp1774
                  tlp1778
                  tlp1779
                  tlp1780
                  tlp1781
                  tlp1782
                  tlp1783
                  tlp1784
                  tlp1785
                  tlp1788
                  tlp1789
                  tlp1790
                  tlp1791
                  tlp1792
                  tlp1793
                  tlp1794
                  tlp1795
                  tlp1799
                  tlp1800
                  tlp1801
                  tlp1803
                  tlp1804
                  tlp1805
                  tlp1806
                  tlp1807
                  tlp1808
                  tlp1809
                  tlp1810
                  tlp1811
                  tlp1815
                  tlp1816
                  tlp1817))))))
       (list
        (mytest
         (tlp1820 `(#(#\d symbols #\a) #f 2 (3 #f these)))
         '((#\d symbols #\a) (3 #f these)))
        (mytest
         (tlp1820
           `(((+ 4 (+ 1 1)) #\c 6)
             #\d
             #\b
             (#\d
              (() #\a () #&(symbols are #t))
              ((#\b #f 0) 18 #(0 (+ 4 5) #\d these) . #t)
              .
              #t)))
         '(((+ 4 (+ 1 1)) #\c 6)
           #\d
           #\b
           #\d
           #\a
           symbols
           are
           #t
           (#\b #f 0)
           18
           0
           (+ 4 5)
           #\d
           these))
        (mytest
         (tlp1820
           `(,inst-struct-name1776
             (+ 4 (+ 1 1))
             ,inst-struct-name1797
             ,inst-struct-name1818))
         '(symbols
            (#\d #\c (+ 4 (+ 1 1)))
            (symbols #\a #\d)
            8
            #f
            #f
            (+ 4 (+ 1 1))
            (9 #\c #t)
            #f
            (+ 4 (+ 1 1))
            these
            #t
            (+ 4 (+ 1 1))
            0
            #\a
            0
            #t
            (#\a #t 7)
            9
            #\b
            symbols
            3
            (#\b #\c 0)
            #t
            (+ 4 5)
            #\a
            (9 #\c symbols)
            #f
            symbols
            #\d
            #\b
            are
            #\d
            (+ 4 (+ 1 1))
            #f
            #\b
            are
            8
            #\d
            (#\a these symbols)))))))

  (define (mult-match-lambda-tests)
    (list
     (let ((tlp1951
             (match-lambda
               (((tlp1907 ___) () 'symbols 'these) (list tlp1907))
               (((#t (and () ()) (and tlp1908 tlp1909) (tlp1910 ___))
                 #(tlp1911 ___)
                 ((not (not #\b)) '(+ 1 2) tlp1912 . #\a)
                 tlp1913)
                (list tlp1908 tlp1910 tlp1911 tlp1912 tlp1913))
               ((#((and (? test-struct? tlp1920)
                        (= test-struct-a tlp1914)
                        (= test-struct-b tlp1915)
                        (= test-struct-c #&(tlp1916 tlp1917 tlp1918))
                        (= test-struct-d #\b))
                   (_ (tlp1921 tlp1922 tlp1923 tlp1924) #f . #\d)
                   (and (? test-struct? tlp1936)
                        (= test-struct-a #&(tlp1925 tlp1926 tlp1927))
                        (= test-struct-b (tlp1928 tlp1929 tlp1930 tlp1931))
                        (= test-struct-c #&(tlp1932 tlp1933 tlp1934))
                        (= test-struct-d _))
                   (and (? test-struct? tlp1949)
                        (= test-struct-a '(+ 4 5))
                        (=
                         test-struct-b
                         (or ($ test-struct tlp1937 tlp1938 tlp1939 tlp1940)
                             ($ test-struct tlp1937 tlp1938 tlp1939 tlp1940)))
                        (= test-struct-c (tlp1942 __3))
                        (=
                         test-struct-d
                         ($ test-struct tlp1943 tlp1944 tlp1945 tlp1946))))
                 #t
                 _
                 tlp1950)
                (list
                 tlp1914
                 tlp1915
                 tlp1916
                 tlp1917
                 tlp1918
                 tlp1921
                 tlp1922
                 tlp1923
                 tlp1924
                 tlp1925
                 tlp1926
                 tlp1927
                 tlp1928
                 tlp1929
                 tlp1930
                 tlp1931
                 tlp1932
                 tlp1933
                 tlp1934
                 tlp1937
                 tlp1938
                 tlp1939
                 tlp1940
                 tlp1942
                 tlp1943
                 tlp1944
                 tlp1945
                 tlp1946
                 tlp1950)))))
       (list
        (mytest (tlp1951 `((#\b 5 these) () symbols these)) '((#\b 5 these)))
        (mytest
         (tlp1951
           `((#t () #\d ((+ 1 2) 0 1))
             #(#\c (+ 1 2) these)
             (#\b (+ 1 2) (+ 4 5) . #\a)
             #\d))
         '(#\d ((+ 1 2) 0 1) (#\c (+ 1 2) these) (+ 4 5) #\d))
        (mytest
         (tlp1951
           `(#(,inst-struct-name1919
               (#\b ((+ 4 (+ 1 1)) #\d are #t) #f . #\d)
               ,inst-struct-name1935
               ,inst-struct-name1948)
             #t
             these
             #f))
         '((+ 1 2)
           #\b
           (+ 1 2)
           #\a
           #t
           (+ 4 (+ 1 1))
           #\d
           are
           #t
           2
           #\c
           #\c
           #f
           0
           6
           #f
           1
           #\a
           symbols
           #\b
           (+ 4 5)
           #f
           are
           (1 #\b (+ 4 5))
           (+ 4 (+ 1 1))
           #\d
           5
           #\d
           #f))))
     (let ((tlp2015
             (match-lambda
               (((tlp1952
                   (or ($ test-struct 2 tlp1953 tlp1954 ())
                       ($ test-struct 2 tlp1953 tlp1954 ()))
                   ($
                    test-struct
                    #(tlp1956 tlp1957 tlp1958 tlp1959)
                    (and #(tlp1960 ___) #(tlp1961 ___))
                    (or #t #t)
                    ())
                   quote
                   these)
                 (not (not '(+ 4 (+ 1 1))))
                 #(tlp1963 ___)
                 ())
                (list
                 tlp1952
                 tlp1953
                 tlp1954
                 tlp1956
                 tlp1957
                 tlp1958
                 tlp1959
                 tlp1960
                 tlp1963))
               (((tlp1964 ..3)
                 (or (and (? number? tlp1965) (? even? tlp1966))
                     (and (? number? tlp1965) (? even? tlp1966)))
                 (and (? test-struct? tlp1978)
                      (= test-struct-a tlp1967)
                      (= test-struct-b 'symbols)
                      (= test-struct-c tlp1968)
                      (=
                       test-struct-d
                       (#(tlp1969 ...)
                        tlp1970
                        (and (? test-struct? tlp1976)
                             (= test-struct-a tlp1971)
                             (= test-struct-b tlp1972)
                             (= test-struct-c tlp1973)
                             (= test-struct-d tlp1974))
                        .
                        5)))
                 (and () ()))
                (list
                 tlp1964
                 tlp1965
                 tlp1967
                 tlp1968
                 tlp1969
                 tlp1970
                 tlp1971
                 tlp1972
                 tlp1973
                 tlp1974))
               ((()
                 tlp1979
                 ($
                  test-struct
                  (and (? number? tlp1980) (? even? tlp1981))
                  ()
                  (and (and (? test-struct? tlp1997)
                            (=
                             test-struct-a
                             (tlp1982 tlp1983 tlp1984 . tlp1985))
                            (=
                             test-struct-b
                             (tlp1986 tlp1987 tlp1988 . tlp1989))
                            (= test-struct-c tlp1990)
                            (=
                             test-struct-d
                             ($ test-struct tlp1991 tlp1992 tlp1993 tlp1994)))
                       (and (? test-struct? tlp1998)
                            (=
                             test-struct-a
                             (tlp1999 tlp2000 tlp2001 . tlp2002))
                            (=
                             test-struct-b
                             (tlp2003 tlp2004 tlp2005 . tlp2006))
                            (= test-struct-c tlp2007)
                            (=
                             test-struct-d
                             ($ test-struct tlp2008 tlp2009 tlp2010 tlp2011))))
                  (tlp2012 ..3))
                 tlp2014)
                (list
                 tlp1979
                 tlp1980
                 tlp1982
                 tlp1983
                 tlp1984
                 tlp1985
                 tlp1986
                 tlp1987
                 tlp1988
                 tlp1989
                 tlp1990
                 tlp1991
                 tlp1992
                 tlp1993
                 tlp1994
                 tlp2012
                 tlp2014)))))
       (list
        (mytest
         (tlp2015
           `((#f ,inst-struct-name1955 ,inst-struct-name1962 . these)
             (+ 4 (+ 1 1))
             #(#t #\a #\c)
             ()))
         '(#f 5 #\c #t are #\b #f (#f #f (+ 4 5)) (#t #\a #\c)))
        (mytest
         (tlp2015 `((these #t #t) 18 ,inst-struct-name1977 ()))
         '((these #t #t)
           18
           symbols
           (+ 4 (+ 1 1))
           (2 (+ 4 5) #\b)
           are
           (+ 1 2)
           (+ 1 2)
           #\c
           #f))
        (mytest
         (tlp2015 `(() (+ 1 2) ,inst-struct-name2013 (+ 4 (+ 1 1))))
         '((+ 1 2)
           18
           #\c
           (+ 4 (+ 1 1))
           2
           #\a
           #\b
           #\c
           (+ 4 (+ 1 1))
           are
           #\b
           #\a
           (+ 4 5)
           8
           8
           (are #\c symbols)
           (+ 4 (+ 1 1))))))
     (let ((tlp2066
             (match-lambda
               (((not (not #t)) tlp2016 () tlp2017) (list tlp2016 tlp2017))
               ((((tlp2018 (not (not '(+ 1 2))) 'are tlp2019)
                  ((tlp2020 tlp2021 tlp2022 tlp2023)
                   (and (tlp2024 tlp2025 tlp2026 . tlp2027)
                        (tlp2028 tlp2029 tlp2030 . tlp2031))
                   (tlp2032 __3)
                   _)
                  (and (? number? tlp2033) (? even? tlp2034))
                  .
                  #\b)
                 _
                 (tlp2035 __3)
                 #f)
                (list
                 tlp2018
                 tlp2019
                 tlp2020
                 tlp2021
                 tlp2022
                 tlp2023
                 tlp2024
                 tlp2025
                 tlp2026
                 tlp2027
                 tlp2032
                 tlp2033
                 tlp2035))
               ((((tlp2036 ...)
                  (and (and (? test-struct? tlp2042)
                            (= test-struct-a tlp2037)
                            (= test-struct-b 7)
                            (= test-struct-c #&(tlp2038 tlp2039 tlp2040))
                            (= test-struct-d (not (not #t))))
                       (and (? test-struct? tlp2043)
                            (= test-struct-a tlp2044)
                            (= test-struct-b 7)
                            (= test-struct-c #&(tlp2045 tlp2046 tlp2047))
                            (= test-struct-d (not (not #t)))))
                  (and #((and tlp2048 tlp2049)
                         (tlp2050 tlp2051 tlp2052 tlp2053)
                         (and tlp2054 tlp2055)
                         tlp2056)
                       #((and tlp2057 tlp2058)
                         (tlp2059 tlp2060 tlp2061 tlp2062)
                         (and tlp2063 tlp2064)
                         tlp2065))
                  #\c)
                 'symbols
                 ()
                 (not (not '(+ 4 5))))
                (list
                 tlp2036
                 tlp2037
                 tlp2038
                 tlp2039
                 tlp2040
                 tlp2048
                 tlp2050
                 tlp2051
                 tlp2052
                 tlp2053
                 tlp2054
                 tlp2056)))))
       (list
        (mytest (tlp2066 `(#t #\c () #\c)) '(#\c #\c))
        (mytest
         (tlp2066
           `((((+ 4 5) (+ 1 2) are these)
              ((#\d 3 #\b #\c)
               ((+ 4 (+ 1 1)) #\c (+ 4 (+ 1 1)) + 4 (+ 1 1))
               (#\a 1 #\d)
               0)
              0
              .
              #\b)
             #\c
             (#\d these #t)
             #f))
         '((+ 4 5)
           these
           #\d
           3
           #\b
           #\c
           (+ 4 (+ 1 1))
           #\c
           (+ 4 (+ 1 1))
           (+ 4 (+ 1 1))
           (#\a 1 #\d)
           0
           (#\d these #t)))
        (mytest
         (tlp2066
           `(((#\a #f #\a)
              ,inst-struct-name2041
              #((+ 4 5) (#\a (+ 1 2) (+ 4 5) #\c) #f 7)
              #\c)
             symbols
             ()
             (+ 4 5)))
         '((#\a #f #\a)
           (+ 1 2)
           #\c
           #\d
           (+ 4 5)
           (+ 4 5)
           #\a
           (+ 1 2)
           (+ 4 5)
           #\c
           #f
           7))))
     (let ((tlp2095
             (match-lambda
               (((and (? number? tlp2067) (? even? tlp2068))
                 _
                 (or 'these 'these)
                 8)
                (list tlp2067))
               (((tlp2069 ..3)
                 (tlp2070 ..3)
                 #&((not (not 'are))
                    (and (? number? tlp2071) (? even? tlp2072))
                    tlp2073)
                 (#(() tlp2074 tlp2075 (tlp2076 tlp2077 tlp2078 quote symbols))
                  tlp2079
                  (and (? test-struct? tlp2086)
                       (= test-struct-a #(tlp2080 tlp2081 tlp2082 tlp2083))
                       (= test-struct-b _)
                       (= test-struct-c (not (not #\c)))
                       (= test-struct-d (tlp2084 __3)))
                  quote
                  are))
                (list
                 tlp2069
                 tlp2070
                 tlp2071
                 tlp2073
                 tlp2074
                 tlp2075
                 tlp2076
                 tlp2077
                 tlp2078
                 tlp2079
                 tlp2080
                 tlp2081
                 tlp2082
                 tlp2083
                 tlp2084))
               ((#\b
                 _
                 ($
                  test-struct
                  (tlp2087 ...)
                  (and (? number? tlp2088) (? even? tlp2089))
                  (or tlp2090 tlp2090)
                  #((not (not '(+ 1 2))) tlp2091 () tlp2092))
                 tlp2094)
                (list tlp2087 tlp2088 tlp2090 tlp2091 tlp2092 tlp2094)))))
       (list
        (mytest (tlp2095 `(18 1 these 8)) '(18))
        (mytest
         (tlp2095
           `((#f #\a #\c)
             (#f #f are)
             #&(are 10 #f)
             (#(() #\b are (#t symbols 0 . symbols))
              6
              ,inst-struct-name2085
              .
              are)))
         '((#f #\a #\c)
           (#f #f are)
           10
           #f
           #\b
           are
           #t
           symbols
           0
           6
           #t
           (+ 4 5)
           (+ 4 5)
           #t
           (these these 7)))
        (mytest
         (tlp2095 `(#\b these ,inst-struct-name2093 #\a))
         '((#\b 9 #\b) 16 #\a 9 2 #\a))))
     (let ((tlp2164
             (match-lambda
               (((and (? number? tlp2096) (? even? tlp2097))
                 (and (? test-struct? tlp2105)
                      (= test-struct-a #\b)
                      (=
                       test-struct-b
                       (or (and (tlp2098 ___) (tlp2099 ___))
                           (and (tlp2098 ___) (tlp2099 ___))))
                      (=
                       test-struct-c
                       (and (? number? tlp2100) (? even? tlp2101)))
                      (=
                       test-struct-d
                       (and (? number? tlp2102) (? even? tlp2103))))
                 #t
                 #\a)
                (list tlp2096 tlp2098 tlp2100 tlp2102))
               (((and (? test-struct? tlp2128)
                      (= test-struct-a _)
                      (= test-struct-b ())
                      (=
                       test-struct-c
                       ($
                        test-struct
                        ($ test-struct tlp2106 tlp2107 tlp2108 tlp2109)
                        #(tlp2111 ...)
                        tlp2112
                        #(tlp2113 tlp2114 tlp2115 tlp2116)))
                      (=
                       test-struct-d
                       #(#&(tlp2118 tlp2119 tlp2120)
                         ($ test-struct tlp2121 tlp2122 tlp2123 tlp2124)
                         tlp2126
                         ())))
                 (and (? test-struct? tlp2139)
                      (= test-struct-a _)
                      (=
                       test-struct-b
                       (and (? test-struct? tlp2134)
                            (= test-struct-a _)
                            (= test-struct-b #(tlp2129 ___))
                            (=
                             test-struct-c
                             (tlp2130 tlp2131 tlp2132 quote symbols))
                            (= test-struct-d 'these)))
                      (=
                       test-struct-c
                       (and (? number? tlp2135) (? even? tlp2136)))
                      (= test-struct-d tlp2137))
                 #(tlp2140 ...)
                 tlp2141)
                (list
                 tlp2106
                 tlp2107
                 tlp2108
                 tlp2109
                 tlp2111
                 tlp2112
                 tlp2113
                 tlp2114
                 tlp2115
                 tlp2116
                 tlp2118
                 tlp2119
                 tlp2120
                 tlp2121
                 tlp2122
                 tlp2123
                 tlp2124
                 tlp2126
                 tlp2129
                 tlp2130
                 tlp2131
                 tlp2132
                 tlp2135
                 tlp2137
                 tlp2140
                 tlp2141))
               ((($
                  test-struct
                  (($ test-struct tlp2142 tlp2143 tlp2144 tlp2145)
                   (not (not #f))
                   tlp2147
                   .
                   #t)
                  _
                  tlp2148
                  ((and (? test-struct? tlp2154)
                        (= test-struct-a tlp2149)
                        (= test-struct-b tlp2150)
                        (= test-struct-c tlp2151)
                        (= test-struct-d tlp2152))
                   (and tlp2155 tlp2156)
                   #&(tlp2157 tlp2158 tlp2159)
                   .
                   #\b))
                 (tlp2161 __3)
                 'these
                 (and (? number? tlp2162) (? even? tlp2163)))
                (list
                 tlp2142
                 tlp2143
                 tlp2144
                 tlp2145
                 tlp2147
                 tlp2148
                 tlp2149
                 tlp2150
                 tlp2151
                 tlp2152
                 tlp2155
                 tlp2157
                 tlp2158
                 tlp2159
                 tlp2161
                 tlp2162)))))
       (list
        (mytest
         (tlp2164 `(14 ,inst-struct-name2104 #t #\a))
         '(14 (symbols 1 (+ 4 5)) 12 0))
        (mytest
         (tlp2164
           `(,inst-struct-name2127
             ,inst-struct-name2138
             #(#\a #\a (+ 4 5))
             #\d))
         '(#\a
           #\a
           these
           symbols
           (#t 4 7)
           (+ 1 2)
           2
           symbols
           (+ 4 (+ 1 1))
           5
           are
           6
           #\c
           #\c
           (+ 4 (+ 1 1))
           #\c
           2
           9
           (#\b #\a #\d)
           (+ 4 5)
           #\d
           #\b
           10
           #\d
           (#\a #\a (+ 4 5))
           #\d))
        (mytest
         (tlp2164 `(,inst-struct-name2160 (#\a 0 are) these 0))
         '(9
           #t
           #t
           (+ 4 5)
           3
           #\c
           #\c
           symbols
           (+ 4 (+ 1 1))
           #\a
           #f
           7
           0
           2
           (#\a 0 are)
           0))))))

  (define (mult-match-lambda*-tests)
    (list
     (let ((tlp2271
             (match-lambda*
               (((not (not #\b)) '(+ 4 (+ 1 1)) #(tlp2260 ...) _)
                (list tlp2260))
               ((()
                 (and (? test-struct? tlp2264)
                      (= test-struct-a (not (not #\a)))
                      (= test-struct-b #(tlp2261 ...))
                      (= test-struct-c (or () ()))
                      (=
                       test-struct-d
                       ('(+ 1 2) (tlp2262 ...) (not (not #\b)) quote these)))
                 (or (tlp2265 __3) (tlp2265 __3))
                 ())
                (list tlp2261 tlp2262 tlp2265))
               (((tlp2266 ___)
                 ($ test-struct (tlp2267 __3) (not (not 'are)) tlp2268 ())
                 (tlp2270 __3)
                 (and #f #f))
                (list tlp2266 tlp2267 tlp2268 tlp2270)))))
       (list
        (mytest
         (tlp2271 `#\b `(+ 4 (+ 1 1)) `#((+ 1 2) #f symbols) `#\d)
         '(((+ 1 2) #f symbols)))
        (mytest
         (tlp2271 `() `,inst-struct-name2263 `(symbols 1 symbols) `())
         '((#\d symbols (+ 4 5)) (are #\b #\d) (symbols 1 symbols)))
        (mytest
         (tlp2271
           `(#t #\b (+ 4 5))
           `,inst-struct-name2269
           `((+ 4 (+ 1 1)) #f these)
           `#f)
         '((#t #\b (+ 4 5)) (symbols #\a #\a) #f ((+ 4 (+ 1 1)) #f these)))))
     (let ((tlp2315
             (match-lambda*
               ((tlp2273
                  #&(($
                      test-struct
                      #(tlp2274 ___)
                      #(tlp2275 ___)
                      #(tlp2276 ...)
                      (and (tlp2277 ___) (tlp2278 ___)))
                     (and (? number? tlp2280) (? even? tlp2281))
                     ((and tlp2282 tlp2283) tlp2284 _ (tlp2285 ___)))
                  _
                  (and (? number? tlp2286) (? even? tlp2287)))
                (list
                 tlp2273
                 tlp2274
                 tlp2275
                 tlp2276
                 tlp2277
                 tlp2280
                 tlp2282
                 tlp2284
                 tlp2285
                 tlp2286))
               ((tlp2288
                  (and (('(+ 4 5)
                         (tlp2289 tlp2290 tlp2291 tlp2292)
                         (not (not #\a))
                         .
                         #\b)
                        (()
                         #\a
                         (and (? number? tlp2293) (? even? tlp2294))
                         quote
                         these)
                        (tlp2295 ___)
                        .
                        6)
                       (('(+ 4 5)
                         (tlp2296 tlp2297 tlp2298 tlp2299)
                         (not (not #\a))
                         .
                         #\b)
                        (()
                         #\a
                         (and (? number? tlp2300) (? even? tlp2301))
                         quote
                         these)
                        (tlp2302 ___)
                        .
                        6))
                  #&(()
                     _
                     (and (? test-struct? tlp2306)
                          (= test-struct-a tlp2303)
                          (= test-struct-b ())
                          (= test-struct-c #t)
                          (= test-struct-d (or (tlp2304 ..3) (tlp2304 ..3)))))
                  ($
                   test-struct
                   tlp2307
                   ()
                   (not (not 'symbols))
                   (or (and (? number? tlp2308) (? even? tlp2309))
                       (and (? number? tlp2308) (? even? tlp2309)))))
                (list
                 tlp2288
                 tlp2289
                 tlp2290
                 tlp2291
                 tlp2292
                 tlp2293
                 tlp2295
                 tlp2303
                 tlp2304
                 tlp2307
                 tlp2308))
               ((tlp2311 (or tlp2312 tlp2312) #(tlp2313 ___) tlp2314)
                (list tlp2311 tlp2312 tlp2313 tlp2314)))))
       (list
        (mytest
         (tlp2315
           `1
           `#&(,inst-struct-name2279 4 (#f #f #\d (#\b #\c #t)))
           `(+ 1 2)
           `12)
         '(1
           (#\a #\b symbols)
           ((+ 4 (+ 1 1)) #\d 2)
           (#f #\b (+ 1 2))
           (these #\b are)
           4
           #f
           #f
           (#\b #\c #t)
           12))
        (mytest
         (tlp2315
           `#f
           `(((+ 4 5) (#f symbols (+ 4 (+ 1 1)) #\a) #\a . #\b)
             (() #\a 0 . these)
             (symbols symbols #\c)
             .
             6)
           `#&(() #\d ,inst-struct-name2305)
           `,inst-struct-name2310)
         '(#f
           #f
           symbols
           (+ 4 (+ 1 1))
           #\a
           0
           (symbols symbols #\c)
           #\c
           (#\c 0 9)
           #\b
           18))
        (mytest
         (tlp2315 `(+ 4 (+ 1 1)) `(+ 1 2) `#(6 4 #t) `#\d)
         '((+ 4 (+ 1 1)) (+ 1 2) (6 4 #t) #\d))))
     (let ((tlp2414
             (match-lambda*
               (((not (not #\b))
                 _
                 ((tlp2317 ...)
                  (tlp2318 ..3)
                  #(tlp2319
                    _
                    #&(tlp2320 tlp2321 tlp2322)
                    ($ test-struct tlp2323 tlp2324 tlp2325 tlp2326))
                  '(+ 4 5))
                 (tlp2328 __3))
                (list
                 tlp2317
                 tlp2318
                 tlp2319
                 tlp2320
                 tlp2321
                 tlp2322
                 tlp2323
                 tlp2324
                 tlp2325
                 tlp2326
                 tlp2328))
               ((tlp2329
                  (tlp2330 ...)
                  (($
                    test-struct
                    _
                    ($ test-struct tlp2331 tlp2332 tlp2333 tlp2334)
                    _
                    ())
                   tlp2337
                   (and (and #&(tlp2338 tlp2339 tlp2340)
                             #&(tlp2341 tlp2342 tlp2343))
                        (and #&(tlp2344 tlp2345 tlp2346)
                             #&(tlp2347 tlp2348 tlp2349)))
                   (and #\d #\d))
                  ((($ test-struct tlp2350 tlp2351 tlp2352 tlp2353)
                    (or ($ test-struct tlp2355 tlp2356 tlp2357 tlp2358)
                        ($ test-struct tlp2355 tlp2356 tlp2357 tlp2358))
                    (tlp2360 tlp2361 tlp2362 . #\c)
                    .
                    #\a)
                   (tlp2363 ___)
                   ()
                   .
                   3))
                (list
                 tlp2329
                 tlp2330
                 tlp2331
                 tlp2332
                 tlp2333
                 tlp2334
                 tlp2337
                 tlp2338
                 tlp2339
                 tlp2340
                 tlp2350
                 tlp2351
                 tlp2352
                 tlp2353
                 tlp2355
                 tlp2356
                 tlp2357
                 tlp2358
                 tlp2360
                 tlp2361
                 tlp2362
                 tlp2363))
               (((and (? test-struct? tlp2396)
                      (=
                       test-struct-a
                       (and (? test-struct? tlp2371)
                            (= test-struct-a tlp2364)
                            (= test-struct-b tlp2365)
                            (= test-struct-c (tlp2366 tlp2367 tlp2368 tlp2369))
                            (= test-struct-d ())))
                      (=
                       test-struct-b
                       ($
                        test-struct
                        (and (? test-struct? tlp2377)
                             (= test-struct-a tlp2372)
                             (= test-struct-b tlp2373)
                             (= test-struct-c tlp2374)
                             (= test-struct-d tlp2375))
                        (tlp2378 ..3)
                        (and (? number? tlp2379) (? even? tlp2380))
                        #(tlp2381 ...)))
                      (=
                       test-struct-c
                       #&((tlp2383 ..3)
                          (and (? test-struct? tlp2389)
                               (= test-struct-a tlp2384)
                               (= test-struct-b tlp2385)
                               (= test-struct-c tlp2386)
                               (= test-struct-d tlp2387))
                          tlp2390))
                      (=
                       test-struct-d
                       (and (and (? number? tlp2391) (? even? tlp2392))
                            (and (? number? tlp2393) (? even? tlp2394)))))
                 ('(+ 1 2)
                  #(tlp2397 ___)
                  ($
                   test-struct
                   #(tlp2398 tlp2399 tlp2400 tlp2401)
                   (and (? number? tlp2402) (? even? tlp2403))
                   tlp2404
                   (tlp2405 tlp2406 tlp2407 quote symbols))
                  _)
                 ($
                  test-struct
                  (tlp2409 ..3)
                  (not (not #\a))
                  tlp2410
                  (and (? number? tlp2411) (? even? tlp2412)))
                 ())
                (list
                 tlp2364
                 tlp2365
                 tlp2366
                 tlp2367
                 tlp2368
                 tlp2369
                 tlp2372
                 tlp2373
                 tlp2374
                 tlp2375
                 tlp2378
                 tlp2379
                 tlp2381
                 tlp2383
                 tlp2384
                 tlp2385
                 tlp2386
                 tlp2387
                 tlp2390
                 tlp2391
                 tlp2397
                 tlp2398
                 tlp2399
                 tlp2400
                 tlp2401
                 tlp2402
                 tlp2404
                 tlp2405
                 tlp2406
                 tlp2407
                 tlp2409
                 tlp2410
                 tlp2411)))))
       (list
        (mytest
         (tlp2414
           `#\b
           `#\c
           `((#f (+ 4 (+ 1 1)) #\a)
             (#\c are (+ 4 (+ 1 1)))
             #(are #\c #&(#t 5 (+ 4 (+ 1 1))) ,inst-struct-name2327)
             (+ 4 5))
           `((+ 4 5) #\b #\a))
         '((#f (+ 4 (+ 1 1)) #\a)
           (#\c are (+ 4 (+ 1 1)))
           are
           #t
           5
           (+ 4 (+ 1 1))
           #\d
           #f
           these
           3
           ((+ 4 5) #\b #\a)))
        (mytest
         (tlp2414
           `#t
           `(#\b #\c (+ 4 5))
           `(,inst-struct-name2336 #\b #&(#\a symbols #\d) #\d)
           `((,inst-struct-name2354
              ,inst-struct-name2359
              ((+ 4 (+ 1 1)) #\a (+ 1 2) . #\c)
              .
              #\a)
             (#t 7 6)
             ()
             .
             3))
         '(#t
           (#\b #\c (+ 4 5))
           #f
           #\c
           #\d
           2
           #\b
           #\a
           symbols
           #\d
           (+ 1 2)
           #\b
           0
           #\d
           #\d
           #f
           (+ 4 (+ 1 1))
           #f
           (+ 4 (+ 1 1))
           #\a
           (+ 1 2)
           (#t 7 6)))
        (mytest
         (tlp2414
           `,inst-struct-name2395
           `((+ 1 2) #(#\a 7 #f) ,inst-struct-name2408 (+ 1 2))
           `,inst-struct-name2413
           `())
         '(#\d
           #\c
           these
           6
           #\a
           (+ 1 2)
           (+ 1 2)
           1
           3
           #\a
           (#\b (+ 4 5) (+ 1 2))
           18
           (8 #\a (+ 4 5))
           (#\c #\c (+ 1 2))
           #f
           4
           3
           #t
           (+ 4 5)
           14
           (#\a 7 #f)
           #\d
           #f
           these
           symbols
           10
           these
           (+ 4 (+ 1 1))
           symbols
           (+ 4 5)
           (0 #\b #\b)
           symbols
           2))))
     (let ((tlp2448
             (match-lambda*
               ((tlp2416
                  (not (not #t))
                  (or ((and (tlp2417 ...) (tlp2418 ...))
                       #((tlp2419 tlp2420 tlp2421 . tlp2422)
                         #\c
                         tlp2423
                         (tlp2424 tlp2425 tlp2426 . tlp2427))
                       9
                       quote
                       are)
                      ((and (tlp2417 ...) (tlp2418 ...))
                       #((tlp2419 tlp2420 tlp2421 . tlp2422)
                         #\c
                         tlp2423
                         (tlp2424 tlp2425 tlp2426 . tlp2427))
                       9
                       quote
                       are))
                  (tlp2428
                    (and (? number? tlp2429) (? even? tlp2430))
                    (not (not '(+ 4 5)))
                    (or ($
                         test-struct
                         #&(tlp2431 tlp2432 tlp2433)
                         #f
                         #(tlp2434 tlp2435 tlp2436 tlp2437)
                         _)
                        ($
                         test-struct
                         #&(tlp2431 tlp2432 tlp2433)
                         #f
                         #(tlp2434 tlp2435 tlp2436 tlp2437)
                         _))))
                (list
                 tlp2416
                 tlp2417
                 tlp2419
                 tlp2420
                 tlp2421
                 tlp2422
                 tlp2423
                 tlp2424
                 tlp2425
                 tlp2426
                 tlp2427
                 tlp2417
                 tlp2419
                 tlp2420
                 tlp2421
                 tlp2422
                 tlp2423
                 tlp2424
                 tlp2425
                 tlp2426
                 tlp2427
                 tlp2428
                 tlp2429
                 tlp2431
                 tlp2432
                 tlp2433
                 tlp2434
                 tlp2435
                 tlp2436
                 tlp2437))
               ((#(() (not (not #\d)) #&((not (not 9)) () 'symbols) ())
                 _
                 tlp2439
                 (tlp2440 _ tlp2441 . #t))
                (list tlp2439 tlp2440 tlp2441))
               (((or (not (not #\b)) (not (not #\b)))
                 (tlp2442 () () (and #(tlp2443 ...) #(tlp2444 ...)))
                 (tlp2445 ___)
                 (or (and (? number? tlp2446) (? even? tlp2447))
                     (and (? number? tlp2446) (? even? tlp2447))))
                (list tlp2442 tlp2443 tlp2445 tlp2446)))))
       (list
        (mytest
         (tlp2448
           `3
           `#t
           `((#\b #f are)
             #((#\d these symbols . #f) #\c #\c (these 4 #t . are))
             9
             .
             are)
           `(#\b 8 (+ 4 5) ,inst-struct-name2438))
         '(3
           (#\b #f are)
           #\d
           these
           symbols
           #f
           #\c
           these
           4
           #t
           are
           (#\b #f are)
           #\d
           these
           symbols
           #f
           #\c
           these
           4
           #t
           are
           #\b
           8
           (+ 4 (+ 1 1))
           (+ 1 2)
           (+ 4 5)
           (+ 4 (+ 1 1))
           9
           #t
           #\c))
        (mytest
         (tlp2448
           `#(() #\d #&(9 () symbols) ())
           `#\a
           `#\c
           `(#\c #\a (+ 4 (+ 1 1)) . #t))
         '(#\c #\c (+ 4 (+ 1 1))))
        (mytest
         (tlp2448
           `#\b
           `((+ 4 (+ 1 1)) () () #(#\a these these))
           `((+ 4 5) these (+ 4 5))
           `18)
         '((+ 4 (+ 1 1)) (#\a these these) ((+ 4 5) these (+ 4 5)) 18))))
     (let ((tlp2485
             (match-lambda*
               (((not (not #t))
                 tlp2450
                 (or #&(($
                         test-struct
                         #(tlp2451 ...)
                         #\d
                         (or tlp2452 tlp2452)
                         #(tlp2453 ___))
                        (tlp2455 ..3)
                        tlp2456)
                     #&(($
                         test-struct
                         #(tlp2451 ...)
                         #\d
                         (or tlp2452 tlp2452)
                         #(tlp2453 ___))
                        (tlp2455 ..3)
                        tlp2456))
                 (or (tlp2457 ...) (tlp2457 ...)))
                (list tlp2450 tlp2451 tlp2452 tlp2453 tlp2455 tlp2456 tlp2457))
               (((#&((or _ _) () (tlp2458 ..3)) tlp2459 #(tlp2460 ___) . #t)
                 (and (? test-struct? tlp2470)
                      (= test-struct-a (not (not #\b)))
                      (=
                       test-struct-b
                       ($
                        test-struct
                        ()
                        (not (not #\b))
                        'symbols
                        (or #(tlp2461 tlp2462 tlp2463 tlp2464)
                            #(tlp2461 tlp2462 tlp2463 tlp2464))))
                      (= test-struct-c #\d)
                      (=
                       test-struct-d
                       #&((and tlp2466 tlp2467) #(tlp2468 ___) _)))
                 #(tlp2471 ___)
                 (not (not 9)))
                (list
                 tlp2458
                 tlp2459
                 tlp2460
                 tlp2461
                 tlp2462
                 tlp2463
                 tlp2464
                 tlp2466
                 tlp2468
                 tlp2471))
               ((#((and (? number? tlp2472) (? even? tlp2473))
                   ()
                   ($
                    test-struct
                    #(tlp2474 tlp2475 tlp2476 tlp2477)
                    (tlp2478 ___)
                    (tlp2479 ..3)
                    '(+ 1 2))
                   (or _ _))
                 (tlp2481 ..3)
                 tlp2482
                 (and (? number? tlp2483) (? even? tlp2484)))
                (list
                 tlp2472
                 tlp2474
                 tlp2475
                 tlp2476
                 tlp2477
                 tlp2478
                 tlp2479
                 tlp2481
                 tlp2482
                 tlp2483)))))
       (list
        (mytest
         (tlp2485
           `#t
           `#\d
           `#&(,inst-struct-name2454 ((+ 1 2) #t #\a) (+ 1 2))
           `(symbols #\b #\c))
         '(#\d
           (#\a symbols 8)
           #f
           (1 #\c 4)
           ((+ 1 2) #t #\a)
           (+ 1 2)
           (symbols #\b #\c)))
        (mytest
         (tlp2485
           `(#&(#t () (3 these #t)) #\c #(are 3 (+ 4 (+ 1 1))) . #t)
           `,inst-struct-name2469
           `#(#f (+ 1 2) #t)
           `9)
         '((3 these #t)
           #\c
           (are 3 (+ 4 (+ 1 1)))
           #t
           (+ 4 5)
           #t
           #\d
           #\b
           (4 are 1)
           (#f (+ 1 2) #t)))
        (mytest
         (tlp2485 `#(10 () ,inst-struct-name2480 #\b) `(4 #\c (+ 4 5)) `#\c `6)
         '(10
           (+ 1 2)
           7
           are
           #\b
           (#\b symbols #\d)
           (#\d symbols symbols)
           (4 #\c (+ 4 5))
           #\c
           6))))))
  ;(define (extra-tests) (list (include "hand-made-match-tests.scm")))
  (define (test-all2)
    (list
   ;  (let-tests)
   ;  (define-tests)
   ;  (sing-match-tests)
     (mult-match-tests)
   ;  (sing-match-lambda-tests)
     (mult-match-lambda-tests)
   ;  (sing-match-lambda*-tests)
     (mult-match-lambda*-tests)
     ;(extra-tests)
     ))

(test-all2)

(report-errs)