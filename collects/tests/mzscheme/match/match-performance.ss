(load-relative "../loadtest.ss")
(SECTION 'MATCH-PERFORMANCE)

(require (lib "pretty.ss"))
(require-for-syntax (lib "pretty.ss"))
(require (lib "include.ss"))
(require (lib "plt-match.ss"))

(include "match-compile-perf.scm")
(include "drom-algol-structs.scm")
(include "misc-structs.scm")

(match-performance-test
 (file
  ;; this is the name of the underlying file that handles the 
  ;; persistance of test data between invocations of this performance test
  ;; Each time a test is run one has the option to only compare current
  ;; data with previous data or one can modify the database file by appending
  ;; hopefullly improved data.
  ;; For most purposes it is advisable not to alter the database.
  (name "match-perf-hist.db")
  ;; onlly one of the following options is allowed
  ;; write-new   ;; uncomment to write an new test database 
                 ;; (this requires the whole-list display option below)
  ;; add-results ;; uncomment to append the results of the current test
                 ;; to the database
  )
 (tag
  ;; only one option is allowed 
  ;; this is the name of the tag placed on data generted by this test
  backtrack
  ;; other options are 
  ;; date ;; to tag new date with a date
  ;; time ;; to tag new date with a time
  )
 (display
  ;; only one option is allowed
  ;; remember negative -> good  and positive -> bad 
  ;; positive-change       ;; displays increases in node counts
  ;; other options are:
  ;; negative-change    ;; displays decreases in node counts
  ;; change             ;; to display any change in node count
  percent-change             ;; to display any change in node count
  ;; rt-percent-change             ;; to display any change in node count
  ;; rt-positive-change ;; same as above but for real compile time
  ;; rt-negative-change
  ;; rt-change
  ;; whole-list         ;; display whole database line with current test appended
  ;; last-two ;; display only the current result with the previous result
  )
 (patterns
  (pattern (list 1 2 3)
           (list 4 5 6))
  (pattern (list 1 2 3 4 5)
           (list 2 3 4 5 6)
           (list 4 5 3 6 2)
           (list 7 8 9 3 4)
           (list 4 8 3 2 3))
  ;; dromedary
  (pattern
   (struct |Minus| ((struct <tuple> ((list (struct |Val| ((struct |Num| (n1)))) (struct |Val| ((struct |Num| (n2)))))))))
   (struct |Minus| ((struct <tuple> ((list (struct |Val| (v1)) m2)))))
   (struct |Minus| ((struct <tuple> ((list m1 m2)))))
   (struct |Times| ((struct <tuple> ((list (struct |Val| ((struct |Num| (n1)))) (struct |Val| ((struct |Num| (n2)))))))))
   (struct |Times| ((struct <tuple> ((list (struct |Val| (v1)) m2)))))
   (struct |Times| ((struct <tuple> ((list m1 m2)))))
   (struct |App| ((struct <tuple> ((list (struct |Val| ((struct |Lam| ((struct <tuple>
                                                                               ((list x t m))))))) (struct |Val| (v)))))))
   (struct |App| ((struct <tuple> ((list (struct |Val| (f)) m2)))))
   (struct |App| ((struct <tuple> ((list m1 m2)))))
   (struct |IfZero| ((struct <tuple> ((list (struct |Val| ((struct |Num| (0)))) m2 m3)))))
   (struct |IfZero| ((struct <tuple> ((list (struct |Val| ((struct |Num| (n)))) m2 m3)))))
   (struct |IfZero| ((struct <tuple> ((list m1 m2 m3)))))
   (struct |Fix| ((struct |Val| ((struct |Lam| ((struct <tuple> ((list x t m)))))))))
   (struct |Fix| (m))
   _)
;; algol test
  (pattern
   (struct a60:block (decls statements))
   (struct a60:branch (test (struct a60:goto (then)) (struct a60:goto (else))))
   (struct a60:goto (label))
   (struct a60:dummy ())
   (struct a60:call (proc args))
   (struct a60:assign (vars val))
   else)
  (pattern
   (? (lambda (x) (and (syntax? x) (number? (syntax-e x)))) n)
   (? (lambda (x) (and (syntax? x) (boolean? (syntax-e x)))) n)
   (? (lambda (x) (and (syntax? x) (string? (syntax-e x)))) n)
   (? identifier? i)
   (? symbol? i)
   (struct a60:subscript (array index))
   (struct a60:binary (t argt op e1 e2))
   (struct a60:unary (t argt op e1))
   (struct a60:variable (var subscripts))
   (struct a60:app (func args))
   (struct a60:if (test then else))
   else)
  (pattern
   (struct a60:subscript (array index))
   (struct a60:binary (type argtype op e1 e2))
   (struct a60:unary (type argtype op e1))
   (struct a60:variable (var subscripts))
   (struct a60:app (func args))
   else)

  ;; tahoe test
  (pattern
   `(fish1 fish1 fish1 fish1 fish1)
   `(,(or `fish1 `fish2) ,(or `fish1 `fish2) ,(or `fish1 `fish2) ,(or `fish1 `fish2) ,(or `fish1 `fish2))
   `(,(or `fish1 `fish3) ,(or `fish1 `fish3) ,(or `fish1 `fish3) ,(or `fish1 `fish3) ,(or `fish1 `fish3))
   `(fish1 fish1 fish1 fish1 ,_)
    `(,(or `fish1 `fish2) ,(or `fish1 `fish2) ,(or `fish1 `fish2) ,(or `fish1 `fish2) ,_)
    `(,(or `fish1 `fish3) ,(or `fish1 `fish3) ,(or `fish1 `fish3) ,(or `fish1 `fish3) ,_)
   `(fish1 fish1 fish1 ,_ ,_)
   `(,(or `fish1 `fish2) ,(or `fish1 `fish2) ,(or `fish1 `fish2) ,_ ,_)
   `(,(or `fish1 `fish3) ,(or `fish1 `fish3) ,(or `fish1 `fish3) ,_ ,_)
   `(fish1 fish1 ,_ ,_ ,_)
  `(,(or `fish1 `fish2) ,(or `fish1 `fish2) ,_ ,_ ,_)
  `(,(or `fish1 `fish3) ,(or `fish1 `fish3) ,_ ,_ ,_)
  `(,(or `fish1 `fish2 `fish3) ,_ ,_ ,_ ,_)
   `(bells bells bells bells bells)
   `(bells bells bells bells ,_)
   `(bells bells bells ,_ ,_)
   `(eggplant eggplant eggplant eggplant eggplant)
   `(eggplant eggplant eggplant eggplant ,_)
   `(eggplant eggplant eggplant ,_ ,_)
   `(cherry cherry cherry cherry cherry)
   `(cherry cherry cherry cherry ,_)
   `(cherry cherry cherry ,_ ,_)
   `(watermelon watermelon watermelon watermelon watermelon)
   `(watermelon watermelon watermelon watermelon ,_)
   `(watermelon watermelon watermelon ,_ ,_)
   `(orange orange orange orange orange)
   `(orange orange orange orange ,_)
   `(orange orange orange ,_ ,_)
   `(lure1 lure1 lure1 lure1 lure1)
   (or `(,_ lure1 lure1 lure1 lure1)
       `(lure1 lure1 lure1 lure1 ,_))
   (or `(,_ ,_ lure3 lure3 lure3)
       `(,_ lure3 lure3 lure3 ,_)
       `(lure3 lure3 lure3 ,_ ,_)
       `(,_ ,_ lure2 lure2 lure2)
       `(,_ lure2 lure2 lure2 ,_)
       `(lure2 lure2 lure2 ,_ ,_)
       `(,_ ,_ lure1 lure1 lure1)
       `(,_ lure1 lure1 lure1 ,_)
       `(lure1 lure1 lure1 ,_ ,_))
   else)

;; misc patterns
  (pattern
   (list-no-order 5 4 2 3 1 c b)
   )

  (pattern
   (list-no-order 5 a 2 b 1 c 7)
   )

  (pattern
   (list-no-order 5 4 2 3 1 g ...)
   )

  (pattern
   (list-no-order a b c 3 ...)
   )

  (pattern
   (list-no-order a b c ...)
   )

  (pattern
   (list-no-order
    (and (? number?) a)
    (and (? number?) b)
    (and (? number?) c)
    (and (? number?) d)
    (and (? symbol?) e)
    f
    ...)
   )

  (pattern
   (list-no-order
    (and (? number?) a)
    (and (? symbol?) b)
    (and (? symbol?) c)
    (and (? symbol?) d)
    (and (? symbol?) e)
    f
    ...)
   )

  (pattern
   (list-no-order)
   )

  (pattern
   (hash-table ('a a) b ('c c))
   )

  (pattern
   (hash-table ('c c) ('a a) ('b b))
   )

  (pattern
   (hash-table (a 3) (b 1) (c 2))
   )

  (pattern
   (hash-table (c 3) ('b b) (a 1))
   )

  (pattern
   (hash-table (c 3) ('b b) (a 1) ...)
   )

  (pattern
   (hash-table (c 3) ('b b) (a 1) ..4)
   )

  (pattern
   (hash-table (c 3) ('b b) (a 1) ..5)
   _
   )

  (pattern
   (hash-table (a1 a2) (a (? number?)) ...)
   )

  (pattern
   (hash-table (a1 a2) (a (and (not "hello") b)) ...)
   )

  (pattern
   (hash-table (k v) ...)
   )

  (pattern
   (hash-table ('e v1) ('b v2) rest ...)
   )

  (pattern
   (hash-table)
   )

  (pattern
   (list (struct shape (a)) 2)
   (list (struct ovoid (a b c)) 2)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   (list (struct shape (a)) 2)
   (list (struct ovoid (a b c)) 5)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   (list (struct shape (a)) 5)
   (list (struct ovoid (a b c)) 5)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   (list (struct shape (a)) 5)
   (list (struct ovoid (a b c)) 5)
   (list (struct circle (a b c d)) 5)
   (list (struct shape (a)) 6)
   )

  (pattern
   (list (struct rectangle (a b c)) 5)
   (list (struct shape (a)) 2)
   (list (struct ovoid (a b c)) 2)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   (list (struct rectangle (a b c)) 5)
   (list (struct shape (a)) 2)
   (list (struct ovoid (a b c)) 5)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   (list (struct rectangle (a b c)) 5)
   (list (struct shape (a)) 5)
   (list (struct ovoid (a b c)) 5)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   (list (struct square (a b c d)) 5)
   (list (struct shape (a)) 2)
   (list (struct ovoid (a b c)) 2)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   (list (struct square (a b c d)) 5)
   (list (struct shape (a)) 2)
   (list (struct ovoid (a b c)) 5)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   (list (struct square (a b c d)) 5)
   (list (struct shape (a)) 5)
   (list (struct ovoid (a b c)) 5)
   (list (struct circle (a b c d)) 5)
   )

  (pattern
   `(a ())
   )

  (pattern
   `(1 2 ,@(list 3 4) . ,b)
   )

  (pattern
   `(,b 2 ,@(list 3 4) . b)
   )

  (pattern
   `(,@`(,x ,y) ,@`(,a ,b))
   )

  (pattern
   `(,a ,b ,c)
   )

  (pattern
   `(c a b ,@(list a b c) r f i)
   )

  (pattern
   `(3 4 #\c a b ,a ,(list b `(c e)))
   `(3 4 #\c a b ,a ,(list b `(c d)))
   )

  (pattern
   `#3(x ,x x)
   )

  (pattern
   `#7(c a b ,@(list derby b c) r f i)
   )

  (pattern
   `#&(c a b ,@(list a b c) r f i)
   )

  (pattern
   `("hi"
     1
     there
     #\c
     #t
     #f
     (a b c)
     (a b . c)
     (a b c ..2)
     #3(a b c)
     #4(a b c ..2)
     #&(a b c)
     ,(list a b c)
     ,(list-rest c1 d e)
     ,(list f g h ...)
     ,(vector i j k)
     ,(box (list l m n o))
     ,@(list 1 2 3 4 p))
   )

  (pattern
   `#18("hi"
        1
        there
        #\c
        #t
        #f
        (a b c)
        (a b . c)
        (a b c ..2)
        #3(a b c)
        #4(a b c ..2)
        #&(a b c)
        ,(list a b c)
        ,(list-rest c1 d e)
        ,(list f g h ...)
        ,(vector i j k)
        ,(box (list l m n o))
        ,@(list 1 2 3 4 p))
   )

  (pattern
   `#&("hi"
       1
       there
       #\c
       #t
       #f
       (a b c)
       (a b . c)
       (a b c ..2)
       #3(a b c)
       #4(a b c ..2)
       #&(a b c)
       ,(list a b c)
       ,(list-rest c1 d e)
       ,(list f g h ...)
       ,(vector i j k)
       ,(box (list l m n o))
       ,@(list 1 2 3 4 p))
   )

  (pattern
   (list
    (list
     (box
      (list
       (and (? number? tlp327) (? even? tlp328))
       (vector tlp329 ...)
       (list tlp330 ..3)))
     (list)
     tlp331
     (or (vector tlp332 ...) (vector tlp332 ...))))
   )

  (pattern
   (list
    (vector tlp333 ...)
    (list-rest
     (and (? test-struct? tlp335)
          (app test-struct-a (and _ _))
          (app test-struct-b (not (not #f)))
          (app test-struct-c '(+ 4 5))
          (app test-struct-d 7))
     (and (? number? tlp336) (? even? tlp337))
     tlp338
     'these)
    (list-rest
     tlp339
     (not (not 'these))
     (struct
      test-struct
      ((list)
       '(+ 4 (+ 1 1))
       (and (box (list tlp340 tlp341 tlp342)) (box (list tlp343 tlp344 tlp345)))
       (vector tlp346 tlp347 tlp348 tlp349)))
     #\a)
    (vector tlp351 ...))
   )

  (pattern
   (list
    (list
     tlp352
     (vector
      (box
       (list
        (or _ _)
        (and (? number? tlp353) (? even? tlp354))
        (struct test-struct (tlp355 tlp356 tlp357 tlp358))))
      (list
       _
       (list tlp360 ..3)
       (and (? number? tlp361) (? even? tlp362))
       (or (list tlp363 ..3) (list tlp363 ..3)))
      (list
       (not (not 'symbols))
       (and (and tlp364 tlp365) (and tlp366 tlp367))
       (struct test-struct (tlp368 tlp369 tlp370 tlp371))
       _)
      tlp373)
     (list-rest (list tlp374 ...) (list tlp375 ...) _ #\c)
     (and (? number? tlp376) (? even? tlp377)))
    (list (list) (box (list (list) tlp378 (not (not #\c)))) #\b tlp379))
   )

  (pattern
   (list
    (list
     (box
      (list
       (list-rest
        (not (not 8))
        (vector tlp380 ...)
        (and (? test-struct? tlp386)
             (app test-struct-a tlp381)
             (app test-struct-b tlp382)
             (app test-struct-c tlp383)
             (app test-struct-d tlp384))
        #\a)
       (box (list #t (vector tlp387 ___) (list tlp388 ..3)))
       (and tlp389 tlp390)))
     (not (not 6))
     (list)
     '(+ 4 (+ 1 1)))
    (list (list tlp391 __3) (not (not 3)) tlp392 (list tlp393 ...)))
   )

  (pattern
   (list
    tlp394
    (struct
     test-struct
     ((and (? number? tlp395) (? even? tlp396))
      (list)
      tlp397
      (vector tlp398 ___)))
    _
    (vector tlp400 ...))
   )

  (pattern
   (list tlp401 tlp402 (list tlp403 ..3) (vector tlp404 ...))
   )

  (pattern
   (list
    (struct test-struct (tlp406 '(+ 4 (+ 1 1)) (list tlp407 __3) _))
    _
    (struct
     test-struct
     ((list-rest
       (vector tlp409 tlp410 tlp411 tlp412)
       (and (? test-struct? tlp418)
            (app test-struct-a tlp413)
            (app test-struct-b tlp414)
            (app test-struct-c tlp415)
            (app test-struct-d tlp416))
       (list-rest tlp419 tlp420 tlp421 'these)
       #f)
      (list tlp422 ...)
      (list)
      (list tlp423 ...)))
    (and (? test-struct? tlp434)
         (app test-struct-a (list))
         (app test-struct-b tlp425)
         (app
          test-struct-c
          (and (? test-struct? tlp432)
               (app test-struct-a 'these)
               (app test-struct-b (and (not (not 3)) (not (not 3))))
               (app test-struct-c (vector tlp426 tlp427 tlp428 tlp429))
               (app test-struct-d (list tlp430 ___))))
         (app test-struct-d 'are)))
   )

  (pattern
   (list (list tlp437 ..3) _ (and (list tlp438 ..3) (list tlp439 ..3)) tlp440)
   )

  (pattern
   (list
    (and (list tlp443 ___) (list tlp444 ___))
    (box
     (list
      (box (list #f tlp445 tlp446))
      (vector
       (list tlp447 __3)
       (list)
       5
       (or (vector tlp448 ___) (vector tlp448 ___)))
      (box
       (list
        (or (not (not '(+ 4 (+ 1 1)))) (not (not '(+ 4 (+ 1 1)))))
        (list)
        (list-rest tlp449 tlp450 tlp451 #f)))))
    (vector
     #\d
     _
     (box
      (list
       (vector tlp452 ___)
       (and (list) (list))
       (and (vector tlp453 ___) (vector tlp454 ___))))
     (vector
      (list tlp455 ...)
      (and (? number? tlp456) (? even? tlp457))
      (vector tlp458 tlp459 tlp460 tlp461)
      #\b))
    (list))
   (list
    (list tlp462 ..3)
    tlp463
    (list-rest
     (list-rest
      #\d
      (list tlp464 tlp465 tlp466 tlp467)
      (list-rest tlp468 tlp469 tlp470 'these)
      #\b)
     'symbols
     (and (? test-struct? tlp478)
          (app test-struct-a tlp471)
          (app test-struct-b (list))
          (app test-struct-c tlp472)
          (app test-struct-d (vector tlp473 tlp474 tlp475 tlp476)))
     7)
    tlp479)
   )

  (pattern
   (list
    (list-rest tlp482 tlp483 (list) #\a)
    (struct
     test-struct
     ((list-rest (list tlp484 ___) tlp485 (not (not #t)) 0)
      (list)
      tlp486
      (not (not 7))))
    (and (? number? tlp488) (? even? tlp489))
    #\a)
   (list
    (list tlp490 ..3)
    (box
     (list
      (and (? number? tlp491) (? even? tlp492))
      (and (vector
            (vector tlp493 ...)
            tlp494
            (vector tlp495 ...)
            (box (list tlp496 tlp497 tlp498)))
           (vector
            (vector tlp499 ...)
            tlp500
            (vector tlp501 ...)
            (box (list tlp502 tlp503 tlp504))))
      (and (? number? tlp505) (? even? tlp506))))
    tlp507
    #\c)
   )

  (pattern
   (list
    (box
     (list
      #\d
      (box (list tlp509 'are _))
      (struct
       test-struct
       ((struct test-struct (tlp510 tlp511 tlp512 tlp513))
        (list-rest tlp515 tlp516 tlp517 #\a)
        (list-rest tlp518 tlp519 tlp520 #\b)
        tlp521))))
    (vector tlp523 ___)
    (and (? test-struct? tlp548)
         (app test-struct-a 'are)
         (app
          test-struct-b
          (list
           tlp524
           (list tlp525 ...)
           (and (struct test-struct (tlp526 tlp527 tlp528 tlp529))
                (struct test-struct (tlp531 tlp532 tlp533 tlp534)))
           tlp535))
         (app test-struct-c (list tlp536 ...))
         (app
          test-struct-d
          (and (? test-struct? tlp546)
               (app
                test-struct-a
                (and (? test-struct? tlp542)
                     (app test-struct-a tlp537)
                     (app test-struct-b tlp538)
                     (app test-struct-c tlp539)
                     (app test-struct-d tlp540)))
               (app test-struct-b (and (? number? tlp543) (? even? tlp544)))
               (app test-struct-c (list))
               (app test-struct-d (list)))))
    tlp549)
   )

  (pattern
   (list
    tlp551
    (struct test-struct ((vector tlp552 ...) 'are _ tlp553))
    (list)
    (list))
   )

  (pattern
   (list
    (not (not #f))
    (box
     (list
      tlp556
      (and (list tlp557 ___) (list tlp558 ___))
      (and (? number? tlp559) (? even? tlp560))))
    tlp561
    (list))
   (list
    _
    (list tlp562 ..3)
    '(+ 4 (+ 1 1))
    (vector
     (list
      (vector tlp563 tlp564 tlp565 tlp566)
      tlp567
      (box (list tlp568 tlp569 tlp570))
      tlp571)
     (and tlp572 tlp573)
     (box
      (list
       (box (list tlp574 tlp575 tlp576))
       (not (not #f))
       (list-rest tlp577 tlp578 tlp579 #\b)))
     #f))
   )

  (pattern
   (list
    (and (? number? tlp581) (? even? tlp582))
    tlp583
    (box
     (list
      '(+ 4 5)
      (box
       (list
        tlp584
        (and (? number? tlp585) (? even? tlp586))
        (or (or tlp587 tlp587) (or tlp587 tlp587))))
      '(+ 4 5)))
    (vector tlp588 ...))
   (list
    (and (? test-struct? tlp601)
         (app test-struct-a #f)
         (app test-struct-b (list tlp589 ...))
         (app
          test-struct-c
          (list
           (and (? number? tlp590) (? even? tlp591))
           (vector tlp592 ...)
           (and (? test-struct? tlp598)
                (app test-struct-a tlp593)
                (app test-struct-b tlp594)
                (app test-struct-c tlp595)
                (app test-struct-d tlp596))
           (list tlp599 __3)))
         (app test-struct-d '(+ 4 (+ 1 1))))
    #f
    (vector tlp602 ___)
    (list tlp603 ___))
   )

  (pattern
   (list tlp606 tlp607 (list tlp608 ___) tlp609)
   )

  (pattern
   (list
    (list
     '(+ 4 5)
     (vector tlp612 ___)
     (list tlp613 __3)
     (struct
      test-struct
      ((box (list tlp614 tlp615 tlp616)) tlp617 (list) (list))))
    tlp619
    (box (list 7 '(+ 4 (+ 1 1)) (list)))
    _)
   )

  (pattern
   (list
    (box
     (list
      (and (? number? tlp622) (? even? tlp623))
      (list-rest
       (list tlp624 ___)
       (and (? number? tlp625) (? even? tlp626))
       #\c
       #\a)
      tlp627))
    tlp628
    (list tlp629 ___)
    #\c)
   (list
    tlp630
    (or (vector
         _
         (vector tlp631 ...)
         (vector
          (list-rest tlp632 tlp633 tlp634 tlp635)
          (and tlp636 tlp637)
          #\b
          (struct test-struct (tlp638 tlp639 tlp640 tlp641)))
         (list))
        (vector
         _
         (vector tlp631 ...)
         (vector
          (list-rest tlp632 tlp633 tlp634 tlp635)
          (and tlp636 tlp637)
          #\b
          (struct test-struct (tlp638 tlp639 tlp640 tlp641)))
         (list)))
    #f
    (and (? number? tlp643) (? even? tlp644)))
   )

  (pattern
   (list
    (vector
     #t
     (list)
     (list
      tlp647
      (or (box (list tlp648 tlp649 tlp650)) (box (list tlp648 tlp649 tlp650)))
      (list tlp651 ...)
      tlp652)
     #f)
    'these
    tlp653
    tlp654)
   (list
    (list-rest
     (list tlp655 __3)
     (list tlp656 ___)
     (struct
      test-struct
      ((and (? test-struct? tlp662)
            (app test-struct-a tlp657)
            (app test-struct-b tlp658)
            (app test-struct-c tlp659)
            (app test-struct-d tlp660))
       (list-rest tlp663 tlp664 tlp665 #\a)
       (list-rest tlp666 tlp667 tlp668 #\b)
       _))
     'are)
    (not (not 'symbols))
    (and tlp670 tlp671)
    _)
   )

  (pattern
   (list a b c)
   )

  (pattern
   (list (list a b c) (list d e f))
   )

  (pattern
   (list (list a b))
   )

  (pattern
   (list (list _) ...)
   )

  (pattern
   (list _ _ (list (set! set-it) _))
   )

  (pattern
   (list _ _ (list _ (set! set-it)))
   )

  (pattern
   (list (set! set-it) _ (list _ _))
   )

  (pattern
   (list _ (set! set-it) (list _ _))
   )

  (pattern
   (list-rest (set! set-it) _ (list-rest _ _) _)
   )

  (pattern
   (list-rest _ (set! set-it) (list-rest _ _) _)
   )

  (pattern
   (list-rest _ _ (list-rest (set! set-it) _) _)
   )

  (pattern
   (list-rest _ _ (list-rest _ (set! set-it)) _)
   )

  (pattern
   (list-rest _ _ (list-rest _ _) (set! set-it))
   )

  (pattern
   (vector _ (set! set-it))
   )

  (pattern
   (vector (set! set-it) _)
   )

  (pattern
   (box (set! set-it))
   )

  (pattern
   (box (list _ (set! set-it)))
   )

  (pattern
   (box (vector _ (set! set-it)))
   )

  (pattern
   (list _ _ (list (get! get-it) _))
   )

  (pattern
   (list _ _ (list (set! set-it) _))
   )

  (pattern
   (list _ _ (list _ (get! get-it)))
   )

  (pattern
   (list (get! get-it) _ (list _ _))
   )

  (pattern
   (list _ (get! get-it) (list _ _))
   )

  (pattern
   (list-rest (get! get-it) _ (list-rest _ _) _)
   )

  (pattern
   (list-rest _ (get! get-it) (list-rest _ _) _)
   )

  (pattern
   (list-rest _ _ (list-rest (get! get-it) _) _)
   )

  (pattern
   (list-rest _ _ (list-rest _ (get! get-it)) _)
   )

  (pattern
   (list-rest _ _ (list-rest _ _) (get! get-it))
   )

  (pattern
   (vector _ (get! get-it))
   )

  (pattern
   (vector (get! get-it) _)
   )

  (pattern
   (box (get! get-it))
   )

  (pattern
   (box (list _ (get! get-it)))
   )

  (pattern
   (box (vector _ (get! get-it)))
   )

  (pattern
   (vector (vector (vector a ...) ...) ...)
   )

  (pattern
   (list (list (list a ...) ...) ...)
   )

  (pattern
   (list (list (list (list (list (list a ...) ...) ...) ...) ...) ...)
   )

  (pattern
   (vector (vector (vector (vector (vector (vector a ...) ...) ...) ...) ...) ...)
   )

  (pattern
   (list (vector (list (vector (list (vector a ...) ...) ...) ...) ...) ...)
   )

  (pattern
   (list (list (list (list (list (list a ..2) ..2) ..2) ..2) ..2) ..2)
   )

  (pattern
   (vector (vector (vector (vector (vector (vector a ..2) ..2) ..2) ..2) ..2) ..2)
   )

  (pattern
   (list (vector (list (vector (list (vector a ..2) ..2) ..2) ..2) ..2) ..2)
   )

  (pattern
   (list (list (list (list (list (list _ ...) ...) ...) ...) ...) ...)
   _
   )

  (pattern
   (vector (vector (vector (vector (vector (vector _ ...) ...) ...) ...) ...) ...)
   _
   )

  (pattern
   (list (vector (list (vector (list (vector _ ...) ...) ...) ...) ...) ...)
   _
   )

  (pattern
   (list (list (list (list (list (list a b) ...) ...) ...) ...) ...)
   )

  (pattern
   (vector (vector (vector (vector (vector (vector a b) ...) ...) ...) ...) ...)
   )

  (pattern
   (list (vector (list (vector (list (vector a b) ...) ...) ...) ...) ...)
   )

  (pattern
   (list (list 1 ... a ...) ... 7 ...)
   (list (list 1 ... a ...) ... 6 ...)
   (list (list 1 ... a ...) ... 5 ...)
   )

  (pattern
   (list 1 ... 2 2 2 2)
   )

  (pattern
   (list 1 ... 2 ...)
   )

  (pattern
   (list (and (not 2) a) ... 2 ...)
   )

  (pattern
   (list a ... 2 ...)
   )

  (pattern
   (list _ ... 2 ...)
   )

  (pattern
   (list (and (not 'in) a) ... (and (not 'is) b) ... c ...)
   )

  (pattern
   (list (list 1 ... 2 ...) ... 5 ...)
   )

  (pattern
   (vector (and (? odd?) a) ... 8 (and (? even?) b) ... 7 r ...)
   )

  (pattern
   (vector (vector 1 ... 2 ...) ... 5 ...)
   )

  (pattern
   (vector (vector 1 ... a ...) ... 7 ...)
   (vector (vector 1 ... a ...) ... 6 ...)
   (vector (vector 1 ... a ...) ... 5 ...)
   )

  (pattern
   (vector (vector _ _) ..3 a ...)
   )

  (pattern
   (vector a b (and c (not 5)) ... d)
   )

  (pattern
   (list-no-order a b 1 c)
   )

  (pattern
   (list-rest 1 ... a)
   )

  (pattern
   (list-rest (and 1 b) ... a)
   )

  (pattern
   (list-rest (and 1 b) ... a (and c 4))
   )

  (pattern
   (list (var _) b c)
   )

  (pattern
   (list (var ..3) b c)
   )

  (pattern
   (list (var quasiquote) b c)
   )

  (pattern
   (list (var quote) b c)
   )

  (pattern
   (list (var unquote) b c)
   )

  (pattern
   (list (var unquote-splicing) b c)
   )

  (pattern
   (list $ b c)
   )

  (pattern
   (list var b c)
   )

  (pattern
   (list = b c)
   )

  (pattern
   (list and b c)
   )

  (pattern
   (list or b c)
   )

  (pattern
   (list not b c)
   )

  (pattern
   (list ? b c)
   )

  (pattern
   (list set! b c)
   )

  (pattern
   (list get! b c)
   )

  (pattern
   (list
    (list tlp3121 __3)
    (list)
    (list tlp3122 __3)
    (and (? number? tlp3123) (? even? tlp3124)))
   )

  (pattern
   (list
    (and (and (? test-struct? tlp3158)
              (app
               test-struct-a
               (box
                (list
                 (list tlp3125 tlp3126 tlp3127 tlp3128)
                 (list tlp3129 ___)
                 (and (? test-struct? tlp3135)
                      (app test-struct-a tlp3130)
                      (app test-struct-b tlp3131)
                      (app test-struct-c tlp3132)
                      (app test-struct-d tlp3133)))))
              (app
               test-struct-b
               (list-rest
                (list tlp3136 tlp3137 tlp3138 tlp3139)
                (and (? test-struct? tlp3145)
                     (app test-struct-a tlp3140)
                     (app test-struct-b tlp3141)
                     (app test-struct-c tlp3142)
                     (app test-struct-d tlp3143))
                (vector tlp3146 tlp3147 tlp3148 tlp3149)
                #\d))
              (app test-struct-c (list tlp3150 __3))
              (app
               test-struct-d
               (list
                #\b
                tlp3151
                (and (? number? tlp3152) (? even? tlp3153))
                (box (list tlp3154 tlp3155 tlp3156)))))
         (and (? test-struct? tlp3159)
              (app
               test-struct-a
               (box
                (list
                 (list tlp3160 tlp3161 tlp3162 tlp3163)
                 (list tlp3164 ___)
                 (and (? test-struct? tlp3165)
                      (app test-struct-a tlp3166)
                      (app test-struct-b tlp3167)
                      (app test-struct-c tlp3168)
                      (app test-struct-d tlp3169)))))
              (app
               test-struct-b
               (list-rest
                (list tlp3170 tlp3171 tlp3172 tlp3173)
                (and (? test-struct? tlp3174)
                     (app test-struct-a tlp3175)
                     (app test-struct-b tlp3176)
                     (app test-struct-c tlp3177)
                     (app test-struct-d tlp3178))
                (vector tlp3179 tlp3180 tlp3181 tlp3182)
                #\d))
              (app test-struct-c (list tlp3183 __3))
              (app
               test-struct-d
               (list
                #\b
                tlp3184
                (and (? number? tlp3185) (? even? tlp3186))
                (box (list tlp3187 tlp3188 tlp3189))))))
    (not (not #\a))
    (struct
     test-struct
     ((vector tlp3190 ...)
      tlp3191
      (and (? test-struct? tlp3198)
           (app test-struct-a (list tlp3192 __3))
           (app test-struct-b tlp3193)
           (app test-struct-c (and tlp3194 tlp3195))
           (app test-struct-d (list tlp3196 ...)))
      tlp3199))
    (list tlp3201 __3))
   )

  (pattern
   (list
    (list
     (list (and tlp3202 tlp3203) (list tlp3204 ___) tlp3205 tlp3206)
     tlp3207
     tlp3208
     tlp3209))
   )

  (pattern
   (list (and (list) (list)) tlp3210 tlp3211 _)
   )

  (pattern
   (list
    (list
     (and (? number? tlp3212) (? even? tlp3213))
     (and (? number? tlp3214) (? even? tlp3215))
     (and (list tlp3216 ___) (list tlp3217 ___))
     tlp3218))
   )

  (pattern
   (list
    (list (vector tlp3219 ...) (vector tlp3220 ...) (vector tlp3221 ___) (list))
    (list 'are tlp3222 #\a 9)
    (list
     tlp3223
     (list
      (list-rest
       #\b
       (struct test-struct (tlp3224 tlp3225 tlp3226 tlp3227))
       #f
       'these)
      (list)
      (list-rest
       (vector tlp3229 ...)
       (or (and (? test-struct? tlp3235)
                (app test-struct-a tlp3230)
                (app test-struct-b tlp3231)
                (app test-struct-c tlp3232)
                (app test-struct-d tlp3233))
           (and (? test-struct? tlp3235)
                (app test-struct-a tlp3230)
                (app test-struct-b tlp3231)
                (app test-struct-c tlp3232)
                (app test-struct-d tlp3233)))
       (vector tlp3236 ___)
       5)
      (list))
     (list tlp3237 ..3)
     (and (? test-struct? tlp3240)
          (app test-struct-a (or (list) (list)))
          (app test-struct-b (vector tlp3238 ...))
          (app test-struct-c (not (not #\c)))
          (app test-struct-d #\b)))
    (list
     (list tlp3241 ___)
     (list)
     (list
      (and (and (? test-struct? tlp3253)
                (app test-struct-a (list-rest tlp3242 tlp3243 tlp3244 tlp3245))
                (app test-struct-b (vector tlp3246 tlp3247 tlp3248 tlp3249))
                (app test-struct-c (and (? number? tlp3250) (? even? tlp3251)))
                (app test-struct-d _))
           (and (? test-struct? tlp3254)
                (app test-struct-a (list-rest tlp3255 tlp3256 tlp3257 tlp3258))
                (app test-struct-b (vector tlp3259 tlp3260 tlp3261 tlp3262))
                (app test-struct-c (and (? number? tlp3263) (? even? tlp3264)))
                (app test-struct-d _)))
      (vector
       #\a
       (or 'symbols 'symbols)
       (vector tlp3265 tlp3266 tlp3267 tlp3268)
       (box (list tlp3269 tlp3270 tlp3271)))
      (and (? number? tlp3272) (? even? tlp3273))
      (list tlp3274 ..3))
     tlp3275)
    (list
     (list-rest
      tlp3276
      (struct
       test-struct
       (tlp3277
        (and (vector tlp3278 tlp3279 tlp3280 tlp3281)
             (vector tlp3282 tlp3283 tlp3284 tlp3285))
        (and (not (not #t)) (not (not #t)))
        tlp3286))
      (box (list (list tlp3288 ___) (list tlp3289 __3) tlp3290))
      #\b)
     (list
      (or (list) (list))
      (and (? test-struct? tlp3298)
           (app test-struct-a _)
           (app test-struct-b tlp3291)
           (app test-struct-c (vector tlp3292 ...))
           (app test-struct-d (vector tlp3293 tlp3294 tlp3295 tlp3296)))
      tlp3299
      (and (? test-struct? tlp3313)
           (app test-struct-a (box (list tlp3300 tlp3301 tlp3302)))
           (app
            test-struct-b
            (struct test-struct (tlp3303 tlp3304 tlp3305 tlp3306)))
           (app
            test-struct-c
            (or (list tlp3308 tlp3309 tlp3310 tlp3311)
                (list tlp3308 tlp3309 tlp3310 tlp3311)))
           (app test-struct-d _)))
     (box
      (list
       (and (? test-struct? tlp3323)
            (app test-struct-a (list tlp3314 ...))
            (app test-struct-b (list-rest tlp3315 tlp3316 tlp3317 #\a))
            (app test-struct-c (vector tlp3318 ___))
            (app test-struct-d (box (list tlp3319 tlp3320 tlp3321))))
       (box (list (list tlp3324 __3) (vector tlp3325 ___) (list tlp3326 ___)))
       (list (list tlp3327 ...) tlp3328 (list tlp3329 ...) (list tlp3330 __3))))
     (list tlp3331 ___)))
   )

  (pattern
   (list
    #\d
    (list (not (not '(+ 4 5))) tlp3332 (not (not #f)) #t)
    (box
     (list
      tlp3333
      (not (not 7))
      (list-rest
       (vector tlp3334 tlp3335 tlp3336 tlp3337)
       (box (list tlp3338 tlp3339 tlp3340))
       tlp3341
       9)))
    (or (list tlp3342 ___) (list tlp3342 ___)))
   )

  (pattern
   (list
    (list-rest (and (list tlp3343 ...) (list tlp3344 ...)) (list) tlp3345 #\a)
    (list-rest (not (not #t)) _ _ #f)
    #\a
    (vector
     _
     (or (list
          _
          tlp3346
          (list tlp3347 tlp3348 tlp3349 tlp3350)
          (and (? test-struct? tlp3356)
               (app test-struct-a tlp3351)
               (app test-struct-b tlp3352)
               (app test-struct-c tlp3353)
               (app test-struct-d tlp3354)))
         (list
          _
          tlp3346
          (list tlp3347 tlp3348 tlp3349 tlp3350)
          (and (? test-struct? tlp3356)
               (app test-struct-a tlp3351)
               (app test-struct-b tlp3352)
               (app test-struct-c tlp3353)
               (app test-struct-d tlp3354))))
     (box
      (list
       (vector tlp3357 tlp3358 tlp3359 tlp3360)
       #\d
       (and (? number? tlp3361) (? even? tlp3362))))
     (vector
      (struct test-struct (tlp3363 tlp3364 tlp3365 tlp3366))
      (struct test-struct (tlp3368 tlp3369 tlp3370 tlp3371))
      (and (? number? tlp3373) (? even? tlp3374))
      tlp3375)))
   )

  (pattern
   (list (and tlp3376 tlp3377) #\a (list) (vector tlp3378 ...))
   )

  (pattern
   (list
    _
    (vector
     (list-rest
      (list)
      (and (vector tlp3379 tlp3380 tlp3381 tlp3382)
           (vector tlp3383 tlp3384 tlp3385 tlp3386))
      (list tlp3387 ..3)
      3)
     (list
      (list-rest tlp3388 tlp3389 tlp3390 #\a)
      (or (list tlp3391 ..3) (list tlp3391 ..3))
      _
      (struct test-struct (tlp3392 tlp3393 tlp3394 tlp3395)))
     (and (? number? tlp3397) (? even? tlp3398))
     tlp3399)
    tlp3400
    (list))
   )

  (pattern
   (list
    (vector tlp3401 ___)
    '(+ 1 2)
    (vector
     _
     '(+ 1 2)
     (and (box (list (list tlp3402 tlp3403 tlp3404 tlp3405) tlp3406 '(+ 1 2)))
          (box (list (list tlp3407 tlp3408 tlp3409 tlp3410) tlp3411 '(+ 1 2))))
     (list tlp3412 ..3))
    _)
   )

  (pattern
   (list
    (list
     _
     (list tlp3413 ...)
     (list
      (box
       (list
        (vector tlp3414 ___)
        (struct test-struct (tlp3415 tlp3416 tlp3417 tlp3418))
        (and (? number? tlp3420) (? even? tlp3421))))
      (list-rest
       (not (not #\b))
       (list-rest tlp3422 tlp3423 tlp3424 #\a)
       (list tlp3425 tlp3426 tlp3427 tlp3428)
       #\c)
      (list tlp3429 ...)
      tlp3430)
     tlp3431)
    (list tlp3432 tlp3433 (list tlp3434 ...) (list))
    (list
     (list tlp3435 ...)
     (and (? test-struct? tlp3445)
          (app test-struct-a (list tlp3436 ..3))
          (app test-struct-b (list))
          (app
           test-struct-c
           (list-rest
            (list tlp3437 ___)
            tlp3438
            (vector tlp3439 tlp3440 tlp3441 tlp3442)
            5))
          (app test-struct-d (vector tlp3443 ___)))
     (vector
      (and (and (? test-struct? tlp3456)
                (app test-struct-a (list))
                (app test-struct-b (list-rest tlp3446 tlp3447 tlp3448 tlp3449))
                (app test-struct-c (box (list tlp3450 tlp3451 tlp3452)))
                (app test-struct-d (and tlp3453 tlp3454)))
           (and (? test-struct? tlp3457)
                (app test-struct-a (list))
                (app test-struct-b (list-rest tlp3458 tlp3459 tlp3460 tlp3461))
                (app test-struct-c (box (list tlp3462 tlp3463 tlp3464)))
                (app test-struct-d (and tlp3465 tlp3466))))
      tlp3467
      #t
      (box (list (not (not 2)) (box (list tlp3468 tlp3469 tlp3470)) _)))
     (and (? test-struct? tlp3475)
          (app
           test-struct-a
           (list-rest _ (or tlp3471 tlp3471) (not (not #f)) #\d))
          (app test-struct-b tlp3472)
          (app test-struct-c (list))
          (app test-struct-d (list tlp3473 ...))))
    (list
     (struct
      test-struct
      ((and (? number? tlp3476) (? even? tlp3477))
       (list
        (or (and (? test-struct? tlp3483)
                 (app test-struct-a tlp3478)
                 (app test-struct-b tlp3479)
                 (app test-struct-c tlp3480)
                 (app test-struct-d tlp3481))
            (and (? test-struct? tlp3483)
                 (app test-struct-a tlp3478)
                 (app test-struct-b tlp3479)
                 (app test-struct-c tlp3480)
                 (app test-struct-d tlp3481)))
        (vector tlp3484 tlp3485 tlp3486 tlp3487)
        (box (list tlp3488 tlp3489 tlp3490))
        '(+ 4 5))
       (vector tlp3491 ___)
       tlp3492))
     (list tlp3494 ..3)
     (list tlp3495 ..3)
     tlp3496)
    (list
     (list
      (box
       (list
        (list tlp3497 tlp3498 tlp3499 tlp3500)
        (and _ _)
        (and (? test-struct? tlp3506)
             (app test-struct-a tlp3501)
             (app test-struct-b tlp3502)
             (app test-struct-c tlp3503)
             (app test-struct-d tlp3504))))
      (list tlp3507 ...)
      (and (? test-struct? tlp3513)
           (app test-struct-a (box (list tlp3508 tlp3509 tlp3510)))
           (app test-struct-b (list))
           (app test-struct-c _)
           (app test-struct-d (list tlp3511 ___)))
      (or (and (struct test-struct (tlp3514 tlp3515 tlp3516 tlp3517))
               (struct test-struct (tlp3519 tlp3520 tlp3521 tlp3522)))
          (and (struct test-struct (tlp3514 tlp3515 tlp3516 tlp3517))
               (struct test-struct (tlp3519 tlp3520 tlp3521 tlp3522)))))
     (list
      (not (not 9))
      #f
      '(+ 4 5)
      (box
       (list
        (and (? test-struct? tlp3528)
             (app test-struct-a tlp3523)
             (app test-struct-b tlp3524)
             (app test-struct-c tlp3525)
             (app test-struct-d tlp3526))
        (list tlp3529 __3)
        (vector tlp3530 tlp3531 tlp3532 tlp3533))))
     (not (not #\d))
     9))
   )

  (pattern
   (list
    (list
     tlp3534
     tlp3535
     (list-rest tlp3536 _ tlp3537 'symbols)
     (or (or tlp3538 tlp3538) (or tlp3538 tlp3538)))
    (list
     (list tlp3539 __3)
     tlp3540
     (and (? number? tlp3541) (? even? tlp3542))
     (and (? number? tlp3543) (? even? tlp3544)))
    (list
     (or tlp3545 tlp3545)
     (or #f #f)
     (struct test-struct (tlp3546 (list tlp3547 ..3) (list tlp3548 ___) tlp3549))
     (list-rest (vector tlp3551 ...) 7 #f #\b))
    (list
     (box
      (list
       (box
        (list
         (or tlp3552 tlp3552)
         tlp3553
         (and (? number? tlp3554) (? even? tlp3555))))
       tlp3556
       (list tlp3557 ___)))
     (list
      (list
       (list-rest tlp3558 tlp3559 tlp3560 #\b)
       (or (vector tlp3561 ___) (vector tlp3561 ___))
       (list-rest tlp3562 tlp3563 tlp3564 #\d)
       tlp3565)
      (and (? test-struct? tlp3579)
           (app
            test-struct-a
            (struct test-struct (tlp3566 tlp3567 tlp3568 tlp3569)))
           (app
            test-struct-b
            (or (and (? number? tlp3571) (? even? tlp3572))
                (and (? number? tlp3571) (? even? tlp3572))))
           (app
            test-struct-c
            (struct test-struct (tlp3573 tlp3574 tlp3575 tlp3576)))
           (app test-struct-d (not (not 9))))
      (and tlp3580 tlp3581)
      (and (? number? tlp3582) (? even? tlp3583)))
     (vector tlp3584 ___)
     (or tlp3585 tlp3585))
    (list
     (vector tlp3586 ...)
     #f
     (list
      (and (? test-struct? tlp3596)
           (app test-struct-a (list tlp3587 ...))
           (app test-struct-b (list-rest tlp3588 tlp3589 tlp3590 #\c))
           (app test-struct-c (vector tlp3591 tlp3592 tlp3593 tlp3594))
           (app test-struct-d (not (not #\b))))
      (struct
       test-struct
       (#\a (and (? number? tlp3597) (? even? tlp3598)) _ tlp3599))
      (list-rest
       (and (? test-struct? tlp3606)
            (app test-struct-a tlp3601)
            (app test-struct-b tlp3602)
            (app test-struct-c tlp3603)
            (app test-struct-d tlp3604))
       (list tlp3607 __3)
       (and (vector tlp3608 ...) (vector tlp3609 ...))
       6)
      '(+ 1 2))
     #\b))
   )

  (pattern
   (list
    (list _ (not (not #f)) (not (not #f)) (vector tlp3610 ___))
    (list
     (list tlp3611 ...)
     (vector
      (list tlp3612 ...)
      (and tlp3613 tlp3614)
      (list tlp3615 ...)
      (and (? test-struct? tlp3626)
           (app test-struct-a (and (list) (list)))
           (app test-struct-b tlp3616)
           (app test-struct-c (vector tlp3617 tlp3618 tlp3619 tlp3620))
           (app test-struct-d (vector tlp3621 tlp3622 tlp3623 tlp3624))))
     (and (vector
           tlp3627
           (vector tlp3628 ...)
           (and (? test-struct? tlp3640)
                (app test-struct-a _)
                (app
                 test-struct-b
                 (struct test-struct (tlp3629 tlp3630 tlp3631 tlp3632)))
                (app test-struct-c (box (list tlp3634 tlp3635 tlp3636)))
                (app test-struct-d (and tlp3637 tlp3638)))
           tlp3641)
          (vector
           tlp3642
           (vector tlp3643 ...)
           (and (? test-struct? tlp3644)
                (app test-struct-a _)
                (app
                 test-struct-b
                 (struct test-struct (tlp3645 tlp3646 tlp3647 tlp3648)))
                (app test-struct-c (box (list tlp3649 tlp3650 tlp3651)))
                (app test-struct-d (and tlp3652 tlp3653)))
           tlp3654))
     tlp3655)
    (list
     tlp3656
     #\c
     tlp3657
     (list-rest (list tlp3658 __3) #\d (list tlp3659 ___) #\c))
    (list tlp3660 _ (list) _)
    (list 'these tlp3661 (list) tlp3662))
   )

  (pattern
   (list
    (vector
     (vector tlp3663 ___)
     (list tlp3664 ___)
     (and (? number? tlp3665) (? even? tlp3666))
     (box (list (vector tlp3667 ...) (list tlp3668 ..3) _)))
    tlp3669
    (list tlp3670 __3)
    tlp3671)
   )

  (pattern
   (list
    (box
     (list
      (struct
       test-struct
       ((vector tlp3672 ...)
        tlp3673
        (and (not (not #\c)) (not (not #\c)))
        (list-rest tlp3674 tlp3675 tlp3676 #\d)))
      tlp3678
      (and (list tlp3679 ...) (list tlp3680 ...))))
    _
    tlp3681
    tlp3682)
   )

  (pattern
   (list
    tlp3683
    (list-rest
     (and #\d #\d)
     (or (list tlp3684 __3) (list tlp3684 __3))
     (list)
     #\d)
    (and (vector tlp3685 ___) (vector tlp3686 ___))
    (list tlp3687 ___))
   )

  (pattern
   (list
    tlp3688
    tlp3689
    (list
     (box
      (list
       (list tlp3690 ___)
       (and (? number? tlp3691) (? even? tlp3692))
       (not (not #\c))))
     (and (list tlp3693 ...) (list tlp3694 ...))
     tlp3695
     (not (not #f)))
    (vector tlp3696 ...))
   )

  (pattern
   (list
    (struct
     test-struct
     ((not (not #\b))
      tlp3697
      (box (list tlp3698 (list tlp3699 ..3) tlp3700))
      (or _ _)))
    (list-rest (list) (vector tlp3702 ...) (and 'are 'are) #\d)
    (box
     (list
      tlp3703
      (vector
       (list tlp3704 ___)
       (list tlp3705 ...)
       (list tlp3706 ___)
       (vector tlp3707 ...))
      (list
       (struct test-struct (tlp3708 tlp3709 tlp3710 tlp3711))
       #t
       (list)
       (list tlp3713 __3))))
    (vector tlp3714 ___))
   )

  (pattern
   (list
    3
    (and (? number? tlp3716) (? even? tlp3717))
    (and #f #f)
    (or (vector tlp3718 ___) (vector tlp3718 ___)))
   )

  (pattern
   (list
    tlp3721
    tlp3722
    (or (list
         (and (? number? tlp3723) (? even? tlp3724))
         tlp3725
         (list)
         (vector tlp3726 (list tlp3727 tlp3728 tlp3729 tlp3730) (list) tlp3731))
        (list
         (and (? number? tlp3723) (? even? tlp3724))
         tlp3725
         (list)
         (vector tlp3726 (list tlp3727 tlp3728 tlp3729 tlp3730) (list) tlp3731)))
    (or tlp3732 tlp3732))
   )

  (pattern
   (list
    (struct
     test-struct
     ((and (list tlp3735 __3) (list tlp3736 __3))
      tlp3737
      (and (? number? tlp3738) (? even? tlp3739))
      (and (? test-struct? tlp3754)
           (app
            test-struct-a
            (and (? test-struct? tlp3745)
                 (app test-struct-a tlp3740)
                 (app test-struct-b tlp3741)
                 (app test-struct-c tlp3742)
                 (app test-struct-d tlp3743)))
           (app test-struct-b (and (vector tlp3746 ___) (vector tlp3747 ___)))
           (app test-struct-c _)
           (app
            test-struct-d
            (struct test-struct (tlp3748 tlp3749 tlp3750 tlp3751))))))
    #t
    (and (vector
          (list
           _
           (and tlp3756 tlp3757)
           (and (? test-struct? tlp3763)
                (app test-struct-a tlp3758)
                (app test-struct-b tlp3759)
                (app test-struct-c tlp3760)
                (app test-struct-d tlp3761))
           (not (not 'are)))
          _
          (struct
           test-struct
           ((box (list tlp3764 tlp3765 tlp3766))
            (vector tlp3767 tlp3768 tlp3769 tlp3770)
            tlp3771
            (struct test-struct (tlp3772 tlp3773 tlp3774 tlp3775))))
          _)
         (vector
          (list
           _
           (and tlp3778 tlp3779)
           (and (? test-struct? tlp3780)
                (app test-struct-a tlp3781)
                (app test-struct-b tlp3782)
                (app test-struct-c tlp3783)
                (app test-struct-d tlp3784))
           (not (not 'are)))
          _
          (struct
           test-struct
           ((box (list tlp3785 tlp3786 tlp3787))
            (vector tlp3788 tlp3789 tlp3790 tlp3791)
            tlp3792
            (struct test-struct (tlp3793 tlp3794 tlp3795 tlp3796))))
          _))
    (and (? test-struct? tlp3823)
         (app
          test-struct-a
          (and (? test-struct? tlp3806)
               (app
                test-struct-a
                (and (and tlp3797 tlp3798) (and tlp3799 tlp3800)))
               (app test-struct-b (box (list tlp3801 tlp3802 tlp3803)))
               (app test-struct-c (or '(+ 4 5) '(+ 4 5)))
               (app test-struct-d tlp3804)))
         (app
          test-struct-b
          (box
           (list (box (list tlp3807 tlp3808 tlp3809)) #\c (list tlp3810 ..3))))
         (app test-struct-c _)
         (app
          test-struct-d
          (and (? test-struct? tlp3821)
               (app test-struct-a tlp3811)
               (app test-struct-b (list tlp3812 ...))
               (app
                test-struct-c
                (and (? test-struct? tlp3818)
                     (app test-struct-a tlp3813)
                     (app test-struct-b tlp3814)
                     (app test-struct-c tlp3815)
                     (app test-struct-d tlp3816)))
               (app test-struct-d (vector tlp3819 ___))))))
   )

  (pattern
   (list
    (box
     (list
      tlp3826
      tlp3827
      (list-rest
       (list-rest tlp3828 tlp3829 tlp3830 #t)
       (list tlp3831 __3)
       (list-rest tlp3832 tlp3833 tlp3834 'these)
       #\b)))
    (list)
    (and (? test-struct? tlp3857)
         (app test-struct-a (box (list tlp3835 #\a tlp3836)))
         (app
          test-struct-b
          (and (list
                (list tlp3837 __3)
                #t
                (and (? number? tlp3838) (? even? tlp3839))
                (vector tlp3840 ...))
               (list
                (list tlp3841 __3)
                #t
                (and (? number? tlp3842) (? even? tlp3843))
                (vector tlp3844 ...))))
         (app test-struct-c (list (vector tlp3845 ...) #t tlp3846 (or #f #f)))
         (app
          test-struct-d
          (or (and (? test-struct? tlp3855)
                   (app test-struct-a #\b)
                   (app test-struct-b _)
                   (app
                    test-struct-c
                    (struct test-struct (tlp3847 tlp3848 tlp3849 tlp3850)))
                   (app
                    test-struct-d
                    (and (? number? tlp3852) (? even? tlp3853))))
              (and (? test-struct? tlp3855)
                   (app test-struct-a #\b)
                   (app test-struct-b _)
                   (app
                    test-struct-c
                    (struct test-struct (tlp3847 tlp3848 tlp3849 tlp3850)))
                   (app
                    test-struct-d
                    (and (? number? tlp3852) (? even? tlp3853)))))))
    (list
     (list-rest
      (list-rest tlp3858 tlp3859 tlp3860 'these)
      (box (list tlp3861 tlp3862 tlp3863))
      (not (not '(+ 4 (+ 1 1))))
      #\a)
     (and (list-rest
           (vector tlp3864 tlp3865 tlp3866 tlp3867)
           (vector tlp3868 ...)
           '(+ 4 5)
           #\d)
          (list-rest
           (vector tlp3869 tlp3870 tlp3871 tlp3872)
           (vector tlp3873 ...)
           '(+ 4 5)
           #\d))
     (not (not #\b))
     (vector
      tlp3874
      (not (not '(+ 1 2)))
      (list tlp3875 tlp3876 tlp3877 tlp3878)
      (list tlp3879 tlp3880 tlp3881 tlp3882))))
   )

  (pattern
   (list
    (and (? test-struct? tlp3903)
         (app
          test-struct-a
          (and (? test-struct? tlp3891)
               (app test-struct-a tlp3885)
               (app test-struct-b (list tlp3886 tlp3887 tlp3888 tlp3889))
               (app test-struct-c (not (not #\d)))
               (app test-struct-d _)))
         (app test-struct-b '(+ 4 5))
         (app test-struct-c (list))
         (app
          test-struct-d
          (vector
           (list tlp3892 tlp3893 tlp3894 tlp3895)
           (and (? test-struct? tlp3901)
                (app test-struct-a tlp3896)
                (app test-struct-b tlp3897)
                (app test-struct-c tlp3898)
                (app test-struct-d tlp3899))
           _
           (or (list) (list)))))
    (list tlp3904 ..3)
    (list tlp3905 ...)
    (vector
     (list tlp3906 ...)
     (list-rest
      (list tlp3907 ..3)
      (list-rest tlp3908 tlp3909 tlp3910 #\d)
      5
      'are)
     (list-rest
      tlp3911
      (and (? number? tlp3912) (? even? tlp3913))
      (list tlp3914 ...)
      #\d)
     (list tlp3915 #\b (vector tlp3916 ...) (list))))
   )

  (pattern
   (list
    (not (not 'are))
    (list-rest
     tlp4432
     (list-rest tlp4433 (list tlp4434 ...) tlp4435 'symbols)
     (not (not 2))
     'symbols)
    (not (not #\a))
    (list
     (and (? test-struct? tlp4445)
          (app test-struct-a (list-rest tlp4436 tlp4437 tlp4438 9))
          (app test-struct-b (and (and tlp4439 tlp4440) (and tlp4441 tlp4442)))
          (app test-struct-c '(+ 1 2))
          (app test-struct-d (list tlp4443 ___)))
     tlp4446
     (list-rest 'these tlp4447 (list tlp4448 ..3) 'these)
     (list tlp4449 ___)))
   )

  (pattern
   (list
    tlp4451
    tlp4452
    (and (? test-struct? tlp4471)
         (app
          test-struct-a
          (and (? test-struct? tlp4461)
               (app test-struct-a (box (list tlp4453 tlp4454 tlp4455)))
               (app test-struct-b (list tlp4456 ...))
               (app test-struct-c (list-rest tlp4457 tlp4458 tlp4459 'symbols))
               (app test-struct-d '(+ 4 5))))
         (app test-struct-b (and (? number? tlp4462) (? even? tlp4463)))
         (app test-struct-c tlp4464)
         (app
          test-struct-d
          (or (list-rest
               (list)
               tlp4465
               (list tlp4466 tlp4467 tlp4468 tlp4469)
               #\b)
              (list-rest
               (list)
               tlp4465
               (list tlp4466 tlp4467 tlp4468 tlp4469)
               #\b))))
    tlp4472)
   )

  (pattern
   (list
    (list-rest
     (and (list
           (vector tlp4474 tlp4475 tlp4476 tlp4477)
           (vector tlp4478 tlp4479 tlp4480 tlp4481)
           (list)
           (or tlp4482 tlp4482))
          (list
           (vector tlp4483 tlp4484 tlp4485 tlp4486)
           (vector tlp4487 tlp4488 tlp4489 tlp4490)
           (list)
           (or tlp4491 tlp4491)))
     (and (? number? tlp4492) (? even? tlp4493))
     (list '(+ 1 2) (list tlp4494 ...) tlp4495 tlp4496)
     #f)
    (and (? test-struct? tlp4517)
         (app
          test-struct-a
          (or (box
               (list
                (and (? test-struct? tlp4502)
                     (app test-struct-a tlp4497)
                     (app test-struct-b tlp4498)
                     (app test-struct-c tlp4499)
                     (app test-struct-d tlp4500))
                (list)
                (vector tlp4503 tlp4504 tlp4505 tlp4506)))
              (box
               (list
                (and (? test-struct? tlp4502)
                     (app test-struct-a tlp4497)
                     (app test-struct-b tlp4498)
                     (app test-struct-c tlp4499)
                     (app test-struct-d tlp4500))
                (list)
                (vector tlp4503 tlp4504 tlp4505 tlp4506)))))
         (app
          test-struct-b
          (and (list-rest
                (list tlp4507 ___)
                (and (? number? tlp4508) (? even? tlp4509))
                (list tlp4510 __3)
                'these)
               (list-rest
                (list tlp4511 ___)
                (and (? number? tlp4512) (? even? tlp4513))
                (list tlp4514 __3)
                'these)))
         (app test-struct-c (or (list tlp4515 ..3) (list tlp4515 ..3)))
         (app test-struct-d (list)))
    tlp4518
    tlp4519)
   )

  (pattern
   (list
    (and (? number? tlp4521) (? even? tlp4522))
    (or tlp4523 tlp4523)
    tlp4524
    (list tlp4525 ...))
   )

  (pattern
   (list
    (list tlp4527 __3)
    (list
     (and (? test-struct? tlp4540)
          (app
           test-struct-a
           (and (struct test-struct (tlp4528 tlp4529 tlp4530 tlp4531))
                (struct test-struct (tlp4533 tlp4534 tlp4535 tlp4536))))
          (app test-struct-b (list tlp4537 ..3))
          (app test-struct-c #\d)
          (app test-struct-d (vector tlp4538 ...)))
     (and (? test-struct? tlp4556)
          (app test-struct-a (not (not #\b)))
          (app
           test-struct-b
           (and (vector tlp4541 tlp4542 tlp4543 tlp4544)
                (vector tlp4545 tlp4546 tlp4547 tlp4548)))
          (app test-struct-c (vector tlp4549 ...))
          (app
           test-struct-d
           (struct test-struct (tlp4550 tlp4551 tlp4552 tlp4553))))
     (box
      (list (and (list tlp4557 __3) (list tlp4558 __3)) tlp4559 (not (not 4))))
     tlp4560)
    (list)
    (or 'these 'these))
   )

  (pattern
   (list tlp4984 'these (not (not #\b)) tlp4985)
   )

  (pattern
   (list
    _
    (vector tlp4988 ...)
    tlp4989
    (box
     (list
      (not (not '(+ 4 (+ 1 1))))
      (struct
       test-struct
       ((list-rest tlp4990 tlp4991 tlp4992 4)
        (struct test-struct (tlp4993 tlp4994 tlp4995 tlp4996))
        tlp4998
        #t))
      tlp5000)))
   )

  (pattern
   (list #\b (and (list) (list)) '(+ 4 (+ 1 1)) tlp5003)
   )

  (pattern
   (list
    (vector tlp5006 ...)
    _
    (vector
     (list tlp5007 ..3)
     (box (list (list tlp5008 ___) _ (not (not #t))))
     tlp5009
     (list tlp5010 (vector tlp5011 ...) (list) (list)))
    (and (? number? tlp5012) (? even? tlp5013)))
   )

  (pattern
   (list
    (and (list
          (list (and (? number? tlp5016) (? even? tlp5017)) (list) #\a (list))
          (and (? number? tlp5018) (? even? tlp5019))
          (list)
          tlp5020)
         (list
          (list (and (? number? tlp5021) (? even? tlp5022)) (list) #\a (list))
          (and (? number? tlp5023) (? even? tlp5024))
          (list)
          tlp5025))
    (or (not (not 'are)) (not (not 'are)))
    tlp5026
    (list
     (struct
      test-struct
      (_
       (or (struct test-struct (tlp5027 tlp5028 tlp5029 tlp5030))
           (struct test-struct (tlp5027 tlp5028 tlp5029 tlp5030)))
       (or (list tlp5032 __3) (list tlp5032 __3))
       (list tlp5033 tlp5034 tlp5035 tlp5036)))
     (and (? test-struct? tlp5041)
          (app test-struct-a '(+ 1 2))
          (app test-struct-b tlp5038)
          (app test-struct-c (list tlp5039 ___))
          (app test-struct-d 'are))
     tlp5042
     (or (and (? number? tlp5043) (? even? tlp5044))
         (and (? number? tlp5043) (? even? tlp5044)))))
   )

  (pattern
   (list
    (or (and #\c #\c) (and #\c #\c))
    (list tlp963 __3)
    (not (not #f))
    (list
     (and (or (list-rest tlp964 tlp965 tlp966 'are)
              (list-rest tlp964 tlp965 tlp966 'are))
          (or (list-rest tlp967 tlp968 tlp969 'are)
              (list-rest tlp967 tlp968 tlp969 'are)))
     (list tlp970 __3)
     tlp971
     tlp972))
   )

  (pattern
   (list
    (box
     (list
      (list
       (list-rest tlp973 tlp974 tlp975 'symbols)
       (not (not 'these))
       tlp976
       (and (? test-struct? tlp982)
            (app test-struct-a tlp977)
            (app test-struct-b tlp978)
            (app test-struct-c tlp979)
            (app test-struct-d tlp980)))
      (list
       (list tlp983 tlp984 tlp985 tlp986)
       (and (? test-struct? tlp992)
            (app test-struct-a tlp987)
            (app test-struct-b tlp988)
            (app test-struct-c tlp989)
            (app test-struct-d tlp990))
       (vector tlp993 tlp994 tlp995 tlp996)
       (box (list tlp997 tlp998 tlp999)))
      (or tlp1000 tlp1000)))
    (and (? test-struct? tlp1005)
         (app test-struct-a #t)
         (app test-struct-b (or tlp1001 tlp1001))
         (app test-struct-c tlp1002)
         (app test-struct-d (list tlp1003 ...)))
    (not (not 'these))
    (list tlp1006 ___))
   )

  (pattern
   (list
    (list)
    (and (? test-struct? tlp1033)
         (app test-struct-a tlp1007)
         (app test-struct-b (and (? number? tlp1008) (? even? tlp1009)))
         (app
          test-struct-c
          (and (? test-struct? tlp1023)
               (app
                test-struct-a
                (and (list-rest tlp1010 tlp1011 tlp1012 tlp1013)
                     (list-rest tlp1014 tlp1015 tlp1016 tlp1017)))
               (app test-struct-b (list))
               (app test-struct-c (and (list tlp1018 ___) (list tlp1019 ___)))
               (app test-struct-d (and (? number? tlp1020) (? even? tlp1021)))))
         (app
          test-struct-d
          (and (? test-struct? tlp1031)
               (app
                test-struct-a
                (or (and (? number? tlp1024) (? even? tlp1025))
                    (and (? number? tlp1024) (? even? tlp1025))))
               (app test-struct-b _)
               (app test-struct-c (vector tlp1026 tlp1027 tlp1028 tlp1029))
               (app test-struct-d _))))
    9
    (list
     (struct
      test-struct
      ((and (and tlp1034 tlp1035) (and tlp1036 tlp1037))
       tlp1038
       (struct test-struct (tlp1039 tlp1040 tlp1041 tlp1042))
       (and (? test-struct? tlp1049)
            (app test-struct-a tlp1044)
            (app test-struct-b tlp1045)
            (app test-struct-c tlp1046)
            (app test-struct-d tlp1047))))
     tlp1051
     1
     _))
   )

  (pattern
   (list
    (list)
    (list tlp1052 ...)
    (list-rest
     (list tlp1053 ..3)
     (vector tlp1054 ...)
     (vector
      tlp1055
      (vector tlp1056 ___)
      (list tlp1057 ...)
      (box (list tlp1058 tlp1059 tlp1060)))
     #\d)
    (and (? number? tlp1061) (? even? tlp1062)))
   )

  (pattern
   (list
    (and (? test-struct? tlp1092)
         (app
          test-struct-a
          (struct
           test-struct
           ((not (not 'are))
            tlp1063
            tlp1064
            (and (? test-struct? tlp1070)
                 (app test-struct-a tlp1065)
                 (app test-struct-b tlp1066)
                 (app test-struct-c tlp1067)
                 (app test-struct-d tlp1068)))))
         (app
          test-struct-b
          (struct
           test-struct
           ((box (list tlp1072 tlp1073 tlp1074))
            (and tlp1075 tlp1076)
            tlp1077
            (and (list tlp1078 ...) (list tlp1079 ...)))))
         (app
          test-struct-c
          (or (vector
               (and tlp1081 tlp1082)
               (and (? test-struct? tlp1088)
                    (app test-struct-a tlp1083)
                    (app test-struct-b tlp1084)
                    (app test-struct-c tlp1085)
                    (app test-struct-d tlp1086))
               #\c
               #f)
              (vector
               (and tlp1081 tlp1082)
               (and (? test-struct? tlp1088)
                    (app test-struct-a tlp1083)
                    (app test-struct-b tlp1084)
                    (app test-struct-c tlp1085)
                    (app test-struct-d tlp1086))
               #\c
               #f)))
         (app test-struct-d (and (? number? tlp1089) (? even? tlp1090))))
    #t
    (list tlp1093 ..3)
    _)
   )

  (pattern
   (list (list tlp1094 __3) tlp1095 (list tlp1096 ..3) (list))
   )

  (pattern
   (list
    (list
     (struct test-struct (tlp1097 (list) (vector tlp1098 ___) (list)))
     (and (? test-struct? tlp1119)
          (app test-struct-a (list tlp1100 tlp1101 tlp1102 #\a))
          (app
           test-struct-b
           (and (? test-struct? tlp1107)
                (app test-struct-a (list-rest tlp1103 tlp1104 tlp1105 8))
                (app test-struct-b _)
                (app test-struct-c (or '(+ 4 (+ 1 1)) '(+ 4 (+ 1 1))))
                (app test-struct-d _)))
          (app test-struct-c tlp1108)
          (app
           test-struct-d
           (box
            (list
             (list tlp1109 tlp1110 tlp1111 tlp1112)
             (list tlp1113 ..3)
             (list tlp1114 tlp1115 tlp1116 tlp1117)))))
     (and (? test-struct? tlp1142)
          (app
           test-struct-a
           (list
            (list)
            tlp1120
            (struct test-struct (tlp1121 tlp1122 tlp1123 tlp1124))
            (vector tlp1126 tlp1127 tlp1128 tlp1129)))
          (app
           test-struct-b
           (and (? test-struct? tlp1140)
                (app test-struct-a _)
                (app
                 test-struct-b
                 (or (list-rest tlp1130 tlp1131 tlp1132 tlp1133)
                     (list-rest tlp1130 tlp1131 tlp1132 tlp1133)))
                (app test-struct-c tlp1134)
                (app test-struct-d (vector tlp1135 tlp1136 tlp1137 tlp1138))))
          (app test-struct-c (list))
          (app test-struct-d (not (not 5))))
     (and (? number? tlp1143) (? even? tlp1144)))
    (list
     '(+ 1 2)
     'symbols
     _
     (struct
      test-struct
      ((list
        (list tlp1145 tlp1146 tlp1147 tlp1148)
        (vector tlp1149 ___)
        (and (? number? tlp1150) (? even? tlp1151))
        (list tlp1152 __3))
       2
       (list-rest (list tlp1153 ___) (vector tlp1154 ___) 'symbols 0)
       tlp1155)))
    (list
     (list-rest
      (list-rest
       #\c
       (struct test-struct (tlp1157 tlp1158 tlp1159 tlp1160))
       (and (list-rest tlp1162 tlp1163 tlp1164 tlp1165)
            (list-rest tlp1166 tlp1167 tlp1168 tlp1169))
       #\b)
      (struct
       test-struct
       ((and (and (? test-struct? tlp1175)
                  (app test-struct-a tlp1170)
                  (app test-struct-b tlp1171)
                  (app test-struct-c tlp1172)
                  (app test-struct-d tlp1173))
             (and (? test-struct? tlp1176)
                  (app test-struct-a tlp1177)
                  (app test-struct-b tlp1178)
                  (app test-struct-c tlp1179)
                  (app test-struct-d tlp1180)))
        tlp1181
        (and tlp1182 tlp1183)
        (box (list tlp1184 tlp1185 tlp1186))))
      (or _ _)
      #\a)
     (list
      _
      (and (? number? tlp1188) (? even? tlp1189))
      (list)
      (and (? test-struct? tlp1199)
           (app test-struct-a (list tlp1190 ..3))
           (app test-struct-b (vector tlp1191 tlp1192 tlp1193 tlp1194))
           (app test-struct-c #f)
           (app test-struct-d (box (list tlp1195 tlp1196 tlp1197)))))
     (and (? number? tlp1200) (? even? tlp1201))
     '(+ 4 5)))
   )

  (pattern
   (list
    (list
     #f
     (list tlp1202 ..3)
     (vector tlp1203 ___)
     (list
      (box (list _ (box (list tlp1204 tlp1205 tlp1206)) (list tlp1207 ___)))
      (and (? number? tlp1208) (? even? tlp1209))
      (list)
      (and (list tlp1210 ...) (list tlp1211 ...))))
    (list 'symbols (and (? number? tlp1212) (? even? tlp1213)) #f 'are)
    (list
     (and tlp1214 tlp1215)
     (and (? test-struct? tlp1233)
          (app test-struct-a (vector tlp1216 ___))
          (app
           test-struct-b
           (and (? test-struct? tlp1222)
                (app test-struct-a tlp1217)
                (app test-struct-b (not (not 4)))
                (app test-struct-c (list))
                (app test-struct-d (list-rest tlp1218 tlp1219 tlp1220 #\d))))
          (app test-struct-c (box (list (list) (not (not '(+ 1 2))) (list))))
          (app
           test-struct-d
           (and (? test-struct? tlp1231)
                (app test-struct-a (list))
                (app test-struct-b (vector tlp1223 ...))
                (app test-struct-c tlp1224)
                (app
                 test-struct-d
                 (struct test-struct (tlp1225 tlp1226 tlp1227 tlp1228))))))
     (list tlp1234 ___)
     _))
   )

  (pattern
   (list
    (and (? test-struct? tlp1239)
         (app test-struct-a (list tlp1235 __3))
         (app test-struct-b tlp1236)
         (app test-struct-c (list))
         (app test-struct-d (list tlp1237 __3)))
    tlp1240
    (list-rest
     (vector tlp1241 ___)
     (or (or (list tlp1242 tlp1243 tlp1244 tlp1245)
             (list tlp1242 tlp1243 tlp1244 tlp1245))
         (or (list tlp1242 tlp1243 tlp1244 tlp1245)
             (list tlp1242 tlp1243 tlp1244 tlp1245)))
     (or (struct
          test-struct
          ((box (list tlp1246 tlp1247 tlp1248))
           (list tlp1249 ___)
           (and (? test-struct? tlp1255)
                (app test-struct-a tlp1250)
                (app test-struct-b tlp1251)
                (app test-struct-c tlp1252)
                (app test-struct-d tlp1253))
           (list tlp1256 tlp1257 tlp1258 tlp1259)))
         (struct
          test-struct
          ((box (list tlp1246 tlp1247 tlp1248))
           (list tlp1249 ___)
           (and (? test-struct? tlp1255)
                (app test-struct-a tlp1250)
                (app test-struct-b tlp1251)
                (app test-struct-c tlp1252)
                (app test-struct-d tlp1253))
           (list tlp1256 tlp1257 tlp1258 tlp1259))))
     'these)
    (list
     tlp1261
     (not (not #t))
     (box
      (list
       (and (? test-struct? tlp1267)
            (app test-struct-a tlp1262)
            (app test-struct-b tlp1263)
            (app test-struct-c tlp1264)
            (app test-struct-d tlp1265))
       #\d
       (list tlp1268 tlp1269 tlp1270 tlp1271)))
     (list-rest
      (list)
      (list-rest tlp1272 tlp1273 tlp1274 #\c)
      (and (? test-struct? tlp1280)
           (app test-struct-a tlp1275)
           (app test-struct-b tlp1276)
           (app test-struct-c tlp1277)
           (app test-struct-d tlp1278))
      #f)))
   )

  (pattern
   (list
    (vector tlp1281 ___)
    tlp1282
    (not (not #\b))
    (or (list tlp1283 ___) (list tlp1283 ___)))
   )

  (pattern
   (list
    (list
     (list tlp1284 ..3)
     (list)
     tlp1285
     (box
      (list
       (and (? number? tlp1286) (? even? tlp1287))
       (or (box (list tlp1288 tlp1289 tlp1290))
           (box (list tlp1288 tlp1289 tlp1290)))
       (box (list tlp1291 tlp1292 tlp1293)))))
    (list tlp1294 __3)
    2
    (vector tlp1295 ___))
   )

  (pattern
   (list
    (struct test-struct ((list tlp1437 ...) (vector tlp1438 ___) #\d 'are))
    (and (struct
          test-struct
          ((list tlp1440 ___)
           (list-rest
            (list-rest tlp1441 tlp1442 tlp1443 tlp1444)
            (list)
            (and (? number? tlp1445) (? even? tlp1446))
            #t)
           (list
            (list-rest tlp1447 tlp1448 tlp1449 tlp1450)
            tlp1451
            (list-rest tlp1452 tlp1453 tlp1454 tlp1455)
            (and (? test-struct? tlp1461)
                 (app test-struct-a tlp1456)
                 (app test-struct-b tlp1457)
                 (app test-struct-c tlp1458)
                 (app test-struct-d tlp1459)))
           (box
            (list
             (and (? number? tlp1462) (? even? tlp1463))
             tlp1464
             (or tlp1465 tlp1465)))))
         (struct
          test-struct
          ((list tlp1467 ___)
           (list-rest
            (list-rest tlp1468 tlp1469 tlp1470 tlp1471)
            (list)
            (and (? number? tlp1472) (? even? tlp1473))
            #t)
           (list
            (list-rest tlp1474 tlp1475 tlp1476 tlp1477)
            tlp1478
            (list-rest tlp1479 tlp1480 tlp1481 tlp1482)
            (and (? test-struct? tlp1483)
                 (app test-struct-a tlp1484)
                 (app test-struct-b tlp1485)
                 (app test-struct-c tlp1486)
                 (app test-struct-d tlp1487)))
           (box
            (list
             (and (? number? tlp1488) (? even? tlp1489))
             tlp1490
             (or tlp1491 tlp1491))))))
    (vector tlp1492 ...)
    (vector
     (box (list _ (vector tlp1493 ___) tlp1494))
     (not (not #\b))
     (and (? test-struct? tlp1503)
          (app test-struct-a (and _ _))
          (app test-struct-b (and (list) (list)))
          (app test-struct-c (box (list tlp1495 tlp1496 tlp1497)))
          (app test-struct-d (vector tlp1498 tlp1499 tlp1500 tlp1501)))
     _))
   (list
    (and (vector
          (and (? test-struct? tlp1509)
               (app test-struct-a #\c)
               (app test-struct-b _)
               (app test-struct-c (list-rest tlp1504 tlp1505 tlp1506 tlp1507))
               (app test-struct-d (list)))
          tlp1510
          (list tlp1511 __3)
          #\a)
         (vector
          (and (? test-struct? tlp1512)
               (app test-struct-a #\c)
               (app test-struct-b _)
               (app test-struct-c (list-rest tlp1513 tlp1514 tlp1515 tlp1516))
               (app test-struct-d (list)))
          tlp1517
          (list tlp1518 __3)
          #\a))
    _
    (struct
     test-struct
     ((struct test-struct (#t #\b #\d (or 6 6)))
      (and (? number? tlp1520) (? even? tlp1521))
      (or (struct
           test-struct
           ((list-rest tlp1522 tlp1523 tlp1524 tlp1525)
            (list-rest tlp1526 tlp1527 tlp1528 tlp1529)
            (not (not '(+ 1 2)))
            (and (? test-struct? tlp1535)
                 (app test-struct-a tlp1530)
                 (app test-struct-b tlp1531)
                 (app test-struct-c tlp1532)
                 (app test-struct-d tlp1533))))
          (struct
           test-struct
           ((list-rest tlp1522 tlp1523 tlp1524 tlp1525)
            (list-rest tlp1526 tlp1527 tlp1528 tlp1529)
            (not (not '(+ 1 2)))
            (and (? test-struct? tlp1535)
                 (app test-struct-a tlp1530)
                 (app test-struct-b tlp1531)
                 (app test-struct-c tlp1532)
                 (app test-struct-d tlp1533)))))
      tlp1537))
    (and (list
          (vector tlp1539 ...)
          #\b
          (struct test-struct (tlp1540 3 tlp1541 tlp1542))
          (list tlp1544 ..3))
         (list
          (vector tlp1545 ...)
          #\b
          (struct test-struct (tlp1546 3 tlp1547 tlp1548))
          (list tlp1549 ..3))))
   (list
    (and tlp1550 tlp1551)
    (list tlp1552 __3)
    (list tlp1553 ___)
    (list tlp1554 ...))
   )

  (pattern
   (list
    (vector
     (and (? test-struct? tlp1564)
          (app test-struct-a '(+ 4 (+ 1 1)))
          (app
           test-struct-b
           (and (? test-struct? tlp1562)
                (app test-struct-a tlp1557)
                (app test-struct-b tlp1558)
                (app test-struct-c tlp1559)
                (app test-struct-d tlp1560)))
          (app test-struct-c '(+ 4 5))
          (app test-struct-d (not (not 3))))
     (and (box
           (list
            (box (list tlp1565 tlp1566 tlp1567))
            (struct test-struct (tlp1568 tlp1569 tlp1570 tlp1571))
            (struct test-struct (tlp1573 tlp1574 tlp1575 tlp1576))))
          (box
           (list
            (box (list tlp1578 tlp1579 tlp1580))
            (struct test-struct (tlp1581 tlp1582 tlp1583 tlp1584))
            (struct test-struct (tlp1585 tlp1586 tlp1587 tlp1588)))))
     (or (list-rest
          (or tlp1589 tlp1589)
          (and (? test-struct? tlp1595)
               (app test-struct-a tlp1590)
               (app test-struct-b tlp1591)
               (app test-struct-c tlp1592)
               (app test-struct-d tlp1593))
          (vector tlp1596 tlp1597 tlp1598 tlp1599)
          #t)
         (list-rest
          (or tlp1589 tlp1589)
          (and (? test-struct? tlp1595)
               (app test-struct-a tlp1590)
               (app test-struct-b tlp1591)
               (app test-struct-c tlp1592)
               (app test-struct-d tlp1593))
          (vector tlp1596 tlp1597 tlp1598 tlp1599)
          #t))
     (and (? test-struct? tlp1610)
          (app test-struct-a (list tlp1600 ...))
          (app test-struct-b (list tlp1601 ...))
          (app test-struct-c (list tlp1602 tlp1603 tlp1604 tlp1605))
          (app test-struct-d (list-rest tlp1606 tlp1607 tlp1608 #\b))))
    (list
     (and (? number? tlp1611) (? even? tlp1612))
     (or (and (? number? tlp1613) (? even? tlp1614))
         (and (? number? tlp1613) (? even? tlp1614)))
     (box
      (list
       (struct test-struct (tlp1615 tlp1616 tlp1617 tlp1618))
       (and (? number? tlp1620) (? even? tlp1621))
       #f))
     (and (? number? tlp1622) (? even? tlp1623)))
    _
    (list))
   (list
    (box
     (list
      (list-rest (vector tlp1624 tlp1625 tlp1626 tlp1627) tlp1628 _ #t)
      (list tlp1629 ___)
      (list
       (vector tlp1630 ___)
       (and (? test-struct? tlp1636)
            (app test-struct-a tlp1631)
            (app test-struct-b tlp1632)
            (app test-struct-c tlp1633)
            (app test-struct-d tlp1634))
       tlp1637
       tlp1638)))
    (list tlp1639 ..3)
    (list-rest _ (list tlp1640 __3) _ #t)
    (box
     (list
      tlp1641
      #t
      (vector
       (and (? number? tlp1642) (? even? tlp1643))
       (list)
       #\a
       (box (list tlp1644 tlp1645 tlp1646))))))
   (list
    (list
     (and (list-rest
           (not (not 'symbols))
           _
           (list-rest tlp1647 tlp1648 tlp1649 tlp1650)
           0)
          (list-rest
           (not (not 'symbols))
           _
           (list-rest tlp1651 tlp1652 tlp1653 tlp1654)
           0))
     _
     (struct
      test-struct
      ((list-rest tlp1655 tlp1656 tlp1657 #f)
       (list tlp1658 ...)
       (and (? test-struct? tlp1664)
            (app test-struct-a tlp1659)
            (app test-struct-b tlp1660)
            (app test-struct-c tlp1661)
            (app test-struct-d tlp1662))
       (list tlp1665 ___)))
     (vector _ (and _ _) (not (not 6)) _))
    (list tlp1667 __3)
    (or 9 9)
    '(+ 4 5))
   )

  (pattern
   (list
    (box (list (list) (list) (list tlp1670 __3)))
    (not (not '(+ 1 2)))
    (box
     (list
      'symbols
      (vector
       (or (list tlp1671 tlp1672 tlp1673 tlp1674)
           (list tlp1671 tlp1672 tlp1673 tlp1674))
       tlp1675
       (vector tlp1676 ...)
       (list tlp1677 tlp1678 tlp1679 tlp1680))
      tlp1681))
    (box
     (list
      (vector tlp1682 #f #\a _)
      (vector
       (and (? test-struct? tlp1688)
            (app test-struct-a tlp1683)
            (app test-struct-b tlp1684)
            (app test-struct-c tlp1685)
            (app test-struct-d tlp1686))
       _
       (list tlp1689 __3)
       (box (list tlp1690 tlp1691 tlp1692)))
      (list))))
   (list
    (struct
     test-struct
     ((vector
       (vector tlp1693 tlp1694 tlp1695 tlp1696)
       (list tlp1697 __3)
       (not (not #\c))
       tlp1698)
      (vector tlp1699 ___)
      (list tlp1700 ..3)
      #\b))
    (struct
     test-struct
     (tlp1702
      (vector
       (list)
       (box (list tlp1703 tlp1704 tlp1705))
       (or (list) (list))
       tlp1706)
      _
      tlp1707))
    #t
    tlp1709)
   (list tlp1710 (vector tlp1711 ___) _ tlp1712)
   )

  (pattern
   (list
    (list-rest
     (list)
     (and (? test-struct? tlp1720)
          (app test-struct-a (box (list tlp1715 tlp1716 tlp1717)))
          (app test-struct-b _)
          (app test-struct-c 0)
          (app test-struct-d tlp1718))
     (not (not '(+ 4 5)))
     8)
    '(+ 4 (+ 1 1))
    (and (? number? tlp1721) (? even? tlp1722))
    (vector
     (and (? number? tlp1723) (? even? tlp1724))
     _
     (box (list (not (not '(+ 4 (+ 1 1)))) (not (not #t)) (list tlp1725 ___)))
     (not (not '(+ 1 2)))))
   (list _ (box (list (list tlp1726 ...) tlp1727 (list))) tlp1728 '(+ 4 5))
   (list
    tlp1729
    #\a
    (vector
     (and (? number? tlp1730) (? even? tlp1731))
     (list-rest
      '(+ 1 2)
      (list tlp1732 tlp1733 tlp1734 tlp1735)
      (list tlp1736 __3)
      #\b)
     (list-rest
      (and (? test-struct? tlp1742)
           (app test-struct-a tlp1737)
           (app test-struct-b tlp1738)
           (app test-struct-c tlp1739)
           (app test-struct-d tlp1740))
      (list)
      (box (list tlp1743 tlp1744 tlp1745))
      'these)
     (list))
    #f)
   )

  (pattern
   (list (vector tlp1748 ...) _ (or _ _) (list tlp1749 __3))
   (list
    (list tlp1750 ..3)
    (and tlp1751 tlp1752)
    tlp1753
    (list-rest
     tlp1754
     (list (list) tlp1755 (list) (box (list tlp1756 tlp1757 tlp1758)))
     (list-rest
      (list tlp1759 ___)
      (and (? number? tlp1760) (? even? tlp1761))
      (vector tlp1762 tlp1763 tlp1764 tlp1765)
      #t)
     #t))
   (list
    (and (? test-struct? tlp1777)
         (app test-struct-a tlp1766)
         (app test-struct-b (vector tlp1767 ...))
         (app test-struct-c (vector tlp1768 ___))
         (app
          test-struct-d
          (struct
           test-struct
           ((list tlp1769 tlp1770 tlp1771 tlp1772)
            (list tlp1773 ___)
            tlp1774
            #t))))
    tlp1778
    (and (? test-struct? tlp1798)
         (app
          test-struct-a
          (list
           (list-rest tlp1779 tlp1780 tlp1781 #\a)
           (and (? test-struct? tlp1787)
                (app test-struct-a tlp1782)
                (app test-struct-b tlp1783)
                (app test-struct-c tlp1784)
                (app test-struct-d tlp1785))
           (list tlp1788 ___)
           (box (list tlp1789 tlp1790 tlp1791))))
         (app test-struct-b (or '(+ 4 5) '(+ 4 5)))
         (app
          test-struct-c
          (struct test-struct (tlp1792 (vector tlp1793 ...) tlp1794 tlp1795)))
         (app test-struct-d _))
    (and (? test-struct? tlp1819)
         (app
          test-struct-a
          (box (list tlp1799 (list tlp1800 ..3) (and tlp1801 tlp1802))))
         (app
          test-struct-b
          (and (? test-struct? tlp1814)
               (app test-struct-a (list tlp1803 tlp1804 tlp1805 tlp1806))
               (app test-struct-b (vector tlp1807 tlp1808 tlp1809 tlp1810))
               (app test-struct-c (and (or tlp1811 tlp1811) (or tlp1812 tlp1812)))
               (app test-struct-d (or (list) (list)))))
         (app test-struct-c (box (list tlp1815 (list) tlp1816)))
         (app test-struct-d (vector tlp1817 ...))))
   )

  (pattern
   (list (list tlp1907 ___) (list) 'symbols 'these)
   (list
    (list #t (and (list) (list)) (and tlp1908 tlp1909) (list tlp1910 ___))
    (vector tlp1911 ___)
    (list-rest (not (not #\b)) '(+ 1 2) tlp1912 #\a)
    tlp1913)
   (list
    (vector
     (and (? test-struct? tlp1920)
          (app test-struct-a tlp1914)
          (app test-struct-b tlp1915)
          (app test-struct-c (box (list tlp1916 tlp1917 tlp1918)))
          (app test-struct-d #\b))
     (list-rest _ (list tlp1921 tlp1922 tlp1923 tlp1924) #f #\d)
     (and (? test-struct? tlp1936)
          (app test-struct-a (box (list tlp1925 tlp1926 tlp1927)))
          (app test-struct-b (list tlp1928 tlp1929 tlp1930 tlp1931))
          (app test-struct-c (box (list tlp1932 tlp1933 tlp1934)))
          (app test-struct-d _))
     (and (? test-struct? tlp1949)
          (app test-struct-a '(+ 4 5))
          (app
           test-struct-b
           (or (struct test-struct (tlp1937 tlp1938 tlp1939 tlp1940))
               (struct test-struct (tlp1937 tlp1938 tlp1939 tlp1940))))
          (app test-struct-c (list tlp1942 __3))
          (app
           test-struct-d
           (struct test-struct (tlp1943 tlp1944 tlp1945 tlp1946)))))
    #t
    _
    tlp1950)
   )

  (pattern
   (list
    (list-rest
     tlp1952
     (or (struct test-struct (2 tlp1953 tlp1954 (list)))
         (struct test-struct (2 tlp1953 tlp1954 (list))))
     (struct
      test-struct
      ((vector tlp1956 tlp1957 tlp1958 tlp1959)
       (and (vector tlp1960 ___) (vector tlp1961 ___))
       (or #t #t)
       (list)))
     'these)
    (not (not '(+ 4 (+ 1 1))))
    (vector tlp1963 ___)
    (list))
   (list
    (list tlp1964 ..3)
    (or (and (? number? tlp1965) (? even? tlp1966))
        (and (? number? tlp1965) (? even? tlp1966)))
    (and (? test-struct? tlp1978)
         (app test-struct-a tlp1967)
         (app test-struct-b 'symbols)
         (app test-struct-c tlp1968)
         (app
          test-struct-d
          (list-rest
           (vector tlp1969 ...)
           tlp1970
           (and (? test-struct? tlp1976)
                (app test-struct-a tlp1971)
                (app test-struct-b tlp1972)
                (app test-struct-c tlp1973)
                (app test-struct-d tlp1974))
           5)))
    (and (list) (list)))
   (list
    (list)
    tlp1979
    (struct
     test-struct
     ((and (? number? tlp1980) (? even? tlp1981))
      (list)
      (and (and (? test-struct? tlp1997)
                (app test-struct-a (list-rest tlp1982 tlp1983 tlp1984 tlp1985))
                (app test-struct-b (list-rest tlp1986 tlp1987 tlp1988 tlp1989))
                (app test-struct-c tlp1990)
                (app
                 test-struct-d
                 (struct test-struct (tlp1991 tlp1992 tlp1993 tlp1994))))
           (and (? test-struct? tlp1998)
                (app test-struct-a (list-rest tlp1999 tlp2000 tlp2001 tlp2002))
                (app test-struct-b (list-rest tlp2003 tlp2004 tlp2005 tlp2006))
                (app test-struct-c tlp2007)
                (app
                 test-struct-d
                 (struct test-struct (tlp2008 tlp2009 tlp2010 tlp2011)))))
      (list tlp2012 ..3)))
    tlp2014)
   )

  (pattern
   (list (not (not #t)) tlp2016 (list) tlp2017)
   (list
    (list-rest
     (list tlp2018 (not (not '(+ 1 2))) 'are tlp2019)
     (list
      (list tlp2020 tlp2021 tlp2022 tlp2023)
      (and (list-rest tlp2024 tlp2025 tlp2026 tlp2027)
           (list-rest tlp2028 tlp2029 tlp2030 tlp2031))
      (list tlp2032 __3)
      _)
     (and (? number? tlp2033) (? even? tlp2034))
     #\b)
    _
    (list tlp2035 __3)
    #f)
   (list
    (list
     (list tlp2036 ...)
     (and (and (? test-struct? tlp2042)
               (app test-struct-a tlp2037)
               (app test-struct-b 7)
               (app test-struct-c (box (list tlp2038 tlp2039 tlp2040)))
               (app test-struct-d (not (not #t))))
          (and (? test-struct? tlp2043)
               (app test-struct-a tlp2044)
               (app test-struct-b 7)
               (app test-struct-c (box (list tlp2045 tlp2046 tlp2047)))
               (app test-struct-d (not (not #t)))))
     (and (vector
           (and tlp2048 tlp2049)
           (list tlp2050 tlp2051 tlp2052 tlp2053)
           (and tlp2054 tlp2055)
           tlp2056)
          (vector
           (and tlp2057 tlp2058)
           (list tlp2059 tlp2060 tlp2061 tlp2062)
           (and tlp2063 tlp2064)
           tlp2065))
     #\c)
    'symbols
    (list)
    (not (not '(+ 4 5))))
   )

  (pattern
   (list (and (? number? tlp2067) (? even? tlp2068)) _ (or 'these 'these) 8)
   (list
    (list tlp2069 ..3)
    (list tlp2070 ..3)
    (box
     (list (not (not 'are)) (and (? number? tlp2071) (? even? tlp2072)) tlp2073))
    (list-rest
     (vector (list) tlp2074 tlp2075 (list-rest tlp2076 tlp2077 tlp2078 'symbols))
     tlp2079
     (and (? test-struct? tlp2086)
          (app test-struct-a (vector tlp2080 tlp2081 tlp2082 tlp2083))
          (app test-struct-b _)
          (app test-struct-c (not (not #\c)))
          (app test-struct-d (list tlp2084 __3)))
     'are))
   (list
    #\b
    _
    (struct
     test-struct
     ((list tlp2087 ...)
      (and (? number? tlp2088) (? even? tlp2089))
      (or tlp2090 tlp2090)
      (vector (not (not '(+ 1 2))) tlp2091 (list) tlp2092)))
    tlp2094)
   )

  (pattern
   (list
    (and (? number? tlp2096) (? even? tlp2097))
    (and (? test-struct? tlp2105)
         (app test-struct-a #\b)
         (app
          test-struct-b
          (or (and (list tlp2098 ___) (list tlp2099 ___))
              (and (list tlp2098 ___) (list tlp2099 ___))))
         (app test-struct-c (and (? number? tlp2100) (? even? tlp2101)))
         (app test-struct-d (and (? number? tlp2102) (? even? tlp2103))))
    #t
    #\a)
   (list
    (and (? test-struct? tlp2128)
         (app test-struct-a _)
         (app test-struct-b (list))
         (app
          test-struct-c
          (struct
           test-struct
           ((struct test-struct (tlp2106 tlp2107 tlp2108 tlp2109))
            (vector tlp2111 ...)
            tlp2112
            (vector tlp2113 tlp2114 tlp2115 tlp2116))))
         (app
          test-struct-d
          (vector
           (box (list tlp2118 tlp2119 tlp2120))
           (struct test-struct (tlp2121 tlp2122 tlp2123 tlp2124))
           tlp2126
           (list))))
    (and (? test-struct? tlp2139)
         (app test-struct-a _)
         (app
          test-struct-b
          (and (? test-struct? tlp2134)
               (app test-struct-a _)
               (app test-struct-b (vector tlp2129 ___))
               (app test-struct-c (list-rest tlp2130 tlp2131 tlp2132 'symbols))
               (app test-struct-d 'these)))
         (app test-struct-c (and (? number? tlp2135) (? even? tlp2136)))
         (app test-struct-d tlp2137))
    (vector tlp2140 ...)
    tlp2141)
   (list
    (struct
     test-struct
     ((list-rest
       (struct test-struct (tlp2142 tlp2143 tlp2144 tlp2145))
       (not (not #f))
       tlp2147
       #t)
      _
      tlp2148
      (list-rest
       (and (? test-struct? tlp2154)
            (app test-struct-a tlp2149)
            (app test-struct-b tlp2150)
            (app test-struct-c tlp2151)
            (app test-struct-d tlp2152))
       (and tlp2155 tlp2156)
       (box (list tlp2157 tlp2158 tlp2159))
       #\b)))
    (list tlp2161 __3)
    'these
    (and (? number? tlp2162) (? even? tlp2163)))
   )

  (pattern
   (list (not (not #\b)) '(+ 4 (+ 1 1)) (vector tlp2260 ...) _)
   (list
    (list)
    (and (? test-struct? tlp2264)
         (app test-struct-a (not (not #\a)))
         (app test-struct-b (vector tlp2261 ...))
         (app test-struct-c (or (list) (list)))
         (app
          test-struct-d
          (list-rest '(+ 1 2) (list tlp2262 ...) (not (not #\b)) 'these)))
    (or (list tlp2265 __3) (list tlp2265 __3))
    (list))
   (list
    (list tlp2266 ___)
    (struct test-struct ((list tlp2267 __3) (not (not 'are)) tlp2268 (list)))
    (list tlp2270 __3)
    (and #f #f))
   )

  (pattern
   (list
    tlp2273
    (box
     (list
      (struct
       test-struct
       ((vector tlp2274 ___)
        (vector tlp2275 ___)
        (vector tlp2276 ...)
        (and (list tlp2277 ___) (list tlp2278 ___))))
      (and (? number? tlp2280) (? even? tlp2281))
      (list (and tlp2282 tlp2283) tlp2284 _ (list tlp2285 ___))))
    _
    (and (? number? tlp2286) (? even? tlp2287)))
   (list
    tlp2288
    (and (list-rest
          (list-rest
           '(+ 4 5)
           (list tlp2289 tlp2290 tlp2291 tlp2292)
           (not (not #\a))
           #\b)
          (list-rest
           (list)
           #\a
           (and (? number? tlp2293) (? even? tlp2294))
           'these)
          (list tlp2295 ___)
          6)
         (list-rest
          (list-rest
           '(+ 4 5)
           (list tlp2296 tlp2297 tlp2298 tlp2299)
           (not (not #\a))
           #\b)
          (list-rest
           (list)
           #\a
           (and (? number? tlp2300) (? even? tlp2301))
           'these)
          (list tlp2302 ___)
          6))
    (box
     (list
      (list)
      _
      (and (? test-struct? tlp2306)
           (app test-struct-a tlp2303)
           (app test-struct-b (list))
           (app test-struct-c #t)
           (app test-struct-d (or (list tlp2304 ..3) (list tlp2304 ..3))))))
    (struct
     test-struct
     (tlp2307
      (list)
      (not (not 'symbols))
      (or (and (? number? tlp2308) (? even? tlp2309))
          (and (? number? tlp2308) (? even? tlp2309))))))
   (list tlp2311 (or tlp2312 tlp2312) (vector tlp2313 ___) tlp2314)
   )

  (pattern
   (list
    (not (not #\b))
    _
    (list
     (list tlp2317 ...)
     (list tlp2318 ..3)
     (vector
      tlp2319
      _
      (box (list tlp2320 tlp2321 tlp2322))
      (struct test-struct (tlp2323 tlp2324 tlp2325 tlp2326)))
     '(+ 4 5))
    (list tlp2328 __3))
   (list
    tlp2329
    (list tlp2330 ...)
    (list
     (struct
      test-struct
      (_ (struct test-struct (tlp2331 tlp2332 tlp2333 tlp2334)) _ (list)))
     tlp2337
     (and (and (box (list tlp2338 tlp2339 tlp2340))
               (box (list tlp2341 tlp2342 tlp2343)))
          (and (box (list tlp2344 tlp2345 tlp2346))
               (box (list tlp2347 tlp2348 tlp2349))))
     (and #\d #\d))
    (list-rest
     (list-rest
      (struct test-struct (tlp2350 tlp2351 tlp2352 tlp2353))
      (or (struct test-struct (tlp2355 tlp2356 tlp2357 tlp2358))
          (struct test-struct (tlp2355 tlp2356 tlp2357 tlp2358)))
      (list-rest tlp2360 tlp2361 tlp2362 #\c)
      #\a)
     (list tlp2363 ___)
     (list)
     3))
   (list
    (and (? test-struct? tlp2396)
         (app
          test-struct-a
          (and (? test-struct? tlp2371)
               (app test-struct-a tlp2364)
               (app test-struct-b tlp2365)
               (app test-struct-c (list tlp2366 tlp2367 tlp2368 tlp2369))
               (app test-struct-d (list))))
         (app
          test-struct-b
          (struct
           test-struct
           ((and (? test-struct? tlp2377)
                 (app test-struct-a tlp2372)
                 (app test-struct-b tlp2373)
                 (app test-struct-c tlp2374)
                 (app test-struct-d tlp2375))
            (list tlp2378 ..3)
            (and (? number? tlp2379) (? even? tlp2380))
            (vector tlp2381 ...))))
         (app
          test-struct-c
          (box
           (list
            (list tlp2383 ..3)
            (and (? test-struct? tlp2389)
                 (app test-struct-a tlp2384)
                 (app test-struct-b tlp2385)
                 (app test-struct-c tlp2386)
                 (app test-struct-d tlp2387))
            tlp2390)))
         (app
          test-struct-d
          (and (and (? number? tlp2391) (? even? tlp2392))
               (and (? number? tlp2393) (? even? tlp2394)))))
    (list
     '(+ 1 2)
     (vector tlp2397 ___)
     (struct
      test-struct
      ((vector tlp2398 tlp2399 tlp2400 tlp2401)
       (and (? number? tlp2402) (? even? tlp2403))
       tlp2404
       (list-rest tlp2405 tlp2406 tlp2407 'symbols)))
     _)
    (struct
     test-struct
     ((list tlp2409 ..3)
      (not (not #\a))
      tlp2410
      (and (? number? tlp2411) (? even? tlp2412))))
    (list))
   )

  (pattern
   (list
    tlp2416
    (not (not #t))
    (or (list-rest
         (and (list tlp2417 ...) (list tlp2418 ...))
         (vector
          (list-rest tlp2419 tlp2420 tlp2421 tlp2422)
          #\c
          tlp2423
          (list-rest tlp2424 tlp2425 tlp2426 tlp2427))
         9
         'are)
        (list-rest
         (and (list tlp2417 ...) (list tlp2418 ...))
         (vector
          (list-rest tlp2419 tlp2420 tlp2421 tlp2422)
          #\c
          tlp2423
          (list-rest tlp2424 tlp2425 tlp2426 tlp2427))
         9
         'are))
    (list
     tlp2428
     (and (? number? tlp2429) (? even? tlp2430))
     (not (not '(+ 4 5)))
     (or (struct
          test-struct
          ((box (list tlp2431 tlp2432 tlp2433))
           #f
           (vector tlp2434 tlp2435 tlp2436 tlp2437)
           _))
         (struct
          test-struct
          ((box (list tlp2431 tlp2432 tlp2433))
           #f
           (vector tlp2434 tlp2435 tlp2436 tlp2437)
           _)))))
   (list
    (vector
     (list)
     (not (not #\d))
     (box (list (not (not 9)) (list) 'symbols))
     (list))
    _
    tlp2439
    (list-rest tlp2440 _ tlp2441 #t))
   (list
    (or (not (not #\b)) (not (not #\b)))
    (list tlp2442 (list) (list) (and (vector tlp2443 ...) (vector tlp2444 ...)))
    (list tlp2445 ___)
    (or (and (? number? tlp2446) (? even? tlp2447))
        (and (? number? tlp2446) (? even? tlp2447))))
   )

  (pattern
   (list
    (not (not #t))
    tlp2450
    (or (box
         (list
          (struct
           test-struct
           ((vector tlp2451 ...) #\d (or tlp2452 tlp2452) (vector tlp2453 ___)))
          (list tlp2455 ..3)
          tlp2456))
        (box
         (list
          (struct
           test-struct
           ((vector tlp2451 ...) #\d (or tlp2452 tlp2452) (vector tlp2453 ___)))
          (list tlp2455 ..3)
          tlp2456)))
    (or (list tlp2457 ...) (list tlp2457 ...)))
   (list
    (list-rest
     (box (list (or _ _) (list) (list tlp2458 ..3)))
     tlp2459
     (vector tlp2460 ___)
     #t)
    (and (? test-struct? tlp2470)
         (app test-struct-a (not (not #\b)))
         (app
          test-struct-b
          (struct
           test-struct
           ((list)
            (not (not #\b))
            'symbols
            (or (vector tlp2461 tlp2462 tlp2463 tlp2464)
                (vector tlp2461 tlp2462 tlp2463 tlp2464)))))
         (app test-struct-c #\d)
         (app
          test-struct-d
          (box (list (and tlp2466 tlp2467) (vector tlp2468 ___) _))))
    (vector tlp2471 ___)
    (not (not 9)))
   (list
    (vector
     (and (? number? tlp2472) (? even? tlp2473))
     (list)
     (struct
      test-struct
      ((vector tlp2474 tlp2475 tlp2476 tlp2477)
       (list tlp2478 ___)
       (list tlp2479 ..3)
       '(+ 1 2)))
     (or _ _))
    (list tlp2481 ..3)
    tlp2482
    (and (? number? tlp2483) (? even? tlp2484)))
   )

  ))

(report-errs)
