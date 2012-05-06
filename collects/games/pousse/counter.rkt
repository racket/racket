;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Rate a move by a combination of counting strategies
;;
;;  This code benefits greatly from mzc compilation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module counter mzscheme
  (require "board.rkt"
           "utils.rkt"
           mzlib/unitsig)
  
  (provide params^
           counter@)
  
  (define-signature params^
    (US-PIECE-WEIGHT
     US-MIDDLE-PIECE-WEIGHT
     US-LINE-PIECE-WEIGHT
     US-CONNECT-PIECE-WEIGHT
     
     THEM-PIECE-WEIGHT
     THEM-MIDDLE-PIECE-WEIGHT
     THEM-LINE-PIECE-WEIGHT
     THEM-CONNECT-PIECE-WEIGHT
     
     turn-number))
  
  
  (define counter@
    (unit/sig ()
      (import (n) params^)
      
      (define (counter-based-goodness board orig-board as-player depth)
        (let ([usses 0]
              [thems 0]
              [middle-usses 0]
              [middle-thems 0]
              [us-in-line 0]
              [them-in-line 0]
              [us-connect 0]
              [them-connect 0])
          (let ([traverse
                 (lambda (board-cell)
                   (n-times n (lambda (i)
                                (let ([start-usses usses]
                                      [start-thems thems]
                                      [prev #f])
                                  (n-times n (lambda (j)
                                               (let ([v (board-cell board i j)])
                                                 (cond
                                                   [(eq? v as-player) (set! usses (add1 usses))
                                                    (set! middle-usses (+ (min i (- (sub1 n) i)) middle-usses))
                                                    (when (eq? v prev)
                                                      (set! us-connect (add1 us-connect)))]
                                                   [(eq? v none) 'nothing]
                                                   [else (set! thems (add1 thems))
                                                         (set! middle-thems (+ (min i (- (sub1 n) i)) middle-thems))
                                                         (when (eq? v prev)
                                                           (set! them-connect (add1 them-connect)))])
                                                 (set! prev v))))
                                  (set! us-in-line (+ us-in-line (* (- usses start-usses) (- usses start-usses))))
                                  (set! them-in-line (+ them-in-line (* (- thems start-thems) (- thems start-thems))))))))])
            ;; Go down columns
            (traverse board-cell)
            ;; Go across rows
            (traverse (lambda (b x y) (board-cell b y x)))
            ;; Counted usses and thems twice
            (set! usses (/ usses 2))
            (set! thems (/ thems 2)))
          
          '(when (= depth 2)
             (eprintf "us: ~a them: ~a  u-m:~a t-m: ~a u-l: ~a t-l: ~a u-c: ~a t-c: ~a\n"
                      usses thems
                      middle-usses middle-thems
                      us-in-line them-in-line
                      us-connect them-connect))
          
          (+ 0
             
             ;; If it's early in the game, bias towards the middle
             (* middle-usses US-MIDDLE-PIECE-WEIGHT)
             (* middle-thems THEM-MIDDLE-PIECE-WEIGHT)
             
             ;; We'd prefer to get rid of thems and not get rid of usses
             (* usses US-PIECE-WEIGHT)
             (* thems THEM-PIECE-WEIGHT)
             
             ;; Working towards a line is good
             (* us-in-line US-LINE-PIECE-WEIGHT)
             (* them-in-line THEM-LINE-PIECE-WEIGHT)
             
             ;; Working towards connected parts is good
             (* us-connect US-CONNECT-PIECE-WEIGHT)
             (* them-connect THEM-CONNECT-PIECE-WEIGHT)
             
             
             ;;;;;;;;;;;;; High-level strategy ;;;;;;;;;;;;;;;;;
             
             ;; o: don't start on the same or opposite edge unless X is immediately
             ;; pushed.
             (if (and (= turn-number 1) (= usses 1) (= thems 1)
                      (ormap (lambda (x) x) 
                             (n-map n (lambda (i) (or (eq? (board-cell board i 0) x)
                                                      (eq? (board-cell board i (sub1 n)) x))))))
                 -500
                 0)
             
             ;; x, first reply: always meddle with o's move if it's not in a corner
             (if (and (= turn-number 2) (= usses 2) (= thems 1)
                      (not (or (eq? (board-cell orig-board 0 0) o)
                               (eq? (board-cell orig-board 0 (sub1 n)) o)
                               (eq? (board-cell orig-board (sub1 n) 0) o)
                               (eq? (board-cell orig-board (sub1 n) 0) o))))
                 
                 (if (ormap (lambda (x) x) (n-map (- n 2) (lambda (i) (eq? (board-cell board (add1 i) 1) o))))
                     ;; We pushed it
                     500
                     
                     ;; Do a push, instead
                     -500)
                 
                 0)
             
             0)))
      
      counter-based-goodness)))
