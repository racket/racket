#lang racket

#|

This is a file from Guillaume that ran very slowly with the 
htdp/image library; here it is used as a performance test.
Porting to #lang racket +2htdp/image consisted of adding requires, 
changing overlay/xy to underlay/xy,  defining empty-scene, and
adding the check-expect macro (and related code).
Also added the timing code at the end.

|#


(require 2htdp/image
         (only-in mrlib/image-core 
                  skip-image-equality-fast-path))

(define-syntax (check-expect stx)
  (syntax-case stx ()
    [(_ a b)
     (with-syntax ([line (syntax-line stx)])
       #'(set! tests (cons (list (λ () a) (λ () b) line)
                           tests)))]))
(define tests '())
(define (run-tests)
  (for-each
   (λ (l)
     (let ([a-res ((list-ref l 0))]
           [b-res ((list-ref l 1))]
           [line (list-ref l 2)])
       (unless (equal? a-res b-res)
         (error 'test "test failed; expected ~s and ~s to be equal, but they weren't, line ~a"
                a-res
                b-res
                line))))
   tests))
#;
(define (empty-scene w h) 
  (overlay
   (rectangle w h 'solid 'white)
   (rectangle w h 'outline 'black)))

;;Program for creating game of croos-circle game
;;contract :image->image

;;defining a union square
;;A square is either
;;A square is blank
;;A square is cross
;;A square is Circle

;;defining width of square
(define square-width 150)

;;defining th height and width of scene
(define width (* square-width 3))
(define height (* square-width 3))


;;defining the image circle 
(define Circle (underlay/xy (circle  20 'solid 'orange) 0 0 (circle  10 'solid 'white)))
;;defining the image cross 
(define cross (underlay/xy (rectangle 10 30 'solid 'green) 0 0 (rectangle 30 10 'solid 'green)))
;;defining the blank image
(define blank (underlay/xy (rectangle square-width square-width 'solid  'red) 0 0
                          (rectangle (- square-width 8) (- square-width 8) 'solid 'white)))

;;Given  a square returns 
;;the image of square
;;draw-square :square ->image
(define (draw-square square)
  (cond[(equal? 'Circle  square)(underlay/xy blank 0 0 Circle)]
       [(equal? 'cross square)(underlay/xy blank 0 0 cross)]
       [(equal? 'blank square)blank]
       ))


;;test
(check-expect(draw-square 'Circle)(underlay/xy blank 0 0 Circle))
(check-expect(draw-square 'cross)(underlay/xy blank 0 0 cross))
(check-expect(draw-square 'blank)blank)

;;== Cross and circles, part #3 ==


;;define a structure for ROW
;;ROW structure used for creating a ROW in the board
;;contract ROW:image image image->image
(define-struct ROW (left middle right) #:transparent)


;; defining a blank row

(define blank-ROW (make-ROW 'blank 'blank 'blank))
;;defining the cross row
(define cross-ROW (make-ROW 'blank 'cross 'blank))

;;defineing the cross-row-blank secoend combination
(define  cross-ROW-blank (make-ROW 'cross 'cross 'blank ))
;;defining a row cross-row
(define cross-row (make-ROW 'cross 'cross 'cross ))
;;defining a row blank-circle
(define blank-circle (make-ROW 'Circle 'blank 'blank))
;;defining a row cross-circle
(define cross-circle  (make-ROW 'cross 'cross 'Circle ))
;;defining a row circle-cross
(define circle-cross (make-ROW 'cross 'Circle 'Circle ))
;;defining a row cross-blank
(define cross-blank (make-ROW 'cross 'blank 'blank ))
;;function for  creating ROW with the square
;;contract:square square square->ROW
;template: for draw-row
;template for ROW
;(define (a-row-function a-row)
;  ... (row-left a-row)      ;; is a square
;  ... (row-mid a-row)      ;; is a square
;  ... (row-right a-row))    ;; is a square



(define (draw-row row)
  (underlay/xy (draw-square(ROW-left row)) (image-width blank) 0
              (underlay/xy (draw-square(ROW-middle row))  (image-width blank) 0 (draw-square(ROW-right row)) )))

;;test

(check-expect (draw-row (make-ROW 'Circle 'cross 'blank))
              (underlay/xy (draw-square 'Circle) (image-width blank) 0
                          (underlay/xy (draw-square 'cross )  (image-width blank) 0 (draw-square 'blank) )))

(check-expect (draw-row (make-ROW 'Circle 'cross 'blank))
              (underlay/xy (draw-square 'Circle) (image-width blank) 0
                          (underlay/xy (draw-square 'cross )  (image-width blank) 0 (draw-square 'blank) )))

(check-expect (draw-row (make-ROW 'Circle  'blank 'cross))
              (underlay/xy (draw-square 'Circle) (image-width blank) 0
                          (underlay/xy (draw-square 'blank )  (image-width blank) 0 (draw-square 'cross) )))

(check-expect (draw-row cross-ROW-blank)
              (underlay/xy (draw-square 'cross) (image-width blank) 0
                          (underlay/xy (draw-square 'cross )  (image-width blank) 0 (draw-square 'blank) )))

(check-expect (draw-row cross-row )
              (underlay/xy (draw-square 'cross) (image-width blank) 0
                          (underlay/xy (draw-square 'cross )  (image-width blank) 0 (draw-square 'cross) )))

;;define a structure for BOARD
;;contract make-BOARD :image image image->image
(define-struct BOARD (top-row center-row bottom-row) #:transparent)

;; purpose : defining an empty board
(define empty-board (make-BOARD blank-ROW
                                blank-ROW 
                                blank-ROW))

;;function for creating board with the row

;template: for draw-board
;(define (a-board-function a-row)
;  ... (top-row a-row)      ;; is a square
;  ... (center-row a-row)      ;; is a square
;  ... (bottom-row a-row))    ;; is a square

;;defining the background
(define background (empty-scene width height))


;;this function will reusing the fuction draw-row for creating row
;;contract:row row row->board

;;test
(check-expect (draw-board (make-BOARD cross-ROW-blank
                                      cross-ROW 
                                      cross-row ))
              (underlay/xy (draw-row cross-ROW-blank) 
                          0 (image-height (draw-row cross-ROW))
                          (underlay/xy (draw-row cross-ROW) 
                                      0 (image-height (draw-row cross-ROW))
                                      (draw-row cross-row ))))

(check-expect (draw-board (make-BOARD  cross-circle
                                       (make-ROW 'Circle 'cross 'blank) 
                                       circle-cross))
              (underlay/xy (draw-row  cross-circle) 
                          0 (image-height (draw-row  cross-circle))
                          (underlay/xy (draw-row (make-ROW 'Circle 'cross 'blank)) 
                                      0 (image-height (draw-row(make-ROW 'Circle 'cross 'blank)))
                                      (draw-row circle-cross))))

(check-expect(draw-board (make-BOARD  cross-circle
                                      (make-ROW 'Circle 'cross 'Circle) 
                                      circle-cross))
             (underlay/xy (draw-row  cross-circle) 
                         0 (image-height (draw-row  cross-circle))
                         (underlay/xy (draw-row  (make-ROW 'Circle 'cross 'Circle)) 
                                     0 (image-height (draw-row (make-ROW 'Circle 'cross 'Circle)))
                                     (draw-row circle-cross))))

(check-expect (draw-board (make-BOARD (make-ROW 'blank 'cross 'Circle)
                                      (make-ROW 'Circle 'cross 'cross) 
                                      circle-cross))              
              (underlay/xy (draw-row (make-ROW 'blank 'cross 'Circle)) 
                          0 (image-height (draw-row (make-ROW 'blank 'cross 'Circle)))
                          (underlay/xy (draw-row  (make-ROW 'Circle 'cross 'cross)) 
                                      0 (image-height (draw-row (make-ROW 'Circle 'cross 'cross)))
                                      (draw-row circle-cross))) )

(check-expect (draw-board (make-BOARD (make-ROW 'blank 'cross 'Circle)
                                      (make-ROW 'Circle 'blank 'cross) 
                                      (make-ROW 'cross 'blank 'Circle)))
              (underlay/xy (draw-row (make-ROW 'blank 'cross 'Circle)) 
                          0 (image-height (draw-row (make-ROW 'blank 'cross 'Circle)))
                          (underlay/xy (draw-row  (make-ROW 'Circle 'blank 'cross)) 
                                      0 (image-height (draw-row (make-ROW 'Circle 'blank 'cross)))
                                      (draw-row (make-ROW 'cross 'blank 'Circle)))))




(define (draw-board board)
  (underlay/xy (draw-row (BOARD-top-row board))
              0 (image-height (draw-row (BOARD-top-row board)))
              (underlay/xy (draw-row (BOARD-center-row board)) 
                          0 (image-height (draw-row(BOARD-center-row board))) 
                          (draw-row (BOARD-bottom-row board)))))                

;;purpose: given the x coordinate of the mouse click  and returns 
;;the symbol 'L, the symbol 'M, or the symbol 'R,
;;depending on whether that X position falls on the right, the middle or the left of the board.
;;contract: which-column:: number -> symbol

;;test 

(check-expect (which-column (* square-width .5)) 'L)
(check-expect (which-column (* square-width 1.5)) 'M)
(check-expect (which-column (* square-width 2.3)) 'R)

(define (which-column x-pos)
  (cond[(and (>= x-pos 0)(<= x-pos square-width))'L]
       [(and (>= x-pos (+ square-width 1))(<= x-pos (* 2 square-width)))'M]
       [(and (>= x-pos (+ (* 2 square-width) 1))(<= x-pos (* 3 square-width)))'R]
       [else "play in the board,you played outside the square"]))



;;purpose: given the y coordinate of the mouse click  and returns 
;;the symbol 'T, the symbol 'C, or the symbol 'B,
;;depending on whether that Y position falls on the top, the center or the bottom of the board.
;;contract: which-row:: number -> symbol

;;test 

(check-expect (which-row (* square-width .6)) 'T)
(check-expect (which-row (* square-width 1.3)) 'C)
(check-expect (which-row (* square-width 2.7)) 'B)

(define (which-row y-pos)
  (cond[(and (>= y-pos 0)(<= y-pos square-width))'T]
       [(and (>= y-pos (+ square-width 1))(<= y-pos (* 2 square-width)))'C]
       [(and (>= y-pos (+ (* 2 square-width) 1))(<= y-pos (* 3 square-width)))'B]
       [else "play in the board,you played outside the square"]))



;;purpose: give the row and the square to be played and returns a new row replacing the left square
;; play-on-left : row square ->row

;;test
(check-expect (play-on-left (make-ROW 'blank 'cross 'Circle) 'Circle)
              (make-ROW 'Circle 'cross 'Circle))

(check-expect (play-on-left (make-ROW 'blank 'cross 'Circle) 'cross)
              cross-circle)

(check-expect (play-on-left cross-ROW 'Circle)
              (make-ROW 'Circle 'cross 'blank))
(define (play-on-left row play)
  (make-ROW play (ROW-middle row) (ROW-right row)))


;;purpose: give the row and the square to be played and returns a new row replacing the middle square
;; play-on-middle : row square ->row

;;test
(check-expect (play-on-middle (make-ROW 'blank 'blank 'Circle) 'Circle)
              (make-ROW 'blank 'Circle 'Circle))

(check-expect (play-on-middle (make-ROW 'blank 'blank 'Circle) 'cross)
              (make-ROW 'blank 'cross 'Circle))

(check-expect (play-on-middle blank-ROW 'Circle)
              (make-ROW 'blank 'Circle 'blank))

(define (play-on-middle row play)
  (make-ROW (ROW-left row) play  (ROW-right row)))


;;purpose: give the row and the square to be played and returns a new row replacing the right square
;; play-on-right : row square ->row

;;test
(check-expect (play-on-right blank-ROW 'Circle)
              (make-ROW 'blank 'blank 'Circle))

(check-expect (play-on-right (make-ROW 'blank 'Circle 'blank) 'cross)
              (make-ROW 'blank 'Circle 'cross))

(check-expect (play-on-right blank-ROW 'Circle)
              (make-ROW 'blank 'blank 'Circle))

(define (play-on-right row play)
  (make-ROW (ROW-left row) (ROW-middle row) play  ))

;;purpose : given the row, which column ,square to be played returns new row replacing the column
;; play-on-row : row square symbol -> row

(check-expect (play-on-row blank-ROW 'L 'Circle)
              (make-ROW 'Circle 'blank 'blank))
(check-expect (play-on-row blank-ROW 'M 'Circle)
              (make-ROW 'blank 'Circle 'blank))
(check-expect (play-on-row blank-ROW 'R 'Circle)
              (make-ROW 'blank 'blank 'Circle))

(define (play-on-row row column-label play)
  (cond [(equal? column-label 'L) (make-ROW play  (ROW-middle row) (ROW-right row))]
        [(equal? column-label 'M) (make-ROW (ROW-left row) play  (ROW-right row))]
        [(equal? column-label 'R) (make-ROW  (ROW-left row) (ROW-middle row) play)]
        [else row]))

;;purpose given a board, a square to be played and the label of the position to be played
;;returns a new board with the square to be played at the labeled position on the top row

;; play-on-board-at-top : board square symbol -> board
;;test
(check-expect (play-on-board-at-top empty-board  'Circle 'L)
              (make-BOARD (make-ROW 'Circle 'blank 'blank)
                          blank-ROW 
                          blank-ROW))


(check-expect (play-on-board-at-top empty-board  'Circle 'M)
              (make-BOARD (make-ROW 'blank 'Circle 'blank)
                          blank-ROW 
                          blank-ROW))


(check-expect (play-on-board-at-top empty-board  'cross 'R)
              (make-BOARD (make-ROW 'blank 'blank 'cross)
                          blank-ROW 
                          blank-ROW))


(define (play-on-board-at-top board play column-label)
  (make-BOARD(play-on-row (BOARD-top-row board) column-label play)
             (BOARD-center-row board)(BOARD-bottom-row board))
  )



;;purpose given a board, a square to be played and the label of the position to be played
;;returns a new board with the square to be played at the labeled position on the middle row

;; play-on-board-at-top : board square symbol -> board
;;test
(check-expect (play-on-board-at-middle empty-board  'Circle 'L)
              (make-BOARD blank-ROW
                          (make-ROW 'Circle 'blank 'blank) 
                          blank-ROW))


(check-expect (play-on-board-at-middle empty-board  'Circle 'M)
              (make-BOARD blank-ROW
                          (make-ROW 'blank 'Circle 'blank) 
                          blank-ROW))


(check-expect (play-on-board-at-middle empty-board  'cross 'R)
              (make-BOARD blank-ROW
                          (make-ROW 'blank 'blank  'cross) 
                          blank-ROW))


(define (play-on-board-at-middle board play column-label)
  (make-BOARD (BOARD-top-row board) (play-on-row (BOARD-center-row board) column-label play)
              (BOARD-bottom-row board))
  )
;;purpose given a board, a square to be played and the label of the position to be played
;;returns a new board with the square to be played at the labeled position on the bottom row

;; play-on-board-at-top : board square symbol -> board
;;test
(check-expect (play-on-board-at-bottom empty-board  'Circle 'L)
              (make-BOARD blank-ROW
                          blank-ROW 
                          (make-ROW 'Circle 'blank 'blank)))


(check-expect (play-on-board-at-bottom empty-board  'Circle 'M)
              (make-BOARD blank-ROW
                          blank-ROW 
                          (make-ROW 'blank 'Circle 'blank)))


(check-expect (play-on-board-at-bottom empty-board  'cross 'R)
              (make-BOARD blank-ROW
                          blank-ROW 
                          (make-ROW 'blank 'blank 'cross)))


(define (play-on-board-at-bottom board play column-label)
  (make-BOARD (BOARD-top-row board) (BOARD-center-row board) 
              (play-on-row (BOARD-bottom-row board)  column-label play)
              )
  )


;;purpose :given the board ,square to be played,column and row label and returns a new board 
;;with the square to be  played at the position referred
;; play-on-board : board square symbol symbol -> board

;;test
(check-expect (play-on-board empty-board  'cross 'R 'T)
              (make-BOARD (make-ROW 'blank 'blank 'cross )
                          blank-ROW 
                          blank-ROW))


(check-expect (play-on-board empty-board  'cross 'L 'C)
              (make-BOARD blank-ROW
                          cross-blank 
                          blank-ROW))


(check-expect (play-on-board empty-board  'cross 'M 'B)
              (make-BOARD blank-ROW
                          blank-ROW 
                          cross-ROW))


(define (play-on-board board play column-label row-label)
  (cond [(equal? row-label 'T) (play-on-board-at-top board play column-label)]
        [(equal? row-label 'C) (play-on-board-at-middle board play column-label)]
        [(equal? row-label 'B) (play-on-board-at-bottom board play column-label)]
        [else board]))


;;purpose : Given a board structure, a return the image of that board centered on the scene.
;;create-board:board->scene

;;test
(check-expect (create-board  (make-BOARD blank-ROW
                                         blank-ROW 
                                         cross-ROW))
              (place-image (draw-board (make-BOARD blank-ROW
                                                   blank-ROW 
                                                   cross-ROW))
                           (/ square-width 2)(/ square-width 2) background))

(check-expect (create-board  (make-BOARD (make-ROW 'Circle 'cross 'Circle) 
                                         blank-ROW 
                                         cross-ROW))
              (place-image (draw-board (make-BOARD (make-ROW 'Circle 'cross 'Circle)
                                                   blank-ROW 
                                                   cross-ROW))
                           (/ square-width 2)(/ square-width 2) background))

(check-expect (create-board  (make-BOARD (make-ROW 'Circle 'cross 'blank) 
                                         blank-ROW 
                                         cross-ROW))
              (place-image (draw-board (make-BOARD (make-ROW 'Circle 'cross 'blank)
                                                   blank-ROW 
                                                   cross-ROW))
                           (/ square-width 2)(/ square-width 2) background))

(define (create-board board)
  (place-image (draw-board board)(/ square-width 2)(/ square-width 2)  background)
  )

;; clack1 : Mouse handler. Plays a cross (always a cross) where the mouse is clicked, on button-up.
;; clack1 : board number number symbol -> board

(define (clack1 board x y event)
  (cond [(symbol=? event 'button-up)
         (play-on-board board 'cross (which-column x) (which-row y))]
        [else board]))

(check-expect (clack1 (make-BOARD blank-ROW
                                  blank-ROW 
                                  cross-ROW) 40 68 'button-up)
              (make-BOARD cross-blank
                          blank-ROW 
                          cross-ROW))

(check-expect (clack1 (make-BOARD blank-ROW
                                  blank-ROW 
                                  cross-ROW) 160 168 'button-up)
              (make-BOARD blank-ROW
                          (make-ROW 'blank 'cross 'blank)
                          cross-ROW))

(check-expect (clack1 (make-BOARD blank-ROW
                                  blank-ROW 
                                  blank-ROW) 310 365 'button-up)
              (make-BOARD blank-ROW
                          blank-ROW
                          (make-ROW  'blank 'blank 'cross)
                          )) 
;; purpose : Given the current player, return which player goes next.
;; other-player : square -> square

(define (other-player play)
  (cond [(equal? play 'Circle) 'cross]
        [(equal? play 'cross) 'Circle]))

(check-expect (other-player 'cross) 'Circle)
(check-expect (other-player 'Circle) 'cross)

;; purpose : Given a horz. pos (either 'L, 'M or 'R), finds the content of that square.
;; lookup-square : row symbol -> square

(define (lookup-square column-label row)
  (cond [(equal? column-label 'L)(ROW-left row)]
        [(equal? column-label 'M)(ROW-middle row)]
        [(equal? column-label 'R)(ROW-right row)]))

(check-expect(lookup-square 'L (make-ROW 'blank 'Circle 'cross)) 'blank)
(check-expect(lookup-square 'M (make-ROW 'blank 'Circle 'cross)) 'Circle)
(check-expect(lookup-square 'R (make-ROW 'blank 'Circle 'cross)) 'cross)

;; lookup-row : Given a vert. pos (either 'T, 'C or 'B), finds that row.
;; lookup-row : board symbol -> row

(define(lookup-row row-label board)
  (cond [(equal? row-label 'T)(BOARD-top-row board)]
        [(equal? row-label 'C)(BOARD-center-row board)]
        [(equal? row-label 'B)(BOARD-bottom-row board)]))


(check-expect(lookup-row 'T (make-BOARD (make-ROW 'cross 'blank 'Circle) 
                                        blank-ROW 
                                        blank-ROW)) (make-ROW 'cross 'blank 'Circle)) 

(check-expect(lookup-row 'C (make-BOARD blank-ROW
                                        (make-ROW 'cross 'blank 'Circle) 
                                        blank-ROW)) (make-ROW 'cross 'blank 'Circle))

(check-expect(lookup-row 'B (make-BOARD blank-ROW 
                                        blank-ROW
                                        (make-ROW 'cross 'blank 'Circle) 
                                        )) (make-ROW 'cross 'blank 'Circle))

;; lookup : Given a horz. and a vert. pos, finds that square.
;; lookup : board symbol symbol -> square

(define (lookup board  column-label row-label)
  (lookup-square column-label (lookup-row row-label board)))

(check-expect(lookup(make-BOARD (make-ROW 'cross 'blank 'Circle) 
                                blank-ROW 
                                blank-ROW) 'L 'T) 'cross)

(check-expect(lookup(make-BOARD blank-ROW
                                (make-ROW 'cross 'blank 'Circle) 
                                blank-ROW) 'M 'C) 'blank)

(check-expect(lookup(make-BOARD blank-ROW 
                                blank-ROW
                                (make-ROW 'cross 'blank 'Circle) 
                                ) 'R 'B) 'Circle)


;; move-legal? : Return true if the square at horizondal and vertical position is blank.
;; move-legal? : board symbol symbol -> boolean

(define(move-legal? board column-label row-label)
  (equal? (lookup board  column-label row-label) 'blank))

(check-expect (move-legal? empty-board 'L 'C) true)
(check-expect (move-legal? (make-BOARD blank-ROW
                                       (make-ROW 'Circle 'cross cross)
                                       blank-ROW)                                       
                           'M 'C) false)
;;define a structure for game
;;contract make-game :square board number->game
(define-struct GAME (next-player board move-count) #:transparent)

;;defining the initial-game
(define initial-game (make-GAME 'cross empty-board 0))

;;purpose: Given a game and a horz. and vert. position, the next player plays in that square, if legal. The move-count goes up by 1,and the next-player switches hand.
;; play-on-game : game symbol symbol -> game

(check-expect(play-on-game initial-game 'L 'T)
             (make-GAME 'Circle 
                        (make-BOARD cross-blank blank-ROW blank-ROW) 1))

(check-expect(play-on-game (make-GAME 'Circle 
                                      (make-BOARD cross-blank blank-ROW blank-ROW) 1)
                           'M 'C )
             (make-GAME 'cross 
                        (make-BOARD cross-blank
                                    (make-ROW 'blank 'Circle 'blank)
                                    blank-ROW) 2))
(check-expect(play-on-game(make-GAME 'cross 
                                     (make-BOARD cross-blank
                                                 (make-ROW 'blank 'Circle 'blank)
                                                 blank-ROW) 2)
                          'R 'B)
             (make-GAME 'Circle 
                        (make-BOARD cross-blank
                                    (make-ROW 'blank 'Circle 'blank)
                                    (make-ROW 'blank 'blank 'cross)) 3))

(define (play-on-game game column-label row-label)
  (cond [ (move-legal? (GAME-board game) column-label row-label)
          (make-GAME (other-player (GAME-next-player game)) 
                     (play-on-board (GAME-board game) (GAME-next-player game)  column-label row-label)
                     (+ (GAME-move-count game)  1))]
        [else game]))

;; game-over? : Returns true when the game is over. 
;; game-over? : game -> boolean
(check-expect (game-over? (make-GAME 'Circle (make-BOARD cross-blank
                                                         (make-ROW 'blank 'Circle 'blank)
                                                         (make-ROW 'blank 'blank 'cross))3)) false)
(check-expect (game-over? (make-GAME 'Circle (make-BOARD cross-ROW-blank
                                                         (make-ROW 'blank 'Circle 'blank)
                                                         (make-ROW 'blank 'blank 'cross))3))  false)
(check-expect (game-over? (make-GAME 'Circle (make-BOARD  cross-circle
                                                          (make-ROW 'cross 'Circle 'cross)
                                                          (make-ROW 'Circle 'cross 'Circle))9))true)
(define (game-over? game)
  (>= (GAME-move-count game) 9))



;; clack2 : Mouse handler. Plays the game on button-up.
;; clack2 : game number number symbol -> game

(check-expect (clack2 initial-game 90 90 'button-up)
              (make-GAME 'Circle 
                         (make-BOARD cross-blank blank-ROW blank-ROW) 1))

(check-expect (clack2 (make-GAME 'Circle 
                                 (make-BOARD cross-blank blank-ROW blank-ROW) 1)
                      160 160  'button-up)
              (make-GAME 'cross 
                         (make-BOARD cross-blank
                                     (make-ROW 'blank 'Circle 'blank)
                                     blank-ROW) 2))

(check-expect (clack2 (make-GAME 'cross 
                                 (make-BOARD cross-blank
                                             (make-ROW 'blank 'Circle 'blank)
                                             blank-ROW) 2)310 310 'button-up)
              (make-GAME 'Circle (make-BOARD cross-blank
                                             (make-ROW 'blank 'Circle 'blank)
                                             (make-ROW 'blank 'blank 'cross)) 3))             


(define (clack2 game x y event)
  (cond [(symbol=? event 'button-up)
         (play-on-game game  (which-column x) (which-row y))]
        [else game]))

;; game->scene : Draws a game
;; game->scene : game -> scene

(check-expect (game->scene (make-GAME 'Circle 
                                      (make-BOARD cross-blank blank-ROW blank-ROW) 1))
              (place-image (draw-board (make-BOARD cross-blank blank-ROW blank-ROW)) 
                           (/ square-width 2)(/ square-width 2) background))


(check-expect (game->scene (make-GAME 'cross 
                                      (make-BOARD (make-ROW 'cross 'blank 'Circle) blank-ROW blank-ROW) 1))
              (place-image (draw-board (make-BOARD (make-ROW 'cross 'blank 'Circle) blank-ROW blank-ROW))
                           (/ square-width 2)(/ square-width 2) background))

(define (game->scene game)
  (place-image (draw-board (GAME-board game)) (/ square-width 2)(/ square-width 2)  background)
  )


;; winning-triple? : Return true if a, b, and c are all the same symbol as player.
;; winning-triple? : symbol symbol symbol symbol -> boolean

(check-expect (winning-triple? 'cross 'cross 'cross 'cross)true)
(check-expect (winning-triple? 'Circle 'Circle 'blank 'cross)false)
(check-expect (winning-triple? 'Circle 'Circle 'Circle 'Circle)true)
(check-expect (winning-triple? 'cross 'blank 'cross 'cross)false)


(define (winning-triple? player a b c) 
  (and(and (equal? player a)(equal? player b))(equal? player c)))


;; winning-row? : Returns true if the indicated row is a win for the given player.
;; winning-row? : board square symbol -> boolean

(check-expect (winning-row? (make-BOARD cross-row  
                                        circle-cross
                                        (make-ROW 'Circle 'blank 'blank))
                            'cross 'T)true)



(check-expect (winning-row? (make-BOARD (make-ROW 'cross 'blank 'Circle) 
                                        circle-cross
                                        (make-ROW 'blank 'cross 'blank))
                            'Circle 'C)false)



(check-expect (winning-row? (make-BOARD (make-ROW 'cross 'Circle 'blank ) 
                                        (make-ROW 'cross 'Circle 'cross)
                                        (make-ROW 'Circle 'Circle 'Circle))
                            'Circle 'B)true)

(define (winning-row? board player vertical-pos)
  (cond[(equal? vertical-pos 'T)(winning-triple? player (ROW-left (BOARD-top-row board)) 
                                                 (ROW-middle (BOARD-top-row board))
                                                 (ROW-right (BOARD-top-row board)))]
       [(equal? vertical-pos 'C)(winning-triple? player (ROW-left (BOARD-center-row board)) 
                                                 (ROW-middle (BOARD-center-row board))
                                                 (ROW-right (BOARD-center-row board)))]
       [(equal? vertical-pos 'B)(winning-triple? player (ROW-left (BOARD-bottom-row board)) 
                                                 (ROW-middle (BOARD-bottom-row board))
                                                 (ROW-right (BOARD-bottom-row board)))]
       [else false]
       ))


;; winning-column? : Return true if the indicated column is a win for the given player.
;; winnnig-column? : board square symbol -> boolean


(check-expect (winning-column? (make-BOARD cross-ROW-blank 
                                           circle-cross
                                           cross-blank)
                               'cross 'L)true)



(check-expect (winning-column? (make-BOARD circle-cross 
                                           circle-cross
                                           (make-ROW 'blank 'Circle 'blank))
                               'Circle 'M)true)



(check-expect (winning-column? (make-BOARD circle-cross 
                                           (make-ROW 'cross 'blank 'Circle)
                                           (make-ROW 'Circle 'Circle 'Circle))
                               'Circle 'R)true)

(check-expect (winning-column? (make-BOARD circle-cross 
                                           cross-blank
                                           (make-ROW 'Circle 'Circle 'Circle))
                               'Circle 'R)false)


(define (winning-column? board player horizontal-pos)
  (cond[(equal? horizontal-pos 'L)(winning-triple? player (ROW-left (BOARD-top-row board)) 
                                                   (ROW-left (BOARD-center-row board))
                                                   (ROW-left (BOARD-bottom-row board)))]
       [(equal? horizontal-pos 'M)(winning-triple? player (ROW-middle (BOARD-top-row board)) 
                                                   (ROW-middle (BOARD-center-row board))
                                                   (ROW-middle (BOARD-bottom-row board)))]
       [(equal? horizontal-pos 'R)(winning-triple? player (ROW-right (BOARD-top-row board)) 
                                                   (ROW-right (BOARD-center-row board))
                                                   (ROW-right (BOARD-bottom-row board)))]
       [else false]
       ))



;; winning-down-diagonal? : Return true if the top-left to bottom-right diagonal is a win.
;; winning-down-diagonal? : board square -> boolean




(check-expect (winning-down-diagonal?(make-BOARD (make-ROW 'Circle  'Circle 'Circle) 
                                                 (make-ROW 'cross 'Circle 'blank)
                                                 (make-ROW  'cross 'blank 'Circle))
                                     'Circle)true)

(check-expect (winning-down-diagonal?(make-BOARD circle-cross 
                                                 cross-blank
                                                 (make-ROW 'Circle  'blank 'Circle))
                                     'Circle)false)
(check-expect (winning-down-diagonal?(make-BOARD (make-ROW 'cross 'blank 'cross ) 
                                                 (make-ROW 'Circle 'cross 'blank)
                                                 (make-ROW  'blank 'Circle 'cross))
                                     'cross)true)


(define (winning-down-diagonal? board player) 
  (and (equal? player (ROW-right (BOARD-bottom-row board))) (and (equal? player(ROW-middle (BOARD-center-row board)))
                                                                 (equal? player (ROW-left (BOARD-top-row board)))))) 


;; winning-up-diagonal? : Return true if the bottom-left to top-right diagonal is a win.
;; winning-up-diagonal? : board square -> boolean

(check-expect (winning-up-diagonal?(make-BOARD circle-cross 
                                               (make-ROW 'cross 'Circle 'blank)
                                               (make-ROW 'Circle  'blank 'Circle))
                                   'Circle)true)

(check-expect (winning-up-diagonal?(make-BOARD circle-cross 
                                               cross-blank
                                               (make-ROW 'Circle  'blank 'Circle))
                                   'Circle)false)
(check-expect (winning-up-diagonal?(make-BOARD (make-ROW 'cross 'blank 'cross ) 
                                               (make-ROW 'Circle 'cross 'blank)
                                               (make-ROW 'cross  'blank 'Circle))
                                   'cross)true)


(define (winning-up-diagonal? board player) 
  (and (equal? player (ROW-left (BOARD-bottom-row board))) (and (equal? player(ROW-middle (BOARD-center-row board)))
                                                                (equal? player (ROW-right (BOARD-top-row board)))))) 

;; winning-board? : Returns true if the given board is a win for the given player.
;; winning-board? : board square -> boolean

(check-expect (winning-board? (make-BOARD cross-row  
                                          circle-cross
                                          blank-circle)
                              'cross)true)

(check-expect (winning-board? (make-BOARD circle-cross
                                          cross-row  
                                          blank-circle)
                              'cross)true)
(check-expect (winning-board? (make-BOARD circle-cross
                                          blank-circle
                                          cross-row )
                              'cross)true)

(check-expect (winning-board? (make-BOARD (make-ROW 'Circle 'cross 'cross) 
                                          (make-ROW  'Circle 'cross 'Circle)
                                          blank-circle)
                              'Circle)true)
(check-expect (winning-board? (make-BOARD (make-ROW 'cross 'Circle 'cross) 
                                          circle-cross
                                          (make-ROW 'Circle 'Circle 'blank))
                              'Circle)true)
(check-expect (winning-board? (make-BOARD  cross-circle 
                                           circle-cross
                                           (make-ROW 'Circle 'blank 'Circle))
                              'Circle)true)

(check-expect (winning-board? (make-BOARD  cross-circle 
                                           circle-cross
                                           blank-circle)
                              'Circle)true)
(check-expect (winning-board? (make-BOARD (make-ROW 'cross 'Circle 'cross) 
                                          cross-circle
                                          (make-ROW 'Circle 'blank 'cross))
                              'cross)true)

(define (winning-board? board player) 
  (or (winning-up-diagonal? board player)
      (or (winning-down-diagonal? board player)
          (or (winning-row? board player 'T)
              (or   (winning-row? board player 'C)
                    (or (winning-row? board player 'B)
                        (or (winning-column? board player 'L)
                            (or (winning-column? board player 'M)
                                (winning-column? board player 'R)))))))))



;; game-over-or-win? : Returns true when the game is over either because the board is full,
;;                     or because someone won.
;; game-over-or-win? : game -> boolean

(check-expect (game-over-or-win? (make-GAME 'Circle 
                                            (make-BOARD (make-ROW 'cross 'blank 'Circle) blank-ROW blank-ROW) 3))false)


(check-expect (game-over-or-win? (make-GAME 'Circle
                                            (make-BOARD (make-ROW 'cross 'blank 'Circle)
                                                        (make-ROW 'blank 'cross 'Circle)
                                                        (make-ROW 'cross 'blank 'Circle))7))true)


(check-expect (game-over-or-win? (make-GAME 'cross 
                                            (make-BOARD  cross-circle
                                                         (make-ROW  'Circle 'cross 'Circle)
                                                         (make-ROW 'cross 'Circle 'cross))9))
              true)

(define (game-over-or-win? game)
  (or (winning-board? (GAME-board game) (GAME-next-player game))
      (game-over? game)))


(collect-garbage) (collect-garbage) (collect-garbage)
(printf "running tests with fast path optimization in place\n")
(time (run-tests))
(printf "running tests without fast path optimization in place\n")
(parameterize ([skip-image-equality-fast-path #t])
  (time (run-tests)))
