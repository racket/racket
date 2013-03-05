#lang racket
;; Checks that all paths in a tree of games leads to the expected
;; winner. It also generates information for known plays to be used to
;; speed up future games (i.e., converts learned strategy to a compact
;; form).

(require racket/unit
         "sig.rkt"
         (only-in "model.rkt" model-unit@)
         (only-in "explore.rkt" explore-unit@)
         (only-in "heuristics.rkt" heuristics-unit@))

(define board-size 3)
(define cannon-size +inf.0)

(define-unit board-config@
  (import)
  (export config^)
  (define BOARD-SIZE board-size))

(define-unit robot-unit@ 
  (import config^ explore^ model^ heuristics^)
  (export)
  (define (mv b p fi fj ti tj k)
    (move b p fi fj ti tj k void))
  
  (define big (sub1 BOARD-SIZE))
  (define med (- BOARD-SIZE 2))
  
  (define 3x3-one-step-win
    ;; One-step win
    (mv empty-board (list-ref red-pieces big) #f #f 0 0
        (lambda (board)
          (mv board (list-ref red-pieces big) #f #f 1 1
              values))))
  
  (define 3x3-two-step-win
    (mv empty-board (list-ref red-pieces big) #f #f 0 0
        (lambda (board)
          (mv board (list-ref yellow-pieces big) #f #f 1 0
              (lambda (board)
                (mv board (list-ref red-pieces big) #f #f 1 1
                    (lambda (board)
                      (mv board (list-ref yellow-pieces big) 1 0 2 2
                          (lambda (board)
                            (mv board (list-ref red-pieces med) #f #f 1 0
                                (lambda (board)
                                  (mv board (list-ref yellow-pieces big) #f #f 1 0
                                      values))))))))))))
  
  (define (test-search depth board who history)
    ((make-search (if (= BOARD-SIZE 3)
                      make-3x3-rate-board
                      make-4x4-rate-board)
                  (if (= BOARD-SIZE 3)
                      make-3x3-no-canned-moves
                      make-4x4-canned-moves)) 
     +inf.0 1 
     depth ; depth
     who board history))
  
  (when (= BOARD-SIZE 3)
    (test-search 1 3x3-one-step-win 'red null)
    (test-search 3 3x3-one-step-win 'red null)
    (test-search 3 3x3-two-step-win 'red null))
  
  ;; Time test
  (let ([start (current-inexact-milliseconds)]
        [m (test-search 5 empty-board 'red null)])
    (printf "[~a secs]\n" (/ (- (current-inexact-milliseconds) start)
                             1000.0))))

(invoke-unit
 (compound-unit/infer
  (import)
  (export)
  (link
   [((CONFIG : config^)) board-config@]
   [((MODEL : model^)) model-unit@]
   [((HEURISTICS : heuristics^)) heuristics-unit@]
   [((EXPLORE : explore^)) explore-unit@]
   [() robot-unit@])))
