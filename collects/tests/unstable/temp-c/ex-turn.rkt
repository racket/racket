#lang racket
(require racket/require
         (path-up "temp-c/dsl.rkt")
         tests/eli-tester)

(define turn%/c
(with-monitor
 (class/c [place!
           ;; place a tile at a specific location, return the tile that the player gets in turn
           ;; -- always works the first time
           ;; -- works the (n+1)st time when the score change for the n-th call to place
           ;; ---- made one of the scores to cross the '18 line'
           ;; -- never works after rack! has been called
           (->dm ([p (and/c placement/c
                            (lambda (p) (send this placable? p))
                            (lambda (p) (send this one-of-my-tiles (placement->tile p))))])
                 (r (or/c tile/c false/c)))]
          [placable?
           ;; is this placement legitimate, considering the state of the game?
           (->m placement/c boolean?)]
          [one-of-my-tiles?
           ;; is this tile one of mine?
           (->m tile/c boolean?)]
          [rack!
           ;; re-rack your tiles ; call the function only if
           ;; -- the score returned from the last place call produces a score
           ;; -- such that none of the current tiles is associated with any of
           ;; -- the least scoring piece
           ;; CHANGE: this may return some of the same tiles
           ;;   IF there aren't enough available on the administrator's side
           (->m (listof tile/c))]
          [end
           ;; signal end of turn
           (-> void?)]))

(seq 
     (rec* x
           (or 
	    (seq
     	     (call 'place! _)
	     (return 'place! _))
            (seq*
             (call 'place! _)
             (return 'place! (? crossed-the-line?)))
             x))
     (call 'place! _)
     (or
      (seq (return 'place! (? a-score-such-that-none-of-the-current-tiles-is-associated-with-any-of-the-least-scoring-piece?))
           (call 'rack!)
	   (return 'rack!))
      (return 'place! _))
     (call 'end)
     (return 'end _))