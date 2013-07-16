#lang racket/load

#| -----------------------------------------------------------------------------
   testing temporal contracts: 

   let t =  (new turn board player-tiles player-score tile-bag)
   let t come with two methods: bump and observe 

        (send . take-turn t)
admin --------------------------------------------------------> player 
       TEMPORAL: don't call bump on t after this call returns 

|#
(require errortrace)

;; -----------------------------------------------------------------------------
;; the interface module, defines turn% and how player is called via take-turn 
(module player-admin-interface racket 
  (require unstable/temp-c/dsl
           unstable/match)
  
  (define turn%
    (class object% 
      (init-field value)
      (define/public (observe) value)
      (define/public (bump) (set! value (+ value 1)))
      (super-new)))
  
  (define mon
    (re->monitor-predicate/serial
     (re
      (complement
       (seq (star _)
            (dseq (monitor:return 'take-turn _ _ _ _ _ (list _ t) _)
                  (seq (star _)
                       (call 'bump (== t)))))))))
  
  (define turn/c
    (object/c [observe (monitor/c mon 'observe (->m natural-number/c))]
              [bump (monitor/c mon 'bump (->m any/c))]))
  
  (define player/c
    (class/c [take-turn (monitor/c mon 'take-turn (->m turn/c any/c))]))
  
  (provide player/c turn%))

;; -----------------------------------------------------------------------------
;; the player module defines a player and slabs on the requires player contract 

(module player racket
  (require 'player-admin-interface)
  
  (define player% 
    (class object% 
      (init-field name)
      (define turn #false)
      (define/public (take-turn t)
        (if turn 
            (send turn bump)
            (send t bump))
        (set! turn t))            
      (super-new)))
  
  (provide/contract
   [player% player/c]))

;; -----------------------------------------------------------------------------
;; the admin module creates player, admin, and has admin call player 

(module admin racket
  (require 'player-admin-interface 'player tests/eli-tester)
  
  (define admin%
    (class object%
      (init-field player)
      
      (define/public (run)
        (define turn1 (new turn% [value 1]))
        (send player take-turn turn1)
        (define value1 (send turn1 observe))
        ;; --- 
        (define turn2 (new turn% [value 10]))
        (test 
         (send player take-turn turn2)
         =error>
         #rx"disallowed call")
        ;; --- 
        (list 'bad-for-turn1: (not (= 2 (send turn1 observe)))
              'bad-for-turn2: (= 10 (send turn2 observe))))
      
      (super-new)))
  
  ;; main
  (define player (new player% [name "a"]))
  (define admin (new admin% [player player]))
  (displayln (send admin run)))

;; -----------------------------------------------------------------------------
;; run program run 

(require 'admin)
