#cs(module protect-play mzscheme
     (require "pingp-sig.ss"
              "protect-play-unit.ss"
              "ping-play-unit.ss"
              "pingp.ss"
              (lib "unitsig.ss"))
     
     (provide protect-play@)
     (define protect-play@
       (compound-unit/sig 
         (import)
         (link
          [PINGP : pingpS (pingpU)]
          [BALL  : ping-protS (ping-play-U PINGP)]
          [PROT  : protectS   (protect-play-U BALL PINGP)]
          (GO : goS ((unit/sig goS (import protectS pingpS)
                       (define n (+ 10 (random 10)))
                       (define (go s)
                         (set! n (+ 10 (random 10)))
                         (printf "You're facing ~a balls. Have fun playing, ~a~n" n s)
                         (protect (mk-balls n)
                                  move-balls
                                  remove-balls-hit-paddle
                                  remove-outside-balls
                                  balls-posn)))
                     PROT
                     (PINGP : pingpS))))
         (export (var (PINGP change-speed))
                 (var (PINGP change-wind))
                 (open GO)))))