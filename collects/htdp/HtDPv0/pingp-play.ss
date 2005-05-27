#cs(module pingp-play mzscheme
     
     (require "pingp-sig.ss"
              "pingp.ss"
              (lib "unitsig.ss")
              "ping-play-unit.ss")
     
     (provide pingp-play@)  
     (define pingp-play@
       (compound-unit/sig 
         (import)
         (link
          [PINGP : pingpS (pingpU)]
          [BALL  : ballS  (ping-play-U PINGP)]
          (GO : goS ((unit/sig goS (import ballS pingpS)
                       (define (go s)
                         (printf "Have fun playing, ~a~n" s)
                         (play make-ball make-speed ball-posn move-in-box)))
                     BALL
                     (PINGP : pingpS))))
         (export (var (PINGP change-speed))
                 (var (PINGP change-wind))
                 (open GO)))))
