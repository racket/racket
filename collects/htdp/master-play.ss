#cs(module master-play mzscheme
  (require 
   "master.ss"
   (lib "prim.ss" "lang"))
  
  (provide go)
  (define-primitive go go/proc)
  
  (define (compare choice1 choice2 guess1 guess2)
    (cond
      [(and (eq? choice1 guess1) (eq? choice2 guess2))
       'perfect!]
      [(or (eq? choice1 guess1) (eq? choice2 guess2))
       'one_color_is_at_proper_place]
      [(or (eq? choice2 guess1) (eq? choice1 guess2))
       'one_color_occurs]
      [else
       'sorry_all_wrong]))

  (define (go/proc s)
    (printf "Have fun playing, ~a~n" s)
    (master compare)))
