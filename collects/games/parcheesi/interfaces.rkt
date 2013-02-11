(module interfaces racket
  (provide player<%>
           game<%>)

  ;; in Java, lists become arrays.
  
  ;; the do-move method gets two numbers if no doubles were rolled
  ;; and gets four numbers if doubles were rolled (and the player has
  ;; all of the pieces in).
  (define player<%>
    (interface ()
      start-game ;; player-color -> string
      do-move ;; board (listof number[1-6]) -> move
      doubles-penalty ;; : -> void
      ))
  
  (define game<%>
    (interface ()
      register ;; player<%> -> void
      start #| -> void |#)))
