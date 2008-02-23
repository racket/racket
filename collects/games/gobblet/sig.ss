(module sig mzscheme
  (require mzlib/unitsig)

  (provide config^ 
	   heuristics^ explore^
	   model^ restart^)

  (define-signature config^
    (BOARD-SIZE))

  (define-signature heuristics^
    (make-3x3-rate-board
     make-3x3-canned-moves
     make-3x3-no-canned-moves
     make-4x4-rate-board
     make-4x4-canned-moves))

  (define-signature explore^
    (make-search 
     apply-play ; a play is (list piece from-i from-j to-i to-j)
     (struct plan (size from-i from-j to-i to-j xform))))

  (define-signature model^
    (move 
     winner? 3-in-a-row?
     red-pieces yellow-pieces
     piece-color piece-size
     empty-board
     board-ref
     fold-board 
     fold-rowcol 
     other
     available-off-board
     compact-board
     make-canonicalize
     apply-xform unapply-xform
     board->string))

  (define-signature restart^
    (new-game
     show-gobblet-help)))


