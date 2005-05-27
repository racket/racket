(module boards mzscheme
  (provide boards
           (struct board (name board)))

  (define-struct board (name board))

  (define boards
    (list
     (make-board
      "1"
      #(#(o o o o o)
         #(o o o o o)
         #(x o x o x)
         #(o o o o o)
         #(o o o o o)))
     (make-board
      "2"
      #(#(x o x o x)
         #(x o x o x)
         #(o o o o o)
         #(x o x o x)
         #(x o x o x)))
     (make-board
      "3"
      #(#(o x o x o)
         #(x x o x x)
         #(x x o x x)
         #(x x o x x)
         #(o x o x o)))
     (make-board
      "4"
      #(#(o o o o o)
         #(x x o x x)
         #(o o o o o)
         #(x o o o x)
         #(x x o x x)))
     (make-board
      "5"
      #(#(x x x x o)
         #(x x x o x)
         #(x x x o x)
         #(o o o x x)
         #(x x o x x)))
     (make-board
      "6"
      #(#(o o o o o)
         #(o o o o o)
         #(x o x o x)
         #(x o x o x)
         #(o x x x o)))
     (make-board
      "7"
      #(#(x x x x o)
         #(x o o o x)
         #(x o o o x)
         #(x o o o x)
         #(x x x x o)))
     (make-board 
      "Diagonal"
      #(#(o o o o x)
         #(o o o x o)
         #(o o x o o)
         #(o x o o o)
         #(x o o o o))))))