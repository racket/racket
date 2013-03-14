#;
(exn-pred "at least one")
#lang typed/racket


(require/typed  (make-main (([Listof Node] [Listof Edge] -> Graph)
                            (State Number Number MouseEvent -> State)
                            (State KeyEvent -> State)
                            (State -> Scene)
                            (Any -> Boolean)
                            (State -> Boolean)
                            (Stop -> Graph)
                            (Any -> Edge)
                            (Edge -> Graph)
                            ->
                            (Boolean -> Graph))))
