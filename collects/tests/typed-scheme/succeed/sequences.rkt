#lang typed/scheme

(ann (for ([z (list 1 2 3)]) (add1 z)) Void)
(ann (for ([z (vector 1 2 3)]) (add1 z)) Void)
(ann (for ([z "foobar"]) (string z)) Void)
(ann (for ([z #"foobar"]) (add1 z)) Void)
(ann (for ([z (open-input-string "foobar")]) (add1 z)) Void)

(ann (for ([z (in-list (list 1 2 3))]) (add1 z)) Void)
(ann (for ([z (in-vector (vector 1 2 3))]) (add1 z)) Void)
