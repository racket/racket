#;
(exn-pred 3)
#lang typed/scheme

(ann (list 1 2 'foo) (Listof Number))

(ann (reverse (list 1 2 'foo)) (Listof Number))





(ann (reverse (list 1 2 'foo)) (List String Number Number))
