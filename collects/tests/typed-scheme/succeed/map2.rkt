#lang typed-scheme

(map add1 #{(list 1 2 3) :: (Listof Integer)})
(map add1 #{(list 1 2 3) :: (Listof Number)})
