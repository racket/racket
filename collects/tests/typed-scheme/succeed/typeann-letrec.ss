#lang typed-scheme

(letrec-syntaxes+values () ([(#{x : Number}) (values 1)]) #{(vector x) :: (Vectorof Number)})
(letrec-values ([(#{x : Number}) (values 1)]) #{(vector x) :: (Vectorof Number)})
(letrec ([#{x : Number} (values 1)]) #{(vector x) :: (Vectorof Number)})
