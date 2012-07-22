#lang racket
(require math/array
         math/matrix)
(define M (list->matrix '[[1 2] [3 4]]))

"Is the matrix created correctly?"
(array->list M)
"Yes!"


"What are the row dimension?"
(matrix-row-dimension M)




