#lang typed/racket

(provide future-count)

(require/typed
 racket/future
 [processor-count  (-> Positive-Integer)])

(: future-count (Parameterof Nonnegative-Integer))
(define future-count (make-parameter (processor-count)))
