#lang typed/racket

(require/typed
 racket/base
 [raise-argument-error  (case-> (Symbol String Any Any * -> Nothing)
                                (Symbol String Natural Any Any * -> Nothing))]
 [raise-result-error  (case-> (Symbol String Any Any * -> Nothing)
                              (Symbol String Natural Any Any * -> Nothing))]
 [raise-arguments-error  (Symbol String Any * -> Nothing)]
 [raise-range-error  (Symbol String String Integer Any Integer Integer (Option Integer) -> Nothing)])

(provide raise-argument-error
         raise-result-error
         raise-arguments-error
         raise-range-error)
