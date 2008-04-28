#lang scheme/base

(require r6rs/private/conds)

(provide &condition
         condition?
         condition
         simple-conditions
         condition-predicate
         condition-accessor
         define-condition-type

         &message make-message-condition message-condition? condition-message
         &warning make-warning warning?
         &serious make-serious-condition serious-condition?
         &error make-error error?
         &violation make-violation violation?
         &assertion make-assertion-violation assertion-violation?
         &irritants make-irritants-condition irritants-condition? condition-irritants
         &who make-who-condition who-condition? condition-who
         &non-continuable make-non-continuable-violation non-continuable-violation?
         &implementation-restriction make-implementation-restriction-violation implementation-restriction-violation?
         &lexical make-lexical-violation lexical-violation?
         &syntax make-syntax-violation syntax-violation? syntax-violation-form syntax-violation-subform
         &undefined make-undefined-violation undefined-violation?)
