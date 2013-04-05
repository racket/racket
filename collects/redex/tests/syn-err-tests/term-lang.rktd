(#rx"before underscore"
 ([not-a-non-term Z_1])
 (term not-a-non-term #:lang syn-err-lang))

(#rx"before underscore"
 ([not-a-non-term AA_1])
 (term (Q_1 Q_2 (Q_3 not-a-non-term)) #:lang syn-err-lang))

(#rx".*define-language.*boglang"
 ([bogus-language boglang])
 (term 5 #:lang bogus-language))
