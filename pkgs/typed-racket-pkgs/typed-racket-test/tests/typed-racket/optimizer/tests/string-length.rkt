#;#;
#<<END
TR opt: string-length.rkt 2:0 (string-length "eh") -- string-length
TR opt: string-length.rkt 3:0 (bytes-length #"eh") -- bytes-length
END
#<<END
2
2

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(string-length "eh")
(bytes-length #"eh")
