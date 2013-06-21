#;
#<<END
TR opt: string-length.rkt 15:0 (string-length "eh") -- string-length
TR opt: string-length.rkt 16:0 (bytes-length #"eh") -- bytes-length
2
2

END

#lang typed/scheme
#:optimize



(string-length "eh")
(bytes-length #"eh")
