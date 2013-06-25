#;
(
TR opt: string-length.rkt 14:0 (string-length "eh") -- string-length
TR opt: string-length.rkt 15:0 (bytes-length #"eh") -- bytes-length
2
2
)

#lang typed/scheme
#:optimize



(string-length "eh")
(bytes-length #"eh")
