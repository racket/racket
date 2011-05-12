#;
(
#f (no location) op -- string-length
#f (no location) op -- bytes-length
2
2
)

#lang typed/scheme
#:optimize



(string-length "eh")
(bytes-length #"eh")
