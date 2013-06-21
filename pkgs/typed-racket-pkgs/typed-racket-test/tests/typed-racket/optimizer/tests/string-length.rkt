#;#;
#<<END
TR opt: string-length.rkt 17:0 (string-length "eh") -- string-length
TR opt: string-length.rkt 18:0 (bytes-length #"eh") -- bytes-length
END
#<<END
2
2

END

#lang typed/scheme
#:optimize



(string-length "eh")
(bytes-length #"eh")
