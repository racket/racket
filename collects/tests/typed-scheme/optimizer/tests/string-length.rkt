#;
(
#f line #f col #f - op - string-length
#f line #f col #f - op - bytes-length
2
2
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(string-length "eh")
(bytes-length #"eh")
