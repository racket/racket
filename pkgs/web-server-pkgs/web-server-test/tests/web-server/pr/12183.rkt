#lang racket
(require tests/eli-tester
         net/url
         web-server/lang/stuff-url
         web-server/stuffers/stuffer
         web-server/stuffers/serialize)

(struct not-serializable ())
(struct serializable (x) #:prefab)


(test
 (stuff-url serialize-stuffer
           (string->url "http://localhost:8000")
           (serializable (not-serializable)))
 =>
 (error 'stuff-url "Cannot stuff '#s(serializable #<not-serializable>) into a URL because it contains non-serializable pieces. Convert #<not-serializable> to a serializable struct"))
