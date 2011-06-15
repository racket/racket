#lang typed/racket

(: s1 String)
(: s2 String)
(: s3 String)
(: s4 String)
(: s5 String)
(: s6 String)

(define s1 "abc")
(define s2 "def")
(define s3 "DEF")
(define s4 (string #\a #\b #\c))
(define s5 (make-string 4 #\b))
(define s6 (build-string 26 integer->char))



(string? s1)
(string? 2)

(string->immutable-string s5)
(string-length s6)
(string-copy s3)
(string-copy! (make-string 4) 0 s4)
(string-copy! (make-string 4) 1 s4 1)
(string-copy! (make-string 4) 1 s4 1 2)

(string-fill! (make-string 5) #\Z)

(string-append)
(string-append s1 s2)
(string-append s1 s2 s3)

(string->list s6)
(list->string (list #\a #\d #\d))

(string=? s2 s3)
(string<? s2 s3)
(string>? s2 s3)
(string<=? s2 s3)
(string>=? s2 s3)

(string-upcase s2)
(string-downcase s3)
(string-titlecase s4)
(string-foldcase s5)


(string-ci=? s2 s3)
(string-ci<? s2 s3)
(string-ci>? s2 s3)
(string-ci<=? s2 s3)
(string-ci>=? s2 s3)


(string-normalize-nfd s2)
(string-normalize-nfkd s3)
(string-normalize-nfc s4)
(string-normalize-nfkc s5)



(string-locale=? s2 s3)
(string-locale<? s2 s3)
(string-locale>? s2 s3)

(string-locale-upcase s2)
(string-locale-downcase s3)


(string-locale-ci=? s2 s3)
(string-locale-ci<? s2 s3)
(string-locale-ci>? s2 s3)

