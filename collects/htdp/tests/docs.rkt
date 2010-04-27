;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname docs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require htdp/docs)

(check-expect (annotation? '<html>) true)
(check-expect (annotation? 'html) false)
(check-expect (annotation? '<p>) true)

(check-expect (end-annotation '<html>) '</html>)

(check-expect 
 (write-file
  (list '<p> 'hello 'world 'is 'the 'most 'stupid 'program 'in 'the 'world '</p>
        "so let's test this" 'with "How's that"))
 true)
