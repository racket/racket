;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname server) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))

(require 2htdp/universe)

;; UniState = (list String)
;; interp. the name of currently participating chatters

;; Message = (list String String)

;; Result = (make-bundle UniState [Listof (make-mail IWorld Message)]) 

;; Universe IWorld -> Universe
;; add the name of the new world to the universe

(check-expect (new-chatter '() iworld1) (list iworld1))

(define (new-chatter u w)
  (cons (iworld-name w) u))

;; Universe IWorld Message -> Result

(define u0 (list iworld1 iworld2 iworld3))
(define name1 (symbol->string (iworld-name iworld1)))
(define name2 (symbol->string (iworld-name iworld2)))
(define name3 (symbol->string (iworld-name iworld3)))

(check-expect (forward u0 iworld1 (list name2 "hello"))
  (make-bundle u0 (list (make-mail iworld1 "hello")) '()))

(check-expect (forward u0 iworld1 (list "*" "hello"))
  (make-bundle u0 
               (list
                (make-mail iworld2 (list (string-append name2 "*") "hello"))
                (make-mail iworld2 (list (string-append name3 "*") "hello")))
               '()))

(define (forward u s msg) (make-bundle u '() '()))
  
