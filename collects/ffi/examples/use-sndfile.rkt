;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname use-sndfile) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
#! /usr/bin/env racket

#lang racket/base

(require "sndfile.ss")

;; (require swindle)

(define (add-half x y)
  (/ (+ x (* y 0.5)) 1.5))

(define (repeated-list x n)
  (let loop ([n n] [r '()])
    (if (zero? n) r (loop (sub1 n) (cons x r)))))

;; N.B.: this won't work unless you have a file in the current working directory called "x.wav".
(let-values ([(data meta) (read-sound* "x.wav")])
  (printf ">>> data-length: ~s\n>>> meta: ~s\n" (length data) meta)
  (let* ([data data #;
               (list-of (list (add-half (1st x) (1st y))
                              (add-half (2nd x) (2nd y)))
                 (x <- data
                  and
                  y <- (append (repeated-list (list 0.0 0.0) 11025) data)
                  and
                  i <- 0.1 0.12 ..))])
    (printf "writing to y.wav\n")
    (write-sound* "y.wav"
                  ;data
                  ;(append data (reverse data))
                  (append data (reverse (map reverse data)))
                  `((artist "Eli") (comment "Comment") (title "Title")
                    (date "1/1/1999") (software "mzscheme")
                    ,@meta))))
