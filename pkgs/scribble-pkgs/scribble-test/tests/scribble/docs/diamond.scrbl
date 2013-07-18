#lang scribble/base
@#reader "diamond.rkt"

◇begin{
  
 This example checks that @ is not an escape character
 if we make a reader that uses a different escape character.

 ◇(define ch "diamond")

 It also makes sure that a non-ASCII character like ◇ch
 is ok as an escape character.

 }
