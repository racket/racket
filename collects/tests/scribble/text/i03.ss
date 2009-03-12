#lang scribble/text

---***---
@(define (angled . body) (list "<" body ">"))
 @(define (shout . body) @angled[(map string-upcase body)])
  @define[z]{blah}

blah @angled{blah @shout{@z} blah} blah

@(define-syntax-rule @twice[x]
   (list x ", " x))

@twice{@twice{blah}}

@include{i03a}

@(let ([name "Eli"]) (let ([foo (include "i03b")]) (list foo "\n" foo)))
Repeating yourself much?
