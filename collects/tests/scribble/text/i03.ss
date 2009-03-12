#lang scribble/text

---***---
@(define (angled . body) (list "<" body ">"))
 @(define (shout . body) @angled[(map string-upcase body)])
  @define[z]{blah}

blah @angled{blah @shout{@z} blah} blah

@(define-syntax-rule @twice[x]
   (list x ", " x))

@twice{@twice{blah}}

@include{i3a}

@(let ([name "Eli"]) (let ([foo (include "i3b")]) (list foo "\n" foo)))
Repeating yourself much?
