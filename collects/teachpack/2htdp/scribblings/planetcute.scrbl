#lang scribble/doc

@(require "shared.rkt" 
          "port.rkt"
          scribble/manual
          scribble/eval
          (for-syntax racket/base)
          2htdp/image
          (for-label 2htdp/image
                     2htdp/planetcute))

@; -----------------------------------------------------------------------------

@title{Planet Cute Images}

@defmodule[2htdp/planetcute]

@(define pc-eval (make-base-eval))
@(interaction-eval #:eval pc-eval (require 2htdp/image))
@(interaction-eval #:eval pc-eval (require 2htdp/planetcute))

The @racketmodname[2htdp/planetcute] library contains the 
Planet Cute images:
@url{http://www.lostgarden.com/2007/05/dancs-miraculously-flexible-game.html}.

The images are designed to be overlaid with each other to build
scenes for use in games. Here is an example image taken
from the Planet Cute website.

@racketblock+eval[#:eval 
                  pc-eval
                  (define (stack arg . args)
                    (cond
                      [(null? args) arg]
                      [else (overlay/xy arg 0 40
                                        (apply stack args))]))]
@interaction[#:eval 
             pc-eval
             (scale
              1
              (beside/align
               "bottom"
               (stack wall-block-tall stone-block)
               (stack character-cat-girl
                      stone-block stone-block
                      stone-block stone-block)
               water-block
               (stack grass-block dirt-block)
               (stack grass-block dirt-block dirt-block)))]

@(close-eval pc-eval)
           
@(require (for-syntax 2htdp/private/planetcute-image-list))
@(define-syntax (defthings stx)
   #`(begin
       #,@(for/list ([img (in-list images)])
            (define req (string->symbol (format "2htdp/planetcute/~a" (name->filename img))))
            #`@defthing[#,img image?]{  @(bitmap #,req) })))

@defthings[]
