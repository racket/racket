#lang scribble/doc

@(require "shared.rkt" 
          "port.rkt"
          scribble/manual
          scribble/eval
          2htdp/image
          racket/runtime-path
          (except-in racket/draw make-color make-pen)
          racket/class
          (for-syntax racket/base)
          (for-label 2htdp/image
                     2htdp/planetcute))

@; -----------------------------------------------------------------------------

@title{Planet Cute Images}

@defmodule[2htdp/planetcute]

@(define pc-eval (make-base-eval))
@(interaction-eval #:eval pc-eval (require 2htdp/image))
@(interaction-eval #:eval pc-eval (require 2htdp/planetcute))

The @racketmodname[2htdp/planetcute] library contains the 
@link["http://www.lostgarden.com/2007/05/dancs-miraculously-flexible-game.html"]{Planet Cute} 
art by Daniel Cook (Lostgarden.com).

The images are designed to be overlaid with each other to build
scenes for use in games. Here is an example image taken
from the Planet Cute website.

@racketblock+eval[#:eval 
                  pc-eval
                  (code:comment "stacks its arguments on each")
                  (code:comment "other, separated by 40 pixels")
                  (define (stack arg . args)
                    (cond
                      [(null? args) arg]
                      [else (overlay/xy arg 0 40
                                        (apply stack args))]))]
@interaction[#:eval 
             pc-eval
             (beside/align
              "bottom"
              (stack wall-block-tall stone-block)
              (stack character-cat-girl
                     stone-block stone-block
                     stone-block stone-block)
              water-block
              (stack grass-block dirt-block)
              (stack grass-block dirt-block dirt-block))]

@(close-eval pc-eval)

The Planet Cute images also include some shadows that can improve the
look of your game; see the @secref["pc:Shadows"] section for an overview
of how to use them.

@(require (for-syntax 2htdp/private/planetcute-image-list))
@(define-syntax (defthings stx)
   (syntax-case stx ()
     [(_ what whatever ...)
      (identifier? #'what)
      (let* ([sym (syntax-e #'what)]
             [sec-title (symbol->string sym)]
             [these-images (cdr (assoc sym images))])
        #`(begin
            @section[#:tag #,(format "pc:~a" sec-title) #,sec-title]
            whatever ...
            #,@(for/list ([img (in-list these-images)])
                 (define req (string->symbol (format "2htdp/planetcute/~a" (name->filename img))))
                 #`@defthing[#,img image?]{  @(bitmap #,req) })))]))

@(begin
   (define-runtime-path PlanetCuteShadow1.png "PlanetCuteShadow1.png")
   (define-runtime-path PlanetCuteShadow2.png "PlanetCuteShadow2.png")
   (define-runtime-path PlanetCuteShadow2b.png "PlanetCuteShadow2b.png")
   (define-runtime-path PlanetCuteShadow3.png "PlanetCuteShadow3.png"))

@defthings[Characters]{}
@defthings[Blocks]{}
@defthings[Items]{}
@defthings[Ramps]{}
@defthings[Buildings]{}
@defthings[Shadows]{The shadow images are intended to be
                    overlaid on the other blocks when they
                    appear in certain configurations, as
                    detailed here.
                    
                    @(read-bitmap PlanetCuteShadow1.png) 
                    @(read-bitmap PlanetCuteShadow2.png)
                    @(read-bitmap PlanetCuteShadow2b.png)
                    @(read-bitmap PlanetCuteShadow3.png) }
