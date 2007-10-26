#lang scribble/doc

@begin[(require
	 (lib "manual.ss" "scribble"))
       
       (require-for-label 
	 (lib "lang.ss" "big"))]

@title[#:tag "questions"]{Questions concerning scribble}

Here are my questions. Feel free to point me to specific pieces of
 documentations. As a matter of fact, I'd almost prefer that. 

@itemize{

@item{
   world.ss imports image.ss and re-exports all the bindings.

   So in world.scrbl, I have to require @scheme[(for-label "world.ss")] but
   when I do so, I don't get bindings for functions from
   @scheme[image.ss].

   If I require @scheme[(for-label "image.ss")] in addition, I get an
   import conflict. What is the proper solution? 
}

@item{
   I'd like to make graphical examples. I looked at the "quick" guide to
   see how you do this, but I can't figure out how to run all this under
   mred's control.
}

@item{
   For the teachpack definitions, I like to spell out data definitions like
   in HowToDesign. For example, 


;; {Mode} is one of the following two symbols or strings: 

;; -- @scheme['solid]

;; -- @scheme['outline]

;; -- @scheme["solid"]

;; -- @scheme["outline"]

   I want the two semi-colons, I want the scheme mode for the constants,
   and I want the first part to look like an ordinary test line. 
   [slatex and thtml do this for me]

   I would also like to use @scheme[Mode] as if it were some exported
   variable so that I can jump to its definition from other places in the
   documentation. But I also don't want to introduce a (fake) visible
   definition. I don't think deftech and defterm are the right tools. But
   perhaps that's all there is to it. 

}

}
