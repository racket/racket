#lang scribble/doc
@(require scribble/manual setup/dirs "private/utils.rkt")

@(define (rl-link path . content)
   (apply link (apply build-path (find-doc-dir) "release-notes" path)
               content))
@(define (mzport doc from to)
   (rl-link (list "racket" doc) (format "Porting from ~a to ~a" from to)))

@main-page['release]

@itemize[#:style "compact"]{

 @item{@rl-link['("racket" "HISTORY.txt")]{Racket core}
       @itemize[#:style "compact"]{
         @item{@mzport["Draw_and_GUI_5_1.txt" "v5.0.x" "v5.1"]}
         @item{@mzport["Racket_5.txt" "v4.x" "v5.x"]}
         @item{@mzport["MzScheme_4.txt" "v3xx" "v4.x"]}
         @item{@mzport["MzScheme_300.txt" "v2xx" "v3xx"]}
         @item{@mzport["MzScheme_200.txt" "v1xx" "v2xx"]}}}
 @item{@rl-link['("drracket" "HISTORY.txt")]{DrRacket}}
 @item{@rl-link['("gracket" "HISTORY.txt")]{GRacket}}
 @item{@rl-link['("teachpack" "HISTORY.txt")]{Teachpacks}}
 @item{@rl-link['("stepper" "HISTORY.txt")]{Stepper}}
 @item{@rl-link['("plai" "HISTORY.txt")]{PLAI}}
 @item{@rl-link['("redex" "HISTORY.txt")]{Redex}}

}
