#lang scheme/base
(require '#%flfxnum)

(provide fx->fl fl->fx
         fxabs
         fx+ fx- fx*
         fxquotient fxremainder fxmodulo 
         fxand fxior fxxor
         fxnot fxrshift fxlshift
         fx>= fx> fx= fx< fx<=
         fxmin fxmax)
