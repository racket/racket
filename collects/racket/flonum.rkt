#lang scheme/base
(require '#%flfxnum)

(provide fl+ fl- fl* fl/
         flabs flsqrt flexp fllog
         flsin flcos fltan flasin flacos flatan
         flfloor flceiling flround fltruncate
         fl= fl< fl<= fl> fl>= flmin flmax
         ->fl fl->exact-integer
         flvector? flvector make-flvector 
         flvector-length flvector-ref flvector-set!)
