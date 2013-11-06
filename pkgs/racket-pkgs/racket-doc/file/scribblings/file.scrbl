#lang scribble/doc
@(require "common.rkt")

@title{File: Racket File and Format Libraries}

@table-of-contents[]

@include-section["convertible.scrbl"]
@include-section["gzip.scrbl"]
@include-section["gunzip.scrbl"]
@include-section["zip.scrbl"]
@include-section["unzip.scrbl"]
@include-section["tar.scrbl"]
@include-section["untar.scrbl"]
@include-section["untgz.scrbl"]
@include-section["md5.scrbl"]
@include-section["sha1.scrbl"]
@include-section["gif.scrbl"]
@include-section["ico.scrbl"]
@include-section["resource.scrbl"]
@include-section["cache.scrbl"]

@(bibliography
  (bib-entry #:key "Gervautz1990"
   #:author "M. Gervautz and W. Purgathofer"
   #:title "A simple method for color quantization: Octree quantization"
   #:location "Graphics Gems"
   #:date "1990")
  
  (bib-entry #:key "Clark1996"
   #:author "Dean Clark"
   #:title "Color Quantization using Octrees"
   #:location "Dr. Dobbs Journal"
   #:date "January 1, 1996"
   #:url "http://www.ddj.com/184409805"))

@index-section[]
