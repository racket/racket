#lang scribble/manual
@(require (for-label scribble/struct
                     scriblib/bibtex
                     scriblib/autobib
                     racket/base
                     racket/contract))

@title[#:tag "bibtex"]{BibTeX Bibliographies}

@defmodule[scriblib/bibtex]

@defform[(define-bibtex-cite bib-pth ~cite-id citet-id generate-bibliography-id)]{

Parses @racket[bib-pth] as a BibTeX database.

Uses @racket[define-cite] from @racketmodname[scriblib/autobib], but augments the @racket[~cite-id] and @racket[citet-id] functions so that rather than accepting @racket[bib?] structures, they accept citation key strings.
       
Each string is broken along spaces into citations keys that are looked up in the BibTeX database and turned into @racket[bib?] structures.

}

@defstruct*[bibdb ([raw (hash/c string? (hash/c string? string?))]
                   [bibs (hash/c string? bib?)])]{
                                             Represents a BibTeX database. The @racket[_raw] hash table maps the labels in the file to hash tables of the attributes and their values. The @racket[_bibs] hash table maps the same labels to Scribble data-structures representing the same information.
                                             }

@defproc[(path->bibdb [path path-string?])
         bibdb?]{
                 Parses a path into a BibTeX database.
                 }

@defproc[(bibtex-parse [ip input-port?])
         bibdb?]{
                 Parses an input port into a BibTeX database.
                 }
