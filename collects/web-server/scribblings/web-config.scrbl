#lang scribble/doc
@(require "web-server.ss"
          (for-label web-server/web-server-unit
                     web-server/web-server-sig
                     web-server/web-config-unit
                     web-server/web-config-sig
                     web-server/configuration/configuration-table))

@title{Web Servers}

A Web server is a unit with the @scheme[web-server^] signature. The most common way to construct one is to provide a @scheme[web-config^] unit to the
@scheme[web-server@] unit. The most common way to construct a @scheme[web-config^] unit is to use @scheme[configuration-table->web-config@] to produce
one from a configuration table file, such as the one that is shipped with PLT Scheme in @scheme[default-configuration-table-path].

@include-section["web-server-unit.scrbl"]
@include-section["web-config-unit.scrbl"]
@include-section["ctable.scrbl"]
@include-section["ctable-structs.scrbl"]
@include-section["responders.scrbl"]
