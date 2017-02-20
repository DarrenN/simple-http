#lang info
(define collection "simple-http")
(define deps '("base"
               "rackunit-lib"
               "html-parsing"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/simple-http.scrbl" ())))
(define pkg-desc "Simple interface for making HTTP requests")
(define version "0.1")
(define pkg-authors '(Darren_N))
