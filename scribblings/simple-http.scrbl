#lang scribble/manual
@require[racket
         scribble/eval
         simple-http
         @for-label[simple-http
                    racket]]

@(define http-eval (make-base-eval))
@(http-eval `(require racket simple-http json))

@title{simple-http}
@author[(author+email "Darren Newton" "info@v25media.com")]

@defmodule[simple-http]

Very small library for making HTTP requests. Attempts to handle the server
response and convert into the correct data type based on Content-Type headers.

@bold{Example JSON request and response:}

@examples[#:eval http-eval
          (define httpbin-org
            (update-ssl (update-host json-requester "httpbin.org") #t))
          (define params '((foo . "12") (bar . "hello")))
          (get httpbin-org "/get" #:params params)]

