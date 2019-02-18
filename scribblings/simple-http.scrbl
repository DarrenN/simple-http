#lang scribble/manual
@require[racket
         scribble/eval
         simple-http
         json
         xml
         @for-label[simple-http
                    json
                    xml
                    racket]]

@(define EVAL
   (make-base-eval
    #:lang 'racket
    '(require simple-http json)))

@title{simple-http}
@author[(author+email "Darren Newton" "info@v25media.com")]

@defmodule[simple-http]

Very small library for making HTTP requests. Attempts to handle the server
response and convert into the correct data type based on Content-Type headers.
Heavily inspired and influenced by
@hyperlink["https://github.com/jackfirth/racket-request"]{racket-request}.

The general use case for this is when you want to make repeated requests to an
API. You can setup a requester with the relevant auth headers and base URL, then
make requests using the available procedures, @racket[get], @racket[put], etc.

Responses are parsed into @racket[jsexpr?], @racket[xexpr?], or @racket[string?]
depending on which @racket[requester?] is used.

@bold{Example JSON request and response:}

@#reader scribble/comment-reader
(racketblock
          ; Setup a json-request using SSL and pointed at httpbin.org
          (define httpbin-org
            (update-headers
              (update-ssl
                (update-host json-requester "httpbin.org") #t)
             '("Authorization: 8675309")))

          ; Query params for the request
          (define params '((foo . "12") (bar . "hello")))
          
          ; Make a GET to https://httpbin.org/get?foo=12&bar=hello
          (define response (get httpbin-org "/get" #:params params)))

@bold{Response:}

@racketblock[
(json-response
 "HTTP/1.1 200 OK"
 '#hasheq((X-Powered-By . ("Flask"))
          (Access-Control-Allow-Credentials . ("true"))
          (Connection . ("close"))
          (Content-Type . ("application/json"))
          (Via . ("1.1 vegur"))
          (Date . ("Sun, 14 May 2017 00:18:16 GMT"))
          (X-Processed-Time . ("0.000797033309937"))
          (Access-Control-Allow-Origin . ("*"))
          (Server . ("meinheld/0.6.1"))
          (Content-Length . ("408")))
 '#hasheq((url . "https://httpbin.org/get?foo=12&bar=hello")
          (headers . #hasheq((Authorization . "8675309")
                             (Host . "httpbin.org")
                             (Connection . "close")
                             (Content-Type . "application/json")
                             (Accept . "application/json")
                             (Accept-Encoding . "gzip")
                             (User-Agent . "Racket/6.8 (net/http-client)")))
          (origin . "255.255.255.255")
          (args . #hasheq((bar . "hello") (foo . "12")))))
 ]

@section{Requesters}

Requests are made by creating requesters and using functions to update them with
relevant data such as base url, use SSL, etc.

@defstruct[
 requester ([host string?] [headers hash?] [port integer?] [ssl boolean?] [type symbol?])
 #:transparent]{
  Stores information about the types of requests to make (headers). You usually
  don't need to use this directly, pre-loaded requesters are provided for you.
}

@deftogether[(
  @defidform[html-requester]
  @defidform[json-requester]
  @defidform[text-requester]
  @defidform[xml-requester]
)]{
 Pre-made requesters for requesting specific formats. Requests made with these
 will send the appropriate Accept and Content-Type headers. Responses will also
 be parsed according to their request type.
}

@defproc[(update-host
          [requester requester?]
          [host string?])
         requester?]{
  Change the host string in a requester. Returns the updated requester.
}

@defproc[(update-port
          [requester requester?]
          [port integer?])
         requester?]{
  Change the port in a requester. This overrides the default port, which is
  443 for SSL requests or 80 otherwise. Returns the updated requester.
}

@defproc[(update-ssl
          [requester requester?]
          [ssl boolean?])
         requester?]{
  Change whether the requester should make SSL requests or not. When set to #t
  requests will be changed to https. Returns the updated requester.
}

@defproc[(update-headers
          [requester requester?]
          [headers (listof string?)])
         requester?]{
  Merges @racket[headers] into the existing headers in the requester and
  returns the requester. Any headers passed to the function will overwrite
  any existing headers in the @racket[requester?] with the same header key.

  @racket[(update-headers req '("Authorization: 8675309" "x-foo: oof"))]
}

@defproc[(authenticate-basic
          [requester requester?]
          [username string?]
          [password string?])
         requester?]{
  Constructs an http-basic header from @racket[username] and
  @racket[password], and then merges that header into the headers of
  the @racket[requester] using @racket[update-headers].

  @racket[(authenticate-basic req "sam@example.com" "opensesame")]
}

@section{Making requests}

Requests are made using requesters to determine how data should be sent and
received. Responses are returned as structs with their data encoded based on the
requester used. If the server returns an error code a
@racket[exn:fail:network:http:error?] will be raised. If the data returned from
the server is in a different format than requested or cannot be properly parsed
then a @racket[exn:fail:network:http:read?] will be raised.

@defproc[(get
          [requester requester?]
          [uri string?]
          [#:params params  (listof pair?) '()])
          (or/c html-response? json-response? text-response? xml-response?)]{
 Makes a @racket[GET] request using the provided @racket[requester].
 @racket[#:params] will be combined onto the URL as query parameters. Responses
 are structs.

 @racketblock[(get
               (update-host json-requester "httpbin.org")
               "/get" #:params '((foo . "12") (bar . "quux")))]
}

@defproc[(post
          [requester requester?]
          [uri string?]
          [#:data data any/c #f]
          [#:params params  (listof pair?) '()])
          (or/c html-response? json-response? text-response? xml-response?)]{
 Makes a @racket[POST] request using the provided @racket[requester].
 @racket[#:params] will be combined onto the URL as query parameters.
 @racket[#:data] is sent to the server in the body of the request. Responses
 are structs.

 @racketblock[(post
               (update-host json-requester "httpbin.org")
               "/post"
               #:data (jsexpr->string (hasheq 'colossal-squid "drumbones"))
               #:params '((sort . "asc") (filter . "hits")))]
}

@defproc[(put
          [requester requester?]
          [uri string?]
          [#:data data any/c #f]
          [#:params params  (listof pair?) '()])
          (or/c html-response? json-response? text-response? xml-response?)]{
 Makes a @racket[PUT] request using the provided @racket[requester].
 @racket[#:params] will be combined onto the URL as query parameters.
 @racket[#:data] is sent to the server in the body of the request. Responses
 are structs.

 @racketblock[(put
               (update-host json-requester "httpbin.org")
               "/put"
               #:data (jsexpr->string (hasheq 'white-zombie "thunderkiss"))
               #:params '((sort . "asc")))]
}


@defproc[(patch
          [requester requester?]
          [uri string?]
          [#:data data any/c #f]
          [#:params params  (listof pair?) '()])
          (or/c html-response? json-response? text-response? xml-response?)]{
 Makes a @racket[PATCH] request using the provided @racket[requester].
 @racket[#:params] will be combined onto the URL as query parameters.
 @racket[#:data] is sent to the server in the body of the request. Responses
 are structs.

 @racketblock[(patch
               (update-host json-requester "httpbin.org")
               "/patch"
               #:data (jsexpr->string (hasheq 'white-zombie "thunderkiss"))
               #:params '((sort . "asc")))]
}

@defproc[(delete
          [requester requester?]
          [uri string?]
          [#:params params  (listof pair?) '()])
          (or/c html-response? json-response? text-response? xml-response?)]{
 Makes a @racket[DELETE] request using the provided @racket[requester].
 @racket[#:params] will be combined onto the URL as query parameters.
 Responses are structs.

 @racketblock[(delete
               (update-host json-requester "httpbin.org")
               "/delete"
               #:params '((sort . "asc")))]
}

@section{Responses}

@deftogether[(
  @defstruct[
    html-response ([status string?] [headers hash?] [body xexpr?])
    #:transparent]
 @defstruct[
   json-response ([status string?] [headers hash?] [body jsexpr?])
   #:transparent]
 @defstruct[
   text-response ([status string?] [headers hash?] [body string?])
   #:transparent]
 @defstruct[
   xml-response ([status string?] [headers hash?] [body xexpr?])
   #:transparent]
)]{
  Response bodies are decoded into a form determined by the requester used to
  make requests. If the response comes back in a form that can't be processed
  or throws an error duing processing then a
  @racket[exn:fail:network:http:read?] exception will be raised.

  @racket[status] is a @racket[string?] such as @racket["HTTP/1.1 200 OK"].
  @racket[headers] are a key value representation of the response headers:
  @racketblock[
    #hasheq((Host . "httpbin.org")
            (Connection . "close")
            (Content-Type . "application/json")
            (Accept . "application/json"))
  ] 
}

@section{Exceptions}

@defstruct[
   exn:fail:network:http:read
   ([message string?]
    [continuation-marks continuation-mark-set?] 
    [type symbol?])
   #:transparent]{
  Raised when a response is either the wrong format (ex: HTML returned for a
JSON request) or malformed and cannot be parsed by the relevant reader.
@racket[type] is a symbol to let you know which requester made the request,
such as @racket['json], etc.
}

@defstruct[
   exn:fail:network:http:error
   ([message string?]
    [continuation-marks continuation-mark-set?]
    [code number?]
    [type symbol?])
   #:transparent]{
  Raised when the server responds with an error code. @racket[code] is the
numeric error code returned by the server. @racket[type] is a @racket[symbol?]
letting you know which requester made the request (ex: @racket['json]).
}
