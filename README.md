# simple-http [![Build Status](https://travis-ci.org/DarrenN/simple-http.svg?branch=master)](https://travis-ci.org/DarrenN/simple-http) [![Coverage Status](https://coveralls.io/repos/github/DarrenN/simple-http/badge.svg?branch=master)](https://coveralls.io/github/DarrenN/simple-http?branch=master)

**:warning: You probably want to use [http-easy](https://docs.racket-lang.org/http-easy/index.html) instead.**

Make simple HTTP requests with Racket

```racket
 (require simple-http) package: simple-http
```

Very small library for making HTTP requests. Attempts to handle the
server response and convert into the correct data type based on
Content-Type headers. Heavily inspired and influenced by
[racket-request](https://github.com/jackfirth/racket-request).

The general use case for this is when you want to make repeated requests
to an API. You can setup a requester with the relevant auth headers and
base URL, then make requests using the available procedures, `get`,
`put`, etc.

Responses are parsed into `jsexpr?`, `xexpr?`, or `string?` depending on
which `requester?` is used.

**Example JSON request and response:**

```racket
; Setup a json-request using SSL and pointed at httpbin.org
(define httpbin-org                                        
  (update-headers                                          
    (update-ssl                                            
      (update-host json-requester "httpbin.org") #t)       
   '("Authorization: 8675309")))                           
                                                           
; Query params for the request                             
(define params '((foo . "12") (bar . "hello")))            
                                                           
; Make a GET to https://httpbin.org/get?foo=12&bar=hello   
(define response (get httpbin-org "/get" #:params params)) 
```

Example:

```racket
> response                                                        
(json-response                                                    
 "HTTP/1.1 200 OK"                                                
 '#hasheq((X-Powered-By . ("Flask"))                              
          (Access-Control-Allow-Credentials . ("true"))           
          (Connection . ("close"))                                
          (Content-Type . ("application/json"))                   
          (Via . ("1.1 vegur"))                                   
          (Date . ("Sun, 14 May 2017 00:34:47 GMT"))              
          (X-Processed-Time . ("0.000745058059692"))              
          (Access-Control-Allow-Origin . ("*"))                   
          (Server . ("meinheld/0.6.1"))                           
          (Content-Length . ("408")))                             
 '#hasheq((url . "https://httpbin.org/get?foo=12&bar=hello")      
          (headers                                                
           .                                                      
           #hasheq((Authorization . "8675309")                    
                   (Host . "httpbin.org")                         
                   (Connection . "close")                         
                   (Content-Type . "application/json")            
                   (Accept . "application/json")                  
                   (Accept-Encoding . "gzip")                     
                   (User-Agent . "Racket/6.8 (net/http-client)")))
          (origin . "255.255.255.255")                              
          (args . #hasheq((bar . "hello") (foo . "12")))))        
```

## 1. Requesters

Requests are made by creating requesters and using functions to update
them with relevant data such as base url, use SSL, etc.

```racket
(struct requester (host headers ssl type)  
    #:extra-constructor-name make-requester
    #:transparent)                         
  host : string?                           
  headers : hash?                          
  ssl : boolean?                           
  type : symbol?                           
```

Stores information about the types of requests to make \(headers\). You
usually don’t need to use this directly, pre-loaded requesters are
provided for you.

```racket
html-requester
json-requester
text-requester
xml-requester 
```

Pre-made requesters for requesting specific formats. Requests made with
these will send the appropriate Accept and Content-Type headers.
Responses will also be parsed according to their request type.

```racket
(update-host requester host) -> requester?
  requester : requester?                  
  host : string?                          
```

Change the host string in a requester. Returns the updated requester.

```racket
(update-ssl requester ssl) -> requester?
  requester : requester?                
  ssl : boolean?                        
```

Change whether the requester should make SSL requests or not. When set
to \#t requests will be changed to https. Returns the updated requester.

```racket
(update-headers requester headers) -> requester?
  requester : requester?                        
  headers : (listof string?)                    
```

Merges `headers` into the existing headers in the requester and returns
the requester. Any headers passed to the function will overwrite any
existing headers in the `requester?` with the same header key.

`(update-headers req '("Authorization: 8675309" "x-foo: oof"))`

## 2. Making requests

Requests are made using requesters to determine how data should be sent
and received. Responses are returned as structs with their data encoded
based on the requester used. If the server returns an error code a
`exn:fail:network:http:error?` will be raised. If the data returned from
the server is in a different format than requested or cannot be properly
parsed then a `exn:fail:network:http:read?` will be raised.

```racket
(get requester uri [#:params params])                                
 -> (or/c html-response? json-response? text-response? xml-response?)
  requester : requester?                                             
  uri : string?                                                      
  params : (listof pair?) = '()                                      
```

Makes a `GET` request using the provided `requester`. `#:params` will be
combined onto the URL as query parameters. Responses are structs.

```racket
(get                                            
 (update-host json-requester "httpbin.org")     
 "/get" #:params '((foo . "12") (bar . "quux")))
```

```racket
(post  requester                                                     
       uri                                                           
      [#:data data                                                   
       #:params params])                                             
 -> (or/c html-response? json-response? text-response? xml-response?)
  requester : requester?                                             
  uri : string?                                                      
  data : any/c = #f                                                  
  params : (listof pair?) = '()                                      
```

Makes a `POST` request using the provided `requester`. `#:params` will
be combined onto the URL as query parameters. `#:data` is sent to the
server in the body of the request. Responses are structs.

```racket
(post                                                        
 (update-host json-requester "httpbin.org")                  
 "/post"                                                     
 #:data (jsexpr->string (hasheq 'colossal-squid "drumbones"))
 #:params '((sort . "asc") (filter . "hits")))               
```

```racket
(put  requester                                                      
      uri                                                            
     [#:data data                                                    
      #:params params])                                              
 -> (or/c html-response? json-response? text-response? xml-response?)
  requester : requester?                                             
  uri : string?                                                      
  data : any/c = #f                                                  
  params : (listof pair?) = '()                                      
```

Makes a `PUT` request using the provided `requester`. `#:params` will be
combined onto the URL as query parameters. `#:data` is sent to the
server in the body of the request. Responses are structs.

```racket
(put                                                         
 (update-host json-requester "httpbin.org")                  
 "/put"                                                      
 #:data (jsexpr->string (hasheq 'white-zombie "thunderkiss"))
 #:params '((sort . "asc")))                                 
```

```racket
(patch  requester                                                    
        uri                                                          
       [#:data data                                                  
        #:params params])                                            
 -> (or/c html-response? json-response? text-response? xml-response?)
  requester : requester?                                             
  uri : string?                                                      
  data : any/c = #f                                                  
  params : (listof pair?) = '()                                      
```

Makes a `PATCH` request using the provided `requester`. `#:params` will
be combined onto the URL as query parameters. `#:data` is sent to the
server in the body of the request. Responses are structs.

```racket
(patch                                                       
 (update-host json-requester "httpbin.org")                  
 "/patch"                                                    
 #:data (jsexpr->string (hasheq 'white-zombie "thunderkiss"))
 #:params '((sort . "asc")))                                 
```

```racket
(delete requester uri [#:params params])                             
 -> (or/c html-response? json-response? text-response? xml-response?)
  requester : requester?                                             
  uri : string?                                                      
  params : (listof pair?) = '()                                      
```

Makes a `DELETE` request using the provided `requester`. `#:params` will
be combined onto the URL as query parameters. Responses are structs.

```racket
(delete                                    
 (update-host json-requester "httpbin.org")
 "/delete"                                 
 #:params '((sort . "asc")))               
```

## 3. Responses

```racket
(struct html-response (status headers body)    
    #:extra-constructor-name make-html-response
    #:transparent)                             
  status : string?                             
  headers : hash?                              
  body : xexpr?                                
(struct json-response (status headers body)    
    #:extra-constructor-name make-json-response
    #:transparent)                             
  status : string?                             
  headers : hash?                              
  body : jsexpr?                               
(struct text-response (status headers body)    
    #:extra-constructor-name make-text-response
    #:transparent)                             
  status : string?                             
  headers : hash?                              
  body : string?                               
(struct xml-response (status headers body)     
    #:extra-constructor-name make-xml-response 
    #:transparent)                             
  status : string?                             
  headers : hash?                              
  body : xexpr?                                
```

Response bodies are decoded into a form determined by the requester used
to make requests. If the response comes back in a form that can’t be
processed or throws an error duing processing then a
`exn:fail:network:http:read?` exception will be raised.

`status` is a `string?` such as `"HTTP/1.1 200 OK"`. `headers` are a key
value representation of the response headers:

```racket
#hasheq((Host . "httpbin.org")             
        (Connection . "close")             
        (Content-Type . "application/json")
        (Accept . "application/json"))     
```

## 4. Exceptions

```racket
(struct exn:fail:network:http:read (message                 
                                    continuation-marks      
                                    type)                   
    #:extra-constructor-name make-exn:fail:network:http:read
    #:transparent)                                          
  message : string?                                         
  continuation-marks : continuation-mark-set?               
  type : symbol?                                            
```

Raised when a response is either the wrong format \(ex: HTML returned
for a JSON request\) or malformed and cannot be parsed by the relevant
reader. `type` is a symbol to let you know which requester made the
request, such as `'json`, etc.

```racket
(struct exn:fail:network:http:error (message                 
                                     continuation-marks      
                                     code                    
                                     type)                   
    #:extra-constructor-name make-exn:fail:network:http:error
    #:transparent)                                           
  message : string?                                          
  continuation-marks : continuation-mark-set?                
  code : number?                                             
  type : symbol?                                             
```

Raised when the server responds with an error code. `code` is the
numeric error code returned by the server. `type` is a `symbol?` letting
you know which requester made the request \(ex: `'json`\).

---

<a href="http://racket-lang.org/"><img src="http://racket-lang.org/img/racket-logo.svg" width="80" height="80" /></a>
