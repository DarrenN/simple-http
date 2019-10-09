#lang racket

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>

(require racket/string
         html-parsing
         json
         net/base64
         net/http-client
         net/uri-codec
         xml)

(provide JSON-HEADERS
         HTML-HEADERS
         XML-HEADERS
         TEXT-HEADERS
         POST-FORM
         html-requester
         json-requester
         xml-requester
         text-requester
         update-host
         update-headers
         update-port
         update-ssl
         authenticate-basic
         get
         post
         put
         patch
         delete
         get-status
         get-response-type
         get-headers
         http-error?
         http-redirect?
         http-success?
         http-exn-of-type?
         (struct-out exn:fail:network:http:read)
         (struct-out exn:fail:network:http:error)
         (struct-out requester)
         (struct-out text-response)
         (struct-out html-response)
         (struct-out json-response)
         (struct-out xml-response))

(define JSON-HEADERS '("Accept: application/json"
                       "Content-Type: application/json"))

(define HTML-HEADERS '("Accept: text/html; charset=utf-8"))
(define XML-HEADERS '("Accept: application/xml"))
(define TEXT-HEADERS '("Accept: text/plain; charset=utf-8"))
(define POST-FORM '("Content-Type: application/x-www-formurlencoded"))

;; Responses are packed into structs to allow easy matching

(struct text-response (status headers body) #:transparent)
(struct html-response (status headers body) #:transparent)
(struct json-response (status headers body) #:transparent)
(struct xml-response (status headers body) #:transparent)

;; Requesters carry context for HTTP calls (host, ssl?, default headers...)
(struct requester (host headers port ssl type) #:transparent)

;; TODO: Maybe add a form-post-requester

(define html-requester (requester "" HTML-HEADERS #f #f 'html))
(define json-requester (requester "" JSON-HEADERS #f #f 'json))
(define xml-requester (requester "" XML-HEADERS #f #f 'xml))
(define text-requester (requester "" TEXT-HEADERS #f #f 'text))

;; Header utilities
;; ================

(define (get-headers resp)
  (cond
    [(json-response? resp) (json-response-headers resp)]
    [(html-response? resp) (html-response-headers resp)]
    [(xml-response? resp) (xml-response-headers resp)]
    [(text-response? resp) (text-response-headers resp)]))

;; Convert header list into immutable hash
(define (map-headers headers)
  (define (make-pairs s)
    (let* ([key (car (regexp-match #rx"[A-Za-z-]*:" s))]
           [val (string-replace s key "")])
      (list (string->symbol
             (string-titlecase (string-trim (string-replace key ":" ""))))
            (string-trim val))))
  (make-immutable-hasheq
   (map (λ (s)
          (make-pairs
           (if (bytes? s)
               (bytes->string/utf-8 s)
               s)))
        headers)))

;; Merges header list onto successive lists.
;; Removes duplicate headers (first set in arg position wins) and sorts list
(define (merge-headers . hs)
  (let* ([headers (apply append (reverse hs))]
         [hm (map-headers headers)]
         [hl (hash->list hm)])
    (sort (map (λ (s) (format "~a: ~a" (car s) (cadr s))) hl) string<?)))

;; Requester updaters
;; ==================

(define (update-host req nhost)
  (struct-copy requester req [host nhost]))

(define (update-headers req nheaders)
  (struct-copy requester req
               [headers (merge-headers
                         nheaders
                         (requester-headers req))]))

(define (update-port req nport)
  (struct-copy requester req [port nport]))

(define (update-ssl req nssl)
  (struct-copy requester req [ssl nssl]))

(define (params->string ps)
  (if (not (empty? ps))
      (format "?~a" (alist->form-urlencoded ps))
      ""))

;; TODO: rename this?
(define (make-uri uri [params '()])
  (format "~a~a" uri (params->string params)))

;; Constructs a basic auth header and adds it
(define (authenticate-basic req username password)
  (let ([auth-header (string-append
                      "Authorization: Basic "
                      (bytes->string/utf-8
                       (base64-encode
                        (string->bytes/utf-8
                         (string-append username ":" password))
                        "")))])
    (update-headers req (list auth-header))))


;; Status Checking
;; ===============

(define (get-status resp)
  (cond
    [(json-response? resp) (json-response-status resp)]
    [(html-response? resp) (html-response-status resp)]
    [(xml-response? resp) (xml-response-status resp)]
    [(text-response? resp) (text-response-status resp)]))

(define (get-response-type resp)
  (cond
    [(json-response? resp) "json"]
    [(html-response? resp) "html"]
    [(xml-response? resp) "xml"]
    [(text-response? resp) "text"]))

;; HTTP Status Predicates
;; https://www.iana.org/assignments/http-status-codes/http-status-codes.xhtml

;; Return HTTP code as a number from response bytes
(define (get-status-code status-bytes)
  (string->number
   (car (regexp-match
         #px"\\d{3}"
         (bytes->string/utf-8 status-bytes)))))

(define (http-error? resp)
  (let ([status (get-status resp)])
    (regexp-match? #rx"4[0-9]*|5[0-9]*" status)))

(define (http-success? resp)
  (let ([status (get-status resp)])
    (regexp-match? #rx"20[0-8]|226" status)))

(define (http-redirect? resp)
  (let ([status (get-status resp)])
    (regexp-match? #rx"30[0-8]" status)))


;; Response handling
;; =================

;; Exception for response read errors
(struct exn:fail:network:http:read exn:fail (type) #:transparent)

;; Exception for HTTP errors - response is parsed
(struct exn:fail:network:http:error exn:fail:network (code type) #:transparent)

;; Regex Content-Type header to figure out what we have
(define (get-content-type headers-hash)
  (match (car (hash-ref headers-hash 'Content-Type))
    [(regexp #rx"application/json") 'json]
    [(regexp #rx"text/html") 'html]
    [(regexp #rx"application/xml") 'xml]
    [else 'text]))

;; Raise an error with the response
(define (make-http-read-exn headers response)
  (let ([type (get-content-type headers)]
        [body (port->string response)])
    (raise
     (exn:fail:network:http:read
      (~a body) (current-continuation-marks) type))))

;; Raise an error code, and inclue the response type
(define (make-http-error-exn code headers response)
  (let* ([type (get-content-type headers)]
         [body (cond
                 [(eq? type 'json) (read-json response)]
                 [(eq? type 'html) (html->xexp response)]
                 [(eq? type 'xml) (read-xml response)]
                 [else (port->string response)])])
    (raise
     (exn:fail:network:http:error
      (~a body) (current-continuation-marks) code type))))

;; Get the type of exception
(define (http-exn-of-type? type v)
  (and (exn:fail:network:http:read? v)
       (eq? type (exn:fail:network:http:read-type v))))

;; Try to read the error response
(define (parse-http-error-body exn)
  (let ([type (exn:fail:network:http:error-type exn)]
        [body (exn-message exn)])
    (cond
        [(eq? type 'json) (read-json body)]
        [(eq? type 'html) (html->xexp body)]
        [(eq? type 'xml) (read-xml body)]
        [else (port->string body)])))

;; Uses Content-Type to determine how to parse response data
(define (create-response status headers response)
  (with-handlers ([exn:fail? (λ (e) (make-http-read-exn headers response))])
    (let ([type (get-content-type headers)])
      (cond
        [(eq? type 'json)
         (json-response status headers (read-json response))]
        [(eq? type 'html)
         (html-response status headers (html->xexp response))]
        [(eq? type 'xml)
         (xml-response status headers (read-xml response))]
        [else
         (text-response status headers (port->string response))]))))

;; Make HTTP Requests
;; ==================

(define (requester-accept-header requester)
  (let ([type (requester-type requester)])
    (cond
      [(eq? type 'html) "text/html"]
      [(eq? type 'json) "application/json"]
      [(eq? type 'xml) "text/xml"]
      [(eq? type 'text) "text/plain"]
      [else "*"])))

; Compare the Accept header from the request to the Content-Type header in the
; response (using a regex) to try and figure out if we have the correct response
; type
(define (correct-content-type? requester resp-headers)
  (let* ([resh (map-headers resp-headers)]
         [accept (requester-accept-header requester)]
         [ctype (hash-ref resh 'Content-Type)])
    (regexp-match (regexp accept) (car ctype))))

;; define-http-method macro defines the interface for request functions
;; (define-http-method get '"GET") -> (get requester "/get")

(define-syntax (define-http-method stx)
  (syntax-case stx ()
    [(_ method verb)
     #'(define (method req uri #:data [data #f] #:params [params '()])
         (let* ([nuri (make-uri uri params)]
                [host (requester-host req)]
                [headers (requester-headers req)]
                [ssl (requester-ssl req)]
                [port (cond [(requester-port req)]
                            [ssl 443]
                            [else 80])])
           (let-values ([(status resp-headers response)
                         (http-sendrecv host nuri #:ssl? ssl #:port port
                                        #:method verb #:headers headers
                                        #:data data)])
             (define response-code (get-status-code status))

             ;; Raise HTTP exn if we get an HTTP error code
             (when (> response-code 399)
               (make-http-error-exn
                response-code
                (map-headers resp-headers)
                response))

             ;; Raise read exn if the requested content type doesn't match
             ;; the response content type and isn't a redirect
             (when
                 (and
                  (or (< response-code 300) (> response-code 309))
                  (false? (correct-content-type? req resp-headers)))
               (make-http-read-exn (map-headers resp-headers) response))

             (create-response
               (bytes->string/utf-8 status)
               (map-headers resp-headers)
               response))))]))

;; Sets up functions named after HTTP verbs

(define-http-method get '"GET")
(define-http-method post '"POST")
(define-http-method put '"PUT")
(define-http-method patch '"PATCH")
(define-http-method delete '"DELETE")

;; =====
;; Tests
;; =====

(module+ test
  (require rackunit)

  ; Requesters
  (check-pred requester? json-requester)
  (check-pred requester? html-requester)
  (check-pred requester? xml-requester)

  ; Headers
  (define headers (map-headers (map string->bytes/utf-8 JSON-HEADERS)))

  (check-pred hash-eq? headers)
  (for ([h (hash-keys headers)])
    (check-true (hash-has-key? headers h)))

  (define h0 (merge-headers '() '(#"foo: a")))
  (define h1 (merge-headers '(#"foo: a") '()))
  (define h2 (merge-headers '(#"foo: a" #"bar: b") '(#"baz: c")))
  (define h3 (merge-headers '(#"foo: a") '(#"foo: a" #"baz: c")))
  (define h4 (merge-headers '(#"foo: a") '(#"foo: b" #"baz: c")))
  (define h5 (merge-headers '(#"foo: b") '(#"foo: a") '(#"baz: c" #"foo: c")))
  (define h6 (merge-headers '("foo: b") '(#"foo: a") '("baz: c" #"foo: c")))
  (define h7 (merge-headers '("Content-Type: b") '(#"Content-type: a")
                            '("content-type: c" #"foo: c")))

  (check-equal? h0 '("Foo: a"))
  (check-equal? h1 '("Foo: a"))
  (check-equal? h2 '("Bar: b" "Baz: c" "Foo: a"))
  (check-equal? h3 '("Baz: c" "Foo: a"))

  ; first set of dupe headers wins: "foo: a"
  (check-equal? h4 '("Baz: c" "Foo: a"))

  ; first set of dupe headers wins: "foo: b"
  (check-equal? h5 '("Baz: c" "Foo: b"))

  ; can take bytes or strings
  (check-equal? h6 '("Baz: c" "Foo: b"))

  ; can deal with kludgy casing in header names
  (check-equal? h7 '("Content-Type: b" "Foo: c"))

  (define host0 (update-host html-requester "groundwork.com"))
  (check-equal? (requester-host html-requester) "")
  (check-equal? (requester-host host0) "groundwork.com")

  (define hdrs0 (update-headers html-requester '("X-Foo: Foo" "X-Bar: Bar")))
  (define hdrs1
    (update-headers
     html-requester
     '("Accept: text/html; charset=latin-1" "X-Bar: Bar")))

  (check-equal?
   (requester-headers hdrs0)
   '("Accept: text/html; charset=utf-8"
     "X-Bar: Bar"
     "X-Foo: Foo"))

  (check-equal?
   (requester-headers hdrs1)
   '("Accept: text/html; charset=latin-1"
     "X-Bar: Bar"))

  (define hdrs2
    (authenticate-basic
     html-requester
     "foo" "bar"))

  (check-equal?
   (requester-headers hdrs2)
   '("Accept: text/html; charset=utf-8"
     "Authorization: Basic Zm9vOmJhcg=="))

  (define port0 (update-port json-requester 8080))
  (check-equal? (requester-port port0) 8080)

  (define ssl0 (update-ssl json-requester #t))
  (check-equal? (requester-ssl ssl0) #t)

  ; Params
  (check-equal? (params->string '()) "")
  (check-equal? (params->string '((foo . "12"))) "?foo=12")
  (check-equal? (params->string '((foo . "12") (bar . "bar"))) "?foo=12&bar=bar")

  ; URI
  (check-equal? (make-uri "groundwork.com") "groundwork.com")
  (check-equal? (make-uri "groundwork.com" '((foo . "12") (bar . "baz")))
                "groundwork.com?foo=12&bar=baz")

  ; Status checking
  (define hds (make-hash '((Access-Control-Allow-Credentials . ("true")))))
  (define body "body")

  (define jresp (json-response "HTTP/1.1 200 OK" hds body))
  (define hresp (html-response "HTTP/1.1 200 OK" hds body))
  (define xresp (xml-response "HTTP/1.1 200 OK" hds body))
  (define tresp (text-response "HTTP/1.1 200 OK" hds body))

  (check-equal? (get-status jresp) "HTTP/1.1 200 OK")
  (check-equal? (get-status hresp) "HTTP/1.1 200 OK")
  (check-equal? (get-status xresp) "HTTP/1.1 200 OK")
  (check-equal? (get-status tresp) "HTTP/1.1 200 OK")

  ; Headers checking
  (check-equal? (get-headers jresp) hds)
  (check-equal? (get-headers hresp) hds)
  (check-equal? (get-headers xresp) hds)
  (check-equal? (get-headers tresp) hds)

  (define r200 (json-response "HTTP/1.1 200 OK" hds body))
  (define r201 (json-response "HTTP/1.1 201 Created" hds body))
  (define r209 (json-response "HTTP/1.1 209 Unassigned" hds body))
  (define r226 (json-response "HTTP/1.1 226 IM Used" hds body))
  (define r301 (json-response "HTTP/1.1 301 Moved Permanently" hds body))
  (define r400 (json-response "HTTP/1.1 400 Bad Request" hds body))
  (define r404 (json-response "HTTP/1.1 404 Not Found" hds body))
  (define r500 (json-response "HTTP/1.1 500 Server Error" hds body))
  (define r504 (json-response "HTTP/1.1 504 Gateway Timeout" hds body))

  (check-pred http-success? r200)
  (check-pred http-success? r201)
  (check-false (http-success? r209))
  (check-pred http-success? r226)

  (check-pred http-redirect? r301)
  (check-false (http-success? r301))

  (check-pred http-error? r400)
  (check-pred http-error? r404)
  (check-pred http-error? r500)
  (check-pred http-error? r504)

  ; Check responses

  (check-pred json-response?
              (call-with-values
               (λ () (values
                      "HTTP 1.1/200 OK"
                      (map-headers '("Content-Type: application/json"))
                      (open-input-bytes #"{\"foo\": 1}")))
               create-response))

  (check-pred html-response?
              (call-with-values
               (λ () (values
                      "HTTP 1.1/200 OK"
                      (map-headers '("Content-Type: text/html; charset=utf-8"))
                      (open-input-bytes
                       #"<html><head><title>Foo</title></head></html>")))
               create-response))

  (check-pred
   xml-response?
   (call-with-values
    (λ () (values
           "HTTP 1.1/200 OK"
           (map-headers '("Content-Type: application/xml; charset=utf-8"))
           (open-input-bytes
            #"<?xml version=\"1.0\" ?><foo>Foo</foo>")))
    create-response))

  (check-pred text-response?
              (call-with-values
               (λ () (values
                      "HTTP 1.1/200 OK"
                      (map-headers '("Content-Type: application/scheme"))
                      (open-input-bytes
                       #"'(foo bar baz quux)")))
               create-response))

  ; Check exceptions

  (check-exn exn:fail:network:http:read?
             (λ () (call-with-values
                    (λ () (values
                           "HTTP 1.1/200 OK"
                           (map-headers '("Content-Type: application/json"))
                           (open-input-bytes #"blurg")))
                    create-response)))

  (check-exn exn:fail:network:http:read?
             (λ () (call-with-values
                    (λ () (values
                           "HTTP 1.1/200 OK"
                           (map-headers '("Content-Type: application/xml"))
                           (open-input-bytes #"blarg")))
                    create-response)))

  ;; HTML Parsing is very permissive, so it won't really fail
  (check-not-exn
   (λ () (call-with-values
          (λ () (values
                 "HTTP 1.1/200 OK"
                 (map-headers '("Content-Type: text/html"))
                 (open-input-bytes #"<hurgle\n")))
          create-response)))

  ;; Check that the exception carries the request type, so we know what it was
  ;; we were asking for, ex: an exception on a JSON request should have
  ;; an http-exn-of-type? of 'json

  ;; Unreadable JSON response
  (check-true
   (http-exn-of-type?
    'json
    (with-handlers ([exn:fail:network:http:read? (λ (exn) exn)])
      (call-with-values
       (λ () (values
              "HTTP 1.1/200 OK"
              (map-headers '("Content-Type: application/json"))
              (open-input-bytes #"blurg")))
       create-response))))

  ;; Unreadable XML response
  (check-true
   (http-exn-of-type?
    'xml
    (with-handlers ([exn:fail:network:http:read? (λ (exn) exn)])
      (call-with-values
       (λ () (values
              "HTTP 1.1/200 OK"
              (map-headers '("Content-Type: application/xml"))
              (open-input-bytes #"blurg")))
       create-response)))))

;; Integrations tests
(module+ integration-test
  (require rackunit
           json)

  ;; JSON GET requests

  (define httpbin-json (update-host json-requester "httpbin.org"))
  (define httpbin-html (update-host html-requester "httpbin.org"))
  (define json-get (get httpbin-json "/get"))
  (define json-get-body (json-response-body json-get))

  (check-pred http-success? json-get)
  (check-equal? (json-response-status json-get) "HTTP/1.1 200 OK")
  (check-pred jsexpr? json-get-body)

  (define https-json (update-ssl httpbin-json #t))
  (define json-ssl-get (get https-json "/get"))

  (check-equal?
   (hash-ref (json-response-body json-ssl-get) 'url)
   "https://httpbin.org/get")

  (define json-ssl-params (get https-json "/get" #:params
                               '((query . "huevos"))))

  (check-equal?
   (hash-ref (hash-ref (json-response-body json-ssl-params) 'args) 'query)
   "huevos")

  (check-exn
   exn:fail:network:http:read?
   (λ () (get httpbin-json "/html")))

  ;; Error codes have a Content-Type
  (check-equal?
   (with-handlers
       ([exn:fail:network:http:error?
         (λ (e) (exn:fail:network:http:error-type e))])
     (get httpbin-json "/code/405")) 'html)

  ;; JSON POST/PUT/PATCH requests

  (define json-data
    (jsexpr->string (make-hasheq '((colossal-squid . "drumbones")))))

  (define json-post (post httpbin-json "/post" #:data json-data))
  (check-equal?
   (hash-ref (json-response-body json-post) 'data)
   json-data)

  (define json-put (put httpbin-json "/put" #:data json-data))
  (check-equal?
   (hash-ref (json-response-body json-put) 'data)
   json-data)

  (define json-patch (patch httpbin-json "/patch" #:data json-data))
  (check-equal?
   (hash-ref (json-response-body json-patch) 'data)
   json-data)

  ;; Error Handling

  ; 405
  (check-exn
   exn:fail:network:http:error?
   (λ () (post httpbin-json "/put" #:data json-data)))

  ; 404
  (check-exn
   exn:fail:network:http:error?
   (λ () (get httpbin-json "/status/404")))

  ; 503
  (check-exn
   exn:fail:network:http:error?
   (λ () (get httpbin-json "/status/503")))

  ; Error response is parsed
  (check-pred
   xexpr?
   (with-handlers ([exn:fail:network:http:error? (λ (e) (exn-message e))])
     (get httpbin-json "/status/503")))

  ;; Redirect Handling
  (check-pred http-redirect? (get httpbin-json "/redirect/1"))
  (check-pred http-redirect? (get httpbin-json "/redirect/5"))
  (check-pred http-redirect?
              (get httpbin-json "/redirect-to" #:params '((url . "foo"))))

  ;; Success Handling
  (check-pred http-success? (get httpbin-html "/status/201"))
  (check-pred http-success? (get httpbin-html "/status/200")))
