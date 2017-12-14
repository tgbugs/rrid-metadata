#lang racket/base

(require web-server/servlet
         web-server/servlet-env
         ;web-server/http/request-structs
         web-server/http/redirect)
(require "rrid-metadata-example.rkt")

(define (resolver req)
  (displayln req)
  (let* ([proto-path (path/param-path (car (url-path (request-uri req))))]  ; oh let* you are imperative...
         [proto-ext (member #\. (string->list proto-path))]
         [ext (if proto-ext
                  (list->string (cdr proto-ext))
                  'xml)]
         [path (if proto-ext
                   (list->string (reverse (cdr (member #\. (reverse (string->list proto-path))))))
                   proto-path)]
         [code-resp (rrid->response path #:xml #t)]  ; TODO more types
         [proto-code (car code-resp)]
         [code (if (and (= proto-code 303) proto-ext) 200 proto-code)]  ; explicit requests for xml should ↓
         [resp (cond ((not (= code proto-code)) (rrid->record path #:xml #t))  ; oof... this is crappy code, everything is out of order
                     ((or (equal? path "index.html") (equal? path "")) "")  ; FIXME doesn't actually work
                     ((< code 400) (cadr code-resp))(#t ""))]
         [message (if (< code 400) #"OK" #"Not Found etc.")]
         [mime-type (if (< code 300) #"application/xml" #"text/html")])
    ;(displayln resp)
    ;(when (equal? path "RRID:sleep") (sleep 10))  ; continuations are cool
    (cond ((= code 303) (redirect-to resp see-other))
          ((= code 302) (redirect-to resp temporarily))
          (#t (response/full code message (current-seconds) mime-type '() (list (string->bytes/utf-8 resp)))))))
        ;(response code (string->bytes/utf-8 resp) (current-seconds) mime-type '() void))))  ; instead of void could stream

(define (responder url exn)
  (response/full 500 #"Internal Server Error"
                 (current-seconds) #"text/html"
                 '() '(#"Internal Server Error")))

(define path "/")
(serve/servlet resolver
                #:port 4483
                #:servlet-path path
                #:servlet-regexp (regexp (format "^~a(RRID:.+)*$" (regexp-quote path)))
                #:servlet-responder responder  ; prevent stack trace from leaking
                #:stateless? #t
                #:banner? #f
                #:launch-browser? #f
                #:command-line? #t)
