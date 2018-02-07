#lang racket/base

(require racket/dict racket/string
         web-server/servlet
         web-server/servlet-env
         ;web-server/http/request-structs
         web-server/http/redirect)
(require "example.rkt")

(define mime-types #hash(("xml" . #"application/xml")
                         ("sxml" . #"text/plain")  ; browsers have no idea what to do with this stuff
                         ;("sxml" . #"text/x-sxml")  ; FIXME not 100% sure about this? http://okmij.org/ftp/Scheme/soap-sxml.txt
                         ("json" . #"application/json")
                         ("html" . #"text/html")))

(define (get-mime-type behavior code extension)
  ;(displayln (list 'get-mime-type behavior code extension))
  (if (eq? behavior 'redirect)
      #""
      (cond ((>= code 400) (dict-ref mime-types "html"))
            (extension (dict-ref mime-types extension #"text/html"))
            (#t #"text/html"))))

(define (resolver req)
  ;(displayln req)
  ; note to self proto- means something akin to 'embryonic' or 'not yet fully formed' 'will be modified'
  (let* ([proto-path (path/param-path (car (url-path (request-uri req))))]  ; oh let* you are imperative...
         [proto-ext (member #\. (string->list proto-path))]
         [ext (if proto-ext
                  (list->string (cdr proto-ext))
                  "xml")]
         [path (if proto-ext
                   (list->string (reverse (cdr (member #\. (reverse (string->list proto-path))))))
                   proto-path)]
         [behavior-resp (if proto-ext  ; extensions are always local
                            `(local ,(rrid->record path #:format ext))
                            (rrid->response path #:format ext))]
         [behavior (car behavior-resp)]
         [resp (cond ((not (string-prefix? path "RRID")) null)
                     (#t (cadr behavior-resp)))]
         [code (cond ((null? resp) 404)
                     (proto-ext 200)  ; RRIDs followed by and extension are no longer cooluris
                     ((eq? behavior 'redirect) 302)
                     ((eq? behavior 'local) 303)  ; standard cooluri behavior
                     (#t 404))]
         [message (if (< code 400) #"OK" #"Not Found etc.")]
         [mime-type (get-mime-type behavior code ext)])
    ;(displayln `(proto-path ,proto-path))
    ;(displayln `(resp ,resp))
    ;(displayln `(behavior ,behavior))
    ;(displayln `(code ,code))
    ;(when (equal? path "RRID:sleep") (sleep 10))  ; continuations are cool
    (cond ((null? resp) (response/full code message (current-seconds) mime-type '() (list (string->bytes/utf-8 "Nothing to see here."))))
          ((and (eq? behavior 'redirect) (= code 302)) (redirect-to resp temporarily))
          ;((and (eq? behavior 'redirect) (= code 303)) (redirect-to resp see-other))
          ;((null? proto-path) (response/full code message (current-seconds) mime-type '() (list (string->bytes/utf-8 resp))))
          (#t (response/full code message (current-seconds) mime-type '() (list (string->bytes/utf-8 resp)))))))
        ;(response code (string->bytes/utf-8 resp) (current-seconds) mime-type '() void))))  ; instead of void could stream

(define (responder url exn)
  (response/full 500 #"Internal Server Error"
                 (current-seconds) #"text/html"
                 '() '(#"Internal Server Error")))

(define (404-responder request)
  (response/full 404 #"File not found."
                 (current-seconds) #"text/html"
                 '() '(#"File not found.")))

(define path "/")
(define (run)
  (serve/servlet resolver
                 #:port 4483
                 #:servlet-path path
                 #:server-root-path "/dev/null"  ; FIXME not portable...
                 ;#:servlet-regexp (regexp (format "^~a(RRID:.+)*$" (regexp-quote path)))
                 #:servlet-regexp #rx""
                 #:servlet-responder responder  ; prevent stack trace from leaking
                 #:file-not-found-responder 404-responder
                 #:stateless? #t
                 #:banner? #f
                 #:launch-browser? #f
                 #:command-line? #t))
(run)
