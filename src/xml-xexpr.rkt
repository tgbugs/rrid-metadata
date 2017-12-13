#!/usr/bin/env racket
#lang racket
;; some helpful friends
;; https://github.com/racket/racket/blob/master/racket/collects/xml/plist.rkt#L188
;; https://github.com/interplinternet/Racket-MAL-Client/blob/master/mal.rkt
;; see also
;; https://docs.racket-lang.org/sxml-intro/index.html very much consdier sxml << have considered is much better
(require xml
         xml/xexpr
         sxml
         racket/pretty
         compatibility/mlist)
(provide load-xml save-xml
         load-sxml save-sxml
         save-xexpr
         save-sexp)

(define rdf-no-ws '(rdf:RDF
                    owl:equivalentClass
                    rdfs:subClassOf
                    rdfs:subPropertyOf
                    owl:Ontology
                    owl:AnnotationProperty
                    owl:ObjectProperty
                    owl:Class
                    owl:Restriction
                    owl:Axiom
                    owl:equivalentClass
                    owl:equivalentClass
                    owl:intersectionOf
                    owl:onProperty
                    owl:someValuesFrom
                    owl:allValuesFrom
                    ))

(define (rsort sexp) (sort sexp ls<?))  ; FIXME sorting this way can break the xml semantics because original tree ordering matters


(define (ls<? a b)
  (cond
    ((and (symbol? a) (symbol? b)) (symbol<? a b))
    ((and (symbol? a) (list? b)) #t)
    ((and (symbol? a) (string? b)) #t)
    ((and (string? a) (symbol? b)) #f)
    ((and (string? a) (string? b)) (string<? a b))
    ((and (string? a) (list? b)) #t)
    ((and (list? a) (string? b)) #f)
    ((and (list? a) (symbol? b)) #f)
    ((and (list? a) (list? b)) (ls<? (car (rsort a))
                                     (car (rsort b))))
    (#t (error (format "Neither symbols nor lists ~s ~s" a b)))))

(define (normalize-mal xexpr)
  ; Xexpr -> Xexpr
  (define (normalize-one xpr)
    (cond
      ;[(string? xpr) (remove-html (string-normalize-spaces xpr))]
      [(cons? xpr) (normalize-mal xpr)]
      [else xpr]))
  ; - IN -
  (match xexpr
    ['() '()]
    [(list (? string? str) ...)
     (list (normalize-one (string-append* str)))]
    [(cons head tail)
     (cons (normalize-one head) (normalize-mal tail))]
[_ xexpr]))

(define (load-xml file-name #:tags-no-ws [tags-no-ws rdf-no-ws])
  (define in (open-input-file file-name))
  (define thing
    (parameterize ([xexpr-drop-empty-attributes #t])
      (xml->xexpr
       ((eliminate-whitespace tags-no-ws (λ (x) x))
        (document-element
         (read-xml in))))))
  (close-input-port in)
  (displayln "done parsing")
  thing)

(define (save-xml xexpr file-name)
  (with-output-to-file file-name #:exists 'replace (lambda () (xexpr->xml xexpr))))

(define (save-xexpr xexpr file-name #:key [key (λ (x) x)])
  (define normalized (normalize-mal xexpr))
  (define sorted (key normalized))
  (define out (open-output-file file-name #:exists 'replace))
  (pretty-write sorted out)  ; (write-xexpr thing out)
  (close-output-port out))

(define (xml->sxml input-port namespaces)
  ; wrap ssax:xml->sxml to allow for tag specific default namespaces
  (ssax:xml->sxml namespaces))

(define (load-sxml file-name [namespaces '()])
  (with-input-from-file file-name (lambda () (ssax:xml->sxml (current-input-port) namespaces))))
  ;(ssax:xml->sxml (open-input-file file-name namespaces)))

(define (save-sxml sxexpr file-name)
  ; TODO detect the #f as the default ns and reinject into the relevant elements...
  (with-output-to-file file-name #:exists 'replace (lambda () (srl:sxml->xml sxexpr (current-output-port)))))

(define (save-sexp sexp file-name)
  (with-output-to-file file-name #:exists 'replace (lambda () (pretty-write sexp))))

(define data-cite-no-ws '(resource
                          ;alternateIdentifier
                          alternateIdentifiers
                          contributors
                          contributor
                          creators
                          creator
                          dates
                          descriptions
                          ;description
                          formats
                          fundingReferences
                          fundingReference
                          ;funderIdentifier
                          relatedIdentifiers
                          ;relatedIdentifier
                          geoLocations
                          geoLocation
                          geoLocationBox
                          geoLocationPoint
                          geoLocationPolygon
                          polygonPoint
                          rightsList
                          subjects
                          titles
                          sizes))
                          
(define (funcs)
  (displayln "starting")
  (save-xexpr (load-xml "~/git/NIF-Ontology/ttl/external/so.owl") "/tmp/xexpr-so.rkt" #:key rsort)
  (displayln "so.owl done...")
  ;(save-xexpr (load-xml "~/git/NIF-Ontology/ttl/external/go.owl") "/tmp/xexpr-go.rkt" #:key rsort)
  ;(displayln "go.owl done now...")

  (define inthing (load-sxml "~/ni/dev/scibot/datacite-example-full-v4.1.xml" '((#f . "http://datacite.org/schema/kernel-4")
                                                                                    (xsi . "http://www.w3.org/2001/XMLSchema-instance"))))
  (define mthing (list->mlist inthing))
  (set-mcar! (mcdr mthing) '(|@| (*NAMESPACES*
                                  (xsi "http://www.w3.org/2001/XMLSchema-instance"))))
  (define nthing (mlist->list mthing))
  (define thing ((sxml:modify (list "//resource/@*"
                                    'insert-preceding
                                    '(xmlns "http://datacite.org/schema/kernel-4")))
                 nthing))
  (save-sxml nthing "datacite2.xml")
  (save-sexp nthing "datacite2.rkt")
)

(module+ test
  ; use C-c C-t to start racket test mode in emacs...
  (funcs))

