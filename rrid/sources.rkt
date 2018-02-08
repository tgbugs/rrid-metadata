#lang racket
(require racket/date (for-syntax syntax/parse))

(provide identifier-sources
         ab-rec
         fake-rec
         scr-rec)

;; macros

(define-syntax (bndc stx)
  ; bind cons
  (syntax-parse stx
    [(_bndc variable-stx)
     #'(quasiquote (variable-stx . ,variable-stx))]))

(define-syntax (*bdc stx)
  "apply a function to a variable and bind it to its variable name (*bdc variable function)"
  (syntax-parse stx
    [(_bndc variable-name variable-function-name)
     #'(quasiquote (variable-name . ,(variable-function-name variable-name)))]))

;; utils

(define (epoch->iso-8601 epoch)
  (date-display-format 'iso-8601)
  (string-append (date->string (seconds->date epoch #f) #t) "+00:00"))

;; source invariant metadata

(define identifier-sources '#hash(
   (antibody . #hash((url . "http://antibodyregistry.org")
                     (namespace . "http://antibodyregistry.org/")
                     (source . "Antibody Registry")
                     ;(fields . (catalogNumber))
                     (format . ("(~a Cat# ~a, ~a)" vendor catalogNumber identifier))
                     ;(subjects . (""))  ; TODO there are some potential additional things we could put here
                     (resourceType . "Antibody")
                     (resourceTypeGeneral . "Material")
                     ))
   (fake . #hash((url . "http://fake.example.org")
                 (namespace . "http://fake.example.org/fake/")
                 (source . "Fake Source")
                 (format . ("(~a, ~a)" something identifier))
                 (resourceType . "Cookies")
                 (resourceTypeGeneral . "Material")
                 (resolve->source . #t)))  ; we set the resolve rules at source level
   (digital . #hash((url . "https://scicrunch.org/browse/resourcedashboard")  ; can i just state that this is confusing as
                    (namespace . "http://uri.scicrunch.org/registry/")
                    (source .  "SciCrunch Registry")
                    ;(fields . (resourceTypes))  ; TODO resource types via identifiers (from resources.ttl) RRIDTypeGeneral  ; FIXME this errors if not present
                    (format . ("(~a, ~a)" title identifier))))))

;; record invariant structure per source

(define (fake-rec #:title title
                  #:insert_time submitted
                  #:curate_time updated
                  #:something something)
  (make-hash (list (bndc title)
                   (*bdc submitted epoch->iso-8601)
                   (*bdc updated epoch->iso-8601)
                   (bndc something))))

(define (ab-rec #:title title
                #:insert_time submitted
                #:curate_time updated
                #:vendor vendor  ; TODO yes there are multiple
                #:catalog-number catalogNumber)
  (make-hash (list (bndc title)
                   (*bdc submitted epoch->iso-8601)
                   (*bdc updated epoch->iso-8601)
                   (bndc vendor)
                   `(contributors . (("Distributor" ,vendor)))
                   `(altids  . ((,(format "~a catalog number" vendor) ,catalogNumber)))
                   (bndc catalogNumber)
                   ;(alternateCatalogNumbers . ,atlcats)  ; [12345L also 12345S, 12345Y] row structure for now  TODO
               )))  ; FIXME  there are multiple catalog numbers

(define (scr-rec #:title title
                 #:submitted submitted  ; TODO property sanity checkes eg submitted <= updated
                 #:updated updated
                 #:resourceType resourceType  ; Database ... etc.
                 #:resourceTypeGeneral resourceTypeGeneral  ; Service or Software  (Documentation vs Protocol)
                 #:keywords subjects  ; this goes in the datacite subjects
                 #:url url
                 #:resource-types resourceTypes  ; FIXME these need to be used to translate to resource type
                 #:description description
                 #:synonyms synonyms)
  (make-hash (list (bndc title)
                   (bndc description)
                   (bndc resourceType)
                   (bndc resourceTypeGeneral)
                   (*bdc submitted epoch->iso-8601)
                   (*bdc updated epoch->iso-8601)
                   (bndc subjects)
                   `(relids . ((,url "URL" "Describes" "Service")))  ; TODO map to RTG?
                   (bndc resourceTypes)  ; TODO RRIDTypeGeneral manditory, ResourceTypeGeneral optional
                   (bndc synonyms))))

