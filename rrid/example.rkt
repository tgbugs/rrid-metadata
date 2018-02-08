#lang racket

(require racket/trace)
(require xml sxml)
(require "main.rkt")

(provide rrid->response rrid->record)  ; FIXME this is a bit and brittle, need to decouple make-record from submit-record...

;; notes
; for resourceTypeGeneral
; we use all of the datacite fields
; the only two that apply at the moment are Service and Software
; in their usage our databases are Service
; we only need to add Material (which covers both moving and non-moving physical objects)
; all of our data/information resources fit under Software/Services

;; pass our source info into the db so that it can configure the resolving behavior

(init-db identifier-sources)
 
;; setup make-record

(set-add-rec! add-rec)
(make-gtr! identifier-sources)

;; test records (these will ultimately come from the elastic search bit)

(make-record 'antibody
             'AB_355445 'AB_10083848 ; ab_id_old column name for the old id denormalized
             #:record
             (ab-rec
              #:title "Mouse/Rat Neuropilin-1 Affinity Purified Polyclonal Ab antibody"
              #:insert_time 1385490191 ; FIXME epoch set as the minimum i could find in the table, but possibly innacurate
              #:curate_time 1441819651
              #:vendor "R and D Systems"
              #:catalog-number "AF566"))

(make-record 'digital
             'SCR_003070 'nif-0000-30467 'rid_000070
             #:record
             (scr-rec
              #:title "ImageJ"
              #:submitted 1285027200
              #:updated 1493934720
              #:resourceType "Image Processing Software"
              #:resourceTypeGeneral "Software"
              #:description "A Java image processing program which can display, edit, analyze, process, save and print 8-bit, 16-bit and 32-bit images. It can read many image formats including TIFF, GIF, JPEG, BMP, DICOM, FITS and raw. It runs, either as an online applet or as a downloadable application, on any computer with a Java 1.4 or later virtual machine. Downloadable distributions are available for Windows, Mac OS, Mac OS X and Linux. It supports stacks, a series of images that share a single window. It is multithreaded, so time-consuming operations such as image file reading can be performed in parallel with other operations. It can calculate area and pixel value statistics of user-defined selections. It can measure distances and angles. It can create density histograms and line profile plots. It supports standard image processing functions such as contrast manipulation, sharpening, smoothing, edge detection and median filtering. It does geometric transformations such as scaling, rotation and flips. Image can be zoomed up to 32:1 and down to 1:32. All analysis and processing functions are available at any magnification factor. The program supports any number of windows (images) simultaneously, limited only by available memory. Spatial calibration is available to provide real world dimensional measurements in units such as millimeters. Density or gray scale calibration is also available. ImageJ was designed with an open architecture that provides extensibility via Java plugins. Custom acquisition, analysis and processing plugins can be developed using ImageJ built in editor and Java compiler. User-written plugins make it possible to solve almost any image processing or analysis problem."
              #:url "https://imagej.nih.gov/ij/"
              #:resource-types '("Resource"
                                 "software resource"
                                 "software application"
                                 "image processing software"
                                 "data processing software")
              #:keywords '("imaging"
                           "image processing software"
                           "plug-in"
                           "macros"
                           "microscopy"
                           "java"
                           "magnetic resonance")
              #:synonyms '("ImageJ - Image Processing and Analysis in Java"
                           "Image J")))

(make-record 'fake
             'PREFIX_1234567
             #:record
             (fake-rec
              #:title "Fake resource"
              #:insert_time 1111111111 ; FIXME epoch set as the minimum i could find in the table, but possibly innacurate
              #:curate_time 1222222222
              #:something "We need this for the proper citation"))

; dump the records from the database and write reprs to file

(dict-for-each (car (dump-recs))
               (λ (i rec) 
                 (when (> i -1)
                   (begin (save-sexp rec (format "../examples/test-rec-~a.sxml" i))
                          (save-sxml rec (format "../examples/test-rec-~a.xml" i))
                          (load-sxml (format "../examples/test-rec-~a.xml" i) `((#f . ,schema-url)))  ; roundtrip test
                          void))))

;; not in core

(define (other)

  ; mapping from rrids ->
  '(RRDI:PREFIX_355445 . test-resource)
  '(RRDI:some-other-identifier . test-resource)

  '(rrid:full-record
    (rrid:identifier "RRID:PREFIX_1234567")
    (relatedIdentifiers ; papers would go here if we wanted to give them away
     ; works 
     (relatedIdentifier (@ definingCitation ) "PMID:1234567"))
    (field-1 )
    (field-2 )
    (field-3 )
    (field-4 ))

  '(research-resource-id (@ (resource-id-type "rrid")
                            (vocab="Research Resource Identifier"))
                         "RRID:AB_1234567ABCDEFG")
  (write-xexpr  ; xexpr->xml
   '(resource "http://scicrunch.org/resources/schema/metadata.something"
              (rrid:core
               (dc:identifier "RRID:PREFIX_1234567")
               (rrid:type-schema
                )
               (rrid-types:some-resource
                (resource:field-1 "value 1")
                (resource:field-2 "value 2")
                (resource:field-3 "value 3")
                (resource:field-4 "value 4")
                ))))



  (define (format-generator)
    ; for some other time...
    ; what we really want for this is just a way to map our existing fields to the
    ; defined fields of the record and generate them... which seems an awful lot like
    ; what I'm working on in the other section
    (define (node [name '|@|]
                  [occurance '1] ; '0-1 '1-n '0-n
                  #:obligation [oblication 'optional]  ; 'manditory 'reccomended
                  . subnodes)
      'huehue
      )
    'doublehue
    )

  (define (append-if lst l)
    (if (or (and (list? l)
                 (empty? l))
            (not l))
        lst
        (append lst l)))

  void)
