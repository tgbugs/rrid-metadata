#lang racket
(require syntax/parse/debug)
(define (not-null? thing) (not (null? thing)))
#;
(debug-syntax-parse!)
(require net/url-string sxml ; atm only for the jats bit
         syntax/parse
         racket/pretty
         "syntax-classes.rkt"
         (only-in "utils.rkt" define-string-predicate)
         (for-syntax syntax/parse
                     racket/syntax
                     racket/string
                     racket/pretty
                     (only-in racket/list range remove-duplicates)
                     "syntax-classes.rkt")
         ;(only-in racket/list range)
         ;(for-template (only-in racket/list range))
         ;(for-syntax (only-in racket/list range))
 )

(module+ test
  (require rackunit
           syntax/macro-testing
           (only-in "utils.rkt" check-syntax-exn)))

(provide make-gtr! set-add-rec! make-record schema-url sxml-schema define-syntax-class)

;; macros
(define-syntax (bind stx)  ; FIXME could be called (node stx) or something? make-sxml-node
  (syntax-parse stx
    [(_bind variable-stx)
     #'(quasiquote (variable-stx ,variable-stx))]))

#;(define (->? stx case-statements)
  (syntax-parse stx
    [(_ compile-time-subtree-pattern)
    ;subtree->predicate
    #'(λ (run-time-subtree-path)
        ;(let ([-string (compile-time-subtree-pattern.match run-time-subtree-path)])
        (let ([-string (match run-time-subtree-path)])
          (case (match run-time-subtree-path compile-time-subtree-pattern)
            case-statements)))]))
            ;string-let.case-statements)))]))

(define-syntax stx-sxml (λ () "Can't use stx-sxml outside an sxml-schema declaration"))  ; TODO proper error
(define-syntax (sxml-schema stx)  ; more spec-tree-structure
  (syntax-parse stx
    #:literals (stx-sxml)
    [(_ (~optional (~seq #:name syntax-name:id))
        schema:sc-body)
     #:with (lits-stx ...) (datum->syntax this-syntax (remove-duplicates (syntax->datum #'(schema.literals ...))))
     #:with top-level (if (eq? (syntax->datum #'schema.racket) (syntax->datum #'schema.name))
                          #'(schema.racket)
                          #'schema.racket
                          )
     #:with checker #'(let ([stx-sxml (datum->syntax #f sxml)])  ; FIXME how to get the loc from raw sxml
                                   (syntax-parse stx-sxml
                                     #:disable-colon-notation
                                     #:datum-literals (lits-stx ...)
                                     #:local-conventions (schema.local-conventions ...)
                                     [top-level  ; just validate, there may be better ways
                                      stx-sxml]))

     #:with defines #'(begin schema.syntax-classes ...)
     (when (attribute schema.range)
       (raise-syntax-error 'spec-error
                           (format "The top node ~a cannot specify a name pattern, only 1 e.g. ([top 1])"
                                   #'schema.name)
                           #'schema
                           #'schema.head  ; schema.range is never syntax
                           ))
     ; FIXME sourceloc...
     (let ([out 
            (syntax/loc this-syntax
              (~? (begin
                    schema.syntax-classes ...
                    (define (syntax-name sxml)
                      checker))
                  (λ (sxml)
                    schema.syntax-classes ...
                    checker)
                  ))])

       #;
       (pretty-write `(ct-out: ,(syntax->datum out)))
       out
       )
     ]))

; [tag 1] -> (tag body:expr)
; [tag n] -> (_ (tag body:expr) (tag body:expr) ...)
; [tag (range 0 1)] -> (_ (~optional (tag body:expr)))
; [tag (range 0 n)] -> (_ (~optional (tag body:expr)) ...)
; (head body ... terminal)
;([@ (range 0 1)]
;([*NAMESPACES* (range 0 1)]
;([(pattern *) (range 0 n)] uri?)))

(module+ test
  ; test optional values
  (sxml-schema #:name test-optional
   ([top 1]
    ([optional-subtree (range 0 1)])))
  (test-optional '(top))
  (test-optional '(top (optional-subtree)))
  )

(module+ test
  (check-equal? (syntax->datum ((sxml-schema ([tag 1])) '(tag))) '(tag))
  (check-exn exn:fail:syntax? (thunk (convert-syntax-error ((sxml-schema ([tag 1])) '(not-tag)))))  ; it is normal to see debug on this

  (sxml-schema #:name simple ([(pattern a:*) 1]))
  (check-equal? (syntax->datum (simple '(a:thing))) '(a:thing))
  (define thing-lambda
    (sxml-schema
     ([top 1]
      ([simple (range 0 1)] string?)
      symbol?)))
  (sxml-schema #:name thing
   ([top 1]
    ([simple (range 0 1)] string?)
    symbol?))
  (check-equal? (syntax->datum (thing '(top value))) '(top value))
  (check-equal? (syntax->datum (thing '(top (simple "yes") maybe))) '(top (simple "yes") maybe))
  (check-equal? (syntax->datum (thing #'(top (simple "yes") maybe))) '(top (simple "yes") maybe))

  (sxml-schema #:name simple2
   ([(pattern a:*) 1]
    #;[(pattern a:*) (range 0 n)]  ; this should be failing? and now is
    ([simple (range 0 1)] symbol?)
    symbol?))
  (define sxml1 '(a:hello (simple symb1) symb2))  ; NOTE if you are working with symbols and pass in a quoted list, don't double quote
  (check-equal? (syntax->datum (simple2 sxml1)) sxml1)
  (check-equal? (syntax->datum (simple2 (list 'a:hello (list 'simple 'symb1) 'symb2))) sxml1)
  ;(simple2 '(a:there (nothing) there))  ; fails as expcted since the head [simple (range 0 1)] specifies for the whole tree
  )

(module+ test
  (sxml-schema
   #:name test2-sub-1
   ([(pattern a:*) 1]
    ([(pattern b:*) 1]
     ([(pattern c:*) 1] "u")
     "wat")))
  (test2-sub-1 '(a:* (b:* (c:* "u") "wat")))
  )

(module+ test
  ; example where having multiple bodies limit 1 breaks ~alt
  (sxml-schema
   #:name test2
   ([(pattern a:*) 1]
    ([(pattern b:*) 1]
     ([(pattern c:*) 1] "u")
     "wat")
    ([(pattern d:*) 1] "m8")))

  (test2 '(a:* (b:* (c:* "u") "wat") (d:* "m8")))
  #; ; should fail?
  (test2 '(a:thing (b:maybe "nope")))
  #;
  (test2 '(a:hello (b:there "wat")))
  ;(test2 '(a:* (b:* "wat") (b:* "wat")))  ; now failing correclty
  (let ([schema (sxml-schema ([tag 1] ([tag2 (range 0 1)] string?) integer?))])
    (schema '(tag (tag2 "thing") 109219))
    (schema '(tag 109219)))  ; FIXME have to insert the optional here...
    )

(module+ test
  (sxml-schema #:name sigh2 ([TOP 1] ([a (range 1 n)]) ([b (range 0 n)])))
  (sigh2 '(TOP (a)))
  (sigh2 '(TOP (a) (b)))
  (sigh2 '(TOP (b) (a)))
  (check-exn exn:fail:syntax? (thunk (convert-syntax-error 
                                      (sigh2 '(TOP (b))))))
    )

(module+ test
  (sxml-schema #:name test-multi-optional
               ([TOP 1] ([a (range 0 n)])
                        ([b (range 0 n)])))

  (test-multi-optional '(TOP))
  (test-multi-optional '(TOP (a)))
  (test-multi-optional '(TOP (b)))
  (test-multi-optional '(TOP (a) (b)))
  (test-multi-optional '(TOP (b) (a)))
  (test-multi-optional '(TOP (a) (b) (b) (a))))

(module+ test
  (sxml-schema #:name test-multi-exact ([TOP 1] ([a 1]) ([b 1])))
  (test-multi-exact '(TOP (a) (b)))
  (test-multi-exact '(TOP (b) (a)))
  (check-syntax-exn (test-multi-exact '(TOP (a))))
  (check-syntax-exn (test-multi-exact '(TOP (a) (a))))
  (check-syntax-exn (test-multi-exact '(TOP (a) (b) (a))))
  (check-syntax-exn (test-multi-exact '(TOP (b))))
  (check-syntax-exn (test-multi-exact '(TOP (b) (b))))
  (check-syntax-exn (test-multi-exact '(TOP (b) (a) (b))))
  )

(module+ test
    (sxml-schema #:name multi-body-test ([TOP 1] ([(pattern *body1) (range 0 n)] "hello there")
                                                 ([(pattern *body2) (range 0 n)] "general nobody")
                                                 ;null)) ; FIXME this should fail? null isn't a predicate
                                                 #;null?))

    (multi-body-test `(TOP (a-body1 "hello there")
                           (b-body1 "hello there")
                           (body2 "general nobody")))  ; null doesn't work in quotes
    )

(module+ test
  ; test custom predicates
  ; remove the #:predicates section, it makes more sense to
  ; let people define their predicate functions however they like
  ; yes, it means custom predicates could come from anywhere, but
  ; that is probably a reasonable tradeoff w/ readability
  (define predicate? (λ (value) #t))
  (define my-string? string?)
  (define-string-predicate my-type-string? "myType")
  (sxml-schema
   ([tag 1] ([a 1] predicate?) ([b (range 0 n)] my-type-string?) my-type-string?))
  (sxml-schema ([tag 1] "value"))
  (sxml-schema ([tag 1] ([tag2 (range 0 1)] "value2") "value"))

  (sxml-schema
   ([tag 1]
    ([tag2 (range 0 1)] "value2")
    ([tag3 (range 0 n)] predicate?)
    "value"))

  ;(define (thing? value) (equal? value "thing"))
  (define-string-predicate thing? "thing")
  (sxml-schema ([t1 1] ([t2 1] thing?)))
 )

(module+ test 
  (sxml-schema #:name deep-nesting ([a 1] ([b 1] ([c 1] ([d 1] ([e 1] ([f 1] "g")
                                                                      ([x 1] string?)
                                                                      ([h 1] "i")))))))
  (check-exn exn:fail? (thunk (check-syntax-exn (sxml-schema ([tag 1])))))  ; ah the old doulb test negative... will still break silently
  (check-exn exn:fail:syntax? (thunk (convert-syntax-error (sxml-schema ()))))
  (check-syntax-exn (sxml-schema ([])))
  (check-syntax-exn (sxml-schema ([tag])))
  (check-syntax-exn (sxml-schema ([tag 0])))
  (check-syntax-exn (sxml-schema ([tag (range 0 1)])))
  ((sxml-schema ([tag 1])) '(tag))
  (check-syntax-exn ((sxml-schema ([tag 1])) '(not-tag)))  ; fails as expected
  ((sxml-schema ([tag 1] 0)) '(tag 0))
  (check-syntax-exn ((sxml-schema ([tag 1] 0)) '(tag)))  ; fails as expected
  ((sxml-schema ([tag 1] "")) '(tag ""))
  ((sxml-schema ([tag 1] ([tag2 1] 0) 0)) '(tag (tag2 0) 0))
  (sxml-schema ([tag 1] ([tag2 (range 0 1)] 0) 0))
  (check-syntax-exn (sxml-schema ([tag 1] 0 "")))  ; fails as expected
  (check-syntax-exn (sxml-schema ([tag 1] "" 0)))  ; fails as expected

  ;(sxml-schema ([tag 1] '0))  ; TODO do we want to allow this?
  #;((sxml-schema ([tag 1] list?))
     '(tag (wat if this works i will eat my hat)))  ; FIXME weird error, but sudden insight into how to compose these schemas! just turn a validator into a predicate and stick it in #:predicates and boom, you can compose to your hearts content
  (let ([schema (sxml-schema ([tag 1] ([tag2 (range 0 1)] string?) integer?))])
    (schema '(tag (tag2 "thing") 109219))  ; FIXME wat
    (schema '(tag 109219)))
  (sxml-schema ([tag 1] ([tag2 (range 0 1)] ([tag3 1] symbol?) "ok") string?))
  (sxml-schema ([tag 1] ([tag2 (range 0 1)] ([tag3 1] symbol?)) string?))
  (check-syntax-exn (sxml-schema ([(pattern1 prefix:*) 1])))  ; fails as expected
  (sxml-schema ([(pattern prefix:*) 1]))
  (let ([schema (sxml-schema ([*TOP* 1]
                              ([@ (range 0 1)]
                               ([*NAMESPACES* (range 0 1)] 10))))])
    (check-syntax-exn (schema '(*TOP* (@ (*NAMESPACES* 9)))))
    (schema '(*TOP* (@ (*NAMESPACES* 10)))))

  (sxml-schema #:name test-pred ([top 1] ([head (range 1 n)] string?)))
  (test-pred '(top (head "anything") (head "anything2")))
  (check-syntax-exn (test-pred '(top (head "anything") (head 'not-a-string))))  ; fails as expected
  (sxml-schema #:name test1 ([top 1] ([yeee (range 0 n)] "wat")))
  (check-syntax-exn (test1 '(top (yeee) (yeee))))  ; fails as expected
  (test1 '(top (yeee "wat") (yeee "wat")))
  (sxml-schema #:name test2.1 ([(pattern a:*) 1] ([(pattern b:*) 1] "wat")))
  (check-syntax-exn (test2.1 '(a:* (b:*)))) ; fails as expected
  (check-syntax-exn (test2.1 '(a:* (b:* "not wat"))))  ; fails as expected
  (check-syntax-exn (test2.1 '(should (fail "wat"))))  ; fails as expected
  (check-syntax-exn (test2.1 '(should (fail "and does"))))
  (test2.1 '(a:* (b:* "wat")))
  (check-syntax-exn (test2.1 '(a:* (b:* "wat") (b:* "wat"))))  ; fails as expected
  (sxml-schema #:name test3 ([(pattern a:*) 1] ([(pattern b:*) (range 0 n)] "wat")))  ; test for duplicate pattern defs
  (test3 '(a:* (b:* "wat") (b:* "wat")))
  (test3 '(a:hello (b:there "wat")))
  (check-syntax-exn (test3 '(b:hello (a:there "wat")))) ; failes as expected
  (sxml-schema #:name thing2 ([TOP 1] ([asdf (range 0 n)] "hello there")))

  ((sxml-schema ([tag 1] ([tag2 1 #:restrictions ([(tag3 _) 1])]  ; _ says that we don't care what tag3 says about itself we require 1
                          ; FIXME currently we can't define a tag once and use it by name in many different places...
                          ; basically defining reusable subtrees as with how we work with predicates
                          ([tag3 (range 0 n)] string?))))
   ;'(tag (tag2 (tag3 "stuff")))  ; ok
   ;'(tag (tag2 (tag3 "stuff") (tag3 "stuff")))  ; FIXME should fail? since we can spec range restrictions and this is exact?
   ;'(tag (tag2 "fails"))  ; fails as expected
   ; FIXME should fail
   '(tag (tag2)))
  )

;; utility

(define (filter-empty lst)
  (filter (λ (l) (not (empty? l))) lst))

(define (make-global namespace fragment)
  (string-append namespace fragment))


;; record structure

; current schema url version
(define schema-url "http://scicrunch.org/resources/schema/rrid-core-0")

;(define record-structure
; may have more than one optional...
; wildcard element -> * ?
; _ binds to the child-type of the parent
; eq? in child predicate will do an exact test against the listed next
; cars that are lists is where to start... square brackets help clarify elements
; will probably make parsing easier too
; i think we can make type? optional when it is list? and null?
; ie [symbol? range? type?] => type? -> list?
; and [_ range? type?] => type? -> null? since [_ 1] clearly indicates null? in all cases
; do we need null? at the end of [("member" "check" "list") 1]???
; should be able to lift this into a constraint checker...
; TODO is it possible to generate our ingest/conversion code from this? not entirely clear...
;  maybe could generate using an alternate parser from additional annotations to more subtrees?
; the right way to imlement this feels like using a macro to convert [] into consumer functions...
; might be possible to use a reader macro ala #str([*TOP* 1] ...) as a way to define these...

; see https://lexi-lambda.github.io/blog/2017/08/12/user-programmable-infix-operators-in-racket/

(require #;(for-syntax syntax/parse/class/paren-shape)  ; leave this out for now
         (prefix-in racket/base/ racket/base)
         syntax/parse/define)

(define-syntax (wrapped stx)
  ; this is not working yet... (current does nothing)
  (syntax-parse stx
    [(_nil quote-stx) #'(quote-stx)]))

#;(define-syntax-parser define-schema-structure
    [[~brackets _ arg ...]
     #'(#%node-spec arg ...)]
    ;[(~parens _ arg ...)
    ;#'(list arg ...)]
    )

;(define-schema-structure [*TOP* 1])
;(define-schema-structure '([*TOP* 1] "yes"))

(define (#%node-spec node-name range-expression [expect-type list?] #:warn-missing expect-patterns)
  (define (checker sexp-tree)
    #t)  ; TODO
  checker)

;(define-syntax-parser #%node-spec
;[(_ a b c)])

;;; primary schema definition
(require (only-in srfi/19 string->date))

#|
structure validation syntax
element : ssexp symbol
symbol : SYMBOL
ssexp : '( symbol-restrict ssexp ') | racket-sexp
symbol-restrict : '[ u-sym-m int-or-range child-is-pred* (#:warn-missing expected-sub-tree-patterns)* ']
u-sym-m : _ | symbol | pattern-expression
int-or-range : INTEGER | range-expression
range-expression : '( 'range 0-1 int>prev-or-n ')
0-1 : 0 | 1
int>prev-or-n : INTEGER | n  ; must test INTEGER > 0-1
child-is-pred : racket-predicate  ; must test explicitly whether absense is valid in context
|#
; TODO pull these out
[define resource-type-generals '("Material" "Software" "Service")]
[define relation-types '("IsCompiledBy"
                         "IsIdenticalTo"
                         "IsDerivedFrom"
                         "IsDescribedBy"
                         "Describes"
                         "Other")]
[define contributor-types '("Distributor"  ; antibody vendors
                            "Producer"  ; individual personal antibody maker
                            "Other"  ; antibody vendor "synonyms" aka acquired companies
                            "HostingInstitution"  ; parrent organization (see if this fits)
                            "RegistrationAgency"
                            "RegistrationAuthority")]
[define xml-langs '("en-US")]
[define hit-the-database (λ (value) "totally going to the database I swear" #t)]
[define check-remote #f]

(define (related-identifier-type? value) (member value '("URL" "DOI")))
(define (resource-type-general? value) (member value resource-type-generals))
(define (iso8601-tz-string? value) (string->date value "~Y-~M-~DT~TZ"))  ; TODO make more predicate like...
(define (contributor-type? value) (member value contributor-types))
(define (relation-type? value) (member value relation-types))
(define (date-type? value) (member value '("Submitted" "Updated")))
(define (xml-lang? value) (member value xml-langs))
(define (rrid? value) (if check-remote (hit-the-database value) (string-prefix? value "RRID:")))
(define (uri? value) (regexp-match url-regexp value))

(define-string-predicate doi? "DOI")  ; not entirely sure why I had these implemented as I did in the first place ...
(define-string-predicate RRID? "RRID")
(define-string-predicate URI? "URI")

(sxml-schema
 #:name rrid-schema
 ([*TOP* 1]
  ([@ (range 0 1)]
   ([*NAMESPACES* (range 0 1)]
    ([(pattern *) (range 0 n)] uri?)))
  ([resource 1]
   ([@ (range 0 1)]
    ([xmlns 1] rrid?)  ; FIXME eq? schem-url?
    ([(pattern xmlns:*) (range 0 n)] uri?))
   ([identifier 1]
    ([@ 1]  ; this is the most consistent way to do it
     ([identifierType 1] "RRID"))
    rrid?)
   ([properCitation 1]  ; TODO pc validator
    ([@ 1] ([render-as 1] "Proper Citation")  ; this is the most consistent way to do it
           ([type 1] "Inline Text Citation"))
    string?)
   ([titles 1]
    ([title (range 1 n)]
     ([@ (range 0 1)] ([xml:lang (range 0 1)] xml-lang?))  ; all terminals have only 1 instance in sxml
     string?))
   ([publisher 1] string?)
   ([rightsList (range 0 1)]
    ([rights 1])
    )
   ([description (range 0 1)]
    ([@ (range 0 1)] ([xml:lang (range 0 1)] xml-lang?)) 
    string?)
   ([subjects (range 0 1)]
    ([subject (range 0 n)] ([@ (range 0 1)] ([xml:lang (range 0 1)] xml-lang?)) 
                           string?))
   ([contributors (range 0 1)]  ; list? vs implicit 'only what we list below is allowed'
    ([contributor (range 0 n)]
     ([@ 1] ([contributorType 1] contributor-type?))  ; member?
     ([contributorName 1] string?)))
   ([dates 1 #:restrictions ([(date (@ (dateType _ ))) 1])]  ; subtree restrictions for at most one
    ;([dates 1]
    ([date (range 1 n)
           #:restrictions ([(@ (dateType "Updated")) (range 0 1) #:warn 0]
                           [(@ (dateType "Submitted")) (range 0 1) #:warn 0])]
     ([@ 1] ([dateType 1] date-type?))
     iso8601-tz-string?))
   ([resourceType 1]
    ([@ 1]
     ;([resourceTypeGeneral 1] (member? ,resource-type-generals))  ; TODO
     ([resourceTypeGeneral 1] resource-type-general?))  ; TODO
    ;([resourceTypeGeneral 1 member] [("Material"
    ;"Software"
    ;"Services") 1 null?]))  ; TODO
    string?)
   ([alternateIdentifiers 1]
    ([alternateIdentifier (range 1 n)]
     ([@ 1] ([alternateIdentifierType 1] string?))
     (->? (@ (alternateIdentifierType _)))))
   ;(value->predicate (@ (alternateIdentifierType _)))))
   ([relatedIdentifiers
     1
     #:restrictions ([(@ (relationType "IsCompiledBy")) (range 0 n) #:warn 0]
                     [(@ (relationType "IsIdenticalTo")) (range 0 n) #:warn 0]
                     [(@ (relationType "IsDerivedFrom")) (range 0 n) #:warn 0])]
    ([relatedIdentifier (range 1 n)]
     ([@ 1]
      ([relatedIdentifierType 1] related-identifier-type?)  ; FIXME inconsistent
      ;([relationType 1] (member? ,relation-types))  ; TODO
      ([relationType 1] relation-type?)  ; TODO
      ([resourceTypeGeneral 1] resource-type-general?))  ; TODO
     (->? (@ (relatedIdentifierType _)))))))
 ;(value->predicate (@ (relatedIdentifierType _)))))))
 )

(define (record-format
         #:id primary-id
         #:altids altids
         #:relids relids
         #:pc proper-citation
         #:jc jats-citation
         #:submitted submitted
         #:updated updated
         #:titles titles
         #:creators creators
         #:contributors contributors
         #:resourceType resourceType
         #:description description
         #:subjects subjects
         #:publisher publisher)
  ;#:type-specific-record type-specific-record)
  ; marginally conformant to datacite metadata
  ; note that we are closer to a physical sample use case than a publishing use case
  `(*TOP*
    ;(*PI* xml-stylesheet "href=\"#style\" type=\"text/css\"");(@ (href "#style") (type "text/css")))
    (@ (*NAMESPACES* (rridType "http://scicrunch.org/resources/schema/rrid-type-0")))
    ,(filter-empty
      `(resource
        ;(extras (@ (id "style"))  ; this doesn't really work as desired
        ;"resource { display: block; }
        ;title, publisher { display: block; }
        ;extras { display: none; }
        ;")
        (@ (xmlns ,schema-url))
        (identifier (@ (identifierType "RRID")) ,(string-append "RRID:" primary-id))
        (properCitation (@ (render-as "Proper Citation")  ; XXX RRID addition
                           (type "Inline Text Citation"))
                        ,proper-citation)
        ;,creators  ; nameType Organizational for corporate...
        ,titles
        ,(bind publisher)
        ;(publicationYear , year)  ; not clear that this can be manditory nor that it should be
        ,description  ; descriptionType
        ,subjects
        ,contributors  ; used with Distributor for vendors, a bit of overloading
        (dates
         (date (@ (dateType "Submitted")) ,submitted)
         (date (@ (dateType "Updated")) ,updated))  ; TODO created as well
        ;(language "en-US") ; XXX NO
        ,resourceType ;,ResourceType ResourceTypeGeneral  one way to do would be to declare non-equivalence with datacite on these elements let xmlns deal w/ rest probably do need a really clear distinction between "this resource is a real thing" beyond the source iri type...
        ,altids
        ,relids
        ;size XXX NO
        ;formats XXX not applicate
        ;,version  ; not clear that we can really have this for these kinds of resources...
        ;,rights  ; possibly for some software resources... Commercial EULA vs License issue good thing they deal with it
        ;,geoLocations  ; probably optional for this one since our resources are various an muliplicious (woo mice)
        ;,fundingReferences  ; eventually

        ;(JATSCitation (@ (render-as "JATS Citation")  ; TODO not happy with the rendering for this... with the &lt; &gt;
        ;(type "Typeset SGML Citation"))
        ;,jats-citation)
        ))))

(define (attrs tags values)
  (cons '|@| (for/list ([tag tags]
                        [value values]
                        #:when value)
               `(,tag ,value))))

(define (title-format title [titleType #f])
  ;(define tags (list '|@| (xml:lang "en-US") ()))

  `(title
    ,(attrs '(xml:lang titleType)
            `("en-US" ,titleType))
    ,title))
;(let ([out (cons 'titles (for/list ([t titles]) `(title (@ (xml:lang "en-US")) ,t)))])
;(pretty-write titles)
;(pretty-write out)
;out))

(define (altids-format alternateIdentifierType id) `(alternateIdentifier
                                                     (@ ,(bind alternateIdentifierType))
                                                     ,id))

(define (relids-format id type [relation #f] [rtg #f])
  `(relatedIdentifier ,(attrs '(relatedIdentifierType relationType resourceTypeGeneral)
                              `(,type ,relation ,rtg))
                      ,id))

(define (jats-format prefix rrid)
  ; http://groups.niso.org/apps/group_public/view_comment.php?comment_id=766
  (srl:sxml->xml
   `(research-support-source
     (@ (support-type "research-materials"))
     (research-resource-wrap
      (research-resource ,prefix
                         (research-resource-id
                          (@ (resource-id-type "rrid") (vocab "Research Resource Identifier"))
                          ,rrid))))
   )
  )

(define (subjects-format subjects)
  (let ([out (cons 'subjects (for/list ([s subjects]) `(subject (@ (xml:lang "en-US")) ,s)))])
    ;(pretty-write subjects)
    ;(pretty-write out)
    out))

(define (contrib-format contributorType contributorName)  ; TODO binding macro
  `(contributor (@ ,(bind contributorType)) ,(bind contributorName)))

(define gtr 'you-have-not-initialized-the-database-yet)
(define (set-gtr! function)
  (set! gtr function))
(define add-rec (λ (a b) a))
(define (set-add-rec! function)
  (set! add-rec function))

;; main record creation code

(define (make-record identifier-type -primary-id #:record type-record . alternate-ids)

  ; when treated as a qname `@prefix RRID: <http://scicrunch.org/resolver/> .`
  ; FIXME the semantics of the resolver are extremely confusing because we conflate SCR: and RRID: :/ what to do?
  ;     RRID: -> http://scicrunch.org/resolver/RRID:  but could end up with people inserting RRID:RRID: >_<
  ;     RRID: -> http://scicrunch.org/resolver/
  ;      SCR: -> http://uri.scicrunch.org/default/SCR_1234567

  (define (type-dict-or-record key)
    (let ([dict-result (gtr identifier-type key #f)])
      (if dict-result
          dict-result
          (dict-ref type-record key))))

  (define primary-id (if (symbol? -primary-id)
                         (symbol->string -primary-id)
                         -primary-id))

  (define qname (string-append "RRID:" primary-id))

  ;(set! type-record (cons (cons 'identifier qname) type-record))
  (dict-set! type-record 'identifier qname)
  ;(pretty-write type-record)

  (define to-resolve (map (λ (id) (format "RRID:~a" id))
                          (cons primary-id alternate-ids)))

  (define altids
    (let* ([res (for/list ([id (cdr to-resolve)]) (list "RRID" id))]
           [from-tr (dict-ref type-record 'altids '())]
           [all (append res from-tr)])
      ;[mapl (λ (lst)
      ;(map (λ (l) (apply altids-format l))
      ;(let ([type (car l)][id (cadr l)])
      ;(altids-format type id)))
      ;lst))] )
      (if (empty? all)
          all
          (cons 'alternateIdentifiers (map (λ (l) (apply altids-format l)) all)))))

  (define resourceTypeGeneral (type-dict-or-record 'resourceTypeGeneral))

  (define relids
    (let ([rids (dict-ref type-record 'relids #f)]
          [base `(
                  (,(string-append "http://scicrunch.org/resolver/" qname)
                   "URL" "IsIdenticalTo" ,resourceTypeGeneral)
                  (,(string-append "http://n2t.net/" qname)
                   "URL" "IsIdenticalTo" ,resourceTypeGeneral)
                  (,(string-append "http://identifiers.org/" qname)
                   "URL" "IsIdenticalTo" ,resourceTypeGeneral)
                  (,(make-global (gtr identifier-type 'namespace) primary-id)  ; FIXME symbol vs string
                   "URL" "IsDerivedFrom" ,resourceTypeGeneral)
                  (,(gtr identifier-type 'url)
                   "URL" "IsCompiledBy" "Service")
                  ; TODO supports IsCitedBy
                  ; TODO HasMetadata -> our secondary record
                  )])
      ;(displayln (append base rids))
      ;(displayln rids)
      (cons 'relatedIdentifiers (map (λ (l) (apply relids-format l))
                                     (if rids
                                         (append base rids)
                                         base)))))

  (define proper-citation
    (let ([to-format (gtr identifier-type 'format)])
      (apply format (cons (car to-format)
                          (map (λ (f)
                                 (dict-ref type-record f))
                               (cdr to-format))))))

  (define jats-citation (jats-format
                         (car (string-split (string-trim proper-citation "(") ","))
                         qname))

  (define issuing-source (gtr identifier-type 'source))

  ;(define issuing-url (format (dict-ref type-urls identifier-type) primary-id))

  (define titles (cons 'titles
                       (let ([title (dict-ref type-record 'title)])
                         (list* (title-format title)
                                (if
                                 (dict-has-key? type-record 'synonyms)
                                 (map (λ (v) (title-format v "AlternativeTitle"))
                                      (sort (dict-ref type-record 'synonyms) string<?))
                                 '()))
                         )))

  (define creators '())  ; TODO

  (define subjects
    (if (dict-has-key? type-record 'subjects)
        (subjects-format (dict-ref type-record 'subjects))
        '()))
  ;(pretty-write subjects)

  (define contributors
    (let ([contrib-list (let ([v (dict-ref type-record 'contributors #f)])
                          (cond (v v)
                                (#t '())))])
      (if (empty? contrib-list)
          '()
          (cons 'contributors (map (λ (l) (apply contrib-format l)) contrib-list)))))

  (define type-specific-record (lambda () ;do not run right now...
                                 (cons (string->symbol (format "rridType:~s" identifier-type))
                                       (cons `(identifier (@ (type "local")) ,primary-id)  ; (type "fragment")
                                             (append
                                              (for/list ([f (gtr identifier-type 'fields)])
                                                (cons f
                                                      (let ([rec (dict-ref type-record f)])
                                                        (if (list? rec)
                                                            (let ([str (symbol->string f)])
                                                              (if (string-suffix? str "s")
                                                                  (for/list ([r rec])
                                                                    (list (string->symbol (string-trim str "s" #:left? #f))
                                                                          r))
                                                                  (error (format "Your tag ~a is being used for listing things but doesn't end in s!" f))))
                                                            (list rec)))))
                                              ; FIXME provide a link to the formatting rules doc instead
                                              `((citationFormat ,(format "~s" (gtr identifier-type 'format)))))))))
  (let ([record (record-format
                 #:id primary-id
                 #:altids altids
                 #:relids relids
                 #:pc proper-citation
                 #:jc jats-citation
                 #:submitted (dict-ref type-record 'submitted)
                 #:updated (dict-ref type-record 'updated)
                 #:titles titles
                 #:creators creators
                 #:contributors contributors
                 #:resourceType `(resourceType (@ ,(bind resourceTypeGeneral))
                                               ,(type-dict-or-record 'resourceType))  ; this pattern is really easy to lift if we need
                 #:description (let ([out (dict-ref type-record 'description #f)])
                                 (if out `(description (@ (xml:lang "en-US")) ,out) '()))
                 #:subjects subjects
                 #:publisher issuing-source)])
    (add-rec record to-resolve)))

(define (make-gtr! identifier-sources)  ; TODO further decoupling but not now
  (define no-arg (gensym))
  (define (gtr type field [fail no-arg])
    "gtr
     get-type-record
     Pass to make-make-record"
    (let ([source-record (dict-ref identifier-sources type)])
      (dict-ref source-record field (if (eq? fail no-arg)
                                        (λ () (raise-mismatch-error 'dict-ref
                                                                    (format "no value for key: ~e in: " field)
                                                                    source-record))
                                        fail))))
  (set-gtr! gtr)
  (void))

#;
(module+ test
  (require "database.rkt")
  (require "sources.rkt")
  (make-gtr! identifier-sources)
  ;(set-add-rec! add-rec)  ; FIXME this is a really really terrible way to handle this :/
  ; but I don't want to have to rewrite the stupid function signature :/
  (define rec (make-record 'fake
                           'PREFIX_1234567
                           #:record
                           (fake-rec
                            #:title "Fake resource"
                            #:insert_time 1111111111
                            #:curate_time 1222222222
                            #:something "We need this for the proper citation")))
  ((schema-structure) rec))
