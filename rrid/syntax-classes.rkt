#lang racket/base
(module derp racket/base
  (module double-derp racket/base
    (module blank racket/base
      (provide (all-defined-out))
      (define _ #'i-can-be-whatever-you-need-me-to-be-baby))
    (require syntax/parse
             (for-template 'blank))
    (provide (all-defined-out)
             (for-template (all-from-out 'blank)))
    (define-syntax-class blank-any
      #:literals (_)
      (pattern _))
    (define-syntax-class sc-exact-pat
      ; TODO validating the validation schema means that we should warn if
      ; and exact-cdr-member doesn't match anything
      ; FIXME vs warn-missing on literal leaves? or is that not relevant
      ; there is only one subtree, and it must be direct
      (pattern (name:id (~or* body:sc-exact-pat -match:blank-any))
               ;#:attr cars #'(name body.name) ; body.body.name ; HRM
               #:attr to-match (if (attribute -match)
                                   #'(name to-match)
                                   #'(name body))
               ;#:attr match #'(λ (value) (match value [this-syntax (attribute to-match)]))
               #:attr match #'(λ (value) (match value [this-syntax to-match]))
               )

      ))
  (module symbols racket/base
    (provide (all-defined-out))
    ; big enough for any rrid metadata until the AI's want all their
    ; constitutent identifiers listed on their collaborative papers...
    (define n 99999)
    (define ->? #'value->predicate))

  (require syntax/parse
           'double-derp
           (for-syntax racket/base
                       'double-derp
                       )
           (for-template (only-in racket/list range)
                         (only-in syntax/parse pattern) 
                         'symbols
                         )  ; fixes range unbound in phase 0 -1 relative
           )
  (provide (all-defined-out)
           (all-from-out 'double-derp)
           (for-template (all-from-out 'symbols)))

  (define-syntax-class any-number
    #:literals (n)
    (pattern n))

  (define-syntax-class sc-name-pat
    #:literals (pattern)
    (pattern (pattern name-pattern:id))) 

  (define-syntax-class sc-count
    #:literals (range)
    (pattern number:exact-positive-integer
             ; this is basically 1 or n but we use (range 1 n)
             #:attr start #'number
             #:attr stop #'number)
    (pattern (range start:exact-nonnegative-integer stop:exact-positive-integer))
    (pattern (range start:exact-nonnegative-integer -stop:any-number)
             #:attr stop #'0)) 

  (define-syntax-class sc-restr
    (pattern ([subtree:sc-exact-pat 
               count-spec:sc-count
               (~optional (~seq #:warn on-count:exact-nonnegative-integer))] ...)))

  (define-syntax-class sc-head
    (pattern [(~or* -name:id name-pat:sc-name-pat)
              count-spec:sc-count
              (~optional (~seq #:restrictions restriction:sc-restr))]
             #:attr name (if (attribute -name)
                             #'-name
                             #'name-pat.name-pattern)
             #:attr sc-pat (if (attribute -name)
                               #f  ; FIXME sigh, what is the right tool for dealing with these...
                               #'(define-syntax-class name
                                   (pattern runtime-name:id
                                            #:fail-unless (λ ()
                                                            (let* ([p-s (string-split (symbol->string 'name) "*" #:trim? #f)]
                                                                   [p (car p-s)]
                                                                   [s (cadr p-s)])
                                                              (and (string-prefix runtime-name p)
                                                                   (string-suffix runtime-name s)))))))
             #:attr start #'count-spec.start
             #:attr stop #'count-spec.stop
             ; we can't actually do this inside of there becuase ~optional needs to wrap it
             ;#:attr racket (cond [(attribute count-spec.number) ; TODO name-pat
             ;#'name]
             ;[#t (cond [(eq? (attribute count-spec.start) 1) #'name]
             ;[(eq? (attribute count-spec.start) 0) #'(~optional name body)])])
             ))

  (define-syntax-class sc-pred
    (pattern ([name:id (~or* function:expr function:id)] ...)))

  (define-syntax-class sc-string-pred
    (pattern ([string-value:string predicate:id] ...))))

(require syntax/parse
         racket/pretty
         'derp
         (only-in racket/list flatten)
         (for-syntax racket/base syntax/parse 'derp))
(provide (all-defined-out)
         (all-from-out 'derp))

(define-syntax-class sc-terminal
  #:literals (->?)  ; predicate from sibbling path value
  (pattern (~or* predicate:id
                 exact-value:string 
                 exact-value:integer
                 (->? subtree:sc-exact-pat)))
  ;(pattern predicate:id)
  ;(pattern (~or* exact-value:string exact-value:integer))  ; TODO consider allowing quote literals?
  ;(pattern (->? subtree:sc-exact-pat))  ; make the predicate at compile time?
  )

(define-syntax-class sc-body
  (pattern (head:sc-head body:sc-body ... (~optional terminal:sc-terminal))
           #:attr name (if (attribute head.sc-pat)
                           ; TODO #:declare runtime-name head.sc-pat
                           (car (generate-temporaries '(runtime-name)))
                           #'head.name)
           ;#:do [(println `(ct-body: ,(attribute body)))]
           ;#:do [(println `(ct-body: ,(syntax->datum #'(body ...))))]
           ;#:attr -sc-pat #'head.sc-pat
           ;#:attr sc-pat #'body.-sc-pat
           #:attr abody (attribute body)
           #:attr syntax-classes (if (syntax->datum #'(body.syntax-classes ...))
                                     (if (attribute head.sc-pat)
                                         #'(head.sc-pat body.syntax-classes ...)
                                         #'(body.syntax-classes ...))
                                     (if (attribute head.sc-pat)
                                         #'head.sc-pat
                                         #f))
           #:do [(println `(ct-body-sc: ,(attribute body.syntax-classes)))]
           #:attr head-convention #'[name head.name]
           #:attr local-conventions (if (attribute head.sc-pat)
                                        (let ([thing #'([name head.name] body.head-convention ...)])
                                          (println `(halp: ,thing))
                                          thing)
                                        (let ([blc (map flatten
                                                        (filter (λ (thing) (not (null? thing)))
                                                                (syntax->datum
                                                                 #'(body.local-conventions ...))))])
                                          ;(println `(wat: ,blc))
                                          (if (null? blc)
                                              #'()
                                              (datum->syntax this-syntax blc)
                                              ;(datum->syntax this-syntax (map car asdf))
                                              ;#'(body.local-conventions ...)
                                              ;#f
                                              )))
           #:do [(println `(ct-lc: ,(syntax->datum (attribute local-conventions))))]
           ;#:attr name (if (attribute head.name)
           ;#'head.name
           ; TODO check the name pattern...
           ;#'head.npattern)
           #:attr -literals (if (attribute head.name-pat)
                                #'(name body.-literals ...)
                                #'(body.-literals ...))
           #:attr literals (datum->syntax this-syntax
                                          (let ([lits (attribute -literals)])
                                            ;(println lits)
                                            (flatten (map syntax->datum (flatten lits)))))
           #:attr start (syntax-e #'head.start)
           #:attr stop (syntax-e #'head.stop)
           #:attr head-racket (let* ([-start (attribute start)]
                                     [-stop (attribute stop)]
                                     [start (cond [(= -start 0) #f]
                                                  [(= -start 1) #t]
                                                  [#t (raise-syntax-error 'hrm "HRM allow min 2?")])]
                                     [stop (cond [(= -stop 0) #f]
                                                 [(= -stop 1) #t]
                                                 [#t (raise-syntax-error 'hrm "HRM allow not 1 or n?")])])
                                ;(pretty-print `(start-stop: ,start ,stop ,(attribute name)))
                                (cond  ; FIXME terminal cond
                                  [(and start stop)
                                   #`(#,(attribute name) body.head-racket ...)  ; FIXME terminals
                                   ;#`(#,(attribute name) (body.name body.body ...) ...)  ; FIXME terminals
                                   ]
                                  [(and start (not stop))
                                   (with-syntax ([elip+ (datum->syntax this-syntax '...+)]
                                                 [seq (datum->syntax this-syntax '~seq)])
                                     ; dont need seq since these should always be enclosed?
                                     ; but then how to we stick the elip on?
                                     #`(seq (#,(attribute name) body.head-racket ...) elip+)
                                     ;#`(#,(attribute name) (body.name body.body ...) ... elip+)
                                     )
                                   ]
                                  [(and (not start) stop)
                                   #`(~optional (#,(attribute name) body.head-racket ...))
                                   ;#`(~optional (#,(attribute name) (body.name body.body ...) ...))
                                   ]
                                  [(and (not start) (not stop))
                                   (with-syntax ([elip (datum->syntax this-syntax '...)]
                                                 [opt (datum->syntax this-syntax '~optional)]
                                                 [seq (datum->syntax this-syntax '~seq)]
                                                 )
                                     #`(opt (seq (#,(attribute name) body.head-racket ...) elip))
                                     ;#`(~optional (~seq (#,(attribute name) (body.name body.head-racket ...) ...) elip))
                                     )
                                   ]
                                  [#t (raise-syntax-error 'wat "should not get here")]
                                  ))
           #:attr racket (attribute head-racket); #`(#,(attribute name) body.head-racket ...)
           #:attr -racket '(let ([start (attribute body.start)]
                                 [stop (attribute body.stop)])
                             (if start
                                 (if stop
                                     #'(head.name body.head-racket ...) ; no ...
                                     ;#'(head.name (body.name body.body) ... ...) ; no ...
                                     #'(head.name body.head-racket ...) ; ...
                                     ;#'(head.name (body.name body.body) ... ...) ; ...
                                     )
                                 (if stop
                                     #'(head.name body.head-racket ...)
                                     ;#'(head.name (~optional (body.name body.body)) ... ...) ; opt no ...
                                     ;#'(head.name (~optional (body.name body.body)) ... ...) ; opt ...
                                     )))
           #:attr names (if (attribute body)
                            (cons (attribute head.name) (attribute body.names))
                            (list (attribute head.name))
                            )
           #:attr stx-names (datum->syntax this-syntax (attribute names))
           #:attr test-name (if (attribute head.name)
                                (λ (sxml)
                                  (println `(,(syntax->datum #'head.name) ,(car sxml)))
                                  (let ([out (eq? (syntax->datum #'head.name) (car sxml))])
                                    (println `(test-name: ,out))
                                    out)
                                  )
                                (λ (sxml)
                                  ; TODO handle name-pat
                                  #f)
                                )
           #:attr test-restr (if (attribute head.restriction)
                                 ; this needs more work at the syntax class level
                                 (λ (sxml)
                                   (if #t ;(eq? (car sxml) (attribute head.rest-name))
                                       #t  ; TODO this one is a bit more tricky
                                       #t))
                                 (λ (sxml) #t)
                                 )
           #:attr test-body 'TODO  ; TODO
           #:attr test-term (if (attribute terminal)
                                (if (attribute terminal.predicate)
                                    (λ (value) ((attribute terminal.predicate) value))
                                    (if (attribute terminal.exact-value)
                                        (λ (value) (eq? value (attribute terminal.exact-value)))
                                        (if (attribute terminal.subtree)
                                            (λ (value) #f)  ; TODO
                                            (raise-syntax-error 'wat "how did we get here!??!"))))
                                (λ (sxml) #t)
                                )
           #:attr test (λ (sxml)
                         ((attribute test-name) sxml)
                         ((attribute test-restr) sxml)
                         ((attribute test-term) sxml)  ; FIXME needs to test the terminal
                         )
           #:attr tests (if (attribute body)
                            (cons (attribute test) (attribute body.tests))
                            (list (attribute test)))
           #:attr stx-tests (datum->syntax this-syntax (attribute tests))
           )
  )
