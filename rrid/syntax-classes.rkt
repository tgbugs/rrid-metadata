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
           racket/syntax
           racket/string
           'double-derp
           (for-syntax racket/base
                       'double-derp
                       )
           (for-template (except-in racket/base _)
                         (only-in racket/list range)
                         (only-in syntax/parse pattern define-syntax-class id)
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

  (define count 0)
  (define (next-name)
    (let-values ([(n m) (quotient/remainder count 26)]  ; FIXME why doesn't the error here give a line number :/
           )
      (set! count (add1 count))
      (list->string (build-list (add1 n) (λ (blank) (integer->char (+ 97 m)))))  ; FIXME there has to be a better way...
      ))
  (define (nsuf this-syntax stx)
    (datum->syntax this-syntax (string->symbol (string-append (symbol->string (syntax->datum stx)) (next-name)))))
  (define (syntax-string-prefix? stx-str pref-str)
    (string-prefix? (symbol->string (syntax->datum #'runtime-name)) pref-str))
  (define (syntax-string-suffix? stx-str suff-str)
    (string-suffix? (symbol->string (syntax->datum #'runtime-name)) suff-str))

  (define-syntax-class sc-head
    (pattern [(~or* -name:id name-pat:sc-name-pat)
              count-spec:sc-count
              (~optional (~seq #:restrictions restriction:sc-restr))]
             #:attr name (if (attribute -name)
                             (nsuf this-syntax #'-name)
                             (nsuf this-syntax #'name-pat.name-pattern))
             #:attr match (if (attribute -name)
                              #'-name
                              #'name-pat.name-pattern)
             #:attr sc-pat (if (attribute -name)
                               #'(define-syntax-class name
                                   (pattern runtime-name:id
                                            #:do [(unless (eq? 'match #'runtime-name)
                                                    (raise-syntax-error 'bad-structure
                                                                        (format "expected ~a got ~a"
                                                                                'match
                                                                                (syntax->datum #'runtime-name)))
                                                    )]))
                               #'(define-syntax-class name
                                   (pattern runtime-name:id
                                            #:do [(let* ([p-s (string-split (symbol->string 'match) "*" #:trim? #f)]
                                                         [p (car p-s)]
                                                         [s (cadr p-s)])
                                                    (unless (and (syntax-string-prefix? #'runtime-name p)
                                                                 (syntax-string-suffix? #'runtime-name s))
                                                      (raise-syntax-error 'bad-structure
                                                                          (format "expected ~a got ~a"
                                                                                  'match
                                                                                  (syntax->datum #'runtime-name)))))])))
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
         racket/syntax
         racket/pretty
         'derp
         (only-in racket/list flatten)
         ;(for-template syntax/parse)  ; needed for templating define-syntax-class? though not sure why that issue showed up now...
         (for-syntax racket/base syntax/parse 'derp))
(provide (all-defined-out)
         (all-from-out 'derp))

(define-syntax-class sc-terminal
  #:literals (->?)  ; predicate from sibbling path value
  (pattern (~or* predicate:id
                 exact-value:string 
                 exact-value:integer  ; FIXME when does this happen?!
                 (->? subtree:sc-exact-pat))
           #:with termsc (nsuf this-syntax #'termsc)
           ;#:attr predicate-name (if (attribute predicate) () #f)
           #:attr name (cond [(attribute predicate) (generate-temporary #'predicate)]
                             [(attribute exact-value) #'exact-value])
           #:attr sc-pat (cond [(attribute predicate)
                                #'(define-syntax-class termsc
                                    (pattern runtime-value
                                             #:do [(unless (predicate (syntax->datum #'runtime-value))
                                                     (raise-syntax-error 'bad-structure
                                                                         (format "TODO ~a not a ~a"
                                                                                 #'runtime-value
                                                                                 (symbol->string 'predicate))))]))]
                               ;[(attribute exact-value)
                                ;#'(define-syntax-class terminal
                                    ;(pattern runtime-value
                                             ;#:fail-unless (λ () (eq? runtime-value exact-value))))]
                               [else #'(i have no idea what is going on here)])
           )
  ;(pattern predicate:id)
  ;(pattern (~or* exact-value:string exact-value:integer))  ; TODO consider allowing quote literals?
  ;(pattern (->? subtree:sc-exact-pat))  ; make the predicate at compile time?
  )
(define (not-null? thing) (not (null? thing)))
(define (filter-dots stx this-syntax)
  (datum->syntax this-syntax (filter not-null? (syntax->datum stx)))
  )

(define (flatten-dots stx this-syntax)
  (datum->syntax this-syntax (map flatten (filter not-null? (map flatten (syntax->datum stx)))))
  )

(define-syntax-class sc-body
  (pattern (head:sc-head body:sc-body ... (~optional terminal:sc-terminal))
           #:attr name (if (attribute head.sc-pat)
                           ; TODO #:declare runtime-name head.sc-pat
                           #;(car (generate-temporaries '(runtime-name)))
                           #'head.match
                           #'head.match)
           ;#:do [(println `(ct-body: ,(attribute body)))]
           ;#:do [(println `(ct-body: ,(syntax->datum #'(body ...))))]
           ;#:attr -sc-pat #'head.sc-pat
           ;#:attr sc-pat #'body.-sc-pat
           #:attr abody (attribute body)
           #:attr syntax-classes (if (not (null? (syntax->datum #'(body.syntax-classes ...))))
                                     (if (attribute head.sc-pat)
                                         (if (and (attribute terminal) (attribute terminal.predicate))
                                             (filter-dots #'(head.sc-pat body.syntax-classes ... terminal.sc-pat) this-syntax)
                                             (filter-dots #'(head.sc-pat body.syntax-classes ...) this-syntax))
                                         (if (and (attribute terminal) (attribute terminal.predicate))
                                             (filter-dots #'(body.syntax-classes ... terminal.sc-pat) this-syntax)
                                             (filter-dots #'(body.syntax-classes ...) this-syntax)))
                                     (if (attribute head.sc-pat)
                                         (if (and (attribute terminal) (attribute terminal.predicate))
                                             #'(head.sc-pat terminal.sc-pat)
                                             #'head.sc-pat)
                                         (if (and (attribute terminal) (attribute terminal.predicate))
                                             #'terminal.sc-pat
                                             #'())))
           ;#:do [(println `(ct-body-sc: ,(attribute body.syntax-classes)))]
           #:attr head-convention #'[head.match head.name]
           #:attr term-convention (if (and (attribute terminal)
                                            (attribute terminal.predicate))
                                       #'[terminal.name terminal.termsc]
                                       #'())
           ;#:attr body-conventions (flatten-dots #'(body.local-conventions ...) this-syntax) ; TODO
           #:attr -local-conventions (if (attribute head.sc-pat)  ; FIXME head names aren't making it in... this is sill all wrong
                                        (if (and (attribute terminal) (attribute terminal.predicate))
                                            #'(head-convention body.head-convention ... term-convention)
                                            #'(head-convention body.head-convention ...))
                                        #;(if (and (attribute terminal) (attribute terminal.predicate))
                                              ; TODO
                                              #'(body-conventions term-conventions)
                                              #'body-conventions)
                                        #;(with-syntax ([(body-local-convention ...) body.local-conventions])
                                          (if (and (attribute terminal) (attribute terminal.predicate))
                                              #'(body.local-conventions ... term-conventions)
                                              #'(body.local-conventions ...))
                                          )
                                        (let ([body-local-conventions  ; this works better right now
                                               (map flatten
                                                    (filter (λ (thing) (not (null? thing)))
                                                            (syntax->datum
                                                             (if (and (attribute terminal) (attribute terminal.predicate))
                                                                 #'(body.local-conventions ... term-conventions)
                                                                 #'(body.local-conventions ...)))))])
                                          ;(println `(wat: ,blc))
                                          (if (null? body-local-conventions)
                                              #'()
                                              (datum->syntax this-syntax body-local-conventions)
                                              ;(datum->syntax this-syntax (map car asdf))
                                              ;#'(body.local-conventions ...)
                                              ;#f
                                              )
                                          ))
           #:attr local-conventions (let (;[head-conv #'head-convention]
                                          [body-conv
                                           (let ([body-datum
                                                  (map flatten
                                                       (filter (λ (thing) (not (null? thing)))
                                                               (syntax->datum
                                                                #'(head-convention body.local-conventions ... term-convention))))])
                                             (if (null? body-datum)
                                                 #'()
                                                 (datum->syntax this-syntax body-datum)))]
                                          #;[term-conv (attribute term-convention)])
                                      ;; (apply append) flatten once...
                                      ;#'(head-conv body-conv term-conv)
                                      body-conv
                                      )
           ;#:do [(println `(ct-lc: ,(syntax->datum (attribute local-conventions))))]
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
                                (cond  ; FIXME restriction? no, it should just to in the checker...
                                  [(and start stop)
                                   (if (attribute terminal)
                                       #`(name body.head-racket ... terminal.name)
                                       #`(name body.head-racket ... ))  ; FIXME terminals
                                   ;#`(#,(attribute name) (body.name body.body ...) ...)  ; FIXME terminals
                                   ]
                                  [(and start (not stop))
                                   (with-syntax ([elip+ (datum->syntax this-syntax '...+)]
                                                 [seq (datum->syntax this-syntax '~seq)])
                                     ; dont need seq since these should always be enclosed?
                                     ; but then how to we stick the elip on?
                                     (if (attribute terminal)
                                         #`(seq (name body.head-racket ... terminal.name) elip+)
                                         #`(seq (name body.head-racket ...) elip+))
                                     ;#`(#,(attribute name) (body.name body.body ...) ... elip+)
                                     )
                                   ]
                                  [(and (not start) stop)
                                   (if (attribute terminal)
                                       #`(~optional (name body.head-racket ... terminal.name))
                                       #`(~optional (name body.head-racket ...)))
                                   ;#`(~optional (#,(attribute name) (body.name body.body ...) ...))
                                   ]
                                  [(and (not start) (not stop))
                                   (with-syntax ([elip (datum->syntax this-syntax '...)]
                                                 [opt (datum->syntax this-syntax '~optional)]
                                                 [seq (datum->syntax this-syntax '~seq)]
                                                 )
                                     (if (attribute terminal)
                                         #`(opt (seq (name body.head-racket ... terminal.name) elip))
                                         #`(opt (seq (name body.head-racket ...) elip)))
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
                            (list (attribute head.name)))
           #:attr stx-names (datum->syntax this-syntax (attribute names))
           #:attr test-name (if (attribute head.name)
                                #'(λ (sxml) (eq? head.name (car sxml)))
                                #;(λ (sxml)
                                  (println `(,(syntax->datum #'head.name) ,(car sxml)))
                                  (let ([out (eq? (syntax->datum #'head.name) (car sxml))])
                                    (println `(test-name: ,out))
                                    out)
                                  )
                                #'(λ (sxml)
                                  ; TODO handle name-pat
                                  #f
                                  )
                                )
           #:attr test-restr (if (attribute head.restriction)
                                 ; this needs more work at the syntax class level
                                 #'(λ (sxml)
                                     (if #t ;(eq? (car sxml) (attribute head.rest-name))
                                         #t  ; TODO this one is a bit more tricky
                                         #t))
                                 #'(λ (sxml) #t)
                                 )
           #:attr test-body 'TODO  ; TODO
           #:attr test-term (if (attribute terminal)
                                (if (attribute terminal.predicate)
                                    #;(λ (value) ((attribute terminal.predicate) value))
                                    #'(λ (value) (terminal.predicate value))
                                    (if (attribute terminal.exact-value)
                                        #;(λ (value) (eq? value (attribute terminal.exact-value)))
                                        #'(λ (value) (eq? value terminal.exact-value))
                                        (if (attribute terminal.subtree)
                                            #'(λ (value) #f)  ; TODO
                                            #'(raise-syntax-error 'wat "how did we get here!??!"))))
                                #'(λ (sxml) #t)
                                )
           #:attr test #'(λ (sxml)
                           (and 
                            (test-name sxml)
                            (test-restr sxml)
                            (test-term sxml))
                           ) #;(λ (sxml)
                         ((attribute test-name) sxml)
                         ((attribute test-restr) sxml)
                         ((attribute test-term) sxml)  ; FIXME needs to test the terminal
                         )
           #:attr tests (if (attribute body)
                            #'(λ (sxml) (and (test sxml) (body.test (cdr sxml)) ...))  ; TODO body.test body.value?
                            #;(cons (attribute test) (attribute body.tests))
                            #'test
                            #;(list (attribute test)))
           #:attr stx-tests #'tests #;(datum->syntax this-syntax (attribute tests))
           )
  )
