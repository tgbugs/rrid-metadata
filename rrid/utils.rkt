#lang racket/base
(require
 json
 rackunit
 racket/function
 racket/list
 syntax/macro-testing
 (for-syntax racket/base syntax/parse))
(provide hr check-syntax-exn define-string-predicate)

(define-syntax (check-syntax-exn stx)
  (syntax-parse stx
    [(_ body:expr)
     (syntax/loc #'body (check-exn exn:fail:syntax? (thunk (convert-syntax-error body))))]))

(define-syntax (define-string-predicate stx)
    (syntax-parse stx
      [(_ predicate:id string-to-match:string ...+)
       #'(define (predicate value) (member value (list string-to-match ...)))]))

(define (hr nested-hashes #:keys [keys-please #f] . keys)
  ; DONE just allow user to set #:keys and have that become true? see hrm
  ; #:keys #f does make toggeling eaiser...
  ; DONE allow user to spec that there is a list of hashes see 'over-list and 'any-key
  ; TODO allow multiple values for keys in addition to any-key

  #;
  (define rkeys (reverse keys))

  #;
  (when (and (list? keys) (not (empty? keys)))
    (println (car keys)))
  #;
  (begin
    (when (hash? nested-hashes)
      (println (hash-keys nested-hashes)))
    (println keys)
    (when (not (null? keys))
      (println (list 'syntax? (syntax? (car keys))))
      (println (null? (car keys)))
      #;
      (println (null? (cdr rkeys))))
    (println keys-please))

  (let* ([carkeys (if (null? keys) keys (car keys))]
         [end (cond
               [(null? keys) nested-hashes]
               [(and (syntax? carkeys) (eq? (syntax->datum carkeys) 'over-list))
                (map (λ (list-hash) (apply hr list-hash (cdr keys) #:keys keys-please))
                     nested-hashes)]
               #; ; works but makes it hard to infer the schema when reading
               [(and (syntax? carkeys) (eq? (syntax->datum carkeys) 'any-key) (list? nested-hashes))
                (map (λ (list-hash) (apply hr list-hash (cdr keys) #:keys keys-please))
                     nested-hashes)]
               [(and (syntax? carkeys) (eq? (syntax->datum carkeys) 'any-key))  ; FIXME better errors on type mismatch
                (map (λ (list-hash) (apply hr list-hash (cdr keys) #:keys keys-please))
                     (hash-values nested-hashes))]
               [(null? (cdr keys))  ; FIXME skip on null ... aka splice it out?
                (hash-ref nested-hashes carkeys null)]
               [else (apply hr (hash-ref nested-hashes carkeys) (cdr keys) #:keys keys-please)]
               )])
    (if keys-please
        (cond [(list? end) (remove-duplicates end)]
              [(hash? end) (hash-keys end)]
              [else end])
        end)))

(module+ test
  (require rackunit)
  (define res #hasheq((_source . #hasheq((synonyms . #hasheq((a . 1)
                                                             (b . 2)
                                                             (c . 3)))))))
  (check-equal? (hr res '_source #:keys #t) '(synonyms))
  (hr res '_source 'synonyms #:keys #f))

(define any-key (syntax any-key))
(define over-list (syntax over-list))
(define-syntax (hrm stx)
  (syntax-parse stx
    #:literals (quote)
    #:datum-literals (:keys * /)
    #:disable-colon-notation
    #:local-conventions ([nested-hashes id]
                         [pathspec id]
                         [match-key id]
                         )
    [(_ nested-hashes (~seq (~or* (~seq / (~bind [oper #'any-key]))
                                  (~seq * (~bind [oper #'over-list]))
                                  pathspec)
                            (~optional (~seq #:match ((~alt lone-value [match-key match-value]) ...)))) ...
        (~optional (~seq #:keys (~bind [show-keys #'#t])) #:defaults ([show-keys #'#f])))
     (define out
     #'(hr nested-hashes (~? oper (quote pathspec)) ... #:keys show-keys)
     )
     ;(println out)
     out
     ]))

(module+ test
  (check-equal? (hrm res _source #:keys) '(synonyms))
  )
