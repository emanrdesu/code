
(define-module (macro-help)
  #:export-syntax (with-uniqsyms with-gensyms))

(define-public (uniqsym) (make-symbol ""))

(define-public (c*ar||id x)
  (if (pair? x) (c*ar||id (car x)) x))

(define-public (car||id x)
  ((if (pair? x) car identity) x))

(define-public (cdr||id x)
  ((if (pair? x) cdr identity) x))

(define-public (atom? x)
  (not (or (pair? x) (list? x))))

(define-public (symbolicate . sym||str)
  (define (->str x) ((if (symbol? x) symbol->string identity) x))
  (string->symbol (apply string-append (map ->str sym||str))))

(define-macro (with-uniqsyms vars . body)
  `(let ,(map (λ (v) `(,v (uniqsym))) vars) ,@body))

(define-macro (with-gensyms vars . body)
  `(let ,(map (λ (v) `(,v (gensym))) vars) ,@body))
