
(define-module (goodies)
  #:duplicates (last)
  
  #:use-module (ice-9 q)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 and-let-star)
  
  #:export
  (pi e id replicate mod ^ ○ ! P C
   curry curryr curryc curryn
   gx.fx x.fx fx.x f..x
   flip tuply untup unthunk call sargs
   displayln sow-seed! iotar
   randelt chunk intersperse flatten
   replace replace-multiple take drop
   insert pair->list singleton? length=?
   conjoin disjoin cartesian-product ×
   compound iterate counts)

  #:export-syntax
  (reverse-args thunk swap! values->list with-return
    define-multiple define-multiple-public
    synonymize defun doarray))


(define-macro (thunk . body)
  `(lambda ,(make-symbol "") ,@body))

(define-macro (define-multiple bindings . definitions)
  (cons 'begin (map (λ (b d) `(define* ,b ,d)) bindings definitions)))

(define-macro (synonymize synonym existing)
  `(define-macro (,synonym . body) (cons ',existing body)))

(synonymize defun define*)

(define-macro (reverse-args function . args)
  `(,function ,@(reverse args)))

(define-syntax swap!
  (syntax-rules ()
    ((swap! var1 var2)
     (let ((temp var1))
       (set! var1 var2)
       (set! var2 temp)))))

(define-syntax values->list
  (syntax-rules ()
    ((values->list expr)
     (call-with-values (lambda () expr)
       (lambda vs (apply list vs))))))

(define pi (* (asin 1) 2))
(define e (exp 1))

(define id identity)
(define replicate make-list)
(define mod modulo)
(define ^ expt)
(define ○ compose)

(defun (! n #:optional (a 1))
  (if (= n 0) a (! (- n 1) (* a n))))

(define (P n r)
  (/ (! n) (! (- n r))))

(define (C n r)
  (/ (P n r) (P r r)))

(define (curry function . args)
  (lambda rest (apply function (append args rest))))

(define (curryr function . args)
  (lambda rest (apply function (append rest args))))

(define (curryc function . args)
  (reverse-args apply (procedure-minimum-arity function)
    (lambda (required optional rest?)
      (if (>= (length args) required) (apply function args)
	  (lambda arg (apply curryc function (append args arg)))))))

(define (curryn function . args)
  (lambda rest
    (apply function
      (let loop ((args (map reverse args)) (rest rest))
        (if (null? args) rest
	    (loop (cdr args)
		  (apply insert
		    (cons* (caar args) (1- (cadar args)) (list rest)))))))))

(define (flip function)
  (lambda args (apply function (reverse args))))

(define tuply (curry curry apply))

(define (untup f)
  (lambda args (f args)))

(define (unthunk thunk)
  (lambda ignore (thunk)))

(define (call function . args)
  (apply function args))

(define (gx.fx g f)
  (lambda (x) (cons (g x) (f x))))

(define x.fx (curry gx.fx id))
(define fx.x (curryr gx.fx id))

(define (f..x . functions)
  (lambda (x)
    (map (curryr call x) functions)))

(define (displayln x)
  (display x)
  (newline))

(define (sow-seed!)
  (set! *random-state* (seed->random-state (current-time))))

(define (randelt list)
  (list-ref list (random (length list))))

(define iotar (○ cdr iota))

(define (chunk n list)
  (if (null? list) '()
      (cons (take n list) (chunk n (drop n list)))))

(defun (intersperse x list #:optional (n 1))
  (if (or (null? list) (null? (drop n list))) list
      (append! (take n list) `(,x) (intersperse x (drop n list)))))

(define (flatten lst)
  (cond ((null? lst) '())
	((pair? lst) (append! (flatten (car lst)) (flatten (cdr lst))))
	(else (list lst))))

(defun (replace x y tree #:key (test eqv?))
  (cond ((null? tree) tree)
	((not (or (pair? tree) (list? tree))) (if (test x tree) y tree))
	(else (cons (replace x y (car tree) #:test test)
		    (replace x y (cdr tree) #:test test)))))

(defun (replace-multiple xs ys tree #:key (test eqv?))
  (fold (curryr replace #:test test) tree xs ys))
      
(define (take n list)
  (if (or (null? list) (zero? n)) '()
      (cons (car list) (take (1- n) (cdr list)))))

(define (drop n list)
  (if (or (null? list) (zero? n)) list
      (drop (1- n) (cdr list))))

(define (insert x n list)
  (append! (take n list) (cons x (drop n list))))

(define (pair->list pair)
  (list (car pair) (cdr pair)))

(define singleton? (○ null? cdr))

(define (length=? . lists)
  (apply = (map (disjoin (conjoin list? length) id) lists)))

(define (conjoin . functions)
  (lambda args
    (let loop ((prev #t) (f functions))
      (if (null? f) prev
	  (and-let* ((r (apply (car f) args)))
	    (loop r (cdr f)))))))

(define (disjoin . functions)
  (lambda args
    (let loop ((f functions))
      (if (null? f) #f
	  (or (apply (car f) args) (loop (cdr f)))))))

(define (cartesian-product . lists)
  (if (singleton? lists)
      (cartesian-product (car lists) (car lists))
      (reverse-args fold-right lists '(())
	(lambda (xs ys)
	  (append-map (λ (x) (map (curry cons x) ys)) xs)))))

(define × cartesian-product)

(defun (compound f init until #:key q last (collect #t))
  (let loop ((val init) (col (make-q)))
    (if (until val)
	(if collect
	    ((if q id car) (if last (enq! col val) col))
	    (if last val (q-rear col)))
	(loop (f val) (enq! col val)))))

(defun (iterate producer until #:key q last (collect #t))
  (compound producer (producer) until #:q q #:last last #:collect collect))

(define (counts n)
  (let ((k 0))
    (thunk (set! k (1+ k)) (> k n))))

(define-macro (with-return . body)
  `((lambda () (call/cc (lambda (return) ,@body)))))

(define (sargs f . n)
  (lambda args
    (apply f (map (curryr call args)
		  (map (curry curryr list-ref) (map 1- n))))))


(define-syntax doarray
  (syntax-rules ()
    ((doarray (var var-index seq ret ...) body0 body ...)
     (let ((seqvar seq))
       (array-for-each
	(λ (var var-index) body0 body ...)
	seqvar
	(list->u32vector (iota (array-length seqvar))))
       ret ...))))
