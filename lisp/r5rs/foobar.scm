
(let ((exports
       '(;; MACROS
         define-macro synonymize
         thunk swap!
         values->list values->list* with-return
         optional match define-multiple rawr

         ;; RENAME + MATH
         λ def
         identity id
         replicate mod ^
         pi e ! P C
         adder succ pred

         ;; FUNCTIONS
         Y curry curyr
         flip tuply arged
         zary nary unary sargs
         compose compose * ○ ○* pipe pipe*
         opp
         valued listed listed*
         conjoin disjoin orid conorid dis con
         call key unthunk
         fx.fy gx.fx x.fx fx.x f..x !id

         ;; LIST
         singleton? length=?
         some every
         cons* snoc
         kar ckr cnr
         first second third fourth fifth
         sixth seventh eight ninth tenth
         last last-pair pair->list list->pair boros
         foldl foldr scanl scanr foldl1 foldr1
         span take-while drop-while partition
         take drop chunk zip unzip
         left right
         head tail matry
         heads tails matryoshka
         filter find find-index remove assoc-ref
         filter-map mappend elt nub
         insert remove-index
         replace replacem map-tree
         intersperse flatten
         cartesian-product ×
         range qsort rearrange

         ;; FILES
         read-file
         write-file
         with-input-from
         with-output-to

         ;; MISC
         atom?
         displayln print puts
         explode implode symbol-reverse
         compound iterate
         counts counter)))
  #t)

;; MACROS

(define-syntax define-macro
  (syntax-rules (unquote)
    ((define-macro (name . reserved)
       ((_ . args) . body) ...)

     (define-syntax name
       (syntax-rules reserved
         ((_ . args) . body) ...)))

    ((define-macro name ((_ . args) . body) ...)
     (define-macro (name) ((_ . args) . body) ...))

    ((define-macro (name . args) . body)
     (define-macro name ((name . args) . body)))))



(define-macro (synonymize new sym)
  (define-macro (new . args) (sym . args)))

(define-macro (thunk . body)
  (lambda _ . body))

(define-macro (swap! a b)
  (let ((t a))
    (set! a b)
    (set! b t)))

(define-macro (values->list expr)
  (call-with-values (lambda () expr)
    (lambda vs (apply list vs))))

(define-macro (values->list* expr)
  (call-with-values (lambda () expr)
    (lambda vs
      (if (singleton? vs) (car vs)
          (apply list vs)))))


(define-syntax with-return
  (syntax-rules ()
    ((with-return x . body)
     ((lambda ()
        (call-with-current-continuation
         (lambda (x) . body)))))))


;; (optional list (var ...) . body)
;; best used within lambdas on a var after a . (dot)

;; (define x '())
;; (optional x (a b) (list a b)) => (#f #f)

;; (optional '(1) (a b) (list a b)) => (1 #f)
;; (optional '(1 2 3 4 5) (a b & r) (list a b r)) => (1 2 (3 4 5))

;; (optional '(1) (a (b 3)) (list a b)) => (1 3)
;; (optional '(1) (a (b 3) & r) (list a b r)) => (1 3 ())

;; (optional '(1) (a (b (+ a 2))) (list a b)) => (1 3)
;; (optional '(1 0) (a (b (+ a 2))) (list a b)) => (1 0)

;; alternatively
;; (define x '())
;; the following happen only if x is ()
;; (optional x 1) => sets x to 1
;; (optional x 'a) => sets x to a
;; (optional x a) => sets x to value of a if it was defined, otherwise error
;; (optional x ,(+ 1 2)) => sets x to 3
;; (optional x '(+ 1 2)) => sets x to (+ 1 2)

(define-macro (optional quote unquote)
  ((optional x (unquote . expr))
   (set! x (if (null? x) (begin . expr) (car x))))
  ((optional x (quote . expr))
   (set! x (if (null? x) (quote . expr) (car x))))
  ((optional from (x ...) . body)
   (letrec-syntax
    ((optional-helper
      (syntax-rules (&)
        ((_ n hfrom () . hbody) (begin . hbody))
        ((_ n hfrom (& r) . hbody)
         (let ((r (drop n hfrom)))
           (begin . hbody)))
        ((_ n hfrom ((hx hv) . hrest) . hbody)
         (let ((hx (if (< n (length hfrom))
                       (list-ref hfrom n)
                       hv)))
           (optional-helper (+ n 1) hfrom hrest . hbody)))
        ((_ n hfrom (hx . hrest) . hbody)
         (optional-helper n hfrom ((hx #f) . hrest) . hbody)))))
    (optional-helper 0 from (x ...) . body)))
  ((optional x v)
   (set! x (if (null? x) v (car x)))))


(define-macro (match)
  ((match () . body) (begin . body))
  ((match (v l . rest) . body)
   (optional l v (match rest . body))))

(define-macro (define-multiple (x ...) v ...)
  (begin (define x v) ...))


(define-macro (rawr f args ...)
  (letrec-syntax
    ((rev (syntax-rules ()
            ((rev x) (rev x ()))
            ((rev () a) a)
            ((rev (x . r) a)
             (rev r (x . a))))))

    (rev (args ... f))))

;; RENAME + MATH

(synonymize λ lambda)
(synonymize f lambda)
(synonymize fun lambda)
(synonymize def define)

(define (identity x) x)
(define id identity)
(define replicate make-list)
(define mod modulo)
(define (^ . ns) (foldl1 expt ns))

(define pi (* (asin 1) 2))
(define e (exp 1))

(define (! n)
  (define (factorial n a)
    (if (<= n 0) a (factorial (- n 1) (* a n))))
  (factorial n 1))

(define (P n r)
  (/ (! n) (! (- n r))))

(define (C n r)
  (/ (P n r) (P r r)))

(define (adder x)
  (lambda (y) (+ x y)))

(define succ (adder 1))
(define pred (adder -1))


;; FUNCTIONS

(define (Y f)
  ((lambda (g) (g g))
   (lambda (g)
     (f (lambda x (apply (g g) x))))))


(define (curry f . args)
  (lambda rest (apply f (append args rest))))

(define (curyr f . args)
  (lambda rest (apply f (append rest args))))


(define (flip f)
  (lambda args (apply f (reverse args))))

(define tuply (curry curry apply))

(define (arged f)
  (lambda args (f args)))


(define (zary f)
  (thunk (f)))

(define (nary n f)
  (lambda x (apply f (take n x))))

(define unary (curry nary 1))

(define (sargs f . n)
  (lambda args
    (apply f
           (map (curyr call args)
                (map (curry curyr list-ref) n)))))


(define-values (compose compose*)
  (let ((○ (lambda (f g)
             (lambda x (f (apply g x)))))

        (○* (lambda (f g)
              (lambda x
                (apply f (values->list (apply g x))))))

        (compose-with
         (Y (lambda (self)
              (lambda (f)
                (lambda fs
                  (cond ((null? fs) id)
                        ((null? (cdr fs)) (car fs))
                        (else
                         (f (car fs)
                            (apply (self f) (cdr fs)))))))))))

    (values (compose-with ○)
            (compose-with ○*))))


(define ○ compose)
(define ○* compose*)
(define pipe (flip compose))
(define pipe* (flip compose*))

(define opp (curry ○ not))

(define valued (curry ○ (tuply values)))

(define (listed f)
  (lambda args
    (values->list (apply f args))))

(define (listed* f)
  (lambda args
    (values->list* (apply f args))))


(define (conjoin . fs)
  (lambda xs
    (let loop ((v '(#t)) (f fs))
      (if (null? f) (apply values v)
          (let ((r (values->list (apply (car f) xs))))
            (and (car r) (loop r (cdr f))))))))

(define (disjoin . fs)
  (lambda xs
    (let loop ((f fs))
      (if (null? f) #f
          (let ((r (values->list (apply (car f) xs))))
            (if (car r)
                (apply values r)
                (loop (cdr f))))))))


(define con conjoin)
(define dis disjoin)

(define orid (curyr disjoin id))
(define conorid (○ orid conjoin))

(define (call f . args) (apply f args))

(define (key f g)
  (lambda args (apply f (map g args))))

(define (unthunk thunk)
  (lambda ignore (thunk)))


(define (fx.fy f pair)
  (cons (f (car pair)) (f (cdr pair))))

(define (gx.fx g f)
  (lambda (x) (cons (g x) (f x))))

(define x.fx (curry gx.fx id))
(define fx.x (curyr gx.fx id))

(define (f..x . fs)
  (lambda (x)
    (map (curyr call x) fs)))


(define (!id p)
  (lambda x (begin (apply p x) (apply values x))))


;; LIST

(define singleton?
  (conjoin (opp null?) (○ null? cdr)))

(define (length=? . lists)
  (apply = (map (conorid list? length) lists)))

(define (some p . ls)
  (define (some-helper p l)
    (if (null? l) #f
        (or (p (car l))
            (some-helper p (cdr l)))))

  (if (or (null? ls) (some-helper null? ls)) #f
      (or (apply p (map car ls))
          (apply some p (map cdr ls)))))

(define (every p . ls)
  (foldl eqv? #t (apply map p ls)))


(define (cons* arg . rest)
  (if (null? rest) arg
      (cons arg (apply cons* (car rest) (cdr rest)))))

(define (snoc x lst)
  (append lst (list x)))

(define (kar x)
  (if (pair? x) (car x) '()))

(define (ckr x)
  (if (pair? x) (cdr x) '()))

(define (cnr n)
  (apply ○ (replicate n ckr)))

(define first kar)
(define second (○ kar (cnr 1)))
(define third (○ kar (cnr 2)))
(define fourth (○ kar (cnr 3)))
(define fifth (○ kar (cnr 4)))
(define sixth (○ kar (cnr 5)))
(define seventh (○ kar (cnr 6)))
(define eight (○ kar (cnr 7)))
(define ninth (○ kar (cnr 8)))
(define tenth (○ kar (cnr 9)))

(define (last l) (kar (reverse l)))

(define (last-pair list)
  (if (or (null? list) (null? (cdr list))) list
      (last-pair (cdr list))))

(define (pair->list pair)
  (list (car pair) (cdr pair)))

(define (list->pair list)
  (cons (first list) (second list)))

(define (boros list)
  (let ((x (list-copy list)))
    (set-cdr! (last-pair x) x) x))


(define (foldl kons knil . ls)
  (if (or (null? ls) (some null? ls)) knil
      (apply foldl kons
             (apply kons knil (map car ls))
             (map cdr ls))))

(define (foldr kons knil . ls)
  (if (or (null? ls) (some null? ls)) knil
      (apply kons
        (snoc (apply foldr kons knil (map cdr ls)) (map car ls)))))

(define (scanl kons knil . ls)
  (reverse
   (map (tuply (curry foldl kons knil))
        (apply zip (map heads ls)))))

(define (scanr kons knil . ls)
  (map (tuply (curry foldr kons knil))
       (apply zip (map tails ls))))


(define (foldl1 kons list . fallback)
  (optional fallback #f)
  (cond ((null? list) fallback)
        ((singleton? list) (car list))
        (else
         (foldl kons (car list) (cdr list)))))

(define (foldr1 kons list . fallback)
  (optional fallback #f)
  (cond ((null? list) fallback)
        ((singleton? list) (car list))
        (else
         (foldr kons (last list) (head list)))))


(define (span p list)
  (define (span-helper p a list)
    (cond ((null? list) `(,a ()))
          ((p (car list))
           (span-helper p (snoc (car list) a) (cdr list)))
          (else `(,a ,list))))
  (span-helper p '() list))


(define take-while (pipe span first))
(define drop-while (pipe span second))

(define (partition p list)
  (rawr foldr list '(() ())
    (lambda (x a)
      (match ((l1 l2) a)
        (if (p x)
            `(,(cons x l1) ,l2)
            `(,l1 ,(cons x l2)))))))


(define (take n list)
  (if (or (null? list) (zero? n)) '()
      (cons (car list) (take (- n 1) (cdr list)))))

(define (drop n list)
  (if (or (null? list) (zero? n)) list
      (drop (- n 1) (cdr list))))

(define (chunk n list)
  (if (null? list) '()
      (cons (take n list) (chunk n (drop n list)))))

(define zip
  (compose reverse
    (curry foldl (lambda (a . x) (cons x a)) '())))

(define (unzip lsts)
  (if (or (null? lsts) (some null? lsts)) '()
      (cons (map car lsts)
            (unzip (map cdr lsts)))))

(define (left list)
  (take (floor/ (length list) 2) list))

(define (right list)
  (drop (ceiling (/ (length list) 2)) list))

(define (head list . n)
  (optional n 1)
  (take (- (length list) n) list))

(define (tail list . n)
  (optional n 1)
  (drop n list))

(define (matry list . n)
  (optional n 1)
  (head (tail list n) n))

(define-values (heads tails)
  (apply values
    (map (lambda (f)
           (lambda (list . n)
             (optional n ,(+ (length list) 1))
             (map f (replicate n list) (range n))))
         (list head tail))))

(define (matryoshka list . n)
  (optional n ,(length list))
  (take n (compound matry list null? #t #t)))

(define (filter p list)
  (foldr (lambda (x a) (if (p x) (cons x a) a)) '() list))

(define (find p list . tail?)
  (optional tail? #f)
  (let ((r (drop-while (opp p) list)))
    (if (null? r) #f ((if tail? id car) r))))

(define (find-index p list)
  ((conjoin pair? cdr)
   (find (○ p car)
         (map (x.fx (counter -1)) list))))

(define (remove x list . test)
  (optional test eqv?)
  (filter (curry (opp test) x) list))

(define (assoc-ref k list)
  (let ((r (assoc k list)))
    (and r (values (cdr r) #t))))

(define filter-map
  (○ (curry filter id) map))

(define mappend (○ (tuply append) map))

(define elt (flip list-ref))

(define (nub list . test)
  (optional test eqv?)
  (if (or (null? list) (singleton? list))
      list
      (cons (car list)
            (nub (remove (car list) (cdr list) test) test))))

(define (insert x n list)
  (append (take n list) (cons x (drop n list))))

(define (remove-index n list)
  (append (take n list) (drop (+ n 1) list)))

(define (replace x y tree . test)
  (optional test eqv?)
  (cond ((null? tree) tree)
	      ((not (or (pair? tree) (list? tree))) (if (test x tree) y tree))
	      (else (cons (replace x y (car tree) test)
		                (replace x y (cdr tree) test)))))

(define (replacem xs ys tree . test)
  (optional test eqv?)
  (foldr (curyr replace test) tree xs ys))

(define (map-tree f tree . key)
  (optional key id)
  (if (null? tree) '()
      (if (atom? tree) (f tree)
          (cons (map-tree f (car tree) key)
                (map-tree f (cdr tree) key)))))


(define (intersperse x list . n)
  (optional n 1)
  (if (or (null? list) (null? (drop n list))) list
      (append (take n list) `(,x) (intersperse x (drop n list) n))))

(define (flatten lst)
  (cond ((null? lst) '())
	      ((pair? lst) (append (flatten (car lst)) (flatten (cdr lst))))
	      (else (list lst))))

(define (cartesian-product . ls)
  (if (singleton? ls)
      (cartesian-product (car ls) (car ls))
      (foldr
	      (lambda (xs ys)
	        (mappend (lambda (x) (map (curry cons x) ys)) xs)) '(()) ls)))

(define × cartesian-product)

(define (range . args)
  (match ((a b s) args)
    (case (length args)
      ((1) (range 0 a 1))
      ((2) (range a b 1))
      (else
        (compound (adder s) a
          (curry (if (positive? s) <= >=) b))))))

(define (qsort list p . key)
  (optional key id)
  (if (null? list) '()
      (let ((cp (curry p (key (car list)))))
        (append
         (qsort (filter (○ (opp cp) key) (cdr list)) p key)
         (take 1 list)
         (qsort (filter (○ cp key) (cdr list)) p key)))))


(define (rearrange order xs)
  (map call
       (map (curry (curry elt)) order)
       (replicate (length order) xs)))

;; FILE

(define (read-file f)
  (with-input-from-file f read))

(define (write-file f x)
  (with-output-to-file f
    (thunk (write x))))

(define-macro (with-input-from f . body)
  (with-input-from-file f
    (thunk . body)))

(define-macro (with-output-to f . body)
  (with-output-to-file f
    (thunk . body)))

;; MISC

(define (atom? x)
  (not (or (pair? x) (list? x))))

(define (displayln x)
  (display x) (newline))

(define (print . xs)
  (for-each display xs))

(define (puts . xs)
  (for-each displayln xs))

(define explode
  (pipe symbol->string
        string->list
        (curry map
          (pipe string string->symbol))))

(define implode
  (pipe (curry map symbol->string)
        (tuply string-append)
        string->symbol))

(define symbol-reverse
  (○ implode reverse explode))


(define (compound f x until . args)
  (define (compound-helper f x until last?)
    (if (until x)
        (if last? `(,x) '())
        (cons x (compound-helper f (f x) until last?))))

  (optional args ((collect? #t) last?)
    ((if collect? id last)
     (compound-helper f x until last?))))

(define (iterate producer until . args)
  (optional args ((collect? #t) last?)
    (compound producer (producer) until collect? last?)))


(define (counts n)
  (let ((k 0))
    (lambda x (set! k (+ k 1)) (> k n))))

(define (counter . args)
  (optional args ((x0 0) (f succ))
    (let ((x x0))
      (thunk (set! x (f x)) x))))
