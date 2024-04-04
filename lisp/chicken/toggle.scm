
(import (chicken file)
        (chicken process)
        (chicken process-context))


;;; HELPER

(define (read1 path)
  (with-input-from-file path
    (lambda () (read))))

(define (write-to path . vs)
  (with-output-to-file path
    (lambda () (for-each write vs))))


(define (Join a b)
  (string-append a "/" b))


;;; CACHE DIRECTORY

(define cache-home
  (let ((env get-environment-variable))
    (or (env "XDG_CACHE_HOME")
        (Join (env "HOME") ".cache"))))

(define cache
  (Join cache-home "toggle"))


;;; MAIN FUNCTIONS

(define *toggle* '())

(define (toggle name)
  (let ((path (Join cache (symbol->string name))))
    (if (not (file-exists? path))
        (write-to path 1))

    (let ((value (read1 path)))
      (write-to path (- 1 value))
      (= value 1))))

(define-syntax deftoggle
  (syntax-rules ()
    ((_ name (on ...) (off ...))
     (begin
       (set! *toggle* (cons 'name *toggle*))
       (define (name)
         (for-each process-run
           (if (toggle 'name) '(on ...) '(off ...))))))))


;;; TOGGLE DEFINITIONS

(deftoggle polybar
  ("bspc config top_padding 32"
   "polybar-msg cmd show")
  ("bspc config top_padding 0"
   "polybar-msg cmd hide"))

(deftoggle uncle
  ("unclutter -idle 1 &")
  ("pkill unclutter"))

(deftoggle picom
  ("setsid picom &")
  ("killall -q picom"))


;;; COMMAND LINE PROCESSING

(define ARGV (argv))

(if (= (length ARGV) 2)
    (let ((toggle (string->symbol (list-ref ARGV 1))))
      (if (member toggle *toggle*)
          (eval `(,toggle)))))
