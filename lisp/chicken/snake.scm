
(import utf8)
(import ncurses)
(import (srfi 18))

(define snake-graphic '█)
(define snake)
(define prey '(🪱 🥚 🦎 🐀 🐟 🐦 🐍))
(define elapsed-time 0)

(define time-keeper
  (make-thread
   (lambda ()
     (let loop ()
       (sleep 1)
       (newline)
       (set! elapsed-time (+ elapsed-time 1))
       (loop)))))

;;; initialization
(thread-start! time-keeper)

;; ncurses
(initscr)
(raw)
(noecho)
(curs_set 0)


;;; main event loop
(define exit? #f)

(let loop ((event (getch)))
  (case event
    ((#\h))
    ((#\j))
    ((#\k))
    ((#\l))

    ((#\q) (set! exit? #t)))

  (unless exit?
    (loop (getch))))

;;; cleanup
(endwin)
(thread-terminate! time-keeper)
