
(use-modules
 (common-lisp)
 (goodies)
 (ncurses curses)
 (macro-help)
 (ice-9 match))

(define score 0)
(define game-paused? #f)
(define snake)
(define snake-head)
(define apples '())
(define direction 'north)
(define stdscr)

(define (grow-apple)
  (define (rand thunk)
    (compose random 1- thunk))
  (define randx (rand cols))
  (define randy (rand lines))
  (set! apples
    (reverse-args cons apples
      (let loop ((y (randy)) (x (randx)))
	(if (member `(,y ,x) snake)
	    (loop (randy) (randx)) `(,y ,x))))))

(define-macro (define-display global-ref char)
  `(define (,(symbolicate 'display- global-ref))
     (dolist (z ,global-ref)
       (let ((y (car z))
	     (x (cadr z)))
	 (move stdscr y x)
	 (addch stdscr (normal ,char))))))

(define-display snake #\*)
(define-display apples #\@)

(define (display-score)
  (let ((score (number->string score)))
    (addstr stdscr score
      #:y (- (lines) 2)
      #:x (- (cols) (string-length score) 2))))

(define (snake-head-move head new-direction)
  (set! direction new-direction)
  (match-let (((y x) head))
    (case new-direction
      ((north) `(,(if (< (1- y) 0) (getmaxy stdscr) (1- y)) ,x))
      ((south) `(,(if (> (1+ y) (getmaxy stdscr)) 0 (1+ y)) ,x))
      ((west) `(,y ,(if (< (1- x) 0) (getmaxx stdscr) (1- x))))
      ((east) `(,y ,(if (> (1+ x) (getmaxx stdscr)) 0 (1+ x)))))))

(define (snake-move! new-head)
  (set-cdr! snake-head (list new-head))
  (set! snake-head (cdr snake-head))
  (set! snake (cdr snake)))

(define (snake-ate-self?)
  (not (eq? (member (car snake-head) snake) snake-head)))

(define stdscr (initscr))
(keypad! stdscr #t)
(halfdelay! 1)
(noecho!)
(define snake
  `((,(floor/ (lines) 2)
     ,(floor/ (cols) 2))))
(define snake-head snake)
(sow-seed!)
(for-each call (replicate (+ (random 10) 5) grow-apple))

(let loop ((event #f))
  (clear stdscr)
  (display-snake)
  (display-apples)
  (display-score)
  (refresh stdscr)

  (when (eqv? event #\space)
    (if game-paused?
	(begin (set! game-paused? #f) (halfdelay! 1))
	(begin (set! game-paused? #t) (raw!))))

  (when (member (car snake-head) apples)
    (set! apples (delete (car snake-head) apples))
    (grow-apple)
    (set! score (1+ score))
    (set! snake (cons (car snake) snake)))

  (unless game-paused?
    (snake-move!
     (snake-head-move
      (car snake-head)
      (case event
	((#\h 260) 'west)
	((#\j 258) 'south)
	((#\k 259) 'north)
	((#\l 261) 'east)
	(else direction)))))

  (unless (or (memv event '(#\esc #\q)) (snake-ate-self?))
      (loop (getch stdscr))))

(halfdelay! 7)
(getch stdscr)
(addstr stdscr  "Game "
  #:y (floor/ (lines) 2) #:x (floor/ (cols) 2))
(getch stdscr)
(addstr stdscr "Over")
(raw!)
(getch stdscr)
(endwin)

(format #t "You ate ~A apples.~%" score)
