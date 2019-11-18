(defstruct game
  size ; 8x8 or 10x10
  ; instead of using array for faster access
  ; only lists will be used for the purpose of this project
  ; square access via function "nth" (index = row * square + column)
  board ; state format ( (X/O) ; X - black, O - white, also determines who can move the stack
        ;                (list representing elements (characters X and O) that make up the stack on the current square (if it has no elements it represents an empty square)))
        ;                ;; first element on board stack eq first element in list stack
  player ; X - player with black pieces on the move (also represents the player playing first), O - player with white pieces
  first-player ; human or pc playing first (who goes first, who gets to be X)
  points-X ; player X points
  points-O ; player O points
  win-condition ; first player to reach the amount of points specified in here is a winner
)

(defconstant *win-condition* 2)

(defconstant *international-win-condition* 3)

(defconstant *stack-size* 8)

(defconstant *list-size* 3)

(defconstant *list-square-size* 2)

(defconstant *standard-size* 8)

(defconstant *international-size* 10)

; check if either player reached win condition (8 x 8 - won two stacks, 10 x 10 - won three stacks)
(defun check-if-game-over(game)
  (if (or (= (game-win-condition game) (game-points-o game)) (= (game-win-condition game) (game-points-x game))) t nil))

(defun set-win-condition (game)
  (setf (game-win-condition game) (if (= *standard-size* (game-size game)) *win-condition* *international-win-condition*)))

(defun init-game (game)
  (format t "Enter either ~a for (~a x ~a( or ~a for (~a x ~a) board size~%" *standard-size* *standard-size* *standard-size* *international-size* *international-size* *international-size*)
  (format t "Board size: ")
  (setf (game-size game) (read-size-check))
  (format t "Enter either h (human) or p (PC) to select who plays first~%")
  (format t "First to move: ")
  (setf (game-first-player game) (read-first-player-check)))

(defun read-first-player-check ()
  (let ((temp-player (read)))
      (cond ((equal 'h temp-player) temp-player)
            ((equal 'p temp-player) temp-player)
            (t (format t "Please enter either h (human) or p (PC): ") (read-first-player-check)))))

(defun read-size-check ()
  (let ((temp-size (read)))
      (cond ((equal *standard-size* temp-size) temp-size)
            ((equal *international-size* temp-size) temp-size)
            (t (format t "Please enter either ~a or ~a for size: " *standard-size* *international-size*) (read-size-check)))))

(defun game-constructor ()
  (make-game  :size *standard-size* ; default size
              :board '()
              :player 'X
              :first-player 'h
              :points-X 0
              :points-O 0
              :win-condition *win-condition*))

(defparameter *byte-game* (game-constructor)) ; defparameter assigns again after reloading file

(defun main ()
       (progn (init-game *byte-game*) (make-game-board 0 0 (game-size *byte-game*) *byte-game*)) (display-game-board (game-board *byte-game*) (game-size *byte-game*)))

;; recursively fill first row then second ...
(defun make-game-board (row column size game)
  (reverse (make-inversed-game-board row column size game)))

(defun make-inversed-game-board (row column size game)
  (cond ((= row size) (game-board game))
    ((= column size) (make-game-board (1+ row) 0 size game))
    (t (insert-square game (make-square (if (even-sum row column) 'X 'O) '())) (make-game-board row (1+ column) size game))))

;; make square
(defun make-square (black-or-white stack)
  (list black-or-white stack))

;; insert new square in current board then update
(defun insert-square (game square)
  (setf (game-board game) (cons square (game-board game))))

(defun even-sum (row column)
  (if (= 0 (mod (+ row column) 2)) t nil))

(defun display-game-board (board size)
  (format t "    ")
  (display-columns size 0)
  (format t "~% ")
  (display 0 0 0 board size 0))

(defun display-columns (size index)
  (cond ((= index size) nil)
        (t (format t "~a     " (1+ index)) (display-columns size (1+ index)))))

(defun display (row column inner-index board size repeat-count)
  (let ((matrix (display-squares-matrix 0 0 board size '())))
    (cond ((= row size) "")
    ((= column size) (format t "~%") (if (= repeat-count 0) (format t "~a" (row-index-to-char row)) (format t " ")) (display row 0 (1+ inner-index) matrix size (1+ repeat-count)))
    ((= repeat-count *list-size*)  (display (1+ row) 0 0 matrix size 0))
    (t (format t " ~{~a~^ ~}" (nth inner-index (nth (+ (* row size) column) matrix))) (display row (1+ column) inner-index matrix size repeat-count)))))

(defun display-squares-matrix (row column board size result-matrix)
  (cond ((= row size) (reverse result-matrix))
        ((= column size) (display-squares-matrix (1+ row) 0 board size result-matrix))
        (t (display-squares-matrix row (1+ column) (rest board) size (if (even-sum row column) (cons (display-square-matrix 0 0 *list-size* (nth (1- *list-square-size*) (first board))) result-matrix) (cons (get-blank-square) result-matrix))))))

(defun get-blank-square ()
  (list '(" " " " " ") '(" " " " " ") '(" " " " " ")))

;; size x size
(defun display-square-matrix (row column size stack)
  (let ((num-elements (list-length stack)))
    (display-square-matrix-helper row column size stack num-elements *stack-size* '() '())))

(defun display-square-matrix-helper (row column size stack stack-size index result-list inner-list)
  (cond ((= row size) (reverse result-list))
        ((= column size) (display-square-matrix-helper (1+ row) 0 size stack stack-size index (cons (reverse inner-list) result-list) '()))
        ((< index stack-size) (display-square-matrix-helper row (1+ column) size (rest stack) stack-size (1- index) result-list (cons (first stack) inner-list)))
        (t (display-square-matrix-helper row (1+ column) size stack stack-size (1- index) result-list (cons "." inner-list)))))


(defun char-to-row-index (char)
  (cond
    ((equal char 'A) 0)
    ((equal char 'B) 1)
    ((equal char 'C) 2)
    ((equal char 'D) 3)
    ((equal char 'E) 4)
    ((equal char 'F) 5)
    ((equal char 'G) 6)
    ((equal char 'H) 7)
    ((equal char 'I) 8)
    ((equal char 'J) 9)
    (t nil)))

(defun row-index-to-char (index)
  (cond
    ((equal index 0) 'A)
    ((equal index 1) 'B)
    ((equal index 2) 'C)
    ((equal index 3) 'D)
    ((equal index 4) 'E)
    ((equal index 5) 'F)
    ((equal index 6) 'G)
    ((equal index 7) 'H)
    ((equal index 8) 'I)
    ((equal index 9) 'J)
    (t nil)))

(defun shift-index-to-zero (index)
  (1- index))

(defun check-bounds (row column size)
  (let ((i (char-to-row-index row)) (j (shift-index-to-zero column)))
    (if (null i) nil (and (>= j 0) (< j size)))))

(defun check-index (row column size)
  (if (and (>= row 0) (< row size)) (and (>= column 0) (< column size)) nil))
