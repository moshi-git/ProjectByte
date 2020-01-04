;; Common Lisp documentation is really helpfull

;; forbidden use of car and cdr
;; use first and rest instead

(declaim (optimize (speed 3)))

(defstruct game
  size ; 8x8 or 10x10
  ; instead of using array for faster access
  ; only lists will be used for the purpose of this project
  ; square access via function "nth" (index = row * square + column)
  board ; state format ( (row, column) index
        ;                (X/O) X - black, O - white, also determines who can move the stack
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

(defconstant *standard-size* 8)

(defconstant *international-size* 10)

(defconstant *max-depth* 2)

(defconstant *min-inf* most-negative-fixnum) ;; -1152921504606846976 platform dependent

(defconstant *plus-inf* most-positive-fixnum) ;; 1152921504606846975 platform dependent

; check if either player reached win condition (8 x 8 - won two stacks, 10 x 10 - won three stacks)
(defun check-if-game-over(game)
  (if (or (= (game-win-condition game) (game-points-o game)) (= (game-win-condition game) (game-points-x game))) t nil))

(defun set-win-condition (game)
  (setf (game-win-condition game) (if (= *standard-size* (game-size game)) *win-condition* *international-win-condition*)))

(defun init-game (game)
  (format t "Enter either ~a for (~a x ~a) or ~a for (~a x ~a) board size~%" *standard-size* *standard-size* *standard-size* *international-size* *international-size* *international-size*)
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

;; call this to start the game
(defun main ()
       (init-game *byte-game*) (make-game-board 0 0 (game-size *byte-game*) *byte-game*) (display-game-board (game-board *byte-game*) (game-size *byte-game*)) (game-controller *byte-game*))

;; recursively fill first row then second ...
(defun make-game-board (row column size game)
  (setf (game-board game) (reverse (make-inversed-game-board row column size '()))))

(defun make-inversed-game-board (row column size board)
  (cond ((= row size) board)
    ((= column size) (make-inversed-game-board (1+ row) 0 size board))
    (t (make-inversed-game-board row (1+ column) size (cons (make-square row column (if (even-sum row column) 'X 'O) (if (and (> row 0) (< row (1- *stack-size*)) (even-sum row column)) (if (= 0 (mod row 2)) '(O) '(X)) '())) board)))))

;; make square
(defun make-square (row column black-or-white stack)
  (list (list row column) black-or-white stack))

(defun even-sum (row column)
  (if (= 0 (mod (+ row column) 2)) t nil))

(defun display-game-board (board size)
  (format t "    ")
  (display-columns size 0)
  (format t "~% ")
  (display board size))

(defun display-columns (size index)
  (cond ((= index size) nil)
        (t (format t "~a     " (1+ index)) (display-columns size (1+ index)))))

(defun display (board size)
 (let ((matrix (display-squares-matrix 0 0 board size '())))
  (display-helper 0 0 0 matrix size 0)))

(defun display-helper (row column inner-index matrix size repeat-count)
    (cond ((= row size) (format t "~%"))
    ((= column size) (format t "~%") (if (= repeat-count 0) (format t "~a" (row-index-to-char row)) (format t " ")) (display-helper row 0 (1+ inner-index) matrix size (1+ repeat-count)))
    ((= repeat-count *list-size*)  (display-helper (1+ row) 0 0 matrix size 0))
    (t (format t " ~{~a~^ ~}" (nth inner-index (nth (+ (* row size) column) matrix))) (display-helper row (1+ column) inner-index matrix size repeat-count))))

(defun display-squares-matrix (row column board size result-matrix)
  (cond ((= row size) (reverse result-matrix))
        ((= column size) (display-squares-matrix (1+ row) 0 board size result-matrix))
        (t (display-squares-matrix row (1+ column) (rest board) size (if (even-sum row column) (cons (display-square-matrix 0 0 *list-size* (nth (1- *list-size*) (first board))) result-matrix) (cons (get-blank-square) result-matrix))))))

(defun get-blank-square ()
  (list '(" " " " " ") '(" " " " " ") '(" " " " " ")))

;; size x size
(defun display-square-matrix (row column size stack)
    (display-square-matrix-helper row column size stack (list-length stack) 0 '() '() ))

(defun display-square-matrix-helper (row column size stack stack-size index result-list inner-list)
  (cond ((= row size) result-list)
        ((= column size) (display-square-matrix-helper (1+ row) 0 size stack stack-size index (cons inner-list result-list) '()))
        ((>= index stack-size) (display-square-matrix-helper row (1+ column) size stack stack-size (1+ index) result-list (cons "." inner-list)))
        (t (display-square-matrix-helper row (1+ column) size (rest stack) stack-size (1+ index) result-list (cons (first stack) inner-list)))))



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

(defun sort-nearest (a b)
  (< (first (rest a)) (first (rest b))))

(defun find-distance (row column size result-list dest-row dest-column board)
  (cond ((= row size) (filter-nearest (sort result-list #'sort-nearest)))
        ((= column size) (find-distance (1+ row) 0 size result-list dest-row dest-column board))
        ((or (and (= row dest-row) (= column dest-column)) (null (nth (1- *list-size*) (first board))) ) (find-distance row (1+ column) size result-list dest-row dest-column (rest board)))
        ((even-sum row column) (find-distance row (1+ column) size (cons (list (list row column) (calc-distance row column dest-row dest-column)) result-list) dest-row dest-column (rest board)))
        (t (find-distance row (1+ column) size result-list dest-row dest-column (rest board)))))

(defun calc-distance (row column dest-row dest-column)
  (max (abs (- row dest-row)) (abs (- column dest-column))))

(defun filter-nearest (elements)
  (let ((minimum (first (rest (first elements)))))
    (remove-if-not (lambda (element) (<= (first (rest element)) minimum)) elements)))

(defun check-if-move-leads-to-nearest-stack (row column nearest-stacks)
    (cond ((null nearest-stacks) '())
    (t (let ((element (first nearest-stacks)))
         (if (<= (calc-distance row column (first (first element)) (first (rest (first element)))) (first (rest element))) t (check-if-move-leads-to-nearest-stack row column (rest nearest-stacks)))))))

(defun check-stacks-move (current-stack destination-stack current-player height)
  (let ((current-stack-size (length current-stack))
        (destination-stack-size (length destination-stack)))
    (cond ((null current-stack) '())
      ((or (< height 0) (> height current-stack-size)) '())
      ((not (equal (nth height current-stack) current-player)) '())
      ((and (null destination-stack) (= height 0)) t)
      ((>= height destination-stack-size) '())
      ((> (+ (- current-stack-size height) destination-stack-size) *stack-size*) '())
      (t t))))

;; ((curr-row curr-column) (dest-row dest-column) height)
(defun parse-move (move)
  (cond ((not (listp move)) '())
    ((not (or (= (length move) *list-size*) (= (length move) (1- *list-size*)))) '())
    ((not (parse-row-column-list (first move))) '())
    ((not (parse-row-column-list (first (rest move)))) '())
    ((= (length move) *list-size*) (numberp (first (rest (rest move)))))
    (t t)))

(defun parse-row-column-list (elements)
   (if (listp elements) (if (= (length elements) (1- *list-size*)) (is-first-char-and-second-number elements) '()) '()))

(defun is-first-char-and-second-number (elements)
  (if (and (and (symbolp (first elements)) (= 1 (length (symbol-name (first elements)))))  (numberp (first (rest elements)))) t '()))

(defun read-move (player)
  (let ((move))
  (format t (if (equal player 'X) "Player X: " "Player O: "))
  (setf move (read))
  (if (parse-move move) move (progn (format t "Invalid format!~%Hint: ((curr-row curr-column) (dest-row dest-column) height) where 'height' is optional~%") (read-move player)))))

(defun check-move (row column dest-row dest-column size board player height)
  (cond ((or (null row) (null dest-row)) '())
      ((not (check-index row column size)) '())
      ((not (check-index dest-row dest-column size)) '())
      ((not (even-sum row column)) '())
      ((not (even-sum dest-row dest-column)) '())
      ((not (= (calc-distance row column dest-row dest-column) 1)) '())
      ((not (check-if-move-leads-to-nearest-stack dest-row dest-column (find-distance 0 0 size '() row column board))) '())
      ((not (check-stacks-move (get-stack-from-index row column board size) (get-stack-from-index dest-row dest-column board size) player height)) '())
      (t t)))

(defun get-stack-from-index (row column board size)
  (nth (1- *list-size*) (nth (+ (* row size) column) board)))

(defun make-a-move (game)
  (let* ((move (read-move (game-player game)))
    (row (char-to-row-index (first (first move))))
    (column (1- (first (rest (first move)))))
    (dest-row (char-to-row-index (first (first (rest move)))))
    (dest-column (1- (first (rest (first (rest move))))))
    (height (if (= (length move) *list-size*) (first (rest (rest move))) 0))
    (states (all-possible-states row column (game-size game) (game-board game) (game-player game)))
    (found-state (find-state dest-row dest-column height states)))
    (cond ((null found-state) '())
    (t (setf (game-board game) found-state) t))))

;;(defun change-state (board new-board)
;;  (setf board new-board))

;; go through possible states and return the one that corresponds to the given move
(defun find-state (row column height states)
(cond ((null states) '())
((move-equal-to-state row column height (first states)) (nth (1- *list-size*) (first states)))
(t (find-state row column height (rest states)))))

(defun move-equal-to-state (row column height state)
  (and (equal (list row column) (first state)) (= height (first (rest state)))))

(defun all-possible-states (row column size board player)
  (append (all-possible-states-from-one-square-to-another row column (1- row) (1- column) size board player 0) ; left diagonal up
  (all-possible-states-from-one-square-to-another row column (1+ row) (1- column) size board player 0) ; left diagonal down
  (all-possible-states-from-one-square-to-another row column (1- row) (1+ column) size board player 0) ; right diagonal up
  (all-possible-states-from-one-square-to-another row column (1+ row) (1+ column) size board player 0) ; right diagonal down
  ))

(defun all-possible-states-hash (row column size board player hash dist-hash)
  (append (all-possible-states-from-one-square-to-another-hash row column (1- row) (1- column) size board player 0 hash dist-hash) ; left diagonal up
  (all-possible-states-from-one-square-to-another-hash row column (1+ row) (1- column) size board player 0 hash dist-hash) ; left diagonal down
  (all-possible-states-from-one-square-to-another-hash row column (1- row) (1+ column) size board player 0 hash dist-hash) ; right diagonal up
  (all-possible-states-from-one-square-to-another-hash row column (1+ row) (1+ column) size board player 0 hash dist-hash) ; right diagonal down
  ))

;; format (((row column) height board-state) ...)
(defun all-possible-states-from-one-square-to-another (row column dest-row dest-column size board player height)
  (cond ((= height *stack-size*) '())
  ((check-move row column dest-row dest-column size board player height) (cons (list (list dest-row dest-column) height (play-move row column dest-row dest-column board size height)) (all-possible-states-from-one-square-to-another row column dest-row dest-column size board player (1+ height))))
  (t (all-possible-states-from-one-square-to-another row column dest-row dest-column size board player (1+ height)))))

(defun all-possible-states-from-one-square-to-another-hash (row column dest-row dest-column size board player height hash dist-hash)
  (cond ((= height *stack-size*) '())
  ((check-move-hash row column dest-row dest-column size board player height hash dist-hash) (cons (list (list dest-row dest-column) height (play-move-hash row column dest-row dest-column board size height hash)) (all-possible-states-from-one-square-to-another-hash row column dest-row dest-column size board player (1+ height) hash dist-hash)))
  (t (all-possible-states-from-one-square-to-another-hash row column dest-row dest-column size board player (1+ height) hash dist-hash))))

;; for now it directly changes the state of the board
;;(defun play-move (row column dest-row dest-column board size height)
;; (let ((current-stack (get-stack-from-index row column board size)) (destination-stack (get-stack-from-index dest-row dest-column board size)))
;;   (setf (nth (1- *list-size*) (nth (+ (* dest-row size) dest-column) table)) (if (null destination-stack) (cons (subseq current-stack height) destination-stack) (append destination-stack (subseq current-stack height))))
;;  (setf (nth (1- *list-size*) (nth (+ (* row size) column) table)) (delete-if (constantly t) current-stack :count (- (length current-stack) height) :from-end t))))

;; from the current state of the table return new state on the table after playing a given move
(defun play-move (row column dest-row dest-column board size height)
 (let ((current-stack (get-stack-from-index row column board size)) (destination-stack (get-stack-from-index dest-row dest-column board size)))
  (play-move-helper 0 0 size row column (remove-if (constantly t) current-stack :count (- (length current-stack) height) :from-end t) dest-row dest-column (if (null destination-stack) (append (subseq current-stack height) destination-stack) (append destination-stack (subseq current-stack height))) board)))

(defun play-move-hash (row column dest-row dest-column board size height hash)
(let ((current-stack (gethash (+ (* row size) column) hash)) (destination-stack (gethash (+ (* dest-row size) dest-column) hash)))
  (play-move-helper 0 0 size row column (remove-if (constantly t) current-stack :count (- (length current-stack) height) :from-end t) dest-row dest-column (if (null destination-stack) (append (subseq current-stack height) destination-stack) (append destination-stack (subseq current-stack height))) board)))


;;(defun play-move (row column dest-row dest-column board size height)
;; (let ((current-stack (get-stack-from-index row column board size)) (destination-stack (get-stack-from-index dest-row dest-column board size)))
;;  (setf (game-board *byte-game*)(play-move-helper 0 0 size row column (delete-if (constantly t) current-stack :count (- (length current-stack) height) :from-end t) dest-row dest-column (if (null destination-stack) (cons (subseq current-stack height) destination-stack) (append destination-stack (subseq current-stack height))) board))))

(defun play-move-helper (row column size curr-row curr-column curr-stack dest-row dest-column dest-stack board)
    (cond ((= row size) '())
          ((= column size) (play-move-helper (1+ row) 0 size curr-row curr-column curr-stack dest-row dest-column dest-stack board))
          (t (cons (if (and (= row curr-row) (= column curr-column)) (make-square row column (if (even-sum row column) 'X 'O) curr-stack) (if (and (= row dest-row) (= column dest-column)) (make-square row column (if (even-sum row column) 'X 'O) dest-stack) (first board))) (play-move-helper row (1+ column) size curr-row curr-column curr-stack dest-row dest-column dest-stack (rest board))))))

;(defun play-move (row column dest-row dest-column board size height)
;  (play-move-helper 0 0 size row column dest-row dest-column board height '() '()))


;(defun play-move-helper (row column size curr-row curr-column dest-row dest-column board height curr-stack dest-stack)
;    (cond ((= row size) '())
;          ((= column size) (play-move-helper (1+ row) 0 size curr-row curr-column dest-row dest-column board height curr-stack dest-stack))
;          ((and (= row curr-row) (= column curr-column)) (let* ((current-stack (nth (1- *list-size*) (first board))) (curr-square-stack (remove-if (constantly t) current-stack :count (- (length current-stack) height) :from-end t) )) (cons (make-square row column (if (even-sum row column) 'X 'O) curr-square-stack) (play-move-helper row (1+ column) size curr-row curr-column dest-row dest-column (rest board) height curr-square-stack dest-stack) )))
;          ((and (= row dest-row) (= column dest-column))  (let* ((destination-stack (nth (1- *list-size*) (first board))) (dest-square-stack (if (null destination-stack) (append (subseq curr-stack height) destination-stack) (append destination-stack (subseq curr-stack height)))) ) (cons (make-square row column (if (even-sum row column) 'X 'O) dest-square-stack) (play-move-helper row (1+ column) size curr-row curr-column dest-row dest-column (rest board) height curr-stack dest-square-stack) )))
;          (t (cons (first board) (play-move-helper row (1+ column) size curr-row curr-column dest-row dest-column (rest board) height curr-stack dest-stack)))))


;(defun play-move-helper (row column size curr-row curr-column curr-stack dest-row dest-column dest-stack board)
;  (let ((new-board (copy-list board)))
;    (setf (nth (+ (* curr-row size) curr-column) new-board) (make-square curr-row curr-column (if (even-sum row column) 'X 'O) curr-stack))
;    (setf (nth (+ (* dest-row size) dest-column) new-board) (make-square curr-row curr-column (if (even-sum row column) 'X 'O) dest-stack))
;      new-board))

(defun make-a-move-loop (game)
  (if (null (make-a-move game)) (progn (format t "Invalid move! Try again!~%") (make-a-move-loop game)) t))

;; will be used to controll the course of the game
(defun game-controller (game)
  ;;;;(if (equal (game-first-player game) 'h) (if (equal (game-player game) 'X) (make-a-move-loop game) (play-minmax game (game-board game) *max-depth* (game-player game) (game-size game))) (if (equal (game-player game) 'X) (play-minmax game (game-board game) *max-depth* (game-player game) (game-size game)) (make-a-move-loop game)))
  ;;(make-a-move-loop game)
  (play-minmax game (game-board game) *max-depth* (game-player game) (game-size game))
  (remove-full-stacks-and-update-score (game-size game) (game-board game) game)
  (display-game-board (game-board game) (game-size game))
  (if (equal (game-player game) 'X) (setf (game-player game) 'O) (setf (game-player game) 'X))
  (if (check-if-game-over game) (display-game-over (game-points-x game) (game-points-o game)) (game-controller game))
  )


(defun play-minmax (game state depth player size)
  (let ((new-state (minmax-best-move state depth (if (equal player 'X) 'O 'X) size)))
  (setf (game-board game) (first (rest (rest (rest (rest new-state))))) )))

;; format ((curr-row curr-column) (dest-row dest-column) height board-state eval-value)
(defun minmax-best-move (state depth player size)
  ;(minmax-best-move-helper 0 0 size state state (copy-list state) depth player '()))
 (let ((board-hash (make-board-hash state size)) (dist-hash (make-distance-hash state size)))
  (minmax-best-move-helper 0 0 size state depth player '() board-hash dist-hash)))

(defun minmax-best-move-helper (row column size state depth player best-move hash dist-hash)
    (cond ((= row size) (if (null best-move) (cons '(-1 -1) (cons '(*min-inf* *max-inf*) (cons '(-1 -1) (cons -1 (cons state '()))))) (reduce (lambda (a b) (if (> (first (rest a)) (first (rest b)) ) a b)) best-move)) )
      ((= column size) (minmax-best-move-helper (1+ row) 0 size state depth player best-move hash dist-hash))
      ;((null (nth (1- *list-size*) (first state))) (minmax-best-move-helper row (+ column 2) size (rest (rest state)) full-state depth player best-move))
      ((even-sum row column) (let ((best-move-state (best-move-from-current-square row column size state player depth hash dist-hash))) (minmax-best-move-helper row (1+ column) size state depth player (if (not (null best-move-state)) (cons best-move-state best-move) best-move) hash dist-hash)))
      (t (minmax-best-move-helper row (1+ column) size state depth player best-move hash dist-hash))))


(defun best-move-from-current-square (row column size state player depth hash dist-hash)
  (let* ((states (all-possible-states-hash row column size state player hash dist-hash)) (states-with-minmax-value (best-move-from-current-square-minmax-values states size player depth)))
    (if (null states) '() (cons (list row column) (reduce (lambda (a b) (if (> (first a) (first b) ) a b)) states-with-minmax-value)))))

(defun best-move-from-current-square-minmax-values (states size player depth)
  (cond ((null states) '())
                                                                            ;; check out the rest of the moves
    (t (cons (cons (minmax (first (rest (rest (first states)))) depth player size '() *min-inf* *plus-inf* ) (first states) ) (best-move-from-current-square-minmax-values (rest states) size player depth)))))

(defun find-full-stack (row column size board)
  (cond ((= row size) '())
  ((= column size) (find-full-stack (1+ row) 0 size board))
  (t (if (= (length (nth (1- *list-size*) (first board))) *stack-size*) (list row column) (find-full-stack row (1+ column) size (rest board))))))

(defun remove-full-stacks-and-update-score (size board game)
  (let ((stack (find-full-stack 0 0 size board)))
  (if (not (null stack)) (progn (if (equal (last (get-stack-from-index (first stack) (first (rest stack)) board size)) 'X) (setf (game-points-x game) (1+ (game-points-x game))) (setf (game-points-o game) (1+ (game-points-o game))))
  (setf (nth (1- *list-size*) (nth (+ (* (first stack) size) (first (rest stack))) board)) '())))))

(defun display-game-over (points-x points-o)
  (format t "Game over!~%Final score:~%Player X   ~a - ~a   Player O~%" points-x points-o))

;; states for all legal moves that can be played from the current state
(defun new-states (state size player)
 (let ((board-hash (make-board-hash state size)) (dist-hash (make-distance-hash state size)))
  (new-states-helper 0 0 size state '() player board-hash dist-hash)))

(defun new-states-helper (row column size state result player hash dist-hash)
    (cond ((= row size) result)
        ((= column size) (new-states-helper (1+ row) 0 size state result player hash dist-hash))
        ;((null (nth (1- *list-size*) (first state))) (new-states-helper row (+ column 2) size (rest (rest state)) result player))
        ((even-sum row column) (new-states-helper row (1+ column) size state (append (all-possible-states-minmax row column size state player hash dist-hash) result) player hash dist-hash))
        (t (new-states-helper row (1+ column) size state result player hash dist-hash))))

;; for now random, to be changed later
(defun evaluate-state (state)
  (random 100))

(defun check-move-hash (row column dest-row dest-column size board player height board-hash dist-hash)
  (cond ((or (null row) (null dest-row)) '())
      ((not (check-index row column size)) '())
      ((not (check-index dest-row dest-column size)) '())
      ((not (even-sum row column)) '())
      ((not (even-sum dest-row dest-column)) '())
      ((not (= (calc-distance row column dest-row dest-column) 1)) '())
      ((not (check-if-move-leads-to-nearest-stack dest-row dest-column (gethash (+ (* row size) column) dist-hash))) '())
      ((not (check-stacks-move (gethash (+ (* row size) column) board-hash) (gethash (+ (* dest-row size) dest-column) board-hash) player height)) '())
      (t t)))

(defun make-board-hash (board size)
  (let ((hash (make-hash-table))) (make-board-hash-helper 0 0 size board hash) ))

(defun make-board-hash-helper (row column size board hash)
  (cond ((= row size) hash)
    ((= column size) (make-board-hash-helper (1+ row) 0 size board hash))
    ((and (even-sum row column) (not (null (nth (1- *list-size*) (first board))))) (setf (gethash (+ (* row size) column) hash) (nth (1- *list-size*) (first board))) (make-board-hash-helper row (1+ column) size (rest board) hash)  )
    (t (make-board-hash-helper row (1+ column) size (rest board) hash))))

(defun make-distance-hash (board size)
    (let ((hash (make-hash-table))) (make-distance-hash-helper 0 0 size board hash)))

(defun make-distance-hash-helper (row column size board hash)
   (cond ((= row size) hash)
      ((= column size) (make-distance-hash-helper (1+ row) 0 size board hash))
      ((and (even-sum row column) (not (null (nth (1- *list-size*) (nth (+ (* row  size) column) board))))) (setf (gethash (+ (* row size) column) hash) (find-distance 0 0 size '() row column board)) (make-distance-hash-helper row (1+ column) size board hash)  )
      (t (make-distance-hash-helper row (1+ column) size board hash))))

;; format ((board-state) ...)
;; same function as all-possible-states but different format for the result list
(defun all-possible-states-minmax (row column size board player hash dist-hash)

    (append (all-possible-states-from-one-square-to-another-minmax row column (1- row) (1- column) size board player 0 hash dist-hash) ; left diagonal up
    (all-possible-states-from-one-square-to-another-minmax row column (1+ row) (1- column) size board player 0 hash dist-hash) ; left diagonal down
    (all-possible-states-from-one-square-to-another-minmax row column (1- row) (1+ column) size board player 0 hash dist-hash) ; right diagonal up
    (all-possible-states-from-one-square-to-another-minmax row column (1+ row) (1+ column) size board player 0 hash dist-hash) ; right diagonal down
    ))

;; format ((board-state) ...)
;; same function as all-possible-states-from-one-square-to-another but different format for the result list
(defun all-possible-states-from-one-square-to-another-minmax (row column dest-row dest-column size board player height board-hash dist-hash)
    (cond ((= height *stack-size*) '())
    ((check-move-hash row column dest-row dest-column size board player height board-hash dist-hash) (cons (play-move-hash row column dest-row dest-column board size height board-hash) (all-possible-states-from-one-square-to-another-minmax row column dest-row dest-column size board player (1+ height) board-hash dist-hash)))
    (t (all-possible-states-from-one-square-to-another-minmax row column dest-row dest-column size board player (1+ height) board-hash dist-hash))))

;; state list format - ((eval-value) ...)
;; move - t for pc, move - nil for h
(defun minmax (state depth player size move alpha beta)
   (let ((state-list (new-states state size player)) )
    (cond ((or (= 0 depth) (null state-list)) (evaluate-state state))
      (t (if move (maximizing-node (sort-max state-list) depth (if (equal player 'X) 'O 'X) size (not move) alpha beta) (minimizing-node (sort-min state-list) depth (if (equal player 'X) 'O 'X) size (not move) alpha beta)) )
      )))

(defun sort-max (states)
  (sort states (lambda (a b) (> (evaluate-state a) (evaluate-state b)) )) )

(defun sort-min (states)
  (sort states (lambda (a b) (< (evaluate-state a) (evaluate-state b)) )))

(defun maximizing-node (states depth player size move alpha beta)
 (maximizing-node-helper states depth player size move alpha beta *min-inf*))

(defun maximizing-node-helper (states depth player size move alpha beta max-eval)
  (cond ((null states) max-eval)
    (t (let* ((minmax-value (minmax (first states) (1- depth) player size move alpha beta)) (max-value (max max-eval minmax-value)) (alpha-value (max alpha minmax-value))) (if (<= beta alpha-value) max-value (maximizing-node-helper (rest states) depth player size move alpha-value beta max-value)))
    )))

(defun minimizing-node (states depth player size move alpha beta)
    (minimizing-node-helper states depth player size move alpha beta *plus-inf*))

(defun minimizing-node-helper (states depth player size move alpha beta min-eval)
    (cond ((null states) min-eval)
      (t (let* ((minmax-value (minmax (first states) (1- depth) player size move alpha beta)) (min-value (min min-eval minmax-value)) (beta-value (min beta minmax-value))) (if (<= beta-value alpha) min-value (minimizing-node-helper (rest states) depth player size move alpha beta-value min-value)))
      )))
