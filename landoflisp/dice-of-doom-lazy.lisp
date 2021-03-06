(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 3)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; converts a board grid into an array
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

;; randomly generates a board by randomly putting a player in each hex
;; and randomly adding a number of dice, from 1 to max dice
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

;; give character for player instead of number
(defun player-letter (n)
  (code-char (+ 97 n)))

(defun draw-board (board)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ "  "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                   (second hex))))))

;; returns a list having structure:
;; player, board, (move1 move2 move 3)
;; passing move, if possible, is first. nil indicates passing move
(defun game-tree (board player spare-dice first-move)
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp)))
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
        (setf (gethash rest previous) (apply old-game-tree rest)))))

;; passing move is a new game tree node with:
;; * a board where spare dice have been distributed
;; * it's the next player's turn
;; * there are 0 spare dice
;; * it's the first move
;;
;; returns: (passing-move moves)
;; where moves is a list of attacking moves
;; contrary to name, does not return solely passing moves
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
    (lazy-cons (list nil
                (game-tree (add-new-dice board player (1- spare-dice))
                           (mod (1+ player) *num-players*)
                           0
                           t))
          moves)))

;; spare-dice accumulates with every attack
;; go through each hex. if the hex player is the current player,
;; determine whether the player can attack. if the player can attack,
;; add the attack to the list of moves. move takes the form of
;; ((src-hex, destination-hex) game-tree)
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
                   (car (aref board pos)))
           (dice (pos)
                 (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
           (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst) cur-player))
                       (> (dice src) (dice dst)))
                  (make-lazy
                   (list (list (list src dst)
                               (game-tree (board-attack board
                                                        cur-player
                                                        src
                                                        dst
                                                        (dice src))
                                          cur-player
                                          (+ spare-dice (dice dst))
                                          nil))))
                (lazy-nil)))
            (make-lazy (neighbors src)))
         (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum*
                    collect n)))))

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
         collect p )))

(let ((old-neighbors (symbol-function 'neighbors))
      (previous (make-hash-table)))
  (defun neighbors (pos)
    (or (gethash pos previous)
        (setf (gethash pos previous) (funcall old-neighbors pos)))))

;; generates board resulting from player attacking from src to dst
;; with given number of dice
(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))


;; non tail-call-optimized
;;(defun add-new-dice (board player spare-dice)
;;  (labels ((f (lst n)
;;              (cond ((null lst) nil)
;;                    ((zerop n) lst)
;;                    (t (let ((cur-player (caar lst))
;;                             (cur-dice (cadar lst)))
;;                         (if (and (eq cur-player player) (< cur-dice *max-dice*))
;;                                  (cons (list cur-player (1+ cur-dice))
;;                                        (f (cdr lst) (1- n)))
;;                           (cons (car lst) (f (cdr lst) n))))))))
;;    (board-array (f (coerce board 'list) spare-dice))))

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)
              (cond ((zerop n) (append (reverse acc) lst))
                    ((null lst) (reverse acc))
                    (t (let ((cur-player (caar lst))
                             (cur-dice (cadar lst)))
                         (if (and (eq cur-player player)
                                  (< cur-dice *max-dice*))
                             (f (cdr lst)
                                (1- n)
                                (cons (list cur-player (1+ cur-dice)) acc))
                           (f (cdr lst) n (cons (car lst) acc))))))))
    (board-array (f (coerce board 'list) spare-dice ()))))

;; continues until you reach a tree with no moves.
(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
    (announce-winner (cadr tree))))

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

;; returns new tree when given a move
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
                          (unless (lazy-null moves)
                            (let* ((move (lazy-car moves))
                                   (action (car move)))
                              (fresh-line)
                              (format t "~a. " n)
                              (if action
                                  (format t "~a -> ~a" (car action) (cadr action))
                                (princ "end turn")))
                            (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun winners (board)
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
      (format t "the winner is ~a" (player-letter (car w))))))



(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
                           (rate-position (cadr move) player))
                         (caddr tree))))

(defun score-board (board player)
  (loop for hex across board
        for pos from 0
        sum (if (eq (car hex) player)
                (if (threatened pos board)
                    1
                  2)
              -1)))

(defun threatened (pos board)
  (let* ((hex (aref board pos))
         (player (car hex))
         (dice (cadr hex)))
    (loop for n in (neighbors pos)
          do (let* ((nhex (aref board n))
                    (nplayer (car nhex))
                    (ndice (cadr nhex)))
               (when (and (not (eq player nplayer)) (> ndice dice))
                 (return t))))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
        (cadr tree)
        (if (zerop depth)
            (lazy-nil)
          (lazy-mapcar (lambda (move)
                         (list (car move)
                               (limit-tree-depth (cadr move) (1- depth))))
                       (caddr tree)))))

(defparameter *ai-level* 4)

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))))
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree)))
        ((zerop (car tree)) (play-vs-computer (handle-human tree)))
        (t (play-vs-computer (handle-computer tree)))))

