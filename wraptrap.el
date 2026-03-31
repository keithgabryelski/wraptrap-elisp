;;; wraptrap.el --- Strategic WrapTrap game: Human vs AI -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Keith Gabryelski
;; Version: 1.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: games

;;; Commentary:

;; WrapTrap - A strategic snake game where you play against an intelligent AI.
;; Both snakes grow continuously, and the goal is to trap your opponent.
;; Features wrap-around edges, visible borders, and multiple difficulty modes.
;; The AI uses pathfinding, movement prediction, and path blocking strategies.
;;
;; Usage:
;;   M-x wraptrap
;;
;; Controls:
;;   SPACE - Start game from splash
;;   O - Options menu
;;   Arrow keys or WASD to move
;;   p - pause, q - quit, r - restart

;;; Code:

(require 'gamegrid)
(require 'cl-lib)

;;; Game Constants

(defconst wraptrap-tick-period 0.1
  "Time in seconds between game updates.")

;;; Game Options

(defvar wraptrap-grid-width 30
  "Width of the game grid.")

(defvar wraptrap-grid-height 30
  "Height of the game grid.")

(defvar wraptrap-use-full-width nil
  "Non-nil to use full buffer width, nil to use fixed grid size.")

(defvar wraptrap-wrap-edges t
  "Non-nil to wrap around edges, nil to crash on edges.")

(defvar wraptrap-growth-mode 'periodic
  "Growth mode: \\='periodic (every N frames) or \\='continuous (every frame).")

(defvar wraptrap-growth-rate 3
  "When growth-mode is \\='periodic, worms grow every N frames.")

(defvar wraptrap-two-player-mode nil
  "Non-nil for 2-player mode (WASD vs Arrows), nil for Human vs AI.")

(defvar wraptrap-border-thickness 2
  "Thickness of borders in cells.")

(defun wraptrap-playable-x-min ()
  "Minimum X coordinate for playable area."
  wraptrap-border-thickness)

(defun wraptrap-playable-x-max ()
  "Maximum X coordinate for playable area (inclusive)."
  (- wraptrap-grid-width wraptrap-border-thickness 1))

(defun wraptrap-playable-y-min ()
  "Minimum Y coordinate for playable area."
  wraptrap-border-thickness)

(defun wraptrap-playable-y-max ()
  "Maximum Y coordinate for playable area (inclusive)."
  (- wraptrap-grid-height wraptrap-border-thickness 1))

(defun wraptrap-playable-width ()
  "Width of playable area."
  (- wraptrap-grid-width (* 2 wraptrap-border-thickness)))

(defun wraptrap-playable-height ()
  "Height of playable area."
  (- wraptrap-grid-height (* 2 wraptrap-border-thickness)))

;;; Cell Types

(defconst wraptrap-blank 0)
(defconst wraptrap-human-body 1)
(defconst wraptrap-human-head 2)
(defconst wraptrap-ai-body 3)
(defconst wraptrap-ai-head 4)
(defconst wraptrap-border 5)

;;; Colors and Display Options

(defvar wraptrap-use-color t
  "Non-nil to use color, nil to use characters only.")

(defvar wraptrap-use-glyphs t
  "Non-nil to use graphical glyphs.")

(defconst wraptrap-blank-options
  '(((glyph colorize)
     (t ?\s))
    ((color-x color-x)
     (mono-x grid-x)
     (color-tty color-tty))
    (((glyph color-x) [0 0 0])
     (color-tty "black"))))

(defconst wraptrap-human-body-options
  '(((glyph colorize)
     (emacs-tty ?o)
     (t ?\s))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [0 255 0])
     (color-tty "green"))))

(defconst wraptrap-human-head-options
  '(((glyph colorize)
     (emacs-tty ?O)
     (t ?\s))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [144 238 144])
     (color-tty "green"))))

(defconst wraptrap-ai-body-options
  '(((glyph colorize)
     (emacs-tty ?x)
     (t ?\s))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [255 0 0])
     (color-tty "red"))))

(defconst wraptrap-ai-head-options
  '(((glyph colorize)
     (emacs-tty ?X)
     (t ?\s))
    ((color-x color-x)
     (mono-x mono-x)
     (color-tty color-tty)
     (mono-tty mono-tty))
    (((glyph color-x) [139 0 0])
     (color-tty "red"))))

;;; Game State

(defvar wraptrap-buffer-name "*WrapTrap*"
  "Name of the worms buffer.")

(defvar wraptrap-game-timer nil
  "Timer object for game updates.")

(defvar wraptrap-paused nil
  "Non-nil when game is paused.")

(defvar wraptrap-game-over nil
  "Non-nil when game is over.")

(defvar wraptrap-winner nil
  "Winner of the game: \\='human, \\='ai, or \\='draw.")

(defvar wraptrap-game-state 'splash
  "Current game state: \\='splash, \\='options, \\='playing, or \\='game-over.")

(defvar wraptrap-options-selection 0
  "Current selection in options menu.")

(defvar wraptrap-frame-count 0
  "Current frame number.")

;; Human worm state
(defvar wraptrap-human-body nil
  "List of (x . y) positions for human worm body, head first.")

(defvar wraptrap-human-direction nil
  "Current direction of human worm: \\='up, \\='down, \\='left, or \\='right.")

(defvar wraptrap-human-alive t
  "Non-nil if human worm is alive.")

;; AI worm state
(defvar wraptrap-ai-body nil
  "List of (x . y) positions for AI worm body, head first.")

(defvar wraptrap-ai-direction nil
  "Current direction of AI worm: \\='up, \\='down, \\='left, or \\='right.")

(defvar wraptrap-ai-alive t
  "Non-nil if AI worm is alive.")

;; AI learning
(defvar wraptrap-ai-move-history nil
  "History of human moves for AI pattern recognition.")

;;; Direction Functions

(defun wraptrap-direction-delta (dir)
  "Return (dx . dy) for direction DIR."
  (pcase dir
    ('up    (cons 0 -1))
    ('down  (cons 0 1))
    ('left  (cons -1 0))
    ('right (cons 1 0))))

(defun wraptrap-opposite-direction (dir)
  "Return the opposite of direction DIR."
  (pcase dir
    ('up 'down)
    ('down 'up)
    ('left 'right)
    ('right 'left)))

(defun wraptrap-all-directions ()
  "Return list of all valid directions."
  '(up down left right))

;;; Position Functions

(defun wraptrap-pos-add (pos delta)
  "Add DELTA to position POS."
  (cons (+ (car pos) (car delta))
        (+ (cdr pos) (cdr delta))))

(defun wraptrap-pos-in-bounds-p (pos)
  "Return t if POS is within playable area bounds."
  (and (>= (car pos) (wraptrap-playable-x-min))
       (<= (car pos) (wraptrap-playable-x-max))
       (>= (cdr pos) (wraptrap-playable-y-min))
       (<= (cdr pos) (wraptrap-playable-y-max))))

(defun wraptrap-manhattan-distance (pos1 pos2)
  "Calculate Manhattan distance between POS1 and POS2."
  (+ (abs (- (car pos1) (car pos2)))
     (abs (- (cdr pos1) (cdr pos2)))))

;;; Grid Functions

(defun wraptrap-init-buffer ()
  "Initialize the gamegrid buffer."
  (gamegrid-init-buffer wraptrap-grid-width
                        wraptrap-grid-height
                        ?\s))

(defun wraptrap-get-cell-char (cell-type)
  "Return the character for CELL-TYPE."
  (cond
   ((eq cell-type wraptrap-blank) ?\s)
   ((eq cell-type wraptrap-human-body) ?o)
   ((eq cell-type wraptrap-human-head) ?O)
   ((eq cell-type wraptrap-ai-body) ?x)
   ((eq cell-type wraptrap-ai-head) ?X)
   ((eq cell-type wraptrap-border) ?#)
   (t ?\s)))

(defun wraptrap-get-cell-face (cell-type)
  "Return the face for CELL-TYPE."
  (cond
   ((eq cell-type wraptrap-blank) 'wraptrap-blank-face)
   ((eq cell-type wraptrap-human-body) 'wraptrap-human-body-face)
   ((eq cell-type wraptrap-human-head) 'wraptrap-human-head-face)
   ((eq cell-type wraptrap-ai-body) 'wraptrap-ai-body-face)
   ((eq cell-type wraptrap-ai-head) 'wraptrap-ai-head-face)
   ((eq cell-type wraptrap-border) 'wraptrap-border-face)
   (t 'wraptrap-blank-face)))

(defun wraptrap-put-cell (x y cell-type)
  "Put CELL-TYPE at position X Y in the grid."
  (let ((char (wraptrap-get-cell-char cell-type))
        (face (wraptrap-get-cell-face cell-type))
        (buffer-read-only nil))
    (goto-char (point-min))
    (forward-line y)
    (move-to-column x)
    (delete-char 1)
    (insert (propertize (string char) 'face face))))

(defun wraptrap-draw-grid ()
  "Draw the entire game grid."
  (let ((buffer-read-only nil))
    ;; Clear grid
    (dotimes (y wraptrap-grid-height)
      (dotimes (x wraptrap-grid-width)
        (wraptrap-put-cell x y wraptrap-blank)))

    ;; Draw human worm
    (when wraptrap-human-body
      (let ((head (car wraptrap-human-body))
            (body (cdr wraptrap-human-body)))
        (wraptrap-put-cell (car head) (cdr head) wraptrap-human-head)
        (dolist (pos body)
          (wraptrap-put-cell (car pos) (cdr pos) wraptrap-human-body))))

    ;; Draw AI worm
    (when wraptrap-ai-body
      (let ((head (car wraptrap-ai-body))
            (body (cdr wraptrap-ai-body)))
        (wraptrap-put-cell (car head) (cdr head) wraptrap-ai-head)
        (dolist (pos body)
          (wraptrap-put-cell (car pos) (cdr pos) wraptrap-ai-body))))))

(defun wraptrap-draw-borders ()
  "Draw double-thick borders around the play field for clear visibility."
  (let ((buffer-read-only nil))
    ;; Outer border (edge)
    (dotimes (x wraptrap-grid-width)
      (wraptrap-put-cell x 0 wraptrap-border)
      (wraptrap-put-cell x (1- wraptrap-grid-height) wraptrap-border))
    (dotimes (y wraptrap-grid-height)
      (wraptrap-put-cell 0 y wraptrap-border)
      (wraptrap-put-cell (1- wraptrap-grid-width) y wraptrap-border))
    ;; Inner border (one cell in) for double-thickness
    (when (> wraptrap-grid-width 4)
      (dotimes (x wraptrap-grid-width)
        (when (and (> x 0) (< x (1- wraptrap-grid-width)))
          (wraptrap-put-cell x 1 wraptrap-border)
          (wraptrap-put-cell x (- wraptrap-grid-height 2) wraptrap-border)))
      (dotimes (y wraptrap-grid-height)
        (when (and (> y 0) (< y (1- wraptrap-grid-height)))
          (wraptrap-put-cell 1 y wraptrap-border)
          (wraptrap-put-cell (- wraptrap-grid-width 2) y wraptrap-border))))))

(defun wraptrap-draw-info ()
  "Draw game information in the header line."
  (let* ((player1-label (if wraptrap-two-player-mode "Player 1" "Human"))
         (player2-label (if wraptrap-two-player-mode "Player 2" "AI"))
         (info (format "%s: %d | %s: %d | Frame: %d"
                       player1-label
                       (length wraptrap-human-body)
                       player2-label
                       (length wraptrap-ai-body)
                       wraptrap-frame-count)))
    (setq header-line-format info)))

(defun wraptrap-display-splash ()
  "Display splash screen."
  (let ((buffer-read-only nil))
    (erase-buffer)
    (insert "\n\n")

    ;; ASCII Art - Two snakes with big heads wrapping and attacking
    (insert "        ╔════════════════════════╗\n")
    (insert "        ║   W R A P T R A P      ║\n")
    (insert "        ╚════════════════════════╝\n")
    (insert "\n")
    (insert (propertize "           ╔═══╗" 'face '(:foreground "green")) "          "
            (propertize "╔═══╗\n" 'face '(:foreground "red")))
    (insert (propertize "          ╔╝ ◉ ╚╗" 'face '(:foreground "green")) "        "
            (propertize "╔╝ ◉ ╚╗\n" 'face '(:foreground "red")))
    (insert (propertize "         ╔╝  ▼  ╚╗" 'face '(:foreground "green")) "      "
            (propertize "╔╝  ▼  ╚╗\n" 'face '(:foreground "red")))
    (insert (propertize "         ║       ║" 'face '(:foreground "green")) "      "
            (propertize "║       ║\n" 'face '(:foreground "red")))
    (insert (propertize "         ╚╗     ╔╝" 'face '(:foreground "green")) "      "
            (propertize "╚╗     ╔╝\n" 'face '(:foreground "red")))
    (insert (propertize "          ║     ║" 'face '(:foreground "green")) "        "
            (propertize "║     ║\n" 'face '(:foreground "red")))
    (insert (propertize "          ╚╗   ╔╝" 'face '(:foreground "green")) "        "
            (propertize "╚╗   ╔╝\n" 'face '(:foreground "red")))
    (insert (propertize "           ╚╗ ╔╝" 'face '(:foreground "green")) "          "
            (propertize "╚╗ ╔╝\n" 'face '(:foreground "red")))
    (insert (propertize "            ╚═╝" 'face '(:foreground "green")) "            "
            (propertize "╚═╝\n" 'face '(:foreground "red")))
    (insert "\n")
    (insert (propertize "          [GREEN]" 'face '(:foreground "green")) "        "
            (propertize "[RED]\n" 'face '(:foreground "red")))

    (insert "\n")
    (insert "    Trap your opponent before they trap you!\n")
    (insert "\n\n")
    (insert "    Controls:\n")
    (insert "      Arrow keys or WASD - Move\n")
    (insert "      p - Pause/Resume\n")
    (insert "      q - Quit\n\n")
    (insert "    Press SPACE to start\n")
    (insert "    Press O for options")
    (goto-char (point-min))))

(defun wraptrap-display-options ()
  "Display options menu."
  (let ((buffer-read-only nil)
        (options '(("Grid Size" . (("Fixed 30x30" . nil) ("Full Width" . t)))
                   ("Edge Behavior" . (("Crash" . nil) ("Wrap Around" . t)))
                   ("Growth Mode" . (("Periodic" . periodic) ("Continuous" . continuous)))
                   ("Players" . (("Human vs AI" . nil) ("2-Player" . t))))))
    (erase-buffer)
    (insert "\n")
    (insert "OPTIONS\n")
    (insert "\n")

    (let ((idx 0))
      (dolist (option options)
        (let* ((name (car option))
               (choices (cdr option))
               (selected (= idx wraptrap-options-selection))
               (current-value (cond
                              ((string= name "Grid Size") wraptrap-use-full-width)
                              ((string= name "Edge Behavior") wraptrap-wrap-edges)
                              ((string= name "Growth Mode") wraptrap-growth-mode)
                              ((string= name "Players") wraptrap-two-player-mode))))

          (insert (if selected "> " "  "))
          (insert name)
          (insert ": ")

          (dolist (choice choices)
            (let* ((choice-name (car choice))
                   (choice-value (cdr choice))
                   (is-current (equal current-value choice-value)))
              (insert (if is-current
                         (format "[%s] " choice-name)
                       (format " %s  " choice-name)))))
          (insert "\n")
          (setq idx (1+ idx)))))

    (insert "\n\n")
    (insert "↑/↓ to select, ←/→ to change, SPACE to start")
    (goto-char (point-min))))

(defun wraptrap-display-game-over ()
  "Display game over message."
  (let* ((winner-text (pcase wraptrap-winner
                        ('human (if wraptrap-two-player-mode "PLAYER 1 WINS!" "YOU WIN!"))
                        ('ai (if wraptrap-two-player-mode "PLAYER 2 WINS!" "AI WINS!"))
                        ('draw "DRAW!")
                        (_ "GAME OVER")))
         (player1-label (if wraptrap-two-player-mode "Player 1" "Human"))
         (player2-label (if wraptrap-two-player-mode "Player 2" "AI"))
         (stats (format "%s: %d vs %s: %d"
                       player1-label
                       (length wraptrap-human-body)
                       player2-label
                       (length wraptrap-ai-body)))
         (message-text (format "\n\n%s\n\n%s\n\nPress 'r' to restart, 'q' to quit"
                              winner-text stats)))
    (message "%s" message-text)))

;;; Worm Movement

(defun wraptrap-wrap-position (pos)
  "Wrap POS around playable area if wrap-edges is enabled."
  (if wraptrap-wrap-edges
      (let* ((x (car pos))
             (y (cdr pos))
             (play-width (wraptrap-playable-width))
             (play-height (wraptrap-playable-height))
             (x-min (wraptrap-playable-x-min))
             (y-min (wraptrap-playable-y-min))
             ;; Wrap within playable area
             (wrapped-x (+ x-min (mod (- x x-min) play-width)))
             (wrapped-y (+ y-min (mod (- y y-min) play-height))))
        (cons wrapped-x wrapped-y))
    pos))

(defun wraptrap-move-worm (body direction grow)
  "Move worm BODY in DIRECTION. If GROW is non-nil, don't remove tail."
  (let* ((head (car body))
         (delta (wraptrap-direction-delta direction))
         (new-head-raw (wraptrap-pos-add head delta))
         (new-head (wraptrap-wrap-position new-head-raw)))
    (if grow
        (cons new-head body)
      (cons new-head (butlast body)))))

(defun wraptrap-change-direction (current new)
  "Change from CURRENT direction to NEW if valid."
  (if (eq new (wraptrap-opposite-direction current))
      current
    new))

;;; Collision Detection

(defun wraptrap-check-collision (pos body1 body2)
  "Check if POS collides with walls or bodies."
  (or (not (wraptrap-pos-in-bounds-p pos))
      (member pos body1)
      (member pos body2)))

(defun wraptrap-check-collisions ()
  "Check all collisions and update game state."
  (let ((human-head (car wraptrap-human-body))
        (ai-head (car wraptrap-ai-body))
        (human-crashed nil)
        (ai-crashed nil))

    ;; Check human collisions
    (when wraptrap-human-alive
      (setq human-crashed
            (or (and (not wraptrap-wrap-edges)
                     (not (wraptrap-pos-in-bounds-p human-head)))
                (member human-head (cdr wraptrap-human-body))
                (member human-head wraptrap-ai-body))))

    ;; Check AI collisions
    (when wraptrap-ai-alive
      (setq ai-crashed
            (or (and (not wraptrap-wrap-edges)
                     (not (wraptrap-pos-in-bounds-p ai-head)))
                (member ai-head (cdr wraptrap-ai-body))
                (member ai-head wraptrap-human-body))))

    ;; Head-on collision
    (when (equal human-head ai-head)
      (setq human-crashed t
            ai-crashed t))

    ;; Update game state
    (cond
     ((and human-crashed ai-crashed)
      (setq wraptrap-game-over t
            wraptrap-winner 'draw
            wraptrap-human-alive nil
            wraptrap-ai-alive nil))
     (human-crashed
      (setq wraptrap-game-over t
            wraptrap-winner 'ai
            wraptrap-human-alive nil))
     (ai-crashed
      (setq wraptrap-game-over t
            wraptrap-winner 'human
            wraptrap-ai-alive nil)))))

;;; AI Implementation

(defun wraptrap-ai-flood-fill (start max-depth)
  "Count reachable cells from START using flood fill, up to MAX-DEPTH."
  (let ((visited (make-hash-table :test 'equal))
        (queue (list start))
        (count 0))
    (puthash start t visited)
    (while (and queue (< count max-depth))
      (let* ((pos (pop queue)))
        (setq count (1+ count))
        (dolist (dir (wraptrap-all-directions))
          (let* ((delta (wraptrap-direction-delta dir))
                 (new-pos (wraptrap-pos-add pos delta)))
            (when (and (not (gethash new-pos visited))
                      (wraptrap-pos-in-bounds-p new-pos)
                      (not (member new-pos wraptrap-human-body))
                      (not (member new-pos wraptrap-ai-body)))
              (puthash new-pos t visited)
              (push new-pos queue))))))
    count))

(defun wraptrap-ai-evaluate-space (pos)
  "Evaluate available space from POS."
  (if (or (not (wraptrap-pos-in-bounds-p pos))
          (member pos wraptrap-human-body)
          (member pos wraptrap-ai-body))
      0
    (wraptrap-ai-flood-fill pos 150)))

(defun wraptrap-ai-evaluate-blocking (pos)
  "Evaluate how well POS blocks the human's paths."
  (let* ((human-head (car wraptrap-human-body))
         (human-space (wraptrap-ai-evaluate-space human-head))
         (distance (wraptrap-manhattan-distance pos human-head)))
    (cond
     ((< human-space 50) 20.0)
     ((< human-space 100) 10.0)
     ((and (>= distance 3) (<= distance 8)) 15.0)
     (t 0.0))))

(defun wraptrap-ai-predict-human-move (pos)
  "Predict human movement and score intercept position POS."
  (if (< (length wraptrap-ai-move-history) 3)
      0.0
    (let* ((recent (last wraptrap-ai-move-history 5))
           (human-head (car wraptrap-human-body))
           (first-dir (car recent)))
      ;; Check if moving straight (all recent moves are the same)
      (if (and first-dir (cl-every (lambda (d) (eq d first-dir)) recent))
          (let* ((dir (car recent))
                 (delta (wraptrap-direction-delta dir))
                 (predicted (wraptrap-pos-add human-head
                                          (cons (* 3 (car delta))
                                               (* 3 (cdr delta))))))
            (if (and (wraptrap-pos-in-bounds-p predicted)
                    (< (wraptrap-manhattan-distance pos predicted) 5))
                10.0
              0.0))
        0.0))))

(defun wraptrap-ai-evaluate-move (direction)
  "Evaluate a move in DIRECTION and return a score."
  (let* ((head (car wraptrap-ai-body))
         (delta (wraptrap-direction-delta direction))
         (new-pos (wraptrap-pos-add head delta))
         (score 0.0))

    ;; 1. Available space (most important)
    (let ((space-score (wraptrap-ai-evaluate-space new-pos)))
      (setq score (+ score (* space-score 10.0))))

    ;; 2. Distance to human (aggressive positioning)
    (let* ((human-head (car wraptrap-human-body))
           (distance (wraptrap-manhattan-distance new-pos human-head)))
      (cond
       ((and (>= distance 5) (<= distance 10))
        (setq score (+ score 20.0)))
       ((< distance 5)
        (setq score (+ score 10.0)))))

    ;; 3. Control center (center of playable area)
    (let* ((center-x (+ (wraptrap-playable-x-min) (/ (wraptrap-playable-width) 2)))
           (center-y (+ (wraptrap-playable-y-min) (/ (wraptrap-playable-height) 2)))
           (center (cons center-x center-y))
           (center-dist (wraptrap-manhattan-distance new-pos center)))
      (setq score (+ score (* (- (wraptrap-playable-width) center-dist) 0.5))))

    ;; 4. Blocking potential
    (setq score (+ score (* (wraptrap-ai-evaluate-blocking new-pos) 15.0)))

    ;; 5. Avoid edges of playable area
    (when (or (<= (car new-pos) (+ (wraptrap-playable-x-min) 1))
              (>= (car new-pos) (- (wraptrap-playable-x-max) 1))
              (<= (cdr new-pos) (+ (wraptrap-playable-y-min) 1))
              (>= (cdr new-pos) (- (wraptrap-playable-y-max) 1)))
      (setq score (- score 10.0)))

    ;; 6. Pattern prediction
    (setq score (+ score (* (wraptrap-ai-predict-human-move new-pos) 8.0)))

    score))

(defun wraptrap-ai-get-valid-moves ()
  "Get list of valid moves for AI."
  (let ((valid-moves nil)
        (head (car wraptrap-ai-body)))
    (dolist (dir (wraptrap-all-directions))
      (unless (eq dir (wraptrap-opposite-direction wraptrap-ai-direction))
        (let* ((delta (wraptrap-direction-delta dir))
               (new-pos (wraptrap-pos-add head delta)))
          (when (and (wraptrap-pos-in-bounds-p new-pos)
                    (not (member new-pos wraptrap-human-body))
                    (not (member new-pos wraptrap-ai-body)))
            (push dir valid-moves)))))
    valid-moves))

(defun wraptrap-ai-get-next-move ()
  "Determine the best move for AI."
  (if (not wraptrap-ai-alive)
      wraptrap-ai-direction
    (let ((valid-moves (wraptrap-ai-get-valid-moves)))
      (if (null valid-moves)
          wraptrap-ai-direction
        (let ((best-move nil)
              (best-score -1.0e+INF))
          (dolist (move valid-moves)
            (let ((score (wraptrap-ai-evaluate-move move)))
              (when (> score best-score)
                (setq best-score score
                      best-move move))))
          (or best-move wraptrap-ai-direction))))))

;;; Game Update

(defun wraptrap-update ()
  "Update game state for one frame."
  (when (and (eq wraptrap-game-state 'playing)
             (not wraptrap-paused)
             (not wraptrap-game-over))
    (setq wraptrap-frame-count (1+ wraptrap-frame-count))
    (let ((should-grow (or (eq wraptrap-growth-mode 'continuous)
                          (and (eq wraptrap-growth-mode 'periodic)
                               (= 0 (mod wraptrap-frame-count wraptrap-growth-rate))))))

      ;; AI makes decision (only if not in 2-player mode)
      (unless wraptrap-two-player-mode
        (let ((ai-move (wraptrap-ai-get-next-move)))
          (setq wraptrap-ai-direction (wraptrap-change-direction wraptrap-ai-direction ai-move)))

        ;; Record human move for AI learning
        (push wraptrap-human-direction wraptrap-ai-move-history)
        (when (> (length wraptrap-ai-move-history) 20)
          (setq wraptrap-ai-move-history (butlast wraptrap-ai-move-history))))

      ;; Move worms
      (setq wraptrap-human-body (wraptrap-move-worm wraptrap-human-body
                                              wraptrap-human-direction
                                              should-grow))
      (setq wraptrap-ai-body (wraptrap-move-worm wraptrap-ai-body
                                          wraptrap-ai-direction
                                          should-grow))

      ;; Check collisions
      (wraptrap-check-collisions)

      ;; Update display
      (wraptrap-draw-grid)
      (wraptrap-draw-borders)
      (wraptrap-draw-info)

      ;; Show game over if needed
      (when wraptrap-game-over
        (setq wraptrap-game-state 'game-over)
        (wraptrap-display-game-over)))))

;;; Input Handling

(defun wraptrap-move-up ()
  "Move human worm up."
  (interactive)
  (setq wraptrap-human-direction (wraptrap-change-direction wraptrap-human-direction 'up)))

(defun wraptrap-move-down ()
  "Move human worm down."
  (interactive)
  (setq wraptrap-human-direction (wraptrap-change-direction wraptrap-human-direction 'down)))

(defun wraptrap-move-left ()
  "Move human worm left."
  (interactive)
  (setq wraptrap-human-direction (wraptrap-change-direction wraptrap-human-direction 'left)))

(defun wraptrap-move-right ()
  "Move human worm right."
  (interactive)
  (setq wraptrap-human-direction (wraptrap-change-direction wraptrap-human-direction 'right)))

(defun wraptrap-move-ai-up ()
  "Move AI/player 2 worm up (2-player mode)."
  (interactive)
  (setq wraptrap-ai-direction (wraptrap-change-direction wraptrap-ai-direction 'up)))

(defun wraptrap-move-ai-down ()
  "Move AI/player 2 worm down (2-player mode)."
  (interactive)
  (setq wraptrap-ai-direction (wraptrap-change-direction wraptrap-ai-direction 'down)))

(defun wraptrap-move-ai-left ()
  "Move AI/player 2 worm left (2-player mode)."
  (interactive)
  (setq wraptrap-ai-direction (wraptrap-change-direction wraptrap-ai-direction 'left)))

(defun wraptrap-move-ai-right ()
  "Move AI/player 2 worm right (2-player mode)."
  (interactive)
  (setq wraptrap-ai-direction (wraptrap-change-direction wraptrap-ai-direction 'right)))

(defun wraptrap-start-playing ()
  "Start the actual game from splash or options."
  (interactive)
  (when (memq wraptrap-game-state '(splash options))
    ;; Update grid size if using full width
    (when wraptrap-use-full-width
      (setq wraptrap-grid-width (max 40 (/ (window-width) 2))
            wraptrap-grid-height (max 20 (- (window-height) 4))))

    ;; Initialize buffer with new dimensions
    (wraptrap-init-buffer)

    ;; Start playing
    (setq wraptrap-game-state 'playing)
    (wraptrap-reset-game)
    (wraptrap-draw-grid)
    (wraptrap-draw-borders)
    (wraptrap-draw-info)))

(defun wraptrap-show-options ()
  "Show options screen."
  (interactive)
  (when (eq wraptrap-game-state 'splash)
    (setq wraptrap-game-state 'options
          wraptrap-options-selection 0)
    (wraptrap-display-options)))

(defun wraptrap-options-up ()
  "Move selection up in options menu."
  (interactive)
  (when (eq wraptrap-game-state 'options)
    (setq wraptrap-options-selection (max 0 (1- wraptrap-options-selection)))
    (wraptrap-display-options)))

(defun wraptrap-options-down ()
  "Move selection down in options menu."
  (interactive)
  (when (eq wraptrap-game-state 'options)
    (setq wraptrap-options-selection (min 3 (1+ wraptrap-options-selection)))
    (wraptrap-display-options)))

(defun wraptrap-options-toggle ()
  "Toggle the currently selected option."
  (interactive)
  (when (eq wraptrap-game-state 'options)
    (cond
     ((= wraptrap-options-selection 0)
      (setq wraptrap-use-full-width (not wraptrap-use-full-width)))
     ((= wraptrap-options-selection 1)
      (setq wraptrap-wrap-edges (not wraptrap-wrap-edges)))
     ((= wraptrap-options-selection 2)
      (setq wraptrap-growth-mode (if (eq wraptrap-growth-mode 'periodic)
                                  'continuous
                                'periodic)))
     ((= wraptrap-options-selection 3)
      (setq wraptrap-two-player-mode (not wraptrap-two-player-mode))))
    (wraptrap-display-options)))

(defun wraptrap-pause ()
  "Toggle game pause."
  (interactive)
  (when (eq wraptrap-game-state 'playing)
    (setq wraptrap-paused (not wraptrap-paused))
    (message (if wraptrap-paused "Paused" "Resumed"))))

(defun wraptrap-quit ()
  "Quit the game."
  (interactive)
  (when wraptrap-game-timer
    (cancel-timer wraptrap-game-timer)
    (setq wraptrap-game-timer nil))
  (kill-buffer wraptrap-buffer-name))

(defun wraptrap-restart ()
  "Restart the game."
  (interactive)
  (wraptrap-start-game))

;;; Game Initialization

(defun wraptrap-init-cells ()
  "Initialize gamegrid cells."
  ;; Define faces for each cell type
  (unless (facep 'wraptrap-blank-face)
    (copy-face 'default 'wraptrap-blank-face))

  (unless (facep 'wraptrap-human-body-face)
    (copy-face 'default 'wraptrap-human-body-face)
    (set-face-foreground 'wraptrap-human-body-face "green"))

  (unless (facep 'wraptrap-human-head-face)
    (copy-face 'default 'wraptrap-human-head-face)
    (set-face-foreground 'wraptrap-human-head-face "light green"))

  (unless (facep 'wraptrap-ai-body-face)
    (copy-face 'default 'wraptrap-ai-body-face)
    (set-face-foreground 'wraptrap-ai-body-face "red"))

  (unless (facep 'wraptrap-ai-head-face)
    (copy-face 'default 'wraptrap-ai-head-face)
    (set-face-foreground 'wraptrap-ai-head-face "dark red"))

  (unless (facep 'wraptrap-border-face)
    (copy-face 'default 'wraptrap-border-face)
    (set-face-foreground 'wraptrap-border-face "white"))

  ;; Set up gamegrid display options
  ;; Format: (cell-id char face)
  (setq gamegrid-display-options
        `((,wraptrap-blank ?\s wraptrap-blank-face)
          (,wraptrap-human-body ?o wraptrap-human-body-face)
          (,wraptrap-human-head ?O wraptrap-human-head-face)
          (,wraptrap-ai-body ?x wraptrap-ai-body-face)
          (,wraptrap-ai-head ?X wraptrap-ai-head-face)
          (,wraptrap-border ?# wraptrap-border-face))))

(defun wraptrap-reset-game ()
  "Reset all game state."
  ;; Human worm starts on left side of playable area
  (let ((center-y (+ (wraptrap-playable-y-min) (/ (wraptrap-playable-height) 2))))
    (setq wraptrap-human-body (list (cons (+ (wraptrap-playable-x-min) 3) center-y))
          wraptrap-human-direction 'right
          wraptrap-human-alive t)

    ;; AI worm starts on right side of playable area
    (setq wraptrap-ai-body (list (cons (- (wraptrap-playable-x-max) 3) center-y))
          wraptrap-ai-direction 'left
          wraptrap-ai-alive t))

  ;; Game state
  (setq wraptrap-game-over nil
        wraptrap-winner nil
        wraptrap-frame-count 0
        wraptrap-paused nil
        wraptrap-ai-move-history nil))

(defun wraptrap-start-game ()
  "Start a new game - show splash screen."
  (switch-to-buffer wraptrap-buffer-name)
  (gamegrid-kill-timer)
  (when wraptrap-game-timer
    (cancel-timer wraptrap-game-timer))

  (wraptrap-mode)
  (wraptrap-init-cells)

  ;; Set initial state to splash
  (setq wraptrap-game-state 'splash)
  (wraptrap-display-splash)

  ;; Start game loop (will only update when in 'playing state)
  (setq wraptrap-game-timer
        (run-at-time t wraptrap-tick-period #'wraptrap-update))

  (message "Press SPACE to start, O for options"))

;;; Mode Definition

(defvar wraptrap-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Arrow keys - dual purpose
    (define-key map [up] 'wraptrap-handle-up)
    (define-key map [down] 'wraptrap-handle-down)
    (define-key map [left] 'wraptrap-handle-left)
    (define-key map [right] 'wraptrap-handle-right)
    ;; WASD keys
    (define-key map "w" 'wraptrap-move-up)
    (define-key map "s" 'wraptrap-move-down)
    (define-key map "a" 'wraptrap-move-left)
    (define-key map "d" 'wraptrap-move-right)
    ;; Control keys
    (define-key map " " 'wraptrap-start-playing)
    (define-key map "o" 'wraptrap-show-options)
    (define-key map "p" 'wraptrap-pause)
    (define-key map "q" 'wraptrap-quit)
    (define-key map "r" 'wraptrap-restart)
    map)
  "Keymap for WrapTrap mode.")

(defun wraptrap-handle-up ()
  "Handle up arrow - move or navigate options."
  (interactive)
  (cond
   ((eq wraptrap-game-state 'options)
    (wraptrap-options-up))
   ((and (eq wraptrap-game-state 'playing) wraptrap-two-player-mode)
    (wraptrap-move-ai-up))
   (t
    (wraptrap-move-up))))

(defun wraptrap-handle-down ()
  "Handle down arrow - move or navigate options."
  (interactive)
  (cond
   ((eq wraptrap-game-state 'options)
    (wraptrap-options-down))
   ((and (eq wraptrap-game-state 'playing) wraptrap-two-player-mode)
    (wraptrap-move-ai-down))
   (t
    (wraptrap-move-down))))

(defun wraptrap-handle-left ()
  "Handle left arrow - move or toggle options."
  (interactive)
  (cond
   ((eq wraptrap-game-state 'options)
    (wraptrap-options-toggle))
   ((and (eq wraptrap-game-state 'playing) wraptrap-two-player-mode)
    (wraptrap-move-ai-left))
   (t
    (wraptrap-move-left))))

(defun wraptrap-handle-right ()
  "Handle right arrow - move or toggle options."
  (interactive)
  (cond
   ((eq wraptrap-game-state 'options)
    (wraptrap-options-toggle))
   ((and (eq wraptrap-game-state 'playing) wraptrap-two-player-mode)
    (wraptrap-move-ai-right))
   (t
    (wraptrap-move-right))))

(define-derived-mode wraptrap-mode special-mode "WrapTrap"
  "Major mode for playing WrapTrap.

Human vs AI snake game where both worms grow continuously.
Make your opponent crash first!

Controls:
\\{wraptrap-mode-map}"
  (setq cursor-type nil))

;;; Entry Point

;;;###autoload
(defun wraptrap ()
  "Play WrapTrap - a strategic snake game against an intelligent AI.

Features wrap-around edges, visible borders, and configurable gameplay.
The AI uses pathfinding, movement prediction, and path blocking to trap you.

From splash screen:
  SPACE - Start game
  O - Options menu (grid size, wrap/crash mode, growth rate)
  Q - Quit

In-game:
  Arrow keys or WASD - Move (green snake)
  P - Pause/Resume
  R - Restart
  Q - Quit

Goal: Make the AI (red) crash before you do!"
  (interactive)
  (wraptrap-start-game))

(provide 'wraptrap)

;;; wraptrap.el ends here
