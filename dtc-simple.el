;;; dtc.el --- Drone Theatre Command (refactored)
;;; Minimal roguelike-style engine for Emacs (Emacs Lisp)

;; --------------------------------------------------------------------------
;; Global Variables and Setup
;; --------------------------------------------------------------------------
(require 'cl-lib) ; For cl-lib functions

(defvar dtc-world (make-hash-table :test 'eq)
  "Main hash table storing all world state (player, walls, entities, etc.).")
(defvar dtc-buffer-name "*DTC*"
  "Name of the buffer used for the game.")
(defvar dtc-timer nil
  "Handle for the timer that calls dtc-auto-tick for enemy movement.")

;; --------------------------------------------------------------------------
;; Helper Functions (Access World State)
;; --------------------------------------------------------------------------
(defun dtc-set (key value)
  "Set KEY to VALUE in the world state hash table."
  (puthash key value dtc-world))

(defun dtc-get (key)
  "Retrieve value associated with KEY from the world state."
  (gethash key dtc-world))

(defun dtc-pos-equal-p (pos1 pos2)
  "Return t if two cons-style positions (X . Y) are equal."
  (and (eq (car pos1) (car pos2))
       (eq (cdr pos1) (cdr pos2))))

(defun dtc-to-grid (pos)
  "Convert a float position (X . Y) to an integer grid position."
  (cons (truncate (car pos)) (truncate (cdr pos))))

(defun dtc-assoc-get (key alist)
  "Helper to retrieve a value from an association list (feature data or entity data)."
  (cdr (cl-assoc key alist)))

;; --------------------------------------------------------------------------
;; Keymap and minor mode (buffer-local)
;; --------------------------------------------------------------------------
(defvar dtc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>")    #'dtc-move-up)
    (define-key map (kbd "<down>")  #'dtc-move-down)
    (define-key map (kbd "<left>")  #'dtc-move-left)
    (define-key map (kbd "<right>") #'dtc-move-right)
    (define-key map (kbd "p")       #'dtc-toggle-pause)
    (define-key map (kbd "q")       #'dtc-quit)
    map)
  "Keymap for DTC minor mode.")

(define-minor-mode dtc-mode
  "Minor mode for Drone Theatre Command buffer."
  :init-value nil
  :lighter " DTC"
  :keymap dtc-mode-map
  (if dtc-mode
      (progn
        (setq-local cursor-type nil)
        (setq-local scroll-up-aggressively nil)
        (setq-local buffer-read-only t)
        (visual-line-mode -1)
        (display-line-numbers-mode -1))
    (setq-local cursor-type t)
    (setq-local buffer-read-only nil)))

;; --------------------------------------------------------------------------
;; Map & Feature Generation
;; --------------------------------------------------------------------------
(defun dtc-init-world (&optional width height)
  "Create and initialize `dtc-world` with default size and state."
  (dtc-set 'width (or width 40))
  (dtc-set 'height (or height 20))
  (dtc-set 'player nil)
  (dtc-set 'goal nil)
  (dtc-set 'walls nil)
  (dtc-set 'entities nil)
  (dtc-set 'features nil)
  (dtc-set 'score 0)
  (dtc-set 'turn 0)
  (dtc-set 'paused nil)
  (dtc-set 'blocking-cache nil))

(defun dtc-create-feature (x y char face alist)
  "Create a feature (wall, item, door, etc.) at X, Y with associated data.
Returns the feature alist."
  (let ((feature `((pos . ,(cons (truncate x) (truncate y)))
                   (char . ,char)
                   (face . ,face)
                   (blocking . t))))
    (dolist (pair alist)
      (setq feature (cl-acons (car pair) (cdr pair) feature)))
    feature))

(defun dtc-create-wall-feature (x y)
  "Helper to create a standard blocking wall feature (#)."
  (dtc-create-feature x y "#" 'dtc-wall-face '((type . wall))))

(defun dtc-create-entity (pos char face)
  "Create an entity (enemy) alist at position POS."
  `((pos . ,pos)
    (char . ,char)
    (face . ,face)
    (type . enemy)))


(defun dtc-generate-basic-world (&optional width height)
  "Create a basic world layout: walls, player, goal, entities."
  (let* ((w (dtc-get 'width))
         (h (dtc-get 'height))
         (current-features nil))

    ;; 1. Wall Features (Outer Border and Obstacles)
    (dotimes (x w)
      (push (dtc-create-wall-feature x 0) current-features)
      (push (dtc-create-wall-feature x (1- h)) current-features))
    (dotimes (y h)
      (push (dtc-create-wall-feature 0 y) current-features)
      (push (dtc-create-wall-feature (1- w) y) current-features))

    (push (dtc-create-wall-feature 15 5) current-features)
    (push (dtc-create-wall-feature 15 6) current-features)
    (push (dtc-create-wall-feature 15 7) current-features)

    ;; Non-blocking item
    (push (dtc-create-feature 10 10 "!" 'dtc-goal-face '((type . item) (blocking . nil))) current-features)

    ;; Blocking door
    (push (dtc-create-feature 20 15 "D" 'dtc-wall-face '((type . door))) current-features)

    (dtc-set 'features current-features)

    ;; Set player and goal
    (dtc-set 'player (cons (/ w 2.0) (/ h 2.0)))
    (dtc-set 'goal (cons 1 1))

    ;; Spawn enemies
    (let ((enemies '())
          (num-enemies 5))
      (dotimes (_ num-enemies)
        (let ((pos (dtc-to-grid (dtc-get 'player))))
          (push (dtc-create-entity pos "E" 'dtc-enemy-face) enemies)))
      (dtc-set 'entities enemies))))

;; --------------------------------------------------------------------------
;; Collision/Movement Logic
;; --------------------------------------------------------------------------

(defun dtc-get-blocking-features ()
  "Return a list of integer positions occupied by features marked as 'blocking.
Prioritizes the pre-calculated cache list for performance."
  (let ((cached-list (dtc-get 'blocking-cache)))

    (if cached-list
        cached-list

      (mapcar (lambda (f) (dtc-assoc-get 'pos f))
              (cl-remove-if-not (lambda (f) (dtc-assoc-get 'blocking f))
                                (dtc-get 'features))))))

(defun dtc-in-bounds-p (x y)
  "Return t if X,Y are within world bounds."
  (let ((w (dtc-get 'width))
        (h (dtc-get 'height)))
    (and (>= x 1) (< x (1- w))
         (>= y 1) (< y (1- h)))))

(defun dtc-get-occupied-positions ()
  "Get all positions occupied by entities."
  (mapcar (lambda (e) (dtc-to-grid (dtc-assoc-get 'pos e)))
          (dtc-get 'entities)))

(defun dtc-free-p (x y &optional ignore-entities)
  "Return t if X,Y are the integer coordinates of a walkable tile."
  (let ((grid-pos (cons (truncate x) (truncate y)))
        (player-grid-pos (dtc-to-grid (dtc-get 'player))))

    (and (dtc-in-bounds-p x y)

         ;; 1. Must not be a blocking feature (Uses the cache)
         (not (cl-find grid-pos (dtc-get-blocking-features) :test 'dtc-pos-equal-p))

         ;; 2. Must not be the grid cell currently occupied by the player
         (not (dtc-pos-equal-p grid-pos player-grid-pos))

         ;; 3. Check entities only if requested
         (or ignore-entities (not (cl-find grid-pos (dtc-get-occupied-positions) :test 'dtc-pos-equal-p))))))

(defun dtc-move-player (dx dy)
  "Attempt to move player by DX,DY. Return t if moved."
  (interactive)
  (let* ((player (dtc-get 'player))
         (px (car player))
         (py (cdr player))
         (nx (+ px dx))
         (ny (+ py dy))
         (new-pos (cons nx ny))
         (blocking-list (dtc-get-blocking-features)))

    (dtc-set 'blocking-cache blocking-list)

    (unwind-protect
        (when (dtc-free-p nx ny nil)
          (dtc-set 'player new-pos)
          (dtc-player-tick)
          t)

      (dtc-set 'blocking-cache nil))))

;; --------------------------------------------------------------------------
;; Feature Interaction (e.g., picking up items)
;; --------------------------------------------------------------------------
(defun dtc-interact-at (pos)
  "Handle interaction when the player lands on an integer position POS."
  (let ((features (dtc-get 'features))
        (feature nil))

    (setq feature (cl-find pos features
                           :key (lambda (f) (dtc-assoc-get 'pos f))
                           :test 'dtc-pos-equal-p))

    (when feature
      (let ((type (dtc-assoc-get 'type feature)))
        (message "Interacted with a %S feature at %S" type pos)

        (when (eq type 'item)
          (dtc-set 'score (1+ (dtc-get 'score)))
          (message "Picked up an item! Score: %d" (dtc-get 'score))

          (dtc-set 'features (cl-remove feature features :test 'equal))
          t)))))

;; --------------------------------------------------------------------------
;; Game Loop (Tick Functions)
;; --------------------------------------------------------------------------
(defun dtc-player-tick ()
  "Process game state after the player has moved."
  (interactive)

  (dtc-interact-at (dtc-to-grid (dtc-get 'player)))

  (dtc-move-enemies)

  (dtc-set 'turn (1+ (dtc-get 'turn)))

  (dtc-render)
  (dtc-check-win))

(defun dtc-auto-tick ()
  "Process automatic game updates, run by the dtc-timer."
  (with-current-buffer dtc-buffer-name
    (dtc-move-enemies)
    (dtc-set 'turn (1+ (dtc-get 'turn)))
    (dtc-render)
    (dtc-check-win)))

;; --------------------------------------------------------------------------
;; Pause Logic
;; --------------------------------------------------------------------------
(defun dtc-toggle-pause ()
  "Toggle the game's pause state by stopping or restarting the dtc-timer."
  (interactive)
  (if dtc-timer
      (progn
        (dtc-stop-main-loop)
        (dtc-set 'paused t)
        (dtc-render)
        (message "Game PAUSED."))
    (progn
      (dtc-start-main-loop)
      (dtc-set 'paused nil)
      (dtc-render)
      (message "Game RESUMED."))))

;; --------------------------------------------------------------------------
;; Rendering and Faces
;; --------------------------------------------------------------------------
(defface dtc-player-face '((t (:foreground "yellow"))) "Player character face.")
(defface dtc-goal-face '((t (:foreground "green"))) "Goal character face.")
(defface dtc-enemy-face '((t (:foreground "red"))) "Enemy character face.")
(defface dtc-wall-face '((t (:foreground "white"))) "Wall character face.")

(defun dtc-render ()
  "Render the entire world into the DTC buffer. Includes the buffer clear fix."
  (let* ((w (dtc-get 'width))
         (h (dtc-get 'height))
         (player-pos (dtc-to-grid (dtc-get 'player)))
         (goal (dtc-get 'goal))
         (entities (dtc-get 'entities))
         (features (dtc-get 'features))
         (feature nil)
         (entity nil)
         (current-pos nil))

    ;; FIX: All buffer modification must be wrapped in inhibit-read-only
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (erase-buffer) ; The buffer clear fix from the last request

      (setq feature nil entity nil)

      (dotimes (y h)
        (dotimes (x w)
          (setq current-pos (cons x y))

          ;; 1. Check for Entity (Enemy)
          (setq entity (cl-find current-pos entities
                                :key (lambda (e) (dtc-to-grid (dtc-assoc-get 'pos e)))
                                :test 'dtc-pos-equal-p))

          ;; 2. Check for Feature (Wall, Item, Door)
          (setq feature (cl-find current-pos features
                                 :key (lambda (f) (dtc-assoc-get 'pos f))
                                 :test 'dtc-pos-equal-p))

          (cond
           ((dtc-pos-equal-p current-pos player-pos)
            (insert (propertize "@" 'font-lock-face 'dtc-player-face)))

           (entity
            (insert (propertize (dtc-assoc-get 'char entity)
                                'font-lock-face (dtc-assoc-get 'face entity))))

           ((dtc-pos-equal-p current-pos goal)
            (insert (propertize "$" 'font-lock-face 'dtc-goal-face)))

           (feature
            (insert (propertize (dtc-assoc-get 'char feature)
                                'font-lock-face (dtc-assoc-get 'face feature))))

           ;; Must be empty space
           (t
            (insert " "))))
        (unless (= y (1- h))
          (insert "\n")))

      ;; Status bar and PAUSE indicator
      (insert "\n\n")
      (insert "Turn: " (number-to-string (dtc-get 'turn)) " | Score: " (number-to-string (dtc-get 'score)))

      ;; Display PAUSED message if the flag is set
      (when (dtc-get 'paused)
        (goto-char (point-max))
        (insert "\n\nPAUSED")))))

;; --------------------------------------------------------------------------
;; Main Game Control (start/stop)
;; --------------------------------------------------------------------------
(defun dtc-start-main-loop ()
  "Start the timer that calls the game's tick function."
  (dtc-stop-main-loop)
  (setq dtc-timer
        (run-with-timer 0 0.5 'dtc-auto-tick)))

(defun dtc-stop-main-loop ()
  "Stop the game's main timer."
  (when dtc-timer
    (cancel-timer dtc-timer)
    (setq dtc-timer nil)))

(defun dtc-start ()
  "Start the Drone Theatre Command game."
  (interactive)
  (let ((buf (get-buffer-create dtc-buffer-name)))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (dtc-mode)
      (dtc-init-world)
      (dtc-generate-basic-world)
      (dtc-render)
      (setq-local buffer-read-only t)
      (dtc-start-main-loop)
      (goto-char (point-min))
      (message "Drone Theatre Command initialized in buffer %s" dtc-buffer-name))))

(defun dtc-stop ()
  "Stop DTC and kill the buffer."
  (interactive)
  (dtc-stop-main-loop)
  (when (get-buffer dtc-buffer-name)
    (kill-buffer dtc-buffer-name)
    (message "DTC stopped.")))

;; --------------------------------------------------------------------------
;; STUBS (Required for the game to run without error)
;; --------------------------------------------------------------------------
(defun dtc-random-open-tile ()
  "Stub: Return the player's current grid position to avoid errors."
  (dtc-to-grid (dtc-get 'player)))

(defun dtc-move-enemies ()
  "Stub: Process enemy movement."
  nil)

(defun dtc-check-win ()
  "Stub: Check win/lose conditions."
  nil)

;; FIX: Added (interactive) to make these functions valid Emacs commands
(defun dtc-move-up ()
  (interactive)
  (dtc-move-player 0 -1))
(defun dtc-move-down ()
  (interactive)
  (dtc-move-player 0 1))
(defun dtc-move-left ()
  (interactive)
  (dtc-move-player -1 0))
(defun dtc-move-right ()
  (interactive)
  (dtc-move-player 1 0))
