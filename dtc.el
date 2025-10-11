;;; dtc.el --- Drone Theatre Command (refactored)
;;; Minimal roguelike-style engine for Emacs (Emacs Lisp)
;;; All world state is in a single hash-table `dtc-world`.
;;; Usage:
;;;  - Load/eval this buffer (M-x eval-buffer)
;;;  - Start with: M-x dtc-start
;;;  - Quit with: q (in DTC buffer) or M-x dtc-stop

;; Debugging/Testing
;; (dtc-get 'player) ;; (2 . 1)
;; (dtc-get 'enemies) ;; (nil nil nil nil nil)
;; (dtc-get 'walls) ;; ((1 . 4) (4 . 3) (32 . 15) (38 . 18) (22 . 7) (22 . 4) (30 . 11) (26 . 13) (6 . 6) (19 . 10) (15 . 10) (31 . 7) ...)

;; ---------------------------------------------------------------------------
;; Keymap and minor mode (buffer-local)
;; ---------------------------------------------------------------------------
(defvar dtc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>")    #'dtc-move-up)
    (define-key map (kbd "<down>")  #'dtc-move-down)
    (define-key map (kbd "<left>")  #'dtc-move-left)
    (define-key map (kbd "<right>") #'dtc-move-right)
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
        (font-lock-mode 1))
    (progn
      (setq-local cursor-type t)
      (font-lock-mode -1))))

(defun dtc-quit ()
  "Quit DTC: disable mode and kill buffer."
  (interactive)
  (dtc-mode -1)
  (dtc-stop))

(defvar dtc-buffer-name "*DTC*"
  "Name of the DTC game buffer.")

(defun dtc--ensure-buffer ()
  "Ensure the DTC buffer exists and return it."
  (or (get-buffer dtc-buffer-name)
      (generate-new-buffer dtc-buffer-name)))

(defun dtc--switch-to-buffer ()
  "Switch to the DTC buffer, creating it if needed."
  (let ((buf (dtc--ensure-buffer)))
    (switch-to-buffer buf)
    (dtc-mode)
    buf))

;; ---------------------------------------------------------------------------
;; World storage helpers
;; ---------------------------------------------------------------------------
(defvar dtc-world nil "Hash table holding the entire DTC world state.")

(defun dtc-init-world (&optional width height)
  "Create and initialize `dtc-world`. WIDTH and HEIGHT default to 100x50."
  (setq dtc-world (make-hash-table :test 'equal))
  (dtc-set 'width (or width 100))
  (dtc-set 'height (or height 50))
  (dtc-set 'score 0)
  (dtc-set 'turn 0)
  (dtc-set 'walls nil)
  (dtc-set 'goal '())
  (dtc-set 'player (cons 10 10))
  (dtc-set 'enemies nil)
  (dtc-set 'entities nil)
  (message "World initialized (%dx%d)" (dtc-get 'width) (dtc-get 'height))
  dtc-world)

(defun dtc-get (key)
  "Return KEY from `dtc-world`."
  (gethash key dtc-world))

(defun dtc-set (key value)
  "Set KEY to VALUE in `dtc-world`."
  (puthash key value dtc-world)
  value)

(defun dtc-assoc-get (key alist)
  "Get the value associated with KEY from the A-list ALIST."
  (cdr (assoc key alist)))

(defun dtc-assoc-set (key value alist)
  "Set the value associated with KEY in the A-list ALIST."
  (if (assoc key alist)
      (setcdr (assoc key alist) value)
    (push (cons key value) alist))
  alist)

(defun dtc-get-occupied-positions ()
  "Return a list of all positions currently occupied by entities."
  (mapcar (lambda (obj) (dtc-assoc-get 'pos obj)) (dtc-get 'entities)))

(defun dtc-free-p (x y &optional ignore-entities)
  "Return t if X,Y is walkable. When IGNORE-ENTITIES is non-nil, entities are ignored."
  (and (dtc-in-bounds-p x y)
       (not (member (cons x y) (dtc-get 'walls)))
       (not (dtc-pos-equal-p (cons x y) (dtc-get 'player)))
       ;; *** CHANGE THIS LINE ***
       (or ignore-entities (not (member (cons x y) (dtc-get-occupied-positions))))))

;; Checks
;; (dtc-get 'width)  ;; 40
;; (dtc-get 'height) ;; 20

;; ---------------------------------------------------------------------------
;; Utilities: coordinates and random open tile
;; ---------------------------------------------------------------------------
(defun dtc-make-pos (x y)
  "Return a cons cell representing a position with X and Y."
  (cons x y))

(defun dtc-x (pos) "Return X coordinate of POS." (car pos))
(defun dtc-y (pos) "Retufn Y coordinate of POS." (cdr pos))

(defun dtc-in-bounds-p (x y)
  "Return t when X,Y are within the playfield."
  (and (>= x 0)
       (>= y 0)
       (< x (dtc-get 'width))
       (< y (dtc-get 'height))))

(defun dtc-pos-equal-p (a b)
  (and a b (= (car a) (car b)) (= (cdr a) (cdr b))))

(defun dtc-random-open-tile ()
  "Return a random open tile (not a wall, not occupied).
If random sampling fails after many attempts, scan the map and return
the first available tile, or signal an error if none exist."
  (let* ((w (truncate (dtc-get 'width)))
         (h (truncate (dtc-get 'height)))
         pos ok
         (attempts 0)
         (max-attempts 1000))
    (unless (and (integerp w) (integerp h) (> w 0) (> h 0))
      (error "dtc-world has invalid width/height: %s x %s" w h))
    ;; Try random sampling first (fast)
    (while (and (not ok) (< attempts max-attempts))
      (setq pos (cons (random w) (random h))) ;; now guaranteed integers
      (setq ok (and (dtc-in-bounds-p (car pos) (cdr pos))
                    (not (member pos (dtc-get 'walls)))
                    (not (member pos (dtc-get 'enemies)))
                    (not (dtc-pos-equal-p pos (dtc-get 'player)))
                    ;;(or ignore-entities (not (member (cons x y) (dtc-get-occupied-positions))))
                    ))
      (setq attempts (1+ attempts)))
    (if ok
        pos
      ;; Fallback: scan the full grid deterministically
      (catch 'found
        (dotimes (y h)
          (dotimes (x w)
            (let ((p (cons x y)))
              (when (and (dtc-in-bounds-p x y)
                         (not (member p (dtc-get 'walls)))
                         (not (member p (dtc-get 'enemies)))
                         (not (dtc-pos-equal-p p (dtc-get 'player)))
                         ;;(or ignore-entities (not (member (cons x y) (dtc-get-occupied-positions))))
                         )
                (throw 'found p)))))
        (error "No open tile available on the map")))))

;; (dtc-random-open-tile) ; (19 . 19)

;; ---------------------------------------------------------------------------
;; World generation
;; ---------------------------------------------------------------------------
(defun dtc-generate-walls (count)
  "Generate COUNT random wall positions and store them in world."
  (let ((walls nil)
        (w (dtc-get 'width))
        (h (dtc-get 'height)))
    (dotimes (_ count)
      (push (cons (1+ (random (max 1 (- w 2))))
                  (1+ (random (max 1 (- h 2))))) walls))
    (dtc-set 'walls walls)))

;; (defun dtc-generate-enemies (n)
;;   "Place N enemies at random open positions."
;;   (let ((e nil))
;;     (dotimes (_ n)
;;       (push (dtc-random-open-tile) e))
;;     e))

(defun dtc-generate-enemies (n)
  "Place N enemies at random open positions, replacing any existing list."
  (let ((enemies nil)
        (w (dtc-get 'width))
        (h (dtc-get 'height)))
    (dotimes (_ n)
      (let ((pos (dtc-random-open-tile)))
        (push pos enemies)))
    ;; (dtc-set 'enemies enemies)
    enemies))

;; (dtc-generate-enemies 3) ((21 . 6) (0 . 19) (25 . 16))

(defun dtc-create-entity (char face logic)
  "Create a new entity A-list with a random open position."
  (let ((pos (dtc-random-open-tile)))
    `((pos . ,pos)
      (char . ,char)
      (face . ,face)
      (logic . ,logic))))

(defun dtc-generate-basic-world (&optional width height)
  "Create a basic world layout: walls, player, goal, enemies.
The size of the theatre is given by WIDTH and HEIGHT."
  (dtc-init-world width height)
  (dtc-set 'width (or width (dtc-get 'width)))
  (dtc-set 'height (or height (dtc-get 'height)))

  ;; ;; Fill walls (example: border walls)
  ;; (dotimes (x width)
  ;;   (push (cons x 0) walls)
  ;;   (push (cons x (1- h)) walls))
  ;; (dotimes (y height)
  ;;   (push (cons 0 y) walls)
  ;;   (push (cons (1- w) y) walls))

  ;; walls - a moderate number scaled to area
  (dtc-generate-walls (max 10 (/ (* (dtc-get 'width) (dtc-get 'height)) 20)))
  ;; player near top-left
  (dtc-set 'player (cons 1 1))
  ;; goal near bottom-right
  (dtc-set 'goal (cons (- (dtc-get 'width) 2) (- (dtc-get 'height) 2)))
  ;; enemies
  (dtc-set 'enemies (dtc-generate-enemies (+ 3 (random 3))))
  (dtc-set 'score 0)
  (dtc-set 'turn 0)
  (dtc-get 'player))

;; (dtc-random-open-tile) ;nil
;; (dtc-generate-enemies (+ 3 (random 3))) ; (nil nil nil)

;; ---------------------------------------------------------------------------
;; Free tile test
;; ---------------------------------------------------------------------------
;; (defun dtc-free-p (x y &optional ignore-enemies)
;;   "Return t if X,Y is walkable. When IGNORE-ENEMIES is non-nil, enemies are ignored."
;;   (and (dtc-in-bounds-p x y)
;;        (not (member (cons x y) (dtc-get 'walls)))
;;        (not (dtc-pos-equal-p (cons x y) (dtc-get 'player)))
;;        (or ignore-enemies (not (member (cons x y) (dtc-get 'enemies))))))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------
(defvar dtc-buffer-name "*DTC*" "Name of the DTC display buffer.")

;; If any face chnages are made, run M-x custom-theme-set-faces to reload the
;; face definitions (as these are cached.)
;; Map elements
(defface dtc-wall-face
  '((t (:foreground "gray" :weight semi-bold)))
  "Face for the wall character ('U') in my game."
  :group 'dtc)

(defface dtc-goal-face
  '((t (:foreground "gold" :weight extra-bold)))
  "Face for the goal character ('$') in my game."
  :group 'dtc)

(defface dtc-floor-face
  '((t (:foreground "white smoke"))) ; A subtle, distinct color for the floor
  "Face for the floor character ('.') in my game."
  :group 'dtc)

;; Entities
(defface dtc-player-face
  '((t (:foreground "light blue" :weight bold)))
  "Face for the player character in my game."
  :group 'dtc) ; Replace my-game with your game's name

(defface dtc-guard-face
  '((t (:foreground "green" :weight bold)))
  "Face for a friendly guard character in my game."
  :group 'dtc)

(defface dtc-enemy-face
  '((t (:foreground "red" :weight bold)))
  "Face for the enemy character in my game."
  :group 'dtc)

(defface dtc-heavy-enemy-face
  '((t (:foreground "purple" :weight extra-bold)))
  "Face for a heavy enemy character in my game."
  :group 'dtc)

(defun dtc-render ()
  "Render the entire world into the DTC buffer."
  (let* ((buf (get-buffer-create dtc-buffer-name))
         (w (dtc-get 'width))
         (h (dtc-get 'height))
         (player (dtc-get 'player))
         (enemies (dtc-get 'enemies))
         (walls (dtc-get 'walls))
         (goal (dtc-get 'goal)))

    (message "Player: %S" (dtc-get 'player))
    (message "Enemies: %S" (dtc-get 'enemies))
    (message "Walls: %d" (length (dtc-get 'walls)))

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dotimes (y h)
          (dotimes (x w)
            (cond
             ((dtc-pos-equal-p (cons x y) player)
              ;; (insert "@"))
              (insert (propertize "@" 'font-lock-face 'dtc-player-face)))
             ((member (cons x y) enemies)
              ;; (insert "X"))
              (insert (propertize "X" 'font-lock-face 'dtc-enemy-face)))
             ((dtc-pos-equal-p (cons x y) goal)
              ;; (insert "$"))
              (insert (propertize "$" 'font-lock-face 'dtc-goal-face)))
             ((member (cons x y) walls)
              ;; (insert "U"))
              (insert (propertize "U" 'font-lock-face 'dtc-wall-face)))
             (t
              ;; (insert "."))
              (insert (propertize "." 'font-lock-face 'dtc-floor-face)))
             ))
          (insert "\n")))
      (goto-char (point-min))
      (setq buffer-read-only t)
      (dtc-mode 1)
      (display-buffer buf))
    ;; optionally return buffer
    buf))

;; ---------------------------------------------------------------------------
;; Movement: player
;; ---------------------------------------------------------------------------
(defun dtc-move-player (dx dy)
  "Attempt to move player by DX,DY. Return t if moved."
  (let* ((p (dtc-get 'player))
         (nx (+ (car p) dx))
         (ny (+ (cdr p) dy)))
    (when (dtc-free-p nx ny)
      (dtc-set 'player (cons nx ny))
      t)))

(defun dtc-move-up ()    (interactive) (when (dtc-move-player 0 -1) (dtc-tick)))
(defun dtc-move-down ()  (interactive) (when (dtc-move-player 0  1) (dtc-tick)))
(defun dtc-move-left ()  (interactive) (when (dtc-move-player -1 0) (dtc-tick)))
(defun dtc-move-right () (interactive) (when (dtc-move-player  1 0) (dtc-tick)))

;; ---------------------------------------------------------------------------
;; Enemy movement (simple chase behaviour)
;; ---------------------------------------------------------------------------
(defun dtc-move-enemy-once (pos)
  "Return a new position for enemy POS moving one step toward player if possible."
  (let* ((px (car (dtc-get 'player)))
         (py (cdr (dtc-get 'player)))
         (ex (car pos))
         (ey (cdr pos))
         (dx (cond ((< ex px) 1) ((> ex px) -1) (t 0)))
         (dy (cond ((< ey py) 1) ((> ey py) -1) (t 0)))
         (try-dirs (list (cons dx 0) (cons 0 dy) (cons dx dy) '(0 . 0)))
         new-pos)
    (setq new-pos pos)
    (catch 'moved
      (dolist (dir try-dirs)
        (let ((nx (+ ex (car dir))) (ny (+ ey (cdr dir))))
          (when (dtc-free-p nx ny)
            (setq new-pos (cons nx ny))
            (throw 'moved t)))))
    new-pos))

(defun dtc-move-enemies ()
  "Move each enemy one step (chase player)."
  (let ((new (mapcar #'dtc-move-enemy-once (dtc-get 'enemies))))
    (dtc-set 'enemies new)
    new))

;; ---------------------------------------------------------------------------
;; Tick/update loop
;; ---------------------------------------------------------------------------
(defun dtc-tick ()
  "Advance the game one tick: move enemies, check collisions, render, and increment turn."
  (dtc-move-enemies)
  (dtc-inc-turn)
  (dtc-check-state)
  (dtc-render))

(defun dtc-inc-turn ()
  (dtc-set 'turn (1+ (dtc-get 'turn))))

(defun dtc-check-state ()
  "Check basic win/lose conditions and print messages into the buffer."
  (let ((player (dtc-get 'player))
        (enemies (dtc-get 'enemies))
        (goal (dtc-get 'goal)))
    (when (member player enemies)
      (dtc-show-message "💀 You were caught by an enemy. Press q to quit."))
    (when (dtc-pos-equal-p player goal)
      (dtc-show-message "🎉 You reached the goal. Press q to quit."))))

(defun dtc-show-message (msg)
  "Append MSG to the DTC buffer."
  (with-current-buffer (get-buffer-create dtc-buffer-name)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n" msg "\n"))))

;; ---------------------------------------------------------------------------
;; Commands: start/stop/reset
;; ---------------------------------------------------------------------------
(defun dtc-start ()
  "Start or restart the DTC game, creating and switching to the *DTC* buffer."
  (interactive)
  (let ((buf (dtc--ensure-buffer)))
    ;; Always switch to the game buffer before doing anything else
    (switch-to-buffer buf)
    ;; Ensure correct major mode (sets keymap, buffer-local vars)
    (dtc-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; Initialize world and draw the screen
      (dtc-init-world)
      (dtc-generate-basic-world)
      (dtc-render))
    (goto-char (point-min))
    (message "Drone Theatre Command initialized in buffer %s" dtc-buffer-name)))

(defun dtc-stop ()
  "Stop DTC and kill the buffer."
  (interactive)
  (when (get-buffer dtc-buffer-name)
    (kill-buffer dtc-buffer-name)
    (message "DTC stopped.")))

;; ---------------------------------------------------------------------------
;; Convenience: evaluate a single-expression test to step enemies manually
;; ---------------------------------------------------------------------------
(defun dtc-step-enemies ()
  "Move enemies without moving the player (dev helper)."
  (interactive)
  (dtc-move-enemies)
  (dtc-render))

;; ---------------------------------------------------------------------------
;; DTC world reset (safe and clean) -------------------------
;; ---------------------------------------------------------------------------
(defun dtc-reset-world ()
  "Reset the Drone Theatre Command world to a fresh state."
  (interactive)
  (let* ()
    (clrhash dtc-world)
    (dtc-init-world)
    (dtc-generate-basic-world)

    ;; Optional: clear and redraw
    (when (fboundp 'dtc-render)
      (dtc-render))

    (message "DTC world reset: %dx%d with %d enemies"
             w h num-enemies)))

;; --- Run it immediately ---
;; (dtc-reset-world)

;; ---------------------------------------------------------------------------
;; Ensure symbols are available for users who load file in separate order
;; ---------------------------------------------------------------------------
(provide 'dtc)
;;; dtc.el ends here
