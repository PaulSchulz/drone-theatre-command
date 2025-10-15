;;; dtc.el --- Drone Theatre Command (refactored)
;;; Minimal roguelike-style engine for Emacs (Emacs Lisp)
;;; All world state is in a single hash-table `dtc-world`.
;;; Usage:
;;;  - Load/eval this buffer (M-x eval-buffer)
;;;  - Start with: M-x dtc-start
;;;  - Quit with: q (in DTC buffer) or M-x dtc-stop
;;;
;;; Commentary:
;;;   Put something here.
;;; Code:
;;;   Put something here.

;; ---------------------------------------------------------------------------
;; Keymap and minor mode (buffer-local)
;; ---------------------------------------------------------------------------
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
  (dtc-set 'width (or width 50))
  (dtc-set 'height (or height 30))
  (dtc-set 'score 0)
  (dtc-set 'turn 0)
  (dtc-set 'player (cons 10.0 10.0))
  (dtc-set 'features nil)
  (dtc-set 'entities nil)
  (dtc-set 'paused nil)
  (dtc-set 'blocking-cache nil)
  (message "World initialized (%dx%d)" (dtc-get 'width) (dtc-get 'height))
  dtc-world)

(defun dtc-clear-world ()
  "Reset the world."
  (clrhash dtc-world))

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
  "Set the VALUE associated with KEY in the A-list ALIST."
  (if (assoc key alist)
      (setcdr (assoc key alist) value)
    (push (cons key value) alist))
  alist)

(defun dtc-get-occupied-positions ()
  "Return a list of all integer grid positions currently occupied by *all* entities, including the player."
  (let ((entity-positions
         (mapcar #'dtc-to-grid
                 (mapcar (lambda (obj) (dtc-assoc-get 'pos obj)) (dtc-get 'entities)))))
    ;; Add the player's position to the list of occupied tiles
    (cons (dtc-to-grid (dtc-get 'player)) entity-positions)))


(defun dtc-get-blocking-features ()
  "Return a list of integer positions occupied by features marked as 'blocking."
  (mapcar (lambda (f) (dtc-assoc-get 'pos f))
          (cl-remove-if-not (lambda (f) (dtc-assoc-get 'blocking f))
                            (dtc-get 'features))))

(defun dtc-free-p (x y &optional ignore-entities)
  "Return t if X,Y are the integer coordinates of a walkable tile.
The coordinates X and Y are treated as floats, truncated to check the grid cell.
The IGNORE-ENTITIES switch allows entities to not be considered."
  (let ((grid-pos (cons (truncate x) (truncate y)))
        (blocking-features (dtc-get-blocking-features))
        (occupied-by-entities (dtc-get-occupied-positions)))

    ;; Check 1: Must be in bounds
    (and (dtc-in-bounds-p x y)
         ;; Check 2: Must not be a blocking feature
         (not (cl-find grid-pos blocking-features :test 'dtc-pos-equal-p))

         ;; Check 3: Must not be occupied by *any* entity (player or otherwise),
         ;; unless we are explicitly ignoring entities (e.g., for flying
         ;; movement).
         (or ignore-entities
             (not (cl-find grid-pos occupied-by-entities :test 'dtc-pos-equal-p))))))

;; Checks
;; (dtc-get 'width)  ;; 40
;; (dtc-get 'height) ;; 20

;; ---------------------------------------------------------------------------
;; Utilities: Handling sub-grid modement
;; ---------------------------------------------------------------------------
(defvar dtc-unit-speed 1.0 "The distance covered by the player in one move (float).")

(defun dtc-grid-x (pos) "Return truncated X grid coordinate of POS." (truncate (car pos)))
(defun dtc-grid-y (pos) "Return truncated Y grid coordinate of POS." (truncate (cdr pos)))

(defun dtc-to-grid (pos)
  "Convert float position POS (cons of floats) to integer grid position (cons of integers) by truncation."
  (cons (truncate (car pos)) (truncate (cdr pos))))

;; ---------------------------------------------------------------------------
;; Utilities: coordinates and random open tile
;; ---------------------------------------------------------------------------
(defun dtc-make-pos (x y)
  "Return a cons cell representing a position with X and Y."
  (cons x y))

(defun dtc-x (pos) "Return X coordinate of POS." (car pos))
(defun dtc-y (pos) "Return Y coordinate of POS." (cdr pos))

(defun dtc-in-bounds-p (x y)
  "Return t when X,Y are within the playfield."
  (and (>= x 0)
       (>= y 0)
       (< x (dtc-get 'width))
       (< y (dtc-get 'height))))

(defun dtc-pos-equal-p (a b)
  "Are the two locations A and B the same."
  (and a b (= (car a) (car b)) (= (cdr a) (cdr b))))

(defun dtc-random-open-tile ()
  "Return a random open tile (not a wall, not occupied).
If random sampling fails after many attempts, scan the map and return
the first available tile, or signal an error if none exist."
  (let* ((w (truncate (dtc-get 'width)))
         (h (truncate (dtc-get 'height)))
         pos ok
         (attempts 0)
         (max-attempts 1000)
         (occupied (dtc-get-occupied-positions))
         )
    (unless (and (integerp w) (integerp h) (> w 0) (> h 0))
      (error "dtc-world has invalid width/height: %s x %s" w h))
    ;; Try random sampling first (fast)
    (while (and (not ok) (< attempts max-attempts))
      (setq pos (cons (random w) (random h))) ;; now guaranteed integers
      (setq ok (and (dtc-in-bounds-p (car pos) (cdr pos))
                    ;; (not (member pos (dtc-get 'walls)))
                    (not (member pos occupied))
                    (not (dtc-pos-equal-p pos (dtc-get 'player)))
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
                         ;; (not (member p (dtc-get 'walls)))
                         (not (member p occupied))
                         (not (dtc-pos-equal-p p (dtc-get 'player)))
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

(defun dtc-create-feature (x y char face &optional properties)
  "Create a new map feature A-list at integer coordinates X,Y.
CHAR is the character to use
FACE is the font-fate to use when drawing
PROPERTIES is an optional A-list of extra key/value pairs."
  (let ((feature `((pos . ,(cons x y))
                   (char . ,char)
                   (face . ,face)
                   (blocking . t)))) ; Default to blocking
    ;; Use cl-union to merge the default properties with any optional ones
    (cl-union feature properties :test 'eq :key 'car)))

(defun dtc-create-entity (char face logic speed)
  "Create a new entity A-list with a random open position.
CHAR is the character to display, FACE is the font-face to use and LOGIC
is a function that controls the entities behaviour, and SPEED which is
assumed to be a float."
  (let ((pos (dtc-random-open-tile)))
    `((pos . ,pos)
      (char . ,char)
      (face . ,face)
      (logic . ,logic)
      (speed . ,speed))))

(defun dtc-create-wall-feature (x y)
  "Helper to create a standard blocking wall feature (#) ay X,Y."
  ;; Calls the existing dtc-create-feature, ensuring 'blocking is true by default
  (dtc-create-feature x y "#" 'dtc-wall-face '((type . wall))))

(defun dtc-generate-basic-world (&optional width height)
  "Create a basic world layout: map, features, player, enemies.
The size of the theatre is given by WIDTH and HEIGHT."
  (dtc-init-world width height)

  (let* ((w (dtc-get 'width))
         (h (dtc-get 'height))
         (current-features (dtc-get 'features)))

    ;; Player near top-left
    (dtc-set 'player (cons 1 1))

    ;; Interior Walls/Obstacles
    (push (dtc-create-wall-feature 15 5) current-features)
    (push (dtc-create-wall-feature 15 6) current-features)
    (push (dtc-create-wall-feature 15 7) current-features)

    ;; 3. Add features
    (push (dtc-create-feature 15 8 "D" 'dtc-wall-face '((type . door))) current-features)

    ;; A non-blocking item (Must explicitly set blocking to nil)
    ;; (push (dtc-create-feature 20 5 "!" 'dtc-goal-face '((type . item) (blocking . nil))) current-features)

    ;; Crate
    (push (dtc-create-feature 30 10 "C" 'dtc-goal-face) current-features)

    ;; Save the unified list of walls and features
    (dtc-set 'features current-features)

    (dtc-set 'entities
             (list
              ;; Simple Enemy Drone
              (dtc-create-entity "%" 'dtc-enemy-face 'dtc-chase-logic 1.0)
              ))
    (dtc-set 'score 0)
    (dtc-set 'turn 0)
    (dtc-get 'player))
  )
;; (dtc-random-open-tile) ;nil
;; (dtc-generate-enemies (+ 3 (random 3))) ; (nil nil nil)

;; --------------------------------------------------------------------------
;; Save/Restore Logic
;; --------------------------------------------------------------------------
;; Not yet enabled.
(defvar dtc-save-file "~/.emacs.d/dtc-save.el"
  "Path to the file used for saving and restoring the game state.")

(defun dtc-get-world-as-list ()
  "Convert the 'dtc-world' hash table to a property list (plist)."
  (let (plist)
    (maphash (lambda (key value)
               (push key plist)
               (push value plist))
             dtc-world)
    plist))

(defun dtc-set-world-from-list (plist)
  "Clear 'dtc-world' and populate it from a property list (PLIST)."
  (clrhash dtc-world)
  (while plist
    (puthash (pop plist) (pop plist) dtc-world)))

(defun dtc-save-game ()
  "Save the current state of the game world to a file."
  (interactive)
  (dtc-stop-main-loop) ; Stop the game loop before saving
  (let ((world-list (cl-copy-list (dtc-get-world-as-list)))) ; Convert hash table to plist
    (with-temp-file dtc-save-file
      (insert (prin1-to-string world-list))
      (message "Game saved successfully to %s" dtc-save-file)))
  (dtc-start-main-loop)) ; Restart the game loop after saving

(defun dtc-restore-game ()
  "Restore the game state from the save file."
  (interactive)
  (dtc-stop-main-loop) ; Stop the game loop during restore
  (if (file-exists-p dtc-save-file)
      (let ((saved-list (with-temp-buffer
                          (insert-file-contents dtc-save-file)
                          (read (current-buffer)))))
        (dtc-set-world-from-list saved-list) ; Convert plist back to hash table
        (message "Game state restored from %s" dtc-save-file)
        (dtc-render)
        (dtc-start-main-loop))
    (message "No save file found at %s" dtc-save-file)))

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

(defface dtc-friendly-face
  '((t (:foreground "blue" :weight bold)))
  "Face for the player character in my game."
  :group 'dtc) ; Replace my-game with your game's name

(defface dtc-ally-face
  '((t (:foreground "green" :weight bold)))
  "Face for a friendly guard character in my game."
  :group 'dtc)

(defface dtc-enemy-face
  '((t (:foreground "red" :weight bold)))
  "Face for the enemy character in my game."
  :group 'dtc)

(defun dtc-render ()
  "Render the entire world into the DTC buffer."
  (let* ((buf (get-buffer-create dtc-buffer-name))
         (w (dtc-get 'width))
         (h (dtc-get 'height))
         (player-pos (dtc-to-grid (dtc-get 'player)))
         )
    (message "Player: %S (Grid: %S)" (dtc-get 'player) player-pos)

    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)

        ;; Status
        (insert (format "Turn: %d | Score: %d"
                        (dtc-get 'turn)
                        (dtc-get 'score)))

        (when (dtc-get 'paused)
          (goto-char (point-max))
          (insert " -PAUSED-"))

        (insert "\n")

        ;; Loop through map grid
        (dotimes (y h)
          (dotimes (x w)

            (let* ((current-pos (cons x y))
                   ;; Find if any entity is at this position
                   (entity (cl-find current-pos (dtc-get 'entities)
                                    ;; :key (lambda (obj) (dtc-assoc-get 'pos obj))
                                    :key (lambda (ent) (dtc-to-grid (dtc-assoc-get 'pos ent)))
                                    :test 'dtc-pos-equal-p))
                   ;; Find if any feature is at this position
                   (feature (cl-find current-pos (dtc-get 'features)
                                     :key (lambda (f) (dtc-assoc-get 'pos f))
                                     :test 'dtc-pos-equal-p)))

              (cond
               ((dtc-pos-equal-p current-pos player-pos)
                (insert (propertize "@" 'font-lock-face 'dtc-player-face)))

               (entity
                (insert (propertize (dtc-assoc-get 'char entity)
                                    'font-lock-face (dtc-assoc-get 'face entity))))

               (feature
                (insert (propertize (dtc-assoc-get 'char feature)
                                    'font-lock-face (dtc-assoc-get 'face feature))))

               (t
                ;; (insert "."))
                (insert (propertize "." 'font-lock-face 'dtc-floor-face)))
               )))
          (insert "\n")))
      (goto-char (point-min))

      (setq buffer-read-only t)
      (dtc-mode 1)
      (display-buffer buf))
    ;; optionally return buffer
    buf))

  ;; ---------------------------------------------------------------------------
  ;; Interaction
  ;; ---------------------------------------------------------------------------
  (defun dtc-interact-at (pos)
    "Handle interaction when the player lands on an integer position POS."
    (let ((features (dtc-get 'features))
          (feature nil))

      ;; 1. Find the feature at the position
      (setq feature (cl-find pos features
                             :key (lambda (f) (dtc-assoc-get 'pos f))
                             :test 'dtc-pos-equal-p))

      (when feature
        (let ((type (dtc-assoc-get 'type feature)))
          (message "Interacted with a %S feature at %S" type pos)

          ;; Example: Pickup logic for items
          (when (eq type 'item)
            ;; Increment the score for picking up an item
            (dtc-set 'score (1+ (dtc-get 'score)))
            (message "Picked up an item! Score: %d" (dtc-get 'score))

            ;; Remove the feature from the world
            (dtc-set 'features (cl-remove feature features :test 'equal))
            t)))))

  ;; ---------------------------------------------------------------------------
  ;; Movement: player
  ;; ---------------------------------------------------------------------------
  (defun dtc-move-player (dx dy)
    "Attempt to move player by DX,DY. Return t if moved."
    (let* ((p (dtc-get 'player))
                         (nx (+ (car p) (* (float dx) dtc-unit-speed))) ; <--- Use float speed constant
                         (ny (+ (cdr p) (* (float dy) dtc-unit-speed)))
                         (new-pos (cons nx ny)))
      ;; Check collision against the new grid cell the player moves into
      (when (dtc-free-p (dtc-grid-x new-pos) (dtc-grid-y new-pos))
        (dtc-set 'player new-pos) ; Store the new float position
        (dtc-player-tick)
        t)))

  (defun dtc-move-up () "Move player up." (interactive) (dtc-move-player 0 -1))
  (defun dtc-move-down () "Move player down." (interactive) (dtc-move-player 0  1))
  (defun dtc-move-left ()  "Move player left." (interactive) (dtc-move-player -1 0))
  (defun dtc-move-right () "Move player right." (interactive) (dtc-move-player  1 0))

  ;; ---------------------------------------------------------------------------
  ;; Enemy movement (simple chase behaviour)
  ;; ---------------------------------------------------------------------------
  (defun dtc-move-towards (pos target-pos speed)
    "Calculate the next float position of POS moving SPEEP toward TARGET-POS.
The float SPEED determines how fast to move,"
    (let* ((dx (cond ((< (car pos) (car target-pos)) speed) ((> (car pos) (car target-pos)) (- speed)) (t 0.0)))
                         (dy (cond ((< (cdr pos) (cdr target-pos)) speed) ((> (cdr pos) (cdr target-pos)) (- speed)) (t 0.0)))
                         (nx (+ (car pos) dx))
                         (ny (+ (cdr pos) dy))
                         (new-pos (cons nx ny)))

      ;; Use dtc-free-p to check the *destination grid cell* for walls/player
      ;; We pass T for ignore-entities, as sub-cell movement doesn't use the grid for entity collision.
      (if (dtc-free-p (dtc-grid-x new-pos) (dtc-grid-y new-pos) t)
          new-pos
        pos))) ; Return original position if next grid cell is blocked

  (defun dtc-fly-towards (pos target-pos speed)
    "Calculate the next float position of POS at flying SPEEP toward TARGET-POS.
The float SPEED determines how fast to move,
Flying drones don't interact with the ground."
    (let* ((dx (cond ((< (car pos) (car target-pos)) speed)
                     ((> (car pos) (car target-pos)) (- speed))
                     (t 0.0)))
                         (dy (cond ((< (cdr pos) (cdr target-pos)) speed)
                   ((> (cdr pos) (cdr target-pos)) (- speed))
                   (t 0.0)))
                         (nx (+ (car pos) dx))
                         (ny (+ (cdr pos) dy))
                         (new-pos (cons nx ny)))
      ;; Isn't blocked by other entiries or features.
      new-pos
      ))

  (defun dtc-chase-logic (entity)
    "Entity AI: Move one step toward the player.
Details are stored in ENTITY."
    (let* ((old-pos (dtc-assoc-get 'pos entity))
                         (speed (dtc-assoc-get 'speed entity))
                         (player-pos (dtc-get 'player))
                         (new-pos (dtc-fly-towards old-pos player-pos speed)))
      (if (dtc-pos-equal-p old-pos new-pos)
          entity
        (dtc-assoc-set 'pos new-pos entity))))

  (defun dtc-patrol-logic (entity)
    "Entity AI: Move in a random direction if possible.
Details are stored in ENTITY"
    (let* ((old-pos (dtc-assoc-get 'pos entity))
                         (speed (dtc-assoc-get 'speed entity))
                         (dx (* speed (- (random 3) 1.0))) ; Use float for multiplication
                         (dy (* speed (- (random 3) 1.0)))
                         (nx (+ (car old-pos) dx))
                         (ny (+ (cdr old-pos) dy))
                         (new-pos (cons nx ny)))

      ;; Check collision using the new grid position
      (if (dtc-free-p (dtc-grid-x new-pos) (dtc-grid-y new-pos) t)
          (dtc-assoc-set 'pos new-pos entity)
        entity)))

  (defun dtc-update-entities ()
    "Iterate through all entities and execute their logic functions based on speed."
    (let ((updated-entities
           (mapcar (lambda (entity)
                     (let ((speed (dtc-assoc-get 'speed entity))
                                         (logic-fn (dtc-assoc-get 'logic entity)))

                                     ;; Ensure speed is a number (default to 1.0) and logic exists
                                     (unless (numberp speed)
                       (setq speed 1.0))

                                     ;; The logic function must return the updated object.
                                     ;; Truncate speed to get the whole number of steps (e.g., 1.5 -> 1 step).
                                     (dotimes (_ (truncate speed) entity)
                       (setq entity (funcall logic-fn entity))))
                     entity)
                                 (dtc-get 'entities))))

      (dtc-set 'entities updated-entities)))

  ;; ---------------------------------------------------------------------------
  ;; Timer Loop
  ;; ---------------------------------------------------------------------------
  (defvar dtc-timer nil "Holds the timer object for the main game loop.")
  (defvar dtc-tick-rate 0.5 "Seconds between automatic ticks (e.g., 0.5 for half a second).")

  (defun dtc-start-main-loop ()
    "Start the asynchronous game loop timer."
    (interactive)
    ;; Stop any existing timer first
    (dtc-stop-main-loop)

    ;; Create a new timer that runs dtc-auto-tick every dtc-tick-rate seconds.
    (setq dtc-timer (run-with-timer dtc-tick-rate dtc-tick-rate #'dtc-auto-tick)))

  (defun dtc-stop-main-loop ()
    "Stop the asynchronous game loop timer."
    (interactive)
    (when dtc-timer
      (cancel-timer dtc-timer)
      (setq dtc-timer nil)
      (message "DTC automatic loop stopped.")))

  (defun dtc-toggle-pause ()
    "Toggle the game's pause state by stopping or restarting the main timer loop."
    (interactive)
    (if dtc-timer
        (progn
          ;; Currently running, so pause it
          (dtc-stop-main-loop)
          (dtc-set 'paused t) ; Set flag for visual indicator
          (dtc-render)         ; Force a render to show the PAUSED message
          (message "Game PAUSED."))
      (progn
        ;; Currently paused (dtc-timer is nil), so resume it
        (dtc-start-main-loop)
        (dtc-set 'paused nil) ; Clear pause flag
        (dtc-render)
        (message "Game RESUMED."))))

  ;; ---------------------------------------------------------------------------
  ;; Tick/update loop
  ;; ---------------------------------------------------------------------------
  (defun dtc-auto-tick ()
    "Process automatic actions, run by the 'dtc-timer'."
    (with-current-buffer dtc-buffer-name
      ;; Temporarily disable read-only mode for the *DTC* buffer
      (let ((inhibit-read-only t))
        ;; (dtc-move-enemies)
        (dtc-update-entities)
        (dtc-set 'turn (1+ (dtc-get 'turn)))
        (dtc-render)
        ;; (dtc-check-win)
        )))

  (defun dtc-player-tick ()
    "Called after the player is successfully moved. Increments turn and triggers one entity update."
    (interactive)
    (dtc-interact-at (dtc-to-grid (dtc-get 'player)))

    (dtc-inc-turn) ; Only increment turn on player move
    (dtc-check-state)
    ;;(dtc-render)
    )

  (defun dtc-inc-turn ()
    "Incrument the turn counter."
    (dtc-set 'turn (1+ (dtc-get 'turn))))

  (defun dtc-check-state ()
    "Check basic win/lose conditions and print messages into the buffer."
    (let ((player-grid-pos (dtc-to-grid (dtc-get 'player))))

      ;; 1. Check for collision with hostile entities
      (dolist (entity (dtc-get 'entities))
        (when (and (dtc-pos-equal-p player-grid-pos (dtc-to-grid (dtc-assoc-get 'pos entity)))
                                 (member (dtc-assoc-get 'logic entity) '(dtc-chase-logic)))
          (dtc-show-message (format "💀 You were caught by a %s. Press q to quit."
                                    (dtc-assoc-get 'char entity)))
          (return)))))

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
      (dtc-start-main-loop) ;
      (goto-char (point-min))
      (message "Drone Theatre Command initialized in buffer %s" dtc-buffer-name)))

  (defun dtc-stop ()
    "Stop DTC and kill the buffer."
    (interactive)
    (dtc-stop-main-loop)
    (when (get-buffer dtc-buffer-name)
      (kill-buffer dtc-buffer-name)
      (message "DTC stopped.")))

  ;; ---------------------------------------------------------------------------
  ;; DTC world reset (safe and clean) -------------------------
  ;; ---------------------------------------------------------------------------
  (defun dtc-reset-world ()
    "Reset the Drone Theatre Command world to a fresh state."
    (interactive)
    (clrhash dtc-world)
    (dtc-init-world)
    (dtc-generate-basic-world)

    (when (fboundp 'dtc-render)
      (dtc-render))

    (message "DTC world reset: %dx%d" (dtc-get 'width) (dtc-get 'height)))

  ;; --- Run it immediately ---
  ;; (dtc-reset-world)

  ;; ---------------------------------------------------------------------------
  ;; Ensure symbols are available for users who load file in separate order
  ;; ---------------------------------------------------------------------------
  (provide 'dtc)
;;; dtc.el ends here
