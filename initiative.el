;;; initiative --- An initiative tracker for D&D -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defvar initiative-combatants '()
  "A list of combantants.

Form: '((NAME INITIATIVE HEALTH ENEMY SAVES FAILS STABLE DEAD) ...).")

(make-variable-buffer-local 'initiative-combatants)

(defvar initiative-history '()
  "A history of events which happened during combat.")

(make-variable-buffer-local 'itiniative-history)

(defvar initiative-current-turn 0
  "The position of the current combatant in `initiative-combatants'.")

(defconst initiative-buffer-name "*initiative*"
  "The name of the buffer where initiative is displayed.")

(defun initiative-reset ()
  "Reset the state of this initiative session."
  (setf initiative-combatants   nil
        initiative-history      nil
        initiative-current-turn 0))

(defun initiative-track ()
  "Begin tracking the names of all combatants and their initiatives."
  (interactive)
  (progn
    (switch-to-buffer (get-buffer-create initiative-buffer-name))
    (initiative)
    (initiative-reset)
    (initiative-draw)))

(defun initiative-draw ()
  "Draw the current state of `initiative-combatants' to the current buffer."
  (interactive)
  (progn
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert "Initiative\n")
    (insert "==========\n\n")
    (let ((index 0))
      (mapc (lambda (combatant) (pcase combatant
                                  (`(,name ,initiative ,health ,enemy ,saves ,fails ,stable ,dead)
                                   (progn
                                     (insert (format "[%s] %s (%s):\t%s%s\t"
                                                     (if enemy "E" "P")
                                                     name
                                                     initiative
                                                     health
                                                     (if (and (not enemy) (<= health 0))
                                                         (format " (Saves: %s, Fails: %s)" saves fails)
                                                       "")))
                                     (when (and stable (= health 0))
                                       (insert "[Stable]"))
                                     (when dead
                                       (insert "Dead"))
                                     (when (= index initiative-current-turn)
                                       (insert " <--"))
                                     (insert "\n")
                                     (cl-incf index)))))
            initiative-combatants))))

(defun initiative-enter ()
  "Add a combatants to initiative."
  (interactive)
  (let* ((name       (read-string "Name: "))
         (initiative (read-number "Initiative: "))
         (health     (read-number "Health: "))
         (enemy      (y-or-n-p "Enemy: "))
         (combatant  (list name initiative health enemy 0 0 nil nil)))
    (push (list 'initiative-enter combatant) initiative-history)
    (setf initiative-combatants (thread-first
                                    (cons combatant initiative-combatants)
                                  (cl-copy-seq)
                                  (sort (lambda (this that) (> (cadr this) (cadr that))))))
    (initiative-draw)))

(defun initiative-current-combatant ()
  "Produce the current combatant."
  (nth initiative-current-turn initiative-combatants))

(defun initiative-advance ()
  "Advances the initiative order."
  (interactive)
  (push 'initiative-advance initiative-history)
  (cl-labels
      ((advance ()
                (setf initiative-current-turn (mod (1+ initiative-current-turn)
                                                   (length initiative-combatants)))))
    (advance)
    (let ((count 0))
      (while (or (and (nth 3 (initiative-current-combatant))
                      (<= (nth 2 (initiative-current-combatant)) 0))
                 (and (not (nth 3 (initiative-current-combatant)))
                      (nth 7 (initiative-current-combatant))))
        (when (>= count (length initiative-combatants))
          (error "Could not find a next combatant"))
        (cl-incf count)
        (advance))))
  (initiative-draw))

(defun initiative-fail-save ()
  "Fail the death save for the current player."
  (interactive)
  (let ((combatant (initiative-current-combatant)))
    (when (nth 3 combatant) (error "Current player is an enemy"))
    (when (nth 6 combatant) (error "Current player is stable"))
    (when (nth 7 combatant) (error "Current player is dead"))
    (push (list 'fail-save (car combatant)) initiative-history)
    (cl-incf (nth 5 combatant))
    (when (= (nth 5 combatant) 3) (setf (nth 7 combatant) t))
    (initiative-draw)))

(defun initiative-succeed ()
  "Succeed on a death save for the curretn player."
  (interactive)
  (let ((combatant (initiative-current-combatant)))
    (when (nth 3 combatant) (error "Current player is an enemy"))
    (when (nth 6 combatant) (error "Current player is stable"))
    (when (nth 7 combatant) (error "Current player is dead"))
    (push (list 'succeed-save (car combatant)) initiative-history)
    (cl-incf (nth 4 combatant))
    (when (= (nth 4 combatant) 3) (setf (nth 6 combatant) t))
    (initiative-draw)))

(defun initiative-damage ()
  "Damage a combatant."
  (interactive)
  (let* ((name      (completing-read "Name: " (mapcar #'car initiative-combatants)))
         (damage    (read-number "Damage: "))
         (combatant (cl-find name initiative-combatants :test #'string-equal :key #'car)))
    (if (null combatant)
        (error (format "Could not find %s" name))
      (push (list 'damage name damage) initiative-history)
      (cl-decf (nth 2 combatant) damage)
      (setf (nth 2 combatant) (max 0 (nth 2 combatant)))
      (when (and (= 0 (nth 2 combatant)) (nth 3 combatant))
        (setf (nth 7 combatant) t))))
  (initiative-draw))

(defun initiative-heal ()
  "Heal a combatant."
  (interactive)
  (let* ((name      (completing-read "Name: " (mapcar #'car initiative-combatants)))
         (health    (read-number "Health: "))
         (combatant (cl-find name initiative-combatants :test #'string-equal :key #'car)))
    (if (null combatant)
        (error (format "Could not find %s" name))
      (push (list 'heal name health) initiative-history)
      (cl-incf (nth 2 combatant) health)
      (setf (nth 4 combatant) 0
            (nth 5 combatant) 0
            (nth 6 combatant) nil
            (nth 7 combatant) nil)))
  (initiative-draw))

(define-derived-mode initiative special-mode "Initiative"
  "A special mode to track combatants, health and initiative in D&D.

Key bindings:
 - e: enter initiative.
 - SPC: advance initiative.
 - d: damage.
 - s: succeed at a saving throw
 - f: fail at a saving throw
 - h: heal a player")

(define-key initiative-map (kbd "e")   #'initiative-enter)
(define-key initiative-map (kbd "SPC") #'initiative-advance)
(define-key initiative-map (kbd "d")   #'initiative-damage)
(define-key initiative-map (kbd "s")   #'initiative-succeed)
(define-key initiative-map (kbd "f")   #'initiative-fail-save)
(define-key initiative-map (kbd "h")   #'initiative-heal)
(define-key initiative-map (kbd "g")   #'initiative-draw)

(provide 'initiative)
;;; initiative ends here
