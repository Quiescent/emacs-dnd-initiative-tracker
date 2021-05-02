;;; initiative --- An initiative tracker for D&D -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defvar initiative-combatants '()
  "A list of combantants.

Form: '((NAME INITIATIVE HEALTH ENEMY) ...).")

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
  (progn
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert "Initiative\n")
    (insert "==========\n\n")
    (let ((index 0))
      (mapc (lambda (combatant) (pcase combatant
                                  (`(,name ,initiative ,health ,enemy)
                                   (progn
                                     (insert (format "[%s] %s (%s):\t%s\t"
                                                     (if enemy "E" "P")
                                                     name
                                                     initiative
                                                     health))
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
         (combatant  (list name initiative health enemy)))
    (push (list 'initiative-enter combatant) initiative-history)
    (setf initiative-combatants (thread-first
                                    (cons combatant initiative-combatants)
                                  (copy-seq)
                                  (sort (lambda (this that) (> (cadr this) (cadr that))))))
    (initiative-draw)))

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
      (while (<= (nth 2 (nth initiative-current-turn initiative-combatants)) 0)
        (when (>= count (length initiative-combatants))
          (error "Could not find a next combatant"))
        (cl-incf count)
        (advance))))
  (initiative-draw))

(defun initiative-damage ()
  "Damage a combatant."
  (interactive)
  (let* ((name      (read-string "Name: "))
         (damage    (read-number "Damage: "))
         (combatant (find name initiative-combatants :test #'string-equal :key #'car)))
    (if (null combatant)
        (error (format "Could not find %s" name))
      (push (list 'damage name damage) initiative-history)
      (cl-decf (nth 2 combatant) damage)))
  (initiative-draw))

(define-derived-mode initiative special-mode "Initiative"
  "A special mode to track combatants, health and initiative in D&D.

Key bindings:
 - e: enter initiative.
 - SPC: advance initiative.
 - d: damage.")

(define-key initiative-map (kbd "e")   #'initiative-enter)
(define-key initiative-map (kbd "SPC") #'initiative-advance)
(define-key initiative-map (kbd "d")   #'initiative-damage)

(provide 'initiative)
;;; initiative ends here
