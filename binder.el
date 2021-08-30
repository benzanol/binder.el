;;; Define the minor mode and variables
(define-minor-mode qv/keystate-mode
  "Adjust the current keymap for different functions"
  nil
  (qv/keystate-hook))

(defun qv/keystate-mode (arg)
  (setq myvar arg)
  (if (not qv/keystate-mode)
	  (progn
		(use-global-map global-map)
		(message "Modal editing disabled"))

	;; Update the keystate-mode variable
	(if (and (symbolp arg) (boundp arg))
		(setq qv/keystate-mode arg)
	  (if (and (symbolp qv/default-keystate) (boundp qv/default-keystate))
		  (setq qv/keystate-mode qv/default-keystate)
		(setq qv/keystate-mode nil)
		(error "No valid keystate, not enabling.")))
	(message "Keystate %s activated" qv/keystate-mode)

	;; Set up the minor mode keymap
	(use-global-map
	 (qv/generate-keystate-map
	  (symbol-value qv/keystate-mode)
	  global-map))))

(defvar qv/default-keystate nil
  "Keystate to use when entering keystate mode without
specifying a keystate")

;;; Define functions for managing keystates
(defun qv/add-keymap-prefix (keymap prefix &optional all-levels)
  "Return a new keymap containing each of the keys
in `KEYMAP`modified by `PREFIX`.

`PREFIX` should be a string to add before the
string representation of a keymap to modify it,
for example \"M-\" would add the meta modifier
to a key sequence."
  (when (symbolp keymap) (setq keymap (symbol-value keymap)))
  (unless (keymapp keymap) (error "Not a valid keymap: %s" keymap))
  ;; Make the new keymap the same type as the input keymap
  (let ((new-map (if (char-table-p (cadr keymap))
					 (make-keymap)
				   (make-sparse-keymap))))
	;; If `PREFIX` doesn't end in a space or dash, add a space
	(setq prefix (replace-regexp-in-string "[^ -]$" "\\& " prefix))
	;; Add each modified binding to the new keymap
	(map-keymap
	 (lambda (key binding)
	   (define-key new-map
		 (if (and (vectorp key) (eq (aref key 0) 'remap)) key
		   (kbd (concat prefix (key-description
								(if (vectorp key)
									key (vector key))))))
		 (if (and all-levels (keymapp binding))
			 (qv/add-keymap-prefix binding prefix t)
		   binding)))
	 keymap)
	new-map))

(defun qv/generate-keystate-map (keymaps &optional parent)
  "Return a composed keymap made up of the keymaps
and modifiers specified.

An entry is either a keymap, or a list containing
arguments for `qv/add-keymap-prefix`, specifically
`(KEYMAP PREFIX &optional ALL-LEVELS)`"
  (make-composed-keymap
   (mapcar
	(lambda (arg-list)
	  (cond ((keymapp arg-list) arg-list)
			((symbolp arg-list) (symbol-value arg-list))
			((listp arg-list) (apply 'qv/add-keymap-prefix arg-list))
			(t nil)))
	keymaps)
   (when parent
	 (if (keymapp parent) parent
	   (apply 'qv/add-keymap-prefix parent)))))

(defmacro qv/define-keystate (name &rest keymaps)
  (declare (indent 1))
  (let ((confirm-string "%s `%s` exists, do you want to overwrite it?"))
	(when (and (boundp name)
			   (not (y-or-n-p (format confirm-string "Variable" name))))
	  (user-error "Variable exists, did not create operator `%s`" name))
	(when (and (boundp name)
			   (not (y-or-n-p (format confirm-string "Function" name))))
	  (user-error "Function exists, did not create operator `%s`" name)))

  `(progn
	 (setq ,name ',keymaps)
	 (defun ,name ()
	   (interactive)
	   (qv/keystate-mode ',name))
	 (when (eq qv/keystate-mode ',name)
	   (qv/keystate-mode qv/keystate-mode))))

(defmacro qv/define-keymap (name sparse &rest keys)
  (declare (indent 2))
  `(progn
	 (setq ,name ,(if sparse '(make-sparse-keymap) '(make-keymap)))
	 (dolist (x ',keys)
	   (define-key ,name
		 (if (stringp (car x)) (kbd (car x)) (car x))
		 (cdr x)))))

;;; Define functions for managing motions and operators
(defmacro defoperator (name extra-motions &rest body)
  "Define a function called NAME, with two arguments, `beg` and `end`.

If a visual selection is active when the function is called, `beg`
and `end` will be the point and mark. Otherwise, the user will be
prompted for a motion. If this motion returns a cons cell, the car
and cdr will be `beg` and `end`. Otherwise, the position of the
point before and after running the motion will be `beg` and `end`.

EXTRA-MOTIONS should either be a keymap, a cons cell of the form
`(KEY . BINDING)`, or a symbol holding either. It can also be a
list of any number of said elements. These keys and keymaps will
be used as additional options for motions.

BODY should be made up of lisp expressions which use the values of
`beg` and `end` to perform an operation on the text between them.

Note that `beg` will sometimes come after `end`; this is intended.
They represent the beginning and end of the motion, not the
beginning and end of the text in respect to the document."

  (declare (indent 2))
  (when (and (fboundp name)
			 (not (y-or-n-p
				   (format
					"Function `%s` exists, do you want to overwrite it?"
					name))))
	(user-error "Function exists, did not create operator `%s`" name))

  ;; Macro expansion: the operator function definition
  `(defun ,name (beg end)
	 (interactive
	  (if mark-active (list (mark) (point))

		;; Compose EXTRA-MOTIONS into a single keymap
		(let ((motion-list ',extra-motions)
			  (extra-map (make-sparse-keymap)))
		  (when (or (keymapp motion-list) (not (listp motion-list))) 
			(setq motion-list (list motion-list)))
		  (dolist (x motion-list)
			(when (symbolp x) (setq x (eval x)))
			(cond ((keymapp x)
				   (setq extra-map (make-composed-keymap
									(list extra-map x))))
				  ((consp x) (define-key extra-map
							   (if (stringp (car x)) (kbd (car x)) (car x))
							   (cdr x)))))
		  ;; Read and execute the operator
		  (let* ((prev-point (point))
				 (prev-buffer (current-buffer))
				 (minor-mode-map-alist
				  (cons (cons t extra-map)
						minor-mode-map-alist))
				 (motion (key-binding (read-key-sequence "Motion: ")))
				 (output (and motion (call-interactively motion))))
			(message "%s : %s" motion output)
			(if (eq prev-buffer (current-buffer))
				(if (consp output) (list (car output) (cdr output))
				  (list prev-point (point)))
			  (user-error "Invalid motion `%s`, unable to run operator `%s`" motion ',name))))))
	 (unless (numberp beg) (error "`BEG` not a valid number: `%s`" beg))
	 (unless (numberp end) (error "`END` not a valid number: `%s`" end))

	 ;; Execute the operator specific code
	 ,(cons 'progn body)))

(defmacro defmotion (name &rest body)
  "Define a motion with the name NAME.

If you intend to define a motion that starts at the current point,
you do not need this function; operators will interperet any
function which moves the point as a motion. For example, \"around
sexp\" would use this function but \"forward sexp\" would not.

BODY should be made up of lisp expressions which return a cons cell
with the car as the beginning of the motion, and the cdr as the end
of the motion. The rest of the logic will be handled automatically."

  (declare (indent 1))
  (when (and (fboundp name)
			 (not (y-or-n-p
				   (format
					"Function `%s` exists, do you want to overwrite it?"
					name))))
	(user-error "Function exists, did not create operator `%s`" name))

  `(defun ,name ()
	 (interactive)
	 (let ((motion ,(cons 'progn body)))
	   (when mark-active
		 (push-mark (car motion))
		 (goto-char (cdr motion)))
	   motion)))

;;; Define extra editing commands
;;;; Motions
(defmotion qv/in-line
  (cons (line-beginning-position)
		(line-end-position)))

(defmotion qv/whole-line
  (cons (line-beginning-position)
		(min (1+ (line-end-position)) (point-max))))

;;;; Operators
(defoperator qv/copy
	(qv/motion-keymap
	 ("y" . qv/in-line))
  (kill-ring-save beg end))

(defoperator qv/kill
	(qv/motion-keymap
	 ("s" . qv/in-line))
  (kill-region beg end))

(defoperator qv/delete
	(qv/motion-keymap
	 ("d" . qv/in-line))
  (delete-region beg end))

(defoperator qv/change
	(qv/motion-keymap
	 ("c" . qv/in-line))
  (delete-region beg end)
  (qv/keystate-mode 'insert))

;;;; Jumping to characters
(defun qv/forward-find-letter ()
  (interactive)
  (search-forward (string (read-char "Forward find letter: "))
				  (line-end-position) t)
  (backward-char 1))
(defun qv/forward-to-letter ()
  (interactive)
  (search-forward (string (read-char "Forward to letter: "))
				  (line-end-position) t)
  (backward-char 2))

(defun qv/backward-find-letter ()
  (interactive)
  (search-forward (string (read-char "Backward find letter: "))
				  (line-beginning-position) t))
(defun qv/backward-to-letter ()
  (interactive)
  (search-forward (string (read-char "Backward to letter: "))
				  (line-beginning-position) t)
  (forward-char 1))
;;; Keymaps
(qv/define-keymap qv/text-map t
  ("h" . backward-char)
  ("j" . next-line)
  ("k" . previous-line)
  ("l" . forward-char)
  ("e" . forward-word)
  ("w" . forward-to-word)
  ("b" . backward-word)
  ("f" . qv/forward-find-letter)
  ("F" . qv/backward-find-letter)
  ("t" . qv/forward-to-letter)
  ("T" . qv/backward-to-letter)
  ("y" . qv/copy)
  ("s" . qv/kill)
  ("d" . qv/delete)
  ("c" . qv/change)
  ("p" . yank))

(qv/define-keymap qv/motion-map t
  ("a l" . qv/whole-line)
  ("i l" . qv/in-line))

(qv/define-keymap qv/action-map t
  ("x" . execute-extended-command)
  ("e" . eval-expression)
  (";" . eval-expression)
  ("b" . switch-to-buffer)
  ("w" . delete-window)
  ("W" . kill-buffer)
  ("f" . find-file))

(qv/define-keymap qv/window-map t
  ("h"   . windmove-left)
  ("j"   . windmove-down)
  ("k"   . windmove-up)
  ("l"   . windmove-right)
  ("C-h" . windmove-swap-left)
  ("C-j" . windmove-swap-down)
  ("C-k" . windmove-swap-up)
  ("C-l" . windmove-swap-right)
  ("H"   . (lambda () (interactive) (select-window (split-window nil nil 'left))))
  ("J"   . (lambda () (interactive) (select-window (split-window nil nil 'down))))
  ("K"   . (lambda () (interactive) (select-window (split-window nil nil 'up))))
  ("L"   . (lambda () (interactive) (select-window (split-window nil nil 'right)))))


;;; Keystates
(qv/define-keystate qv/normal
  qv/text-map
  (qv/action-map "A-")
  (qv/window-map "A-"))

(qv/define-keystate qv/insert
  (qv/text-map "A-"))

(defmacro cmd (&rest body)
  `(lambda ()
	 (interactive)
	 . ,body))

