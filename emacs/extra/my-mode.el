;; Source: http://nullprogram.com/blog/2013/02/06    
;;;; My minor mode
;; Main use is to have my key bindings have the highest priority

(defvar my-mode-map (make-sparse-keymap)
  "Keymap while my-mode is active.")

;;;###autoload
;; (define-minor-mode my-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   nil
;;   my-mode-map)
(define-minor-mode my-mode
  "Global minor mode"
  t " Gl" my-mode-map)
;; :global 1)

;; Source: http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-mode))
      (let ((mykeys (assq 'my-mode minor-mode-map-alist)))
        (assq-delete-all 'my-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

; * functions
;;;###autoload
(defun my/kill-buffer()
  (interactive)
	  (kill-buffer (current-buffer))
    (if (> (count-windows) 1)(delete-window)))

;;;###autoload
(defun complete-indent-fold-flyspell ()
  (interactive)
  (if mark-active (indent-for-tab-command)
	(if (and (equal major-mode 'org-mode)
		(or (looking-at org-outline-regexp)
			(looking-at org-block-regexp)
			(looking-at org-drawer-regexp))) (org-cycle)
  (if (looking-at outline-regexp)
    (my/outline-cycle)
    (if (looking-at "\\_>") (company-complete)
      (if (string-match "^[[:space:]]*$" (buffer-substring-no-properties
            (line-beginning-position) (line-end-position)))
          ; line with all spaces
          (insert-char (string-to-char " ") tab-width)
	      (if (string-match "^[[:space:]]*$"
          	(buffer-substring-no-properties (line-beginning-position) (point)))
              ; at the beginning of a non all space line
            (indent-for-tab-command)
	        (if (and (bound-and-true-p flyspell-mode) (not (flyspell-word)))
                ; run flyspell-word at point
	          (helm-flyspell-correct) (insert "\t"))
	        )))))))
;;;###autoload
(defun my/toggle-fill-paragraph ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;; (global-set-key [remap fill-paragraph]
;;                 #'endless/fill-or-unfill)

;;;###autoload
(defun my/home ()
  "Move to the beginning of the current line on the first key stroke,
and to the beginning of the buffer if there is a second key stroke
within `my-double-key-timeout' seconds."
  (interactive)
  (let ((last-called (get this-command 'my-last-call-time)))
    (if (and (eq last-command this-command)
             last-called
             (<= (time-to-seconds (time-since last-called))
                 my-double-key-timeout))
        (beginning-of-buffer)
      (move-beginning-of-line nil)))
  (put this-command 'my-last-call-time (current-time)))

;;;###autoload
(defun sanityinc/newline-at-end-of-line ()
"Move to end of line, enter a newline, and reindent."
(interactive)
(move-end-of-line 1)
(newline-and-indent))

;;;###autoload
(defun my/backspace()
  (interactive)
  (if (string-match "[^[:space:]]+"
                    (buffer-substring-no-properties
     (line-beginning-position) (point))) ;; return nil no match
      (backward-kill-word 1)(evil-shift-left-line 1)))

;;;###autoload
(defun my/eval()
  (interactive)
  (if (region-active-p)
      (eval-region (region-beginning) (region-end) t)
    (call-interactively #'eval-last-sexp)))

;;;###autoload
(defun my/goto-line-beginning-or-indent (&optional $position)
  (interactive)
  (or $position (setq $position (point)))
  (let (($starting-position (progn (back-to-indentation) (point))))
    (if (eq $starting-position $position)
      (move-beginning-of-line 1))))

;;;###autoload
(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))

;;;###autoload
(defun switch-to-scratch-and-back (&optional arg)
  "Toggle between *scratch-MODE* buffer and the current buffer.
If a scratch buffer does not exist, create it with the major mode set to that
of the buffer from where this function is called.
        COMMAND -> Open/switch to a scratch buffer in the current buffer's major mode
    C-0 COMMAND -> Open/switch to a scratch buffer in `fundamental-mode'
    C-u COMMAND -> Open/switch to a scratch buffer in `org-mode'
C-u C-u COMMAND -> Open/switch to a scratch buffer in `emacs-elisp-mode'
Return the scratch buffer opened."
  (interactive "p")
  (if (and (or (null arg)               ; no prefix
               (= arg 1))
           (string-match-p "\\*scratch" (buffer-name)))
      (switch-to-buffer (other-buffer))
    (let* ((mode-str (cl-case arg
                       (0  "fundamental-mode") ; C-0
                       (4  "org-mode") ; C-u
                       (16 "emacs-lisp-mode") ; C-u C-u
                       ;; If the major mode turns out to be a `special-mode'
                       ;; derived mode, a read-only mode like `help-mode', open
                       ;; an `org-mode' scratch buffer instead.
                       (t (if (or (derived-mode-p 'special-mode) ; no prefix
                                  (derived-mode-p 'dired-mode)
                                  (derived-mode-p 'term-mode)
                                  (derived-mode-p 'exwm-mode))
                              "org-mode"
                            (format "%s" major-mode)))))
           (buf (get-buffer-create (concat "*scratch-" mode-str "*"))))
      (switch-to-buffer buf)
      (funcall (intern mode-str))   ; http://stackoverflow.com/a/7539787/1219634
      buf)))

;;;###autoload
(defun turn-on-my-mode ()
  "Turns on my-mode."
  (interactive)
  (my-mode t))

;;;###autoload
(defun turn-off-my-mode ()
  "Turns off my-mode."
  (interactive)
  (my-mode -1))

;;;###autoload
(define-globalized-minor-mode global-my-mode my-mode turn-on-my-mode)

;;;; my functions
;;;###autoload
(defun my/yank()
  (interactive)
 (if (eq last-command 'yank) (yank-pop) (yank)))
;;;; term
(defun last-term-buffer (l)
  "Return most recently used term buffer."
  (when l
    (if (eq 'term-mode (with-current-buffer (car l) major-mode))
	(car l) (last-term-buffer (cdr l)))))
;; (defun get-term ()
;;   "Switch to the term buffer last used, or create a new one if
;;     none exists, or if the current buffer is already a term."
;;   (interactive)
;;   (let ((b (last-term-buffer (buffer-list))))
;;     (if (or (not b) (eq 'term-mode major-mode))
;; 	(ansi-term (getenv "SHELL"))
;;       (switch-to-buffer b))
;;  (get-buffer-process b)))

;; (defun start-or-switch-to (function buffer-name)
;;   "Invoke FUNCTION if there is no buffer with BUFFER-NAME.
;; Otherwise switch to the buffer named BUFFER-NAME.  Don't clobber
;; the current buffer."
;;   (if (not (get-buffer buffer-name))
;;       (progn
;;         ;; (split-window-sensibly (selected-window))
;;         ;; (other-window 1)
;;         (funcall function))
;;     (switch-to-buffer buffer-name)))

;; (defun visit-term-buffer ()
;;   "Create or visit a terminal buffer."
;;   (interactive)
;;   (start-or-switch-to (lambda ()
;;                          (ansi-term (getenv "SHELL")))
;;                       "*ansi-term*"))

;; (defun visit-ielm ()
;;   "Switch to default `ielm' buffer.
;; Start `ielm' if it's not already running."
;;   (interactive)
;;   (prelude-start-or-switch-to 'ielm "*ielm*"))
;; (defun comment-dwim-line (&optional arg)
;; "Replacement for the comment-dwim command.
;; If no region is selected and current line is not blank and we are not at the end of the line,
;; then comment current line.
;; Replaces default behaviour of comment-dwim, when it inserts comment at the end of the line."
;; (interactive "*P")
;; (comment-normalize-vars)
;; (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
;; (progn
;; (comment-or-uncomment-region (line-beginning-position) (line-end-position))
;; (next-line))
;; (comment-dwim arg)))

;;;; outline-minor-mode

(defvar outline-hs-toggle nil
  "Keeps the state of how the buffer was last toggled by TABing.")

;; hide show all
;;;###autoload
(defun outline-cycle-all()
  (interactive)
  (if outline-hs-toggle
      (show-all) (hide-body))
  (setq outline-hs-toggle (not outline-hs-toggle)))

;;;###autoload
(defun shift-region(numcols)
" my trick to expand the region to the beginning and end of the area selected
 much in the handy way I liked in the Dreamweaver editor."
  (if (< (point)(mark))
    (if (not(bolp))    (progn (beginning-of-line)(exchange-point-and-mark) (end-of-line)))
    (progn (end-of-line)(exchange-point-and-mark)(beginning-of-line)))
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil)
	  )))

;;;###autoload
(defun my/backtab()
  (interactive) 
  (if mark-active (shift-region (- tab-width))
    (if (equal major-mode 'org-mode) (org-shifttab)
      (outline-cycle-all))
  ))

(define-key my-mode-map [backtab] 'my/backtab)
(define-key my-mode-map (kbd "S-<backspace>") 'evil-shift-left-line)
;;;; misc

;; (global-unset-key (kbd "C-1"))
;; (define-key my-mode-map (kbd "C-1") 'get-term)
(require 'comment-dwim-2)
(global-unset-key (kbd "M-;"))
(define-key my-mode-map (kbd "M-;") 'comment-dwim-2)

;;;###autoload
(defun copy_work_path()
  (interactive)
  (kill-new default-directory)
  (message "Current directory path Copied"))


;;;; tab
(defvar my-double-key-timeout 0.25
  "The number of seconds to wait for a second key press.")

;;;###autoload
(defun my/tab ()
  "Move to the beginning of the current line on the first key stroke,
and to the beginning of the buffer if there is a second key stroke
within `my-double-key-timeout' seconds."
  (interactive)
  (let ((last-called (get this-command 'my-last-call-time))
       ;; (is-term (derived-mode-p 'term-mode)
       (is-term (string= "term-mode" major-mode)))
       (if (and is-term (term-in-char-mode))
         (term-send-raw-string "\t")
         (if (and (eq last-command this-command)
	  (<= (time-to-seconds (time-since last-called))
	  my-double-key-timeout))
	  (yas-expand)
	  (if (sit-for my-double-key-timeout)
	    (if (bound-and-true-p iedit-mode) (iedit-next-occurrence)
            (complete-indent-fold-flyspell)))))
    (put this-command 'my-last-call-time (current-time))))

(define-key my-mode-map (kbd "<tab>") 'my/tab)
; todo don't change' tab for evil normal mode

;; Turn off the minor mode in the minibuffer
(add-hook 'minibuffer-setup-hook 'turn-off-my-mode)

(provide 'my-mode)

;;; my-mode ends here
