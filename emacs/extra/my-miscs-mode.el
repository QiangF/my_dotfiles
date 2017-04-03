(defvar my-miscs-mode-map (make-sparse-keymap)
  "Keymap while my-miscs-mode is active.")

;;;###autoload
(define-minor-mode my-miscs-mode
  "Global minor mode for collecting defer loading code"
  t " Gl" my-miscs-mode-map)

; * deferred
(which-function-mode 1)
(show-paren-mode 1)
(column-number-mode 1)
(electric-pair-mode 1)

(provide 'my-miscs-mode)

;;; my-miscs-mode ends here
