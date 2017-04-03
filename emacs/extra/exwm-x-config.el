;;; exwm-x-example.el --- a example configure of exwm-x
;; ** Configure
;; Create ~/.xinitrc file or ~/.xsession file which content likes
;; the below example:

;; #+BEGIN_EXAMPLE
;; # You may need to comment out the next line to disable access control
;; xhost +

;; # Kill beep
;; xset b off

;; # Tell emacs load exwm-x-example.el file's content.
;; export exwm_x_enable="yes"

;; # Emacs X input method (exim) setting
;; export XMODIFIERS=@im=exim
;; export GTK_IM_MODULE=xim
;; export QT_IM_MODULE=xim
;; export CLUTTER_IM_MODULE=xim

;; # Launch exwm
;;  exec dbus-launch --exit-with-session emacs

;; #+END_EXAMPLE

;; ** Run exwm
;; Run startx command or login with display-manager

;;; Code:

;; * Code                                                                 :code:

(use-package exwm-x
  :if (string= (getenv "exwm_x_enable") "yes")
  :ensure t
  :demand t
  :config

  ;; Disable dialog boxes since they are unusable in EXWM
  (setq use-dialog-box nil)

  ;; Set floating window border
  (setq exwm-floating-border-width 3)
  (setq exwm-floating-border-color "orange")

  ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
  ;; when a new window class name or title is available.
  ;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
  (add-hook 'exwm-update-class-hook #'exwm-x-rename-exwm-buffer)
  (add-hook 'exwm-update-title-hook #'exwm-x-rename-exwm-buffer)

  (defun exwm-x-rename-exwm-buffer ()
    (exwm-workspace-rename-buffer
     (concat "Exwm:" (exwm-x--get-prefer-name))))

  (defun exwm-x/suspend-computer ()
    (interactive)
    (exwm-x-run-shell-command "systemctl suspend"))

  (defun exwm-x/restart-computer ()
    (interactive)
    (exwm-x-run-shell-command "systemctl reboot"))

  (defun exwm-x/shutdown-commputer ()
    (interactive)
    (exwm-x-run-shell-command "systemctl poweroff"))

  (defun exwm-x/firefox ()
    (interactive)
    (exwm-x-jump-or-exec "Firefox" "firefox" "网"))

  (defun exwm-x/file-manager ()
    (interactive)
    (exwm-x-jump-or-exec "Nautilus" "nautilus --no-desktop" "文"))

  (defun exwm-x/virtualbox ()
    (interactive)
    (exwm-x-jump-or-exec "VirtualBox" "virtualbox" "VBox"))

  (defun exwm-x/htop ()
    (interactive)
    (exwm-x-jump-or-exec "htop" "xfce4-terminal -T htop -e htop" "Top"))

  (defun exwm-x/terminal ()
    (interactive)
    (exwm-x-jump-or-exec "default-terminal" "xfce4-terminal -T default-terminal" "终"))

  (defun exwm-x/new-terminal ()
    (interactive)
    (exwm-x-run-shell-command "xfce4-terminal"))

  (defun exwm-x/power-manager-settings ()
    (interactive)
    (exwm-x-run-shell-command "xfce4-power-manager-settings"))

  (defun exwm-x/power-manager ()
    (interactive)
    (exwm-x-run-shell-command "xfce4-power-manager"))

  (defun exwm-x/volit ()
    (interactive)
    ; contrl volume in system tray
    (exwm-x-run-shell-command "volti"))

  (defun exwm-x/lock-screen ()
    (interactive)
    (exwm-x-run-shell-command "exec xscreensaver-command -lock"))

  ;; The following example demonstrates how to set a key binding only available
  ;; in line mode. It's simply done by first push the prefix key to
  ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
  ;; The example shorten 'C-c q' to 'C-q'.
  (push ?\C-q exwm-input-prefix-keys)
  (push ?\C-\\ exwm-input-prefix-keys)
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; The following example demonstrates how to use simulation keys to mimic the
  ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
  ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
  ;; DEST is what EXWM actually sends to application. Note that SRC must be a key
  ;; sequence (of type vector or string), while DEST can also be a single key.

  ;; Debian menu
  (exwm-x-generate-debian-menus)
)
(provide 'exwm-x-configl)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; exwm-x-config.el ends here
