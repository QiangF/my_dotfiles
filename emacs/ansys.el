; * ansys
; ** tips
;; .mac is the macro suffix of ANSYS i. e. these macros can be called
;; in the ANSYS command prompt like a regular ANSYS function (without
;; the suffix .mac)
;; .dat and .inp are WorkBench's solver input file suffixes
;; (add-to-list 'auto-mode-alist '("\\.dat\\'" . ansys-mode))
;; (add-to-list 'auto-mode-alist '("\\.inp\\'" . ansys-mode)) ; abaqus mode
;; .anf is the suffix for "ANSYS Neutral" files which include mostly
;;  gometric data but also some APDL snippets.

(use-package ansys-mode
  :load-path "extra/ansys/"
  :preface
    (add-hook 'ansys-mode-hook 'ansys-outline-minor-mode) ;enable outlining
  :init
    (autoload 'ansys-mode "ansys-mode" nil t)
    (autoload 'ansys-customise-ansys "ansys-mode" "Activate the function for 
    calling a special ANSYS customisation buffer." 'interactive)
    (autoload 'ansys-abort-file "ansys-mode" "Activate the function for     aborting ANSYS runs." 'interactive)
    (autoload 'ansys-display-error-file "ansys-mode" "Activate the function for inspecting the ANSYS error file." 'interactive)
    (autoload 'ansys-start-ansys-help "ansys-mode" "Activate the function for starting the ANSYS help browser." 'interactive)
    (autoload 'ansys-start-ansys "ansys-mode" "Activate the function for starting the APDL interpreter under GNU-Linux or product launcher under Windows." 'interactive)
  :mode
    (("\\.mac\\'" . ansys-mode)("\\.anf$" . ansys-mode))
  :config
    ; auto insert
    (add-to-list 'ansys-license-types "ane3flds"); add multiphysics with ls-dyna lic
    (add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton-compilation]))
    (add-to-list 'auto-insert-alist '(ansys-mode . [ansys-skeleton-outline-template]))
    (setq ansys-current-ansys-version "170")
    (setq auto-insert-mode 1
      auto-insert-query t ;insert only after request
      ansys-license "ane3flds")
)
