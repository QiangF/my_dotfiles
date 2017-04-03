; * basic packages load first, preface run before init, config run at package loads
; config run at package first loads by a M-x command? or explicit keybinding? or (mode 1)
; function in hydra can be autoloaded
; * packages for usepackage
(setq debug-on-error t)
(setq load-prefer-newer t)
(require 'package)
(setq package-enable-at-startup nil   ; To prevent initialising twice
  package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'load-path "~/.emacs.d/extra")
(setq package-archives '(("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("org-cn"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/org/")
                         ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
      ;; ("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
      ;; ("org-cn"   . "http://elpa.emacs-china.org/org/")
      ;; ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/")

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package)
(setq use-package-verbose t)
(use-package bind-key
  :ensure t)
(require 'bind-key)

(use-package diminish
  :ensure t
  :demand t
  :diminish (visual-line-mode . "ω")
  :diminish hs-minor-mode
  :diminish abbrev-mode
  :diminish auto-fill-function
  :diminish subword-mode)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(eval-when-compile
  (require 'use-package))
;(setq use-package-always-ensure t)


; * Environment fixup
(use-package exec-path-from-shell
  :ensure t
	:init
    (exec-path-from-shell-initialize)
  :config
  (progn
    ;; Import additional environment variables beyond just $PATH
    (dolist (var '("PYTHONPATH"        ; Python modules
           "INFOPATH"        ; Info directories
           "JAVA_OPTS"        ; Options for java processes
           "EMAIL"        ; My personal email
           ))
      (add-to-list 'exec-path-from-shell-variables var))
    ;; Initialize Emacs' environment from the shell
    ;; Tell Emacs about my email address
    (setq user-mail-address (getenv "EMAIL"))

    ;; Re-initialize the `Info-directory-list' from $INFOPATH.    Since package.el
    ;; already initializes info, we need to explicitly add the $INFOPATH
    ;; directories to `Info-directory-list'.  We reverse the list of info paths
    ;; to prepend them in proper order subsequently
    (with-eval-after-load 'info
      (dolist (dir (nreverse (parse-colon-path (getenv "INFOPATH"))))
    (when dir
      (add-to-list 'Info-directory-list dir))))))

; * basic functions package
(require 'subr-x)
; * test init.el
(defun test-emacs ()
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
          "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))

; * hydra
;; multiple inheritance?
(use-package hydra
  :ensure t
)
; * evil-mode
; ** tips
; gJ join line without space
; evil-goto-definition (gd) goto first occurrence of symbol
; middle of visual line (gm)
; *** register
; Every register is accessed using a double quote before its name
; registers from "0 to "9. the latest nth yank
; "., "%, ": and "#
; The last inserted text is stored on ".,
; write the same text twice, no need to yank and paste.
; "% has the current file path, starting from the directory where vim was first opened.
; "# is the name of the alternate file, that you can think of it as
; the last edited file, it's what
; vim uses to switch between files when you use Ctrl-^
; macro
;; qd 	start recording to register d
;; ... 	your complex series of commands
;; q 	stop recording
;; @d 	execute your macro
;; @@ 	execute your macro again 

; ** settings
(use-package evil
  :ensure t
	:preface
    (use-package undo-tree
      :ensure t)
    (use-package goto-chg
      :ensure t)
  :init
    (defhydra hydra-evil (:foreign-keys run :color blue)
      ("<escape>" nil "quit") ;; quit with esc
      ;; ("S-SPC" hydra-pause-resume "pause")
      ("cd" copy_work_path "copy work path" :color blue)
      ("x" er/expand-region "expand region" :color blue) ;; contract is z
      ("o" hydra-outline/body "outline" :color blue)
      ("h" hydra-highlight/body "highlight" :color blue) ;; make sure file is tracked
      ("n" hydra-narrow/body "narrowing")
      ("m" hydra-magit/body "magit")
      ("i" imenu "imenu")
      ("b" hydra-bookmark/body "bookmarks" :color blue)
      ("a" hydra-ipa/body "annotations" :color blue)
      ("k" my/kill-buffer "kill buffer" :color blue)
      ;; ("d" my/delete-window "delete window")
      ("s" my/other-window "other window" :color pink)
      ("cc" org-capture "org capture" :color blue)
      ("u" winner-undo "winner undo" :color pink)
      ("r" winner-redo "winner redo" :color pink)
      ("w" aya-create "create auto yas" :color blue)
      ("y" aya-expand "expand auto yas" :color blue))
    (evil-mode 1)
  :config
    (defun prelude-shift-left-visual ()
    "Shift left and restore visual selection."
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

    (defun prelude-shift-right-visual ()
    "Shift right and restore visual selection."
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

    (setcdr evil-insert-state-map nil)
    ;; ** evil -delete-char without put in the kill-ring
    (setq evil-toggle-key "C-z"
      evil-move-cursor-back nil
      evil-normal-state-cursor '(hollow "white")
      evil-motion-state-cursor '(hollow "white")
      evil-want-fine-undo t)
    (add-to-list 'evil-motion-state-modes 'special-mode)
    (defun my/jump()
      (interactive)
      (let ((last-called (get this-command 'my-last-call-time)))
      (if (and (eq last-command this-command)
        (<= (time-to-seconds (time-since last-called)) my-double-key-timeout))
        (call-interactively 'evilmi-jump-items)
        (if (sit-for my-double-key-timeout)
        (call-interactively 'evil-jump-item)))
      (put this-command 'my-last-call-time (current-time))))
    ; this will be shadowned by evil matchit, so disable evil matchit in mode that
    ; my/jump is desirable
    ; Make horizontal movement cross lines
    ;; don't store kill with x
    (setq-default evil-cross-lines t)
    (evil-set-initial-state 'git-commit-mode 'insert)
    (when (boundp 'global-surround-mode) (global-surround-mode))
    (mapc (lambda (mode) (evil-set-initial-state mode 'emacs))
    ;; find mode via link in help
    '(annots-mode
      backup-walker-mode
      comint-mode
      dired-mode
      eww-mode
      exwm-mode
      fm-bookmarks-mode
      git-rebase-mode
      help-mode
      image-dired-display-image-mode
      image-dired-thumbnail-mode
      inferior-emacs-lisp-mode
      magit-branch-manager-mode
      neotree-mode
      pdf-view-mode
      shell-mode
      sdcv-mode
      term-mode
      ;; volume-electric-mode
      ;; electric-volume-mode
      ;; volume-mode
     ))
  :bind
    (:map evil-insert-state-map
    ("<escape>" . evil-normal-state)
    ([remap evil-toggle-key] . evil-emacs-state)
    ([remap newline] . evil-ret-and-indent)

    :map evil-normal-state-map
    ("SPC" . hydra-evil/body)
    ("C-e" . move-end-of-line)
    ("g-$" . evil-end-of-visual-line)
    ("g-0" . evil-beginning-of-visual-line)
    ("<remap> <evil-next-line>" . evil-next-visual-line)
    ("<remap> <evil-previous-line>" . evil-previous-visual-line)

    :map evil-ex-map
    ("e" . ido-find-file)
    ("w" . ido-write-file)
    ("b" . ido-switch-buffer)

    :map evil-motion-state-map
    ("SPC" . hydra-evil/body)
    ("h" . evil-backward-char)
    ("l" . evil-forward-char)
    ("H-i" . evil-jump-forward)
    ("<remap> <evil-next-line>" . evil-next-visual-line)
    ("<remap> <evil-previous-line>" . evil-previous-visual-line)
    ("<remap> <evil-delete-char>" . delete-forward-char)
    ("TAB" . nil)
    ("y" . evil-yank)

    :map evil-visual-state-map
    (">" . prelude-shift-right-visual)
    ("<" . prelude-shift-left-visual)
    ("%" . my/jump)
    ("C-e" . move-end-of-line)
    ("." . evil-repeat)
    ("<escape>" . evil-normal-state)
  )
)
; * ivy

;; The default matcher will use a .* regex wild card in place of
;; each single space in the input.
;; If you want to use the fuzzy matcher, which instead uses a .*
;; regex wild card between each input letter,
; The ivy-initial-inputs-alist variable is pretty useful in conjunction with the
; default matcher.

;; ivy-switch-buffer, you get a list of not only the currently open
;; buffers but also bookmarks, recently opened files, and window
;; layouts. Ivy calls the window layouts “views”, Because of the way
;; views are named, it's easy to limit your choices to just views
;; when you call ivy-switch-buffer.


(use-package flx
  :ensure t
)
(use-package ivy
  :ensure t
  :bind
    ("C-x b" . ivy-switch-buffer) ;does not handle frame switch? ido-switch-buffer does.
    ;; ("C-c v" . ivy-push-view)
    ;; ("C-c V") . ivy-pop-view)
  :config
    (require 'flx)
    (setq ivy-re-builders-alist
      '((counsel-M-x . ivy--regex-fuzzy)
        (t . ivy--regex-plus))
       ivy-use-virtual-buffers t)
; Use Enter on a directory to navigate into the directory, not open it with dired.
;; (define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)
;; "C-m" 'ivy-done
;; "C-j" 'ivy-alt-done
)
; * helm
;;     ("C-x r l" . helm-filtered-bookmarks)
;;      :map helm-mode-map
;;      ;("C-c h" . helm-execute-persistent-action)
;;      ;("M-o" . helm-previous-source)
;;     (add-to-list 'helm-completing-read-handlers-alist
;;              '(dired . nil))
;;     ;; (defvar winner-boring-buffers-regexp "\\*[hH]elm.*")

; * company
(use-package company            ; Graphical (auto-)completion
  :ensure t
	:defer 20
  :init
    (global-company-mode)
  :diminish company-mode
  :config
    (setq company-tooltip-align-annotations t
      company-tooltip-flip-when-above t
      ;; Easy navigation to candidates with M-<n>
      company-show-numbers t)
    (setq company-dabbrev-other-buffers nil
    company-begin-commands '(self-insert-command)
    company-complete-number t
    company-require-match nil ;; this can be overiden by backends
    company-show-numbers t
    company-minimum-prefix-length 3
    company-selection-wrap-around t
    company-dabbrev-downcase nil
    company-dabbrev-ignore-case nil
    company-echo-delay 0
    company-idle-delay nil         ;nil no 0 immeadiate
    company-auto-complete nil
    company-backends
    '(company-dabbrev company-keywords company-abbrev  company-yasnippet company-dabbrev-code company-files company-capf))
)
; * counsel
;select the current input instead of the current candidate?
;Use C-M-j (ivy-immediate-done)
(use-package counsel
  :ensure t
  :preface
    (use-package swiper
      :ensure t)
    (use-package smex
      :ensure t)
  :bind
    (("C-c r" . counsel-linux-app)
    ("C-M-s" . counsel-grep)
    ("C-x C-f" . counsel-find-file)
    ("C-x d" . counsel-find-file)
    ("C-x C-r" . counsel-recentf)
    ("M-x" . counsel-M-x)
    ("M-y" . counsel-yank-pop)
    ("C-\"" . counsel-imenu))
  :config
    (defun counsel-recoll-function (string &rest _unused)
    "Issue recallq for STRING."
    (if (< (length string) 3)
        (counsel-more-chars 3)
        (counsel--async-command
        (format "recollq -b '%s'" string))
        nil))

    (defun counsel-recoll (&optional initial-input)
    "Search for a string in the recoll database.
    You'll be given a list of files that match.
    Selecting a file will launch `swiper' for that file.
    INITIAL-INPUT can be given as the initial minibuffer input."
    (interactive)
    (ivy-read "recoll: " 'counsel-recoll-function
                :initial-input initial-input
                :dynamic-collection t
                :history 'counsel-git-grep-history
                :action (lambda (x)
                        (when (string-match "file://\\(.*\\)\\'" x)
                            (let ((file-name (match-string 1 x)))
                            (find-file file-name)
                            (unless (string-match "pdf$" x)
                                (swiper ivy-text)))))))
)

; * terminal
; two esc send esc, useful in vi editor
(use-package term
  :preface
    (add-hook 'term-mode-hook (lambda()
          (yas-minor-mode 0)))
  :bind
	   (("C-c t" . ansi-term)
     :map term-mode-map
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ("C-\\" . toggle-input-method)
        ("M-x" . counsel-M-x)
        ("C-=" . my-term-switch-line-char)
	      ("C-c r" . counsel-linux-app)
     :map term-raw-map
        ("M-p" . term-send-up)
        ("M-n" . term-send-down)
        ("M-x" . counsel-M-x)
        ("C-\\" . toggle-input-method)
        ("C-y" . term-paste)
        ("C-c C-c" . term-interrupt-subjob)
        ("C-=" . my-term-switch-line-char)
	      ("C-c r" . counsel-linux-app)
       )
  :config
  (setq explicit-shell-file-name "/bin/bash"
    term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;; C-c C-a to go to the end of prompt
  (defun my-term-switch-line-char ()
    "Switch `term-in-line-mode' and `term-in-char-mode' in `ansi-term'"
    (interactive)
    (cond
     ((term-in-line-mode)
      (term-char-mode)
      (hl-line-mode -1)(evil-emacs-state))
     ((term-in-char-mode)
      (term-line-mode)(evil-exit-emacs-state))))
)

; ** sane term
(use-package sane-term
  :ensure t
  :init
    (bind-key "C-1" #'sane-term)
)


; * pyim
; after company
; tips
; pinyin seperator ', eg fang'an 方案

;; 安装词库，请运行词库管理命令 `pyim-dicts-manager'
;; 1. 使用 `C-h v pyim-dicts' 了解 `Chinese-pyim' 词库文件格式。
;; 2. 了解如何导入其它输入法的词库。
;;    1. 使用 package 管理器查看 Chinese-pyim 包的简介
;;    2. 阅读 chinese-pyim.el 文件 Commentary
;; C-c 	消输入
;; C-g 	消输入并保留已输入的中文
;; TAB 	糊音调整
;; DEL 或 BACKSPACE 	删除最后一个字符
;; C-DEL或 C-BACKSPACE 	删除最后一个拼音
;; M-DEL或 M-BACKSPACE 	删除最后一个拼音
;; 第一种方法：使用命令 `pyim-toggle-full-width-punctuation'，全局切换。
;; 第二种方法：使用命令 `pyim-punctuation-translate-at-point' 只切换光标处标点的样式。
;; 第三种方法：设置变量 `pyim-translate-trigger-char' ，输入变量设定的字符会切换光标处标点的样式。default is v
;; `pyim-create-word-from-selection', 选择一个词条，运行这个命令后，就可以将这个词条添加到个人文件。
;; "将中文词条 `word' 添加拼音后，保存到 personal-file, 全拼模式2v是特殊快捷键，添加光标前两个汉字组成的词条到personal-file
;; `pyim-delete-word-from-personal-buffer' 从个人词频文件对应的 buffer 中删除当前高亮选择的词条。
; (pyim-save-files) 保存personal－file
; `pyim-punctuation-toggle'，全局切换全角半角

;; 词库 第一个空白字符之前的内容为拼音code，空白字符之后为中文词条列表。
;; 拼音词库 *不处理* 中文标点符号。

;; 注意：词库文件必须按行排序（准确的说，是按每一行的 code 排序），因为
;; `Chinese-pyim' 为优化搜索速度，使用二分法寻找词条，而二分法工作的前提就是对
;; 文件按行排序。具体细节请参考：`pyim-bisearch-word' 。当用户手动调整词库文
;; 件后，记得运行 `pyim-update-dict-file' 来对文件排序。

;; setq pyim-dicts
;;      '((:name "dict1" :file "/path/to/pyim-dict1.pyim" :coding utf-8-unix :dict-type pinyin-dict))
(use-package chinese-pyim
  :ensure t
  :bind
    (("C-\\" . toggle-input-method)
     ("C-c a". my-create-word-from-selection)
     ("M-f" . pyim-forward-word)
     ("M-b" . pyim-backward-word))
  :init
     ; personal file 1
       ;; (load "my-chinese-pyim")

     ; personal file 2
    (bind-key "C-\\" #'toggle-input-method) 
    (require 'chinese-pyim-core)
    (setq abbrev-file-name "~/.dotfiles/emacs.d/abbrev_defs") 
    (if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))
    (setq save-abbrevs nil) ;; stop asking to save new abbrevs when quitting emacs
    (defun m-brev-expand
        (interactive)
        (setq pyim-dagger-str
              ; use company mode to dynamically expand abbrev
              (comapny-abbrev pyim-entered-code))
        (pyim-terminate-translation))

    (defun read-lines (file-path)
    "Return a list of lines of a file at filePath."
    (with-temp-buffer
        (insert-file-contents file-path)
        (split-string (buffer-string) "\n" t)))
    (defvar user-dict-path "~/Nutstore/dotfiles/pyim/user.pyim")
    (defvar user-dict (read-lines user-dict-path))

    (defun convert-word-list-to-pyim (file-path)
      (interactive)
      (with-temp-buffer
        (let ((word-list (read-lines file-path)))
          (dotimes (i (length word-list))
            (insert (concat (pyim-hanzi2pinyin (elt word-list i) nil "-" nil nil t) " " (elt word-list i) "\n")))
        (write-file (concat file-path ".pyim")))))
    
    
    (defun my-create-word-from-selection ()
    "Add the selected text as a Chinese word into the user dictionary."
    (interactive)
    (when (region-active-p)
        (let* ((string (buffer-substring-no-properties (region-beginning) (region-end)))
               (string-with-pinyin (concat (pyim-hanzi2pinyin string nil "-" nil nil t) " " string)))
          ( if (not ( member string-with-pinyin user-dict))
               (progn
               (pyim-create-or-rearrange-word string)
               (write-region
               (concat "\n" string-with-pinyin) nil user-dict-path 'append) 
               (message "将词条: %S 插入 user file。" string-with-pinyin))))))

    (defun pyim-export-user-dict ()
      (interactive)
      (with-temp-buffer
      (maphash
      #'(lambda (key value)
          (insert (concat key " " (mapconcat #'identity value " ") "\n")))
      pyim-dcache-icode2word)
      (write-file "~/.emacs.d/pyim/user.pyim")))

    (setq pyim-dicts
        '((:name "user"
                 :file "~/Nutstore/dotfiles/pyim/user.pyim"
                 :coding utf-8-unix
                 :dict-type pinyin-dict)
          (:name "sogou"
                 :file "~/.dotfiles/.emacs.d/extra/pyim-sogou.gz"
                 :coding utf-8-unix
                 :dict-type pinyin-dict)))
          ;; pyim-default-pinyin-scheme 'pyim-shuangpin)
    ;; (require 'chinese-pyim-company)
    (require 'chinese-pyim)
    (setq pyim-use-tooltip 'nil
        pyim-page-tooltip 'nil
        pyim-default-pinyin-scheme 'default ; default is quanpin
        pyim-page-length 5
        pyim-company-max-length 6
        default-input-method "chinese-pyim")

    ;; (setq-default pyim-english-input-switch-functions
    ;;         '(pyim-probe-program-mode)) ; 在该mode只有comment和string里可以有中文
  :config
    (setq-default pyim-english-input-switch-functions
              '(pyim-probe-program-mode pyim-probe-isearch-mode pyim-probe-org-structure-template))
    (setq pyim-isearch-enable-pinyin-search t)
)
; * chinese font
;; (use-package chinese-fonts-setup
;;   :ensure t
;;   :init
;;   ;; 让 chinese-fonts-setup 随着 emacs 自动生效。
;;   (require 'chinese-fonts-setup)
;;   (setq cfs-use-face-font-rescale (eq system-type 'gnu/linux))
;;   (chinese-fonts-setup-enable)
;;   :config
;; )

;; (bind-key "C-+" #'cfs-increase-fontsize)
;; (bind-key "C--" #'cfs-decrease-fontsize)
(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)
;; 让 chinese-fonts-setup 随着 emacs 自动生效。
;; (chinese-fonts-setup-enable)

; Charset 设置
;; (use-package mule
;;   :ensure nil
;;   :config

;;   (set-language-environment "UTF-8")
;;   (set-buffer-file-coding-system 'utf-8-unix)
;;   (set-clipboard-coding-system 'utf-8-unix)
;;   (set-file-name-coding-system 'utf-8-unix)
;;   (set-keyboard-coding-system 'utf-8-unix)
;;   (set-next-selection-coding-system 'utf-8-unix)
;;   (set-selection-coding-system 'utf-8-unix)
;;   (set-terminal-coding-system 'utf-8-unix)

;;   (when (eq system-type 'windows-nt)
;;     (set-language-environment "Chinese-GBK")
;;     (set-selection-coding-system 'gbk-dos)
;;     (set-next-selection-coding-system 'gbk-dos)
;;     (set-clipboard-coding-system 'gbk-dos)))



; * my-mode
(use-package my-mode              ; The one and only Git frontend
  :preface
    (use-package comment-dwim-2
      :ensure t)
  :bind
     (("C-a" . my/goto-line-beginning-or-indent)
     ("C-x C-e" . my/eval)
     ("C-c g" . goto-char)
     ("C-<backspace>" . my/backspace)
     ("<home>" . my/home)
     ("M-q" . my/toggle-fill-paragraph)
     ("C-y" . my/yank)
     ("S-<return>" . sanityinc/newline-at-end-of-line)
     ("<f12>" . toggle-mode-line)
     ("C-7" . switch-to-scratch-and-back)
     ("s-o" . other-window)
     ("s-x" . delete-other-windows)
     ("s-j" . scroll-other-window-down)
     ("s-k" . scroll-other-window)

     :map my-mode-map
     ("C-<tab>" . hydra-evil/body)
     ("M-g" . hydra-error/body)
     ("C-h" . hydra-help/body)
     ("S-SPC" . hydra-pause-resume)
     )
  :init
    (defhydra hydra-help (:color blue)
      ;; ("a" apropos "helm-apropos")
      ("b" describe-bindings "bindings")
      ("c" scratch "scratch")
      ("f" describe-function "function")
      ("F" Info-goto-emacs-command-node "goto command")
      ;; ("g" ag-and-a-half "ag-and-a-half")
      ;; ("g" helm-ag "helm ag")
      ;; ("i" helm-info-emacs "info")
      ("k" describe-key "key")
      ("m" describe-mode "mode")
      ("p" describe-package "package")
      ("t" describe-theme "theme")
      ("v" describe-variable "variable")
      ("@" describe-face "face") ; describe the face name under cursor, change face with set-face-attributes
      ("?" help-for-help "help")
      ("<escape>" nil "quit")
      ("d" find-name-dired "find name dired")
      ("r" revert-buffer "revert buffer")
      ("s" swiper "swiper"))
    (defhydra hydra-highlight (:color pink)
      ("h" highlight-symbol-at-point)
      ("n" highlight-symbol-next)
      ("p" highlight-symbol-prev)
      ("q" highlight-symbol-query-replace)
      ("r" highlight-symbol-remove-all)
      ("<escape>" nil "quit"))
    (defhydra hydra-narrow (:color blue)
      ("d" narrow-to-defun "defun") ;     M-h mark paragraph
      ("p" narrow-to-page "page")
      ("r" narrow-to-region)
      ("w" widen)
      ("s" org-narrow-to-subtree "org-subtree")
      ("b" org-narrow-to-block "org-block")
      ("<escape>" nil "quit"))
    (defhydra hydra-magit (:color blue)
      ("<escape>" nil "quit")
      ("h" magit-view-file-history "file history")
      ("c" magit-status "status")
      ("C" magit-checkout "checkout")
      ("v" magit-branch-manager "branch manager")
      ("m" magit-merge "merge")
      ("l" magit-log "log")
      ("r" vc-resolve-conflicts "resolve conflicts in file")
      ("!" magit-git-command "command")
      ("$" magit-process "process"))
    (defhydra hydra-error (:foreign-keys run)
      "goto-error"
      ("<escape>" nil "quit") ;; quit with esc
      ("h" first-error "first")
      ("j" next-error "next")
      ("k" previous-error "prev")
      ("v" recenter-top-bottom "recenter")
      ("q" nil "quit"))
    (setq my-double-key-timeout 0.25)
    (require 'my-mode) ; 
    (my-mode 1)
)
;* my-miscs-mode
(use-package my-miscs-mode
  :defer 30
  :init
  (require 'my-miscs-mode)
  (my-miscs-mode 1)
)

; * desktop
;; Automatically save and restore sessions
; emacs 25 (save-place-mode 1) saveplace is auto-loaded by save-place-mode. So you do not need to explicitly require it.
(setq-default desktop-dirname          "~/.emacs.d/desktop/"
      desktop-path          (list desktop-dirname)
      desktop-files-not-to-save      "^$" ;reload tramp paths
      save-place t
      save-place-forget-unreadable-files nil ; don't check if filereadable before save place
      save-place-file "~/.emacs.d/places"
      ;; desktop-load-locked-desktop nil
      desktop-load-locked-desktop "ask")
(desktop-save-mode 0)

; * general
;(set-locale-environment "zh_CN.UTF-8")


(setq initial-scratch-message nil
      ; x-select-enable-clipboard t
      pending-delete-mode t
      ;; x-alt-keysym 'meta
      inhibit-startup-screen t)

(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq x-selection-timeout 300)

(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
;; (bind-key* "<backspace>" #'delete-backward-char)
(bind-key* "S-<backspace>" #'evil-shift-left-line)
(bind-key* "C-c h" #'mark-whole-buffer)
(bind-key "C-c C-m" #'execute-extended-command)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; * custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(cfs--current-profile "profile1" t)
 '(cfs--profiles-steps (quote (("profile1" . 1))) t)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain)))

;; underline tabs
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(iedit-occurrence ((t (:foreground "red"))))
 '(my-dired-mark-face ((((class color)) (:foreground "white" :weight bold))) t)
 '(my-tab-face ((((class color)) (:foreground "white" :weight bold :underline t))) t)
 '(my-trailing-space-face ((((class color)) (:background "gray50"))) t))
;;   '(my-long-line-face ((((class color)) (:background "gray90"))) t))

; * aliases
(defalias 'qrr 'query-replace-regexp)
(defalias 'qr 'query-replace)
(defalias 'yes-or-no-p 'y-or-n-p)
;; (use-package bookmark+
;;   :ensure t
;;   :config
;;     (setq bookmark-version-control t
;;     ;; auto-save bookmarks
;;     bookmark-save-flag 1)
;; )

; * GUI settings

(use-package cyberpunk-theme
  :ensure t
  :init
  (add-hook 'after-init-hook
            #'(lambda ()
                (load-theme 'cyberpunk t))))
(use-package fancy-battery        ; Fancy battery info for mode line
  :ensure t
  :defer 360
  :init
  (fancy-battery-mode)
)

(when (display-graphic-p)
  (fringe-mode 1)
  (mouse-wheel-mode t)
  (set-face-attribute 'default nil :font "DejaVu Sans mono-10:bold")
  ;; (set-face-background 'region "gray90")  ;; Color for selected lines
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth))
  (set-fringe-mode '(0 . 0)); Disable fringe because I use visual-line-mode
  (setq x-underline-at-descent-line t
    visible-bell t
    visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)
    scroll-conservatively 10000
    scroll-preserve-screen-position t
    scroll-margin 0
    hscroll-step 1
    hscroll-margin 0
    scroll-step 1
    auto-window-vscroll nil      ;; no auto centering
    blink-cursor-mode nil))
; ** scroll indicator on-screen
(use-package on-screen
  :ensure t
  :defer 20
	:init
    (on-screen-global-mode 1)
  :config
    (setq on-screen-highlight-method 'shadow
      on-screen-delay 1)
)

; * modeline
(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/no-confirm-load-theme t
    ;; sml/theme 'respectful
    sml/theme 'light
    sml/name-width 30
    sml/mode-width `full
    sml/shorten-modes t)
  (sml/setup)
)

;; hide modeline when idle for 1 s (reading)
;; (defun show-modeline ()
;;   (if (equal mode-line-format nil)
;;       (progn (remove-hook 'pre-command-hook 'show-modeline)
;;       (setq mode-line-format (default-value 'mode-line-format))
;;       (redraw-display))))
;; (run-with-idle-timer 20 ; run this after emacs is idle for 1 second after emacs startup
;;   t ; do this just once; don't repeat
;;   (lambda () (setq mode-line-format nil)(redraw-display)
;;   (add-hook 'pre-command-hook 'show-modeline)))

; * backup
(setq delete-old-versions t ;; prevent the 'Delete excess backup versions ... ' message
  vc-make-backup-files t)
(defvar --backup-directory (concat user-emacs-directory "backups"))

(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

(setq make-backup-files t        ; backup of a file the first time it is saved.
      ;; backup-by-copying t           ; don't clobber symlinks
      backup-by-copying-when-linked t
      version-control t            ; version numbers for backup files
      delete-old-versions t        ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 0        ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 5        ; newest versions to keep when a new numbered backup is made (default: 2)
)

;; The typical workflow is:
;;
;;   1) I'm in a buffer and realize I need to check some backups.
;;
;;        M-x backup-walker-start
;;
;;   2) I press <p> to go backwards in history until I see something
;;      interesting.  Then I press <enter> to bring it up.  OOPs this isn't
;;      it, I go back to the backup-walker window and find the right file.
;;
;;   3) I get what I need from the backup, go back to backup-walker, and press
;;      <q> and kill all open backups.
;;
;;   4) the end.
;;
;; Additionally, note that all the diff-mode facilities are available in the
;; `backup-walker' buffer.

(use-package backup-walker
  :init
  :bind
  :config
)

; * autosave

(defvar --temp-directory (concat user-emacs-directory "temp"))
(setq tramp-auto-save-directory --temp-directory)
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/temp/" t)))
      ;; `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'" "~/.emacs.d/temp/" t)))
      ;; `(("\\(?:[^/]*/\\)*\\(.*\\)" ,(concat --temp-directory "\\1") t))) ; for windows
; things run much quicker
(setq auto-save-directory --temp-directory
      ;; auto-save-list-file-prefix (concat temp-directory "/autosave-")
      ;; auto-save-hash-p nil
      auto-save-default t        ; auto-save every buffer that visits a file
      ; auto save either after 30s or after 300 keystrokes
      auto-save-timeout 30        ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 300        ; number of keystrokes between auto-saves (default: 300)
)

;; ediff
;; to do use a custom theme

(use-package ediff
  ;; :preface
    ;; (add-hook 'ediff-load-hook
    ;;   (lambda ()
    ;;     (set-face-underline ediff-current-diff-face-A "black")
    ;;     (set-face-underline ediff-current-diff-face-B "black")
    ;;     (set-face-underline ediff-current-diff-face-C "black")))
  :config
    (setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
)


; * dired
; install ranger
; ** tips
; *** wdired makes file names editable
; C-x C-q to enter, C-c C-c to save
; *** hide file
; `C-x d f*.html' opens Dired on only files that start with `f' and end
; with `.html'
; C-x d binds to ido-dired, use M-x dired open full path
; *** short keys
; s sort, / filter, * or % mark
; C-x C-j jump to file or link
; D delete to trash, C-u D delete for ever to do
; S is symlink
; ** setting

(use-package stripe-buffer        ; Add stripes to a buffer
  :ensure t
  :init (add-hook 'dired-mode-hook #'stripe-buffer-mode)
	:config
  ;; (setq stripe-highlight-face stripe-highlight)
)


(bind-key* "C-x C-j" #'dired-jump)

(use-package dired
	:preface
		(use-package dired-ranger
		  :ensure t)
    (use-package dired-hacks-utils
      :ensure t)
    (use-package dash
      :ensure t)
    (use-package dired+
      :ensure t
      :config
        (set-face-attribute 'diredp-flag-mark nil 
                    :foreground "white"))
  :bind
  (:map dired-mode-map
     ("C-j" . dired-jump-other-window)
     ("s" . hydra-dired-sort/body)
     ("C-x M-o" . dired-omit-switch)
     ;; ("C-x d" . ido-dired)
     ("C-c o" . my/ext-open)
     ("C-c c" . dired-ranger-copy)
     ("C-c p" . dired-ranger-paste)
     ("C-c m" . dired-ranger-move)
     ("0" . dired-back-to-start-of-files))
  :config
    (setq-default diredp-hide-details-initially-flag t
      dired-recursive-deletes 'top
      dired-recursive-copies 'always
      ;; make dired list readable
      image-dired-track-movement t
      dired-listing-switches "-alh --group-directories-first"
      ;; dired-listing-switches "-alhtvgoLBX --group-directories-first"
      dired-omit-files-p nil ; Buffer-local variable
      ; set omit first, others use the setting
      dired-omit-files "^\\..+$"
      dired-dwim-target t
      delete-by-moving-to-trash t)

    (defun dired-back-to-start-of-files ()
      (interactive)
      (backward-char (- (current-column) 2)))
    (defun my/ext-open () ;;dired-open-file ()
      "In dired, open the file named on this line."
      (interactive)
      (let* ((file (if (equal major-mode 'dired-mode)
    (dired-get-filename nil t)
    (if mark-active (buffer-substring (region-beginning) (region-end)) nil))))
    (message "Opening %s..." file)
    (call-process "xdg-open" nil 0 nil file)
    (message "Opening %s done" file)))
    (diredp-toggle-find-file-reuse-dir t)
; *** dired-sort
    (require 'dired-sort-map)
    (defun dired-sort-ctime ()
      "Dired sort by create time."
      (interactive)
      (dired-sort-other (concat dired-listing-switches "ct")))
    (defun dired-sort-utime ()
      "Dired sort by access time."
      (interactive) (dired-sort-other (concat dired-listing-switches "ut")))
    (defhydra hydra-dired-sort (:color blue)
      ("s" (lambda () "sort by Size"
      (interactive) (dired-sort-other (concat dired-listing-switches " -S"))) "size")
      ("x" (lambda () "sort by eXtension"
      (interactive) (dired-sort-other (concat dired-listing-switches " -X"))) "extension")
      ("t" (lambda () "sort by Time"
      (interactive) (dired-sort-other (concat dired-listing-switches " -t"))) "time")
      ("c" 'dired-sort-ctime "created")
      ("a" 'dired-sort-utime "accessed")
      ("n" (lambda () "sort by Name"
      (interactive) (dired-sort-other dired-listing-switches)) "name")
      ("<escape>" nil "quit"))

; *** dired-omit
  (defvar v-dired-omit t
     "If dired-omit-mode enabled by default. Don't setq me.")
   (defun dired-omit-switch ()
     "This function is a small enhancement for `dired-omit-mode', which will
   \"remember\" omit state across Dired buffers."
     (interactive)
     (if (eq v-dired-omit t)
     (setq v-dired-omit nil)
       (setq v-dired-omit t))
     (dired-omit-caller)
     (revert-buffer))

   (defun dired-omit-caller ()
     (if v-dired-omit
     (setq dired-omit-mode t)
       (setq dired-omit-mode nil)))
   (add-hook 'dired-mode-hook 'dired-omit-caller)


;; file and folder size
    (defun dired-get-size ()
    (interactive)
    (let ((files (dired-get-marked-files)))
        (with-temp-buffer
        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
        (message "Size of all marked files: %s"
            (progn
            (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
            (match-string 1))))))
)

; *** other setting
(use-package launch            ; Open files in external programs
  :ensure t
  :defer t
)

; *** sudo
(use-package sudo-edit            ; Edit files as root, through Tramp
  :ensure t
  :defer t
  :bind (("C-c f s" . sudo-edit)
         ("C-c f S" . sudo-edit-current-file))
  :init
    (defun su-buffer ()
      (interactive)
      (require 'tramp)
      (let ((pos (point))
        (dir (expand-file-name default-directory)))
        (if (eq major-mode 'dired-mode)
        (dired (concat "/sudo:root@localhost:" dir))
        ((find-alternate-file
         (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer))))
          ))(goto-char pos)))
)

;; dired image
(use-package image-file            ; Visit images as images
  :preface
    (add-hook 'image-mode-hook
      (hl-line-mode -1))
    (add-hook 'image-dired-thumbnail-mode-hook
      (lambda ()
       (turn-off-my-mode)
       (hl-line-mode -1)))
  :bind
   (:map image-dired-thumbnail-mode-map
     ("w" . my/id-copy-file-name)
     ("j" . my/id-jump-original-dired-buffer)
   )
  :config
    (defun my/id-jump-original-dired-buffer ()
    "Jump to the dired buffer associated with the current image file."
      (interactive)
      (switch-to-buffer (image-dired-associated-dired-buffer))
      (if (not (dired-goto-file (image-dired-original-file-name)))
      (message "Could not track file")))
    (defun my/id-jump-thumbnail-buffer()
      "Jump to thumbnail buffer."
      (interactive)
      (if (get-buffer image-dired-thumbnail-buffer)
          (switch-to-buffer image-dired-thumbnail-buffer)
          (image-dired default-directory)))
    (defun my/id-copy-file-name ()
        (interactive)
        (kill-new (expand-file-name (image-dired-original-file-name))))
    (auto-image-file-mode)
)
; * android

;; This special method uses the Android Debug Bridge for accessing
;; Android devices. The Android Debug Bridge must be installed
;; locally. Some GNU/Linux distributions offer it for installation,
;; otherwise it can be installed as part of the Android SDK. If the
;; adb program is not found via the PATH environment variable, the
;; variable tramp-adb-program must point to its absolute path.

;; Tramp does not connect Android devices to adb. This must be
;; performed outside Emacs. If there is exactly one Android device
;; connected to adb, a host name is not needed in the remote file
;; name.

; The default TRAMP name to be used is /adb::

; connect over wifi: adb connect 192.168.1.100
; /adb:192.168.1.100:

;; therefore. Otherwise, one could find potential host names with
;; the command adb devices.

; * bookmark

(use-package bookmark+
    :ensure t
    :init
        (defhydra hydra-bookmark (:color pink)
        ("<escape>" nil "exit")
        ("C-a" bmkp-annotate-bookmark "add annotation")
        ("C-d" bmkp-set-desktop-bookmark "bookmark desktop")
        ("C-s" bookmark-bmenu-show-all-annotations "show annotation")
        ("C-t" bmkp-toggle-autonamed-bookmark-set/delete "set/delete automatic bookmark")
        ("C-l" bookmark-bmenu-list "list all")
        ("C-n" bmkp-next-bookmark-this-file/buffer-repeat "next in buffer")
        ("C-p" bmkp-previous-bookmark-this-file/buffer-repeat "previous in buffer")
        ("C-h" my/toggle-highlight "toggle highlight in buffer")
        ("C-f" bmkp-switch-bookmark-file-create "switch file"))
    :config
    (setq bookmark-version-control t
        ;; auto-save bookmarks
        bookmark-save-flag 1)
    (defvar my/bmkp-toggle nil
    "Keeps the state of how the bookmark was last toggled by TABing.")
    (defun my/toggle-highlight()
    (interactive)
    (if my/bmkp-toggle
        (bmkp-unlight-bookmarks-this-buffer)(bmkp-light-bookmarks-this-buffer))
    (setq my/bmkp-toggle (not my/bmkp-toggle)))
)

; * fm-bookmarks
(use-package fm-bookmarks
  :ensure t
  :preface
    (defvar fm-bookmarks-hide-duplicated t)
    (defvar fm-bookmarks-enable-mounted-media t)
  :init
    (define-key dired-mode-map (kbd "'") #'fm-bookmarks)
    (bind-key "C-'" #'fm-bookmarks)
  ;; :bind
  ;;  ;; (("C-'" . #'fm-bookmarks)
  ;;  (("C-c `" . #'fm-bookmarks)
  ;;   :map dired-mode-map
  ;;   ("`" . #'fm-bookmarks))
  ;;   ;; ("'" . #'fm-bookmarks))
  :config
    (setq fm-bookmarks-enabled-file-managers '(gnome3)
          fm-bookmarks-custom-bookmarks
          '(("Root" . "/") ("Tmp" . "/tmp/"))
          fm-bookmarks-hide-by-name-pattern '("Bluetooth" "Images")
          fm-bookmarks-hide-by-path-pattern '()
          fm-bookmarks-enable-mounted-media t
          fm-bookmarks-enable-cache t)
)

; * clipmon
;; (use-package clipmon
;;   :ensure t
;;   :bind ("C-2" . clipmon-autoinsert-toggle)
;;   :config
;;     (setq clipmon-timer-interval 1      ; check system clipboard every n secs
;;       clipmon-autoinsert-sound t     ; t for included beep, or path or nil
;;       clipmon-autoinsert-color "red" ; color of cursor when autoinsert is on
;;       clipmon-autoinsert-timeout 5     ; stop autoinsert after n mins inactivity
;;       clipmon-transform-suffix "\n") ; add to end of text
;; )

; * which key
; C-h followed by
;; The commands are:
;;     Cycle through the pages forward with n (or C-n)
;;     Cycle backwards with p (or C-p)
;;     Undo the last entered key (!) with u (or C-u)
;;     Call the default command bound to C-h, usually describe-prefix-bindings, with h (or C-h)
;; This is especially useful for those who like helm-descbinds but also want to use C-h as their which-key paging key.
;; Note C-h is by default equivalent to ? in this context.

(use-package which-key            ; Show help popups for prefix keys
  :ensure t
  :init
    (which-key-mode 1)
  :config
    (setq which-key-idle-delay 1.0)
  :diminish which-key-mode
)
; * flyspell

; * flycheck
(use-package flycheck
  :ensure t
  :defer 30
  :config
    (hook-into-modes #'global-flycheck-mode
         'prog-mode-hook)
    ;;(add-hook 'after-init-hook #'global-flycheck-mode)
    ;; (setq flycheck-checker-error-threshold 400)
)


; * yasnippet
(use-package yasnippet
  :ensure t
  :defer 30
  :bind
    (("C-c l" . yas-insert-snippet)
      :map yas-minor-mode-map
      ("<tab>" . nil)
      ;; yas map when expansion in action
      :map yas-keymap
      ("M-p" . yas-prev-field)
      ("TAB" . nil)
      ("<tab>" . nil)
      ("M-n" . yas-next-field)
      ;; Keys can be written by their ASCII code, using a backslash
      ;; followed by up to six octal digits. This is the only way to
      ;; represent keys with codes above \377.
      ("\7" . nil)
      ("M-ESC" . yas-abort-snippet)
      )
  :preface
    (use-package dropdown-list
			:ensure t)
  :config
    (setq yas/snippet-dirs "~/.emacs.d/snippets"
      yas-fallback-behavior nil
      ;; give yas/dropdown-prompt in yas/prompt-functions a chance
      yas-prompt-functions '(yas-dropdown-prompt
        yas-ido-prompt yas-completing-prompt))
    ;; use yas/completing-prompt when ONLY when `M-x yas-insert-snippet'
    ;; thanks to capitaomorte for providing the trick.
    (defadvice yas-insert-snippet (around use-completing-prompt activate)
    "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
    (let ((yas-prompt-functions '(yas-completing-prompt))) ad-do-it))
    ;; @see http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
    ;; (setq-default mode-require-final-newline nil)
    (add-hook 'yas-minor-mode-hook
              (lambda ()
                (yas-activate-extra-mode 'fundamental-mode)))
    (yas-global-mode 1)
)
(use-package auto-yasnippet
  :ensure t
  ; usage:
  ; 1 one line field$ = document.getElementById("|"); at | call aya-create, the aya-expand, M-n move to the next field
  ; 2 multi placeholder count_of_~red = get_total("~red");
  ; 3 arbitrary text, Emacs-style backticks:
  ;`red'_total = get_total("`red'_values");


)
; * tiny
;  a single C-_ will undo the whole thing and allow you to edit the code
; m{range start:=0}{separator:= }{range end}{Lisp expr:=indentity}|{format expr:=%d}

; ** example
; m1;\n7*xx|hex: 0x%x
;; integer range start: 1
;; integer range end: 7
;; separator to join the expressions: ";\n"
;; Elisp expression to transform the linear range: (* x x)
;; format expression for the result: "hex: 0x%x"

;; (mapconcat
;;  (lambda (x)
;;    (format "hex: 0x%x"
;;            (* x x)))
;;  (number-sequence 1 7)
;;  ";\n")

(use-package tiny
  :ensure t
  :init
    (require 'tiny)
)
; * expansion

(use-package hippie-exp            ; Powerful expansion and completion
  :bind (([remap dabbrev-expand] . hippie-expand))
  :config
    (setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        lunaryorn-try-complete-lisp-symbol-without-namespace))
)
; * fill guide
;; Set the number to the number of columns to use.
(setq-default fill-column 80)

;; fill-column-indicator
(use-package fill-column-indicator
  :ensure t
  :defer 10
  :init
    (add-hook 'prog-mode-hook
      (lambda () (fci-mode 1)(setq fci-rule-column 81)))
    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  :config
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

    (defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

    (defvar sanityinc/fci-mode-suppressed nil)
    (make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

    (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (let ((fci-enabled (sanityinc/fci-enabled-p)))
        (when fci-enabled
        (setq sanityinc/fci-mode-suppressed fci-enabled)
        (turn-off-fci-mode))))

    (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and sanityinc/fci-mode-suppressed
        (null popup-instances))
        (setq sanityinc/fci-mode-suppressed nil)
        (turn-on-fci-mode)))
)

;; Add Autofill mode to mode hooks.
(add-hook 'text-mode-hook '(lambda()
         (turn-on-auto-fill)))
; ** indentguide
(use-package indent-guide
  :ensure t
  :init
    (indent-guide-global-mode)
  :config
    (set-face-background 'indent-guide-face "dimgray")
    (setq indent-guide-delay 0.1)
)
; * smart shift
(use-package smart-shift
  :ensure t
)
(setq-default tab-width 4
  indent-tabs-mode nil)
;(bind-key "<return>" #'newline-and-indent) ;"RET"
; indent
; (tab-to-tab-stop) indent to the next tab position, according to tab-width
; (indent-relative)

; * expand region
; ** tips
; C-M-h select defun
(use-package expand-region
	:ensure t
  :bind
    ("C-," . er/expand-region)
  :config
    (eval-after-load "evil" '(setq expand-region-contract-fast-key "z"))
)
; * iedit & highligh-symbol
(use-package highlight-symbol
  :ensure t
)
(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode)
)

;; You search for a word in the buffer/region, type in the
;; replacement and confirm each one by pressing y or n or just press
;; ! to apply this to everything.

(use-package visual-regexp
  :ensure t
  :commands (vr/query-replace)
  :bind* (("M-m SPC SPC" . vr/query-replace))
  :config
  (use-package visual-regexp-steroids
    :ensure t
    :commands (vr/select-query-replace)))

; renaming workflow
; use rgrep or ag-project-regexp to search for pattern, wgrep and iedit to rename
(eval-after-load 'grep
  '(define-key grep-mode-map
    (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

(eval-after-load 'wgrep
  '(define-key grep-mode-map
    (kbd "C-c C-c") 'wgrep-finish-edit))

; * multiple-cursors

; use multiple-cursors only to add cursors to next sexp (in LISP
; modes) or to next line (other modes, like org or fundamental).

;; evil select lines use A to append to the end and I to insert to the beginning
;; (evil-mc-mode  1) ;; enable
;; (evil-mc-mode -1) ;; disable
;; mc/edit-lines: Adds one cursor to each line in the current region.
;; mc/edit-beginnings-of-lines: Adds a cursor at the start of each line in the current region.
;; mc/edit-ends-of-lines: Adds a cursor at the end of each line in the current region.
;; (evil-mc-make-cursor-here)
;; ;; Create a cursor at point. This command should be used with `evil-mc-pause-cursors'.
;; (evil-mc-pause-cursors)
;; ;; Pause all fake cursors. This can be used with `evil-mc-make-cursor-here'
;; (evil-mc-resume-cursors)
;; ;; Call to resume paused cursors.



; * allignment
; ** zop-to-char
; ** tip
;; Works in minibuffer
;; You can change direction with C-b and C-f. When starting at end of buffer zop-to-char search automatically backward.
;; You can use zop-to-char to move to a place (use C-q).
;; Hit repetitively the character you are searching will move to next.
;; You can copy or kill region from point to last search point.
;; C-g will quit and bring you back to initial position.

(use-package zop-to-char        ; Better zapping
  :ensure t
  :bind (("M-z" . zop-to-char)
     ("M-Z" . zop-up-to-char))
)

(use-package align            ; Align text in buffers
	:ensure t
  :bind (("C-c x a a" . align)
     ("C-c x a c" . align-current))
)
; * ag
; ** tips
;; ag to search for all files and ag-same to search for files of the same type
;; as the current buffer.
;; next-error and previous-error can be used to jump to the matches.
;; ag-find-file and ag-find-same-file use ag to list the files in the current
;; project. It's a convenient, though slow, way of finding files.
; ** setting
(use-package ag-and-a-half
  :init
    (defalias 'ag 'ag-and-a-half)
    (defalias 'ag-same 'ag-and-a-half-same)
    (defalias 'ag-find-file 'ag-and-a-half-find-file)
    (defalias 'ag-find-file-same 'ag-and-a-half-find-file-same)
)

; * git
;; p Visit previous historic version
;; n Visit next historic version
;; w Copy the abbreviated hash of the current historic version
;; W Copy the full hash of the current historic version
;; q Exit the time machine.
(setq vc-follow-symlinks nil)
(use-package git-timemachine
  :ensure t
  :init
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)
  :config
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
)
; * imenu
; ** tips
; imenu-max-item-length
; ** settings

(use-package imenu-tree
	:preface
		(use-package tree-mode
		:ensure t)
		(use-package windata
		:ensure t)
  :init
  (add-hook 'prog-mode-hook #'imenu-my/heading)
  :config
    (defun imenu-my/heading ()
    (setq imenu-sort-function 'imenu--sort-by-name
      imenu-prev-index-position-function nil)
    (add-to-list 'imenu-generic-expression `("Sections"
      ,(concat (regexp-quote comment-start) " \\* \\(.+\\)$") 1)))
)

; * xml
; odt shema
; install nxml-mode
;; (use-package rng-loc
;;   :init
;;   (add-to-list 'rng-schema-locating-files "~/.dotfiles/org/schema/schemas.xml")
;; )

(defun pprint-xml (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this. The function inserts linebreaks to separate tags that have
nothing but whitespace between them. It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
      (backward-char) (insert "\n") (setq end (1+ end)))
    (indent-region begin end)))

; * csv mode
;; After opening your_file.csv using Emacs, you might want to use
;; M-x toggle-truncate-lines to disable the warping of long
;; lines. Then, you can use M-x csv-align-fields to align fields.
;; You can set the variable csv-separators to change the separator

;; align just visible lines:
(use-package csv-mode
  :mode ("\\.csv\\'" . csv-mode)
  :ensure t
  :init
    (defun csv-align-visible (&optional arg)
      "Align visible fields"
      (interactive "P")
      (csv-align-fields nil (window-start) (window-end)))
  :bind
     ("C-c C-c" . csv-align-visible)
  :config
)



; * org-mode
; ** tips
; on heading C-c C-c set tags, C-c C-c on the top of the org file in dired.
; It updates the local config of the file: tags etc
;  C-c \     (org-match-sparse-tree) search tags
; helm-org-in-buffer-headings, there is counsel-outline

; ** settings
(use-package org
  :ensure t
  :defer 5
	:preface
    (defun add-pcomplete-to-capf ()
      (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))
    (add-hook 'org-mode-hook #'add-pcomplete-to-capf)
    ;; display/update images in the buffer after I evaluate
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
  :config
    (setq org-goto-interface 'outline-path-completion
      org-descriptive-links nil
      org-startup-truncated nil ;; wraps the lines in org-mode
      org-use-sub-superscripts nil ;subscript ‘a_{b}’, superscript 'a^{b}'
      org-goto-max-level 8)
    ; *** odt
    ;; M-x org-export-as-odt This should convert an org file to odt. 
    ;; (org-odt-convert-process "LibreOffice")
    (if (file-exists-p "/usr/bin/soffice")
    (setq org-export-odt-convert-processes
        "/usr/bin/soffice --headless --convert-to %f%x --outdir %d %i"
        org-odt-schema-dir "~/.dotfiles/org/schema"
        org-odt-styles-dir "~/.dotfiles/org/styles"
        org-export-odt-preferred-output-format "odt"
        org-export-backends (quote (ascii html latex odt)))
        ;; org-export-odt-convert-process "/usr/bin/soffice")
    (message "SOFFICE path not found"))
    ; *** screenshot
    (defun my/img-maker ()
    "Make folder if not exist, define image name based on time/date"
    (setq myvar/img-folder-path (concat default-directory "img/"))

    ; Make img folder if it doesn't exist.
    (if (not (file-exists-p myvar/img-folder-path)) ;[ ] refactor thir and screenshot code.
        (mkdir myvar/img-folder-path))

    (setq myvar/img-name (concat "img_" (format-time-string "%Y_%m_%d__%H_%M_%S") ".png"))
    (setq myvar/img-Abs-Path (concat myvar/img-folder-path myvar/img-name)) ;Relative to workspace.

    (setq myvar/relative-filename (concat "./img/" myvar/img-name))
    (insert "[[" myvar/relative-filename "]]" "\n"))

    (setq org-startup-with-inline-images nil)
    (defun my/org-screenshot ()
      "Take a screenshot into a time stamped unique-named file in the
      sub-directory (%filenameIMG) as the org-buffer and insert a link to this file."
      (interactive)
      (my/img-maker)
      ;(make-frame-invisible)
      (if (string= (getenv "exwm_enable") "yes")
      (winner-undo) (lower-frame))
      (call-process "import" nil nil nil myvar/img-Abs-Path)
      (if (string= (getenv "exwm_enable") "yes")
      (winner-undo) (raise-frame))
      ;(make-frame-visible)
      (org-display-inline-images))
)

; *** wiki
(use-package plain-org-wiki
  :load-path "extra/plain-org-wiki/"
  :init
    (bind-key "C-0" #'plain-org-wiki)
  :config
    (setq pow-directory "~/Nutstore/org/wiki")
      ;; org-wiki-files (quote ("~/Nutstore/org/wiki")))
    (add-to-list 'auto-mode-alist '("\\.org.gpg\\'" . org-mode))
)
; ** org-plus-contrib
(use-package org-plus-contrib
  :ensure t
)
; ** org download
;org-download-yank to download links like url

(use-package org-download
  :ensure t
  :config
  ;; (setq org-download-method 'attach)
  (defun my-org-download-method (link)
  (let ((filename
         (file-name-nondirectory
          (car (url-path-and-query
                (url-generic-parse-url link))))))
    (message "current-buffer: %s" (current-buffer))

    (expand-file-name filename "~/")))
    (setq org-download-method 'my-org-download-method)
    (setq-default org-download-image-dir "./img")
)

; * org odt
; ** table
;  at table header, use "C-c -" to insert seperating lines
; 通过 "C-c | (create or convert from region)" 这个快捷键来快速创建指定大小的表格。
; 以逗号(,)分隔的 CSV 格式的数据，可以将其拷贝到当前在编辑的 Org mode 文档中，
; 选中然后使用 "C-c |" 这个快捷键，就能将其转换成表格形式
; C-|, to transform the CSV file to an org-mode table.
; 如果数据之间是用空格分隔的，该如何转换呢?选中后使用快捷键"C-u 1 C-c |"即可。
; S-return 	当单元格无内容时，将其上方第一个非空内容拷贝过来;否则拷贝当前内容到下一行并随之移动
;; C-c C-c 	强制表格重新排列, also move cursor to the beginning of the cell
;; C-c ^ 	表格排序
; 在表格下方手工添加以 "+TBLFM:" 开头的行，然后直接添加公式 such as $2=$1+$3
; org table recalculate C-c * and C-u C-c C-c
; 在Org mode的表格公式中，用 "@" 来表示行，用 "$" 来表示列，最简单的，"@3$2" 表
; 示的是第三行第二列的位置。使用快捷键 "C-c }" 可以开启表格的横纵坐标显示
; 左上角为第二行第一列单元格、右下角为第四行第三列单元格的区域， @2$1..@4$3
; "@#" 表示当前行的行号，用 "$#" 表示当前列的列号
; Org mode 默认使用的是 Emacs 中自带的 Calc 这个 package 来进行计算
; 可以在org文件中添加诸如这样的行(define variable): #+CONSTANTS: pi=3.14 eps=2.4e-6 
; *** alignment
;; If you would like to overrule the automatic alignment of
;; number-rich columns to the right and of string-rich columns to
;; the left, you can use ‘<r>’, ‘<c>’1 or ‘<l>’ in a similar
;; fashion. You may also combine alignment and field width like
;; this: ‘<r10>’.



; ** list
; 1. a
; - b
; - c
; Now, hit C-c C-c at the first line "1. a". Unordered list becomes numbered
; list and vice versa


; *** style
; set org-odt-styles-file to A .odt or .ott file to 
; Use the styles.xml contained in the specified OpenDocument Text or Template file 
; * org babel
; ** tips
; C-c ' to edit the current code block, C-c C-c to execute
; Open a REPL using C-c C-v C-z so that you get completion in Python buffers.
; py expand, :session and :file are mutual exclusive
; Be sure to use %matplotlib inline, otherwise graphics won’t work.

; ** org chinese
(use-package org-chinese-utils
  :ensure t
	:init
	  (add-hook 'org-mode-hook 'org-chinese-utils-enable)
  :config
    (require 'org)
    (setq org-default-language "zh-CN")
    (require 'ox)
    (require 'ox-odt)
    ;; latex
    (require 'ox-latex)
    (setq org-latex-coding-system 'utf-8)
    ;; 不要在latex输出文件中插入\maketitle

    ;; (setq org-latex-title-command "")
    (setq org-latex-date-format "%Y-%m-%d")
    (setq org-odt-styles-dir "~/.dotfiles/.emacs.d/org/styles/")

    ;; (setq org-export-with-LaTeX-fragments 'imagemagick)
    (setq org-latex-create-formula-image-program 'dvipng);imagemagick)
    ;;org-create-formula-image-with-imagemagick
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
    (setq org-format-latex-options (plist-put org-format-latex-options :foreground "Black"))
    (setq org-format-latex-options (plist-put org-format-latex-options :background "Transparent"))
    (setq org-format-latex-options
         (plist-put org-format-latex-options :html-scale 2.0))
    (setq org-format-latex-options
         (plist-put org-format-latex-options :foreground "black"))
    (setq org-format-latex-options
         (plist-put org-format-latex-options :background "transparent"))


    (setq org-latex-commands '(("xelatex -interaction nonstopmode -output-directory %o %f"
                                "bibtex %b"
                                "xelatex -interaction nonstopmode -output-directory %o %f"
                                "xelatex -interaction nonstopmode -output-directory %o %f")
                            ("xelatex -interaction nonstopmode -output-directory %o %f")))

    ;; org不建议自定义org-latex-default-package-alist变量，但"inputenc" and "fontenc"两个宏包似乎和
    ;; xelatex有冲突，调整默认值！
    (setf org-latex-default-packages-alist
        (remove '("AUTO" "inputenc" t) org-latex-default-packages-alist))
    (setf org-latex-default-packages-alist
        (remove '("T1" "fontenc" t) org-latex-default-packages-alist))
    (setf org-latex-default-packages-alist
        (remove '("normalem" "ulem" t) org-latex-default-packages-alist))
    ;; latex公式预览, 调整latex预览时使用的header,默认使用ctexart类
    ;; (setq org-format-latex-header
    ;;   (replace-regexp-in-string
    ;;    "\\\\documentclass{.*}"
    ;;    "\\\\documentclass[nofonts,UTF8]{ctexart}"
    ;;    org-format-latex-header))
    (setq org-format-latex-header
    "\\documentclass[border=0pt]{standalone}\n\\usepackage{fontspec}\n\\usepackage{xeCJK}\n\\setCJKmainfont{WenQuanYi Micro Hei}")

    ;; Use XeLaTeX to export PDF in Org-mode
    (setq org-latex-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))
    (setq org-odt-category-map-alist
        '(("__Table__" "Table" "value" "表" org-odt--enumerable-p)
        ("__Figure__" "Figure" "value" "图" org-odt--enumerable-image-p)
        ("__MathFormula__" "Text" "math-formula" "公式" org-odt--enumerable-formula-p)
        ("__DvipngImage__" "Equation" "value" "公式" org-odt--enumerable-latex-image-p)
        ("__Listing__" "Listing" "value" "项" org-odt--enumerable-p))
    )
    (defun eh-org-latex-compile (orig-fun texfile &optional snippet)
    (let ((org-latex-pdf-process
            (if snippet (car (cdr org-latex-commands))
            (car org-latex-commands))))
        (funcall orig-fun texfile snippet)))

    (advice-add 'org-latex-compile :around #'eh-org-latex-compile)

    ;; (setq org-latex-to-mathml-convert-command
    ;;             "java -jar %j -unicode -force -df %o %I"
    ;;             org-latex-to-mathml-jar-file
    ;;             "~/.dotfiles/bin/mathtoweb.jar")
    ;; (use-package ox-latex-chinese
    ;;     :ensure nil
    ;;     :config (oxlc/toggle-ox-latex-chinese t))


)

; *** ob-ipython
(use-package ob-ipython
  :ensure t
  :config
    (setq org-confirm-babel-evaluate nil)    ;don't prompt me to confirm everytime I want to evaluate a block
)
; *** ob-async
;; Simply add the keyword :async to the header-args of any org-babel src block
(use-package ob-async
  :init
   ;; (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)
)


; ** org reveal
(use-package ox-reveal
  ;; :ensure t
  :init
    (setq org-reveal-root "file://home/q/.dotfiles/org/reveal/js/reveal.js")
  :config
    (setq org-reveal-hlevel 2)
)
(use-package ox
  :ensure nil
  :demand t
  :config
    (require 'ox)
    (require 'ox-reveal)
    (require 'ox-html)
)
;; (use-package htmlize
;;   :ensure t
;; )

; * latex

(use-package latex                      ; LaTeX editing
  :ensure auctex
  :preface
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)    ; Easy math input
  ;; set XeTeX mode in TeX/LaTeX
  :config
  ;; Teach TeX folding about KOMA script sections
  ;; (require 'auctex)
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-command-default "XeLaTeX"
    TeX-save-query nil
    TeX-show-compilation t
    TeX-outline-extra `((,(rx (0+ space) "\\section*{") 2)
                            (,(rx (0+ space) "\\subsection*{") 3)
                            (,(rx (0+ space) "\\subsubsection*{") 4)
                            (,(rx (0+ space) "\\minisec{") 5))
    ;; No language-specific hyphens please
    LaTeX-babel-hyphen nil)
)

; * pdf
(setq doc-view-continuous t)
; * outline

(use-package outline-minor-mode
  :preface
    (defun my/heading()
    "custom heading for all prog mode, heading must ends in
    outline-heading-end-regexp,which is : for python mode
    * in heading to differentiate from comment, custom heading has higher level "
    (when (and (not (equal major-mode 'org-mode)) comment-start)
      (make-local-variable 'my/outline-regexp)
      (make-local-variable 'my-outline-max-level)
      (setq my-outline-max-level 8)
      ;; (setq my/outline-regexp (concat (regexp-quote (substring comment-start 0 1))
      ;; match space before heading ?
      ;; (setq my/outline-regexp (concat "\\s-" (regexp-quote comment-start) " [*]\\{1,8\\}"))
      (setq my/outline-regexp (concat (regexp-quote comment-start) " [*]\\{1,8\\}"))
      (setq outline-heading-alist '()) ;; make outline promote work
      (let ((level 0)
        (level-prefix (concat comment-start " ")))
        ;; (level-prefix (concat (substring comment-start 0 1) " ")))
        (while (< level my-outline-max-level)
        (setq outline-heading-alist (cons (cons level-prefix level) outline-heading-alist)
        level (1+ level)
        level-prefix (concat level-prefix "*")))
        (setq outline-heading-alist (nreverse outline-heading-alist)))
      (setq outline-regexp (concat my/outline-regexp "\\|" outline-regexp)
        outline-level (lambda()
         (let* ((data (match-data))
           (start (car data))
           (end (cadr data))
           (level (- end start)))
           (if (looking-at my/outline-regexp)
           (- level 2) ;; subtract two spaces
           (+ level my-outline-max-level));; 8 is the maximum custom heading levels
    )))))
    (add-hook 'outline-minor-mode-hook 'my/heading)
  :init
    (defun my/outline-cycle ()
    (interactive)
    (when (outline-on-heading-p)
        (cond ((not (outline-subheadings-visible-p))
        (show-children))
        ((and (outline-body-p)(not (outline-body-visible-p)))
        (show-entry))
        ((not (outline-subtree-visible-p))
        (show-subtree))
        ((and (outline-subheadings-p)
        (outline-subheadings-visible-p))
        (hide-subtree))
        ((and (outline-body-p)
            (outline-body-visible-p))
        (hide-entry)
        (hide-leaves))
        )))
    (defun outline-body-p ()
    (save-excursion
        (outline-back-to-heading)
        (outline-end-of-heading)
        (and (not (eobp))
        (progn (forward-char 1)
            (not (outline-on-heading-p))))))

    (defun outline-body-visible-p ()
    (save-excursion
        (outline-back-to-heading)
        (outline-end-of-heading)
        (not (outline-invisible-p))))

    (defun outline-subtree-visible-p ()
    (interactive)
    (save-excursion
        (outline-back-to-heading)
        (let ((level (funcall outline-level)))
        (outline-next-heading)
        (cond ((and (not (eobp)) (< level (funcall outline-level)))
        (outline-end-of-heading) (not (outline-invisible-p)))
        (t t)
        ))))

    (defun outline-subheadings-p ()
    (save-excursion
        (outline-back-to-heading)
        (let ((level (funcall outline-level)))
        (outline-next-heading)
        (and (not (eobp))
        (< level (funcall outline-level))))))

    (defun outline-subheadings-visible-p ()
    (interactive)
    (save-excursion
        (outline-next-heading)
        (not (outline-invisible-p))))
    (hook-into-modes #'outline-minor-mode
           'text-mode-hook
           'prog-mode-hook)

    (defadvice evil-goto-line (after expand-after-goto-line activate compile)
    "hideshow-expand affected block when using goto-line in a collapsed buffer"
    (save-excursion (show-all) (show-all)))
    (setq search-invisible nil)

    ; *** evil object
    (defun my/outline-start ()
    (interactive)
    (end-of-line)
    (let (found)
    (while (not found)
        (setq found (re-search-backward my/outline-regexp nil nil))
        (if found (goto-char found)
        (error "before first heading"))
        (if (not (outline-on-heading-p)) (setq found nil)))))

    (defun my/outline-next ()
    (interactive)
    (end-of-line)
    (let (found)
    (while (not found)
        (setq found (re-search-forward my/outline-regexp nil nil))
        (if found (goto-char found)
        (error "before first heading"))
        (if (not (outline-on-heading-p)) (setq found nil))))
    (previous-line))

    (defun my/outline-body-start ()
    (interactive)
    (condition-case nil
        (progn (my/outline-start)(next-line))
        (error (goto-char (point-min)))))

    (defun my/outline-end ()
    (interactive)
    (my/outline-start)
    (condition-case nil
    (outline-forward-same-level 1)
    (error (condition-case nil
        (my/outline-next)
        (error (goto-char (point-max)))))))

    (defun my/evil-object-range (backward forward &optional type)
    (interactive)
        (save-excursion
        (setq beg (progn (funcall backward) (point))))
        (save-excursion
        (funcall forward) (setq end (point)))
        (evil-range beg end))

    (defun evil-an-object-range (count forward &optional backward type newlines)
    "Return a text object range (BEG END) of COUNT objects with whitespace.
    See `evil-inner-object-range' for more details."
    (let ((range (evil-inner-object-range count forward backward type)))
    (save-excursion
    (save-restriction
    (if newlines
    (evil-add-whitespace-to-range range count)
    (narrow-to-region
    (save-excursion
    (goto-char (evil-range-beginning range))
    (line-beginning-position))
    (save-excursion
    (goto-char (evil-range-end range))
    (line-end-position)))
    (evil-add-whitespace-to-range range count))))))
    
    (evil-define-text-object evil-inner-outline (count &optional beg end type)
    "Select around my outline.Return an outer text object range."
    (my/evil-object-range #'my/outline-body-start #'my/outline-end))

    (evil-define-text-object evil-outer-outline (count &optional beg end type)
    "Select around my outline.Return an outer text object range."
    (my/evil-object-range     #'my/outline-start #'my/outline-end))

    (define-key evil-outer-text-objects-map "l" 'evil-outer-outline)
    (define-key evil-inner-text-objects-map "l" 'evil-inner-outline)
  :bind
    (("<S-left>" . outline-promote)
     ("<S-right>" . outline-demote)
     ("M-C--" . hide-sublevels))
)

; * paredit
; M-\ use " to enclose selection or word
(use-package paredit-everywhere
	:ensure t
	:preface
	  (use-package paredit-menu
			:ensure t)
  :bind
    ("M-\\" . paredit-meta-doublequote)
  :init
   (hook-into-modes #'electric-pair-mode
           'text-mode-hook
           'prog-mode-hook)
   ;; make electric-pair-mode work on more brackets
   (setq electric-pair-pairs '(
             (?\' . ?\')
             (?\" . ?\")
             (?\{ . ?\})
             ))
   (hook-into-modes #'paredit-everywhere-mode
           'text-mode-hook
           'prog-mode-hook)
)
; error loading
;; (use-package electric-operator
;;   :ensure t
;;   :init
;;   (hook-into-modes #'electric-operator-mode
;;            'python-mode-hook)
;; )
; * evil-matchit
(use-package evil-matchit
  :ensure t
  :config
    (global-evil-matchit-mode 1)
)
; * corral
(use-package corral
  :bind
    (("M-9" . corral-parentheses-backward)
    ("M-0" . corral-parentheses-forward)
    ("M-[" . corral-brackets-backward)
    ("M-]" . corral-brackets-forward)
    ("M-{" . corral-braces-backward)
    ("M-}" . corral-braces-forward)
    ("M-\"" . corral-double-quotes-backward))
  :config
    ; ** solve conflict with paredit
    (define-key paredit-mode-map (kbd "M-]") nil)
    (define-key paredit-mode-map (kbd "M-{") nil)
    (define-key paredit-mode-map (kbd "M-}") nil)
    (define-key paredit-mode-map (kbd "M-\"") nil)
    (define-key paredit-everywhere-mode-map (kbd "M-]") nil)
    (define-key paredit-everywhere-mode-map (kbd "M-{") nil)
    (define-key paredit-everywhere-mode-map (kbd "M-}") nil)
    (define-key paredit-everywhere-mode-map (kbd "M-\"") nil)
)


; * ido
; ** tips
;; ido-init-completion-maps
;; "\C-a" 'ido-toggle-ignore
;; "\C-c" 'ido-toggle-case
;; "\C-e" 'ido-edit-input
;; "\t" 'ido-complete
;; " " 'ido-complete-space
;; "\C-j" 'ido-select-text
;; "\C-j" 'ido-select-text
;; "\C-m" 'ido-exit-minibuffer
; ** setting
;; (use-package ido
;;   :init
;; 	  (require 'ido)
;;     (ido-everywhere t)
;;   :bind
;;     ("C-x b" . ido-switch-buffer)
;;   :config
;;     (setq ido-enable-flex-matching t
;;     ido-auto-merge-work-directories-length -1
;;     ido-create-new-buffer 'always
;;     ido-max-prospects 10
;;     ido-read-file-name-non-ido nil
;;     ido-use-filename-at-point nil
;;     ido-use-virtual-buffers t)
;; )

; * ibuffer
; ** tips
;; * s (mark star buffers) D (delete all marked buffers)
;; * r k (remove read-only buffers from ibuffer)
;; % n (mark buffers by regex) U (regex replace)
;; q close current window

; ** setting
(use-package ibuffer
  :bind
    (("C-x C-b" . ibuffer)
		:map  ibuffer-mode-map
		("s p" . ibuffer-do-sort-by-pathname)
    ("e" . ibuffer-ediff-marked-buffers)
		)

  :config
   ;; add another sorting method for ibuffer (allow the grouping of
   ;; filenames and dired buffers
   (define-ibuffer-sorter pathname
      "Sort the buffers by their mode and pathname."
      (:description "mode plus filenames")
      (string-lessp
      (with-current-buffer (car a)
      (or (concat (symbol-name major-mode) buffer-file-name)
      (if (eq major-mode 'dired-mode)
      (concat (symbol-name major-mode) (expand-file-name dired-directory)))
      ;; so that all non pathnames are at the end
      "~"))
      (with-current-buffer (car b)
      (or (concat (symbol-name major-mode) buffer-file-name)
      (if (eq major-mode 'dired-mode)
      (concat (symbol-name major-mode) (expand-file-name dired-directory)))
      ;; so that all non pathnames are at the end
      "~"))))

   (defun ibuffer-ediff-marked-buffers ()
    (interactive)
    (let* ((marked-buffers (ibuffer-get-marked-buffers))
       (len (length marked-buffers)))
      (unless (= 2 len)
        (error (format "%s buffer%s been marked (needs to be 2)"
               len (if (= len 1) " has" "s have"))))
      (ediff-buffers (car marked-buffers) (cadr marked-buffers))))

   ;; Ensure ibuffer opens with point at the current buffer's entry.
   (defadvice ibuffer
      (around ibuffer-point-to-most-recent) ()
      "Open ibuffer with cursor pointed to most recent buffer name."
      (let ((recent-buffer-name (buffer-name)))
      ad-do-it
      (ibuffer-jump-to-buffer recent-buffer-name)))
   (ad-activate 'ibuffer)

   (setq ibuffer-formats
      '((mark modified read-only " "
          (name 30 30 :left :elide) ; change: 30s were originally 18s
          " " (size 9 -1 :right) " " (mode 10 12 :left :elide)
          " " filename-and-process)
          (mark " " (name 16 -1) " " filename))
    ibuffer-expert t
    ibuffer-show-empty-filter-groups nil)
)
 ;; hide certain buffer
(use-package ibuf-ext
  :preface
    (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
  :config
  ;; Enable ibuffer-filter-by-filename to filter on directory names too.
  (define-ibuffer-filter filename
     "Toggle current view to buffers with file or directory name matching QUALIFIER."
     (:description "filename"
      :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
     (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
            (buffer-local-value 'dired-directory buf))
       (string-match qualifier it)))
  (add-to-list 'ibuffer-never-show-predicates "^\\*helm")
)

; * lisp
(setq lisp-indent-function 'common-lisp-indent-function)
; C-M-q reindent, C-M-f,b,u,d work on s-expression

(use-package bug-hunter            ; Search init file for bugs
  :ensure t
)
(add-hook 'emacs-lisp-mode-hook
  (lambda ()
  (setq tab-width 2)))
(setq lisp-body-indent 2)

; * lispy
(use-package lispy
  :ensure t
  :init
    ;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
    ;; (defun conditionally-enable-lispy ()
    ;; (when (eq this-command 'eval-expression)
    ;;     (lispy-mode 1)))
    ;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
)

(use-package evil-lispy
  :ensure t
  :init
    ;; (add-hook 'emacs-lisp-mode-hook #'evil-lispy-mode)
    ;; (add-hook 'clojure-mode-hook #'evil-lispy-mode)
)

; * python

(use-package anaconda-mode        ; Powerful Python backend for Emacs
  :ensure t
  :defer 20
  :init
    (add-hook 'python-mode-hook #'anaconda-mode)
  :config
		(use-package company-anaconda        ; Python backend for Company
		:ensure t
		:init (add-to-list 'company-backends 'company-anaconda))
)


;; The package is "python" but the mode is "python-mode":
(use-package python
  :preface
  (setenv "PYTHONPATH" "/my_bin/anaconda2/bin:")
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook '(lambda ()
         (evil-matchit-mode 0)
         (setq company-backends '(company-dabbrev company-anaconda))
         (setq indent-tabs-mode nil tab-width 4)))
  ;; (add-hook 'python-mode-hook 'eldoc-mode)
  ;; (add-hook 'python-mode-hook 'jedi:setup)
	:init
    (evil-define-key 'normal python-mode-map (kbd "SPC") 'hydra-python/body)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  ;; (require 'py-smart-operator)
  ;; (add-hook 'python-mode-hook 'py-smart-operator-mode)
  (setq python-shell-interpreter "ipython"
        python-indent-offset 4
        python-shell-interpreter-args "--pylab")
  ;; (local-set-key (kbd "M-n") 'flymake-goto-next-error)
  ;; (local-set-key (kbd "M-p") 'flymake-goto-prev-error)

  :config
  (define-key python-mode-map (kbd "C-<tab>") 'hydra-python/body)
  (defhydra hydra-python (:inherit (hydra-evil/heads) :color blue)
  "python"
   ("gd" anaconda-mode-goto "anaconda definition")
   ("v" anaconda-mode-view-doc "anaconda doc"))
  ;; (setq jedi:complete-on-dot t)
  ;; (setq python-shell-virtualenv-path "/home/q/anaconda/")
  ;; (setq python-shell-exec-path "/home/q/anaconda/bin")

)

; * highlight
;; underline
(use-package global-hl-line-mode
  :init
    (global-hl-line-mode 1)
  :config
    (set-face-attribute hl-line-face nil :background "gray30" :foreground nil :overline t)
    (add-hook 'font-lock-mode-hook
    (function
    (lambda ()
    (setq font-lock-keywords
    (append font-lock-keywords
        '(("\t+" (0 'my-tab-face t))
        ("[ \t]+$" (0 'my-trailing-space-face t)))
        ;; ("^.\\{81\\}\\(.+\\)$" (1 'my-long-line-face t)))
        )))))
)

(use-package rainbow-delimiters        ; Highlight delimiters by depth
  :ensure t
  :defer t
  :init
  (dolist (hook '(text-mode-hook prog-mode-hook))
    (add-hook hook #'rainbow-delimiters-mode)))

(use-package hi-lock            ; Custom regexp highlights
  :init (global-hi-lock-mode))

(use-package highlight-numbers        ; Fontify number literals
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package highlight-symbol        ; Highlighting and commands for symbols
  :ensure t
  :bind
  (("C-c s %" . highlight-symbol-query-replace)
   ("C-c s n" . highlight-symbol-next-in-defun)
   ("C-c s p" . highlight-symbol-prev-in-defun))
  ;; Navigate occurrences of the symbol under point with M-n and M-p, and
  ;; highlight symbol occurrences
  :init
  (dolist (fn '(highlight-symbol-nav-mode highlight-symbol-mode))
    (add-hook 'prog-mode-hook fn))
  :config
  (setq highlight-symbol-idle-delay 0.4        ; Highlight almost immediately
    highlight-symbol-on-navigation-p t) ; Highlight immediately after
                    ; navigation
  :diminish highlight-symbol-mode)

; * large file
;; see also http://anirudhsasikumar.net/blog/2005.01.21.html
(defun my/large-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 500 1024))
    (buffer-disable-undo)
    (message "my/large-file-hook applied")))

(defun my/huge-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 4096 1024))
    (set (make-local-variable 'backup-inhibited) t)
    (setq buffer-auto-save-file-name nil) ; disable autosave
    (font-lock-mode -1)(company-mode -1)
    (fundamental-mode)
    (message "my/huge-file-hook applied")))

(add-hook 'find-file-hooks 'my/large-file-hook)
(add-hook 'find-file-hooks 'my/huge-file-hook)

(use-package recentf
  :bind
    (("C-x C-r" . recentf-open-files)
     ("M-l" . move-to-window-line-top-bottom)
    )
  :config
    (setq recentf-max-menu-items 30)
)
(use-package saveplace)

(use-package uniquify            ; Make buffer names unique
	:init
    (setq uniquify-buffer-name-style 'forward))

(use-package shackle ;; breaks winner-boring-buffers?
  :ensure t
  :defer 30
  :init
    (shackle-mode)
  :config
    ;; help-mode
    ;; (completion-list-mode :noselect t)
    ;; (compilation-mode :noselect t)
    ;; (grep-mode :noselect t)
    ;; (occur-mode :noselect t)
    ;; ("*Pp Macroexpand Output*" :noselect t)
    ;; "*Shell Command Output*" "*vc-diff*" "*vc-change-log*"
    ;; (" *undo-tree*" :width 60 :position right)
    ;; ("^\\*anything.*\\*$" :regexp t)
    ;; "*slime-apropos*" "*slime-macroexpansion*" "*slime-description*"
    ;; ("*slime-compilation*" :noselect t)
    ;; "*slime-xref*"
    ;; (sldb-mode :stick t)
    ;; slime-repl-mode slime-connection-list-mode)
    ;; ("\\*Tex Help\\*" :regexp t :noselect t)
    (setq shackle-rules
    '((compilation-mode :select t)
      (TeX-errors-mode :select t)
      ("\\`\\*image-dired.*?\\*\\'" :regexp t :select nil) ; any buffer name begins with *image-dired
      (image-dired-thumbnail-mode :select nil) ; image-dired
      (image-dired-image-display-mode :select nil) ; image-dired
      (append special-buffer-regexp (list :regexp t :select t))
      (help-mode :other t :select t)
      (special-mode :other t :select t))
    shackle-default-rule
    '(:select t))
    ;; do I need to unquote it
    ;; (,special-buffer-regexp :regexp t :select t)
    ;; (push '("*Help*" :regexp t :noselect nil) popwin:special-display-config)
    ;; (push '(special-buffer-regexp :regexp t :noselect nil)
    ;;      popwin:special-display-config)
)

; * killring
; ** tips
; repeat C-y do yank-pop, to see the kill ring use M-y
(use-package easy-kill            ; Easy killing and marking on C-w
  :ensure t
  :bind (([remap kill-ring-save] . easy-kill)
    ([remap mark-sexp]     . easy-mark)
    ("C-w" . backward-kill-word) ; it's more efficient to kill word
    ; type again than backspace
    ("C-x C-k" . kill-region)
    )
  :init
    (put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position)
       (line-beginning-position 2)))))

    (defvar kill-ring-entry-length 3)
    (defun my/replace-blank-kill (args)
      (let ((string (car args))
        (replace (cdr args))
        (last (car-safe kill-ring)))
        (when (or (and last (string-blank-p last))
        (< (length last) kill-ring-entry-length))
        (setq replace t))
        (list string replace)))
    ;; (when (version= emacs-version "24.4") ;; no version>=
    (advice-add 'kill-new :filter-args #'my/replace-blank-kill)
)

; * minibuffer
;; (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
(use-package savehist
  :init
    (savehist-mode t)
)
; C-x 3 split horizontally)
(defun toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
    (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame
(bind-key* "C-x 5" #'toggle-frame-split)

; * gpg gpg-agent is that it is similarly to ssh-agent just
;; stores the passwords you have, once you activate it. So you won't
;; be prompted for the password neither when you open a file, nor
;; when you save it (as long as the agent remembers the password)
(use-package epa-file
  :init
    ;; (epa-file-enable)
    ;; ask encyption password once
    (setq epa-file-cache-passphrase-for-symmetric-encryption t)
)

; prevent Emacs spread copies of files with sensitive data.
(use-package sensitive
  :ensure t
)
; * abaqus
; fortran
(setq fortran-comment-region "c")
(use-package abaqus-mode
  :load-path "extra/abaqus/"
  :preface
    (add-hook 'abaqus-mode-hook 'turn-on-font-lock)
    (add-hook 'abaqus-mode-hook 'outline-minor-mode)
  :config
    (define-coding-system-alias 'mbcs 'utf-8)
  :mode (
         ("\\.inp\\'" . abaqus-mode)
         ("\\.pes\\'" . abaqus-mode)
        )
)
; * server
(use-package server
  :config
    (unless (server-running-p) (server-start))
)
; * winner eyebrowse

(use-package winner
  ; winner-boring-buffers, buffers to exclude in winner-undo
  :ensure t
  ;; :bind 
     ;; (("C-9" . winner-redo)
     ;; ("C-8" . winner-undo))
	:init
	  (winner-mode)
    (bind-key "C-9" #'winner-redo)
    (bind-key "C-8" #'winner-undo)
  :config
  (setq winner-boring-buffers
        '("*Apropos*" "*Buffer List*" "*Completions*" "*Compile-Log*" "*cvs*" "*Fuzzy Completions*"
           "*Help*" "*Ibuffer*" "*inferior-lisp*"
           "*helm mini*" "*helm projectile*" "*helm M-x*" "*helm resume*"))

; eyebrowse evil binding
;; C-c C-w < 	Switch to previous window config
;; C-c C-w > 	Switch to next window config
;; C-c C-w ' 	Switch to last window config
;; C-c C-w " 	Close current window config
;; C-c C-w , 	Rename current window config
;; C-c C-w 0 	Switch to window config 0

;; eyebrowse-keymap-prefix (defaults to C-c C-w)
;; insert the customization before enabling eyebrowse-mode.

(use-package eyebrowse
  :ensure t
  :init
    (eyebrowse-mode t) 
  :config
)

; * exwm
; tips
;; find representation of key event (read-key) or (read-event) 
;; C-c C-f 	exwm-layout-set-fullscreen 	Enter fullscreen mode
;; C-c C-h 	exwm-floating-hide 	Hide a floating X window
;; s-Mouse 1: exwm-input-move-event
;; C-c C-k 	exwm-input-release-keyboard 	Switch to char-mode

; floating X window can be moved (resized) by s-<down-mouse-1> (s-<down-mouse-3>)
; exwm-floating-move exwm-layout-{enlarge,shrink}-window[-horizontally]
; leave fullscreen mode with exwm-reset.

;; exwm-mode buffers are always started in line-mode.
;; Global key bindings are available in both line-mode and char-mode.
;; exwm-input-set-key which works precisely like global-set-key.
; (exwm-input-set-key (kbd "M-:") 'eval-expression)

;; exwm-input-prefix-keys only works in line-mode. The only way
;; to make a local key binding is to modify exwm-mode-map with
;; e.g. define-key. You may have to add the prefix key (first key)
;; of the key sequence to exwm-input-prefix-keys if the prefix key
;; is not already there, default '(?\C-c ?\C-x ?\C-u ?\C-h ?\M-x ?\M-` ?\M-& ?\M-:)


)
(use-package xelb
  ;; :load-path "extra/xelb/"
  :ensure t
  :if (string= (getenv "exwm_enable") "yes")
)
(use-package exwm
  ;; :load-path "extra/exwm/"
  :ensure t
  :if (string= (getenv "exwm_enable") "yes")
  :preface
    ;; (use-package exwm-x
    ;;   :ensure t)
   (require 'exim)
   (add-hook 'exwm-init-hook 'exim-start)
  :bind("s-c q" . exwm-input-send-next-key)
  :init
    ; a super modifier (windows key) can be pressed before the key it modifies 
    (define-key key-translation-map (kbd "<menu>") 'event-apply-super-modifier)
    ;; (setq xcb:keysyms:auto-update nil)
    (require 'exwm)
    (exwm-input-set-simulation-keys
     '(([?\C-b] . left)
       ([?\C-f] . right)
       ([?\C-p] . up)
       ([?\C-n] . down)
       ([?\C-a] . home)
       ([?\C-e] . end)
       ([?\M-p] . prior)
       ([?\M-n] . next)
       ([?\C-v] . next)
       ([?\C-y] . (?\C-v))
       ([?\M-w] . (?\C-c))
       ([?\C-w] . (?\C-x))
       ([?\C-/] . (?\C-z))
       ([?\C-d] . delete)
       ([?\C-k] . (S-end delete)))); S-end = shift + end

    (exwm-enable)
    (defun exwm-toggle-fullscreen()
      (interactive)
      (if (memq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
        (exwm-layout-unset-fullscreen) (exwm-layout-set-fullscreen)))
  :config
    (require 'exwm-config)
    (setq exwm-workspace-number 2)
    (setq exwm-manage-force-tiling nil)
    (require 'exwm-randr)
    (defun laptop-display ()
      (interactive)
      (progn 
         (setq exwm-randr-workspace-output-plist '(0 "LVDS1" 1 "VGA1"))
         (start-process-shell-command
         "xrandr" nil "xrandr --output VGA1 --left-of LVDS1 --auto"))
         (exwm-randr--refresh))
    (defun office-display ()
      (interactive)
      (progn 
         (setq exwm-randr-workspace-output-plist '(0 "DVI-D-0" 1 "VGA-0"))
         (start-process-shell-command
         "xrandr" nil "xrandr --output DVI-D-0 --left-of VGA-0 --auto"))
         (exwm-randr--refresh))

    (exwm-randr-enable)

    (exwm-enable-ido-workaround)

    ; no C-x in keysnail
    (add-hook 'exwm-manage-finish-hook
      (lambda ()
        (when (and exwm-class-name
                   (or (string= exwm-class-name "Firefox") (string= exwm-class-name "Firefox-esr")))
          (progn (turn-off-my-mode)
                 (exwm-input-set-local-simulation-keys nil)
                 (setq-local exwm-input-prefix-keys '(?\C-h ?\M-x ?\M-` ?\M-& ?\C-x ?\s-c))))))
                 ;; (setq-local exwm-input-prefix-keys '(?\C-x))))))

    (add-hook 'exwm-manage-finish-hook
      (lambda ()
        (when (and exwm-class-name
                   (string= exwm-class-name "Emacs"))
          (progn (turn-off-my-mode)
                 (exwm-input-set-local-simulation-keys nil)
                 (setq-local exwm-input-prefix-keys '(?\s-c))))))
                 ;; (setq-local exwm-input-prefix-keys '(?\C-x))))))

    (add-hook 'exwm-manage-finish-hook
      (lambda ()
        (when (and exwm-class-name
                   (string= exwm-class-name "xfreerdp"))
          (progn (exwm-floating--unset-floating exwm--id)
                 (setq-local exwm-input-prefix-keys '(?\s-c))))))

   (defun maximise-emacs (arg &rest args)
      (when (and exwm-class-name (string= exwm-class-name "Emacs"))
         (exwm-toggle-fullscreen)))
   (advice-add 'switch-to-buffer :after #'maximise-emacs)


    ;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
    ;; it in `exwm-update-class-hook' and `exwm-update-title-hook'
    ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
    ;; + Only renaming buffer in one hook and avoid it in the other. There's no
    ;;   guarantee on the order in which they are run.
    ;; + For applications with multiple windows (e.g. GIMP), the class names of all
    ;;   windows are probably the same. Using window titles for them makes more sense.
    ;; + Some application change its title frequently (e.g. browser, terminal).
    ;;   Its class name may be more suitable for such case.
    ;; In the following example, we use class names for all windows expect for
    ;; Java applications and GIMP.
    ;; (add-hook 'exwm-update-class-hook
    ;;         (lambda ()
    ;;             (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
    ;;                         (string= "gimp" exwm-instance-name))
    ;;             (exwm-workspace-rename-buffer exwm-class-name))))
    ;; (add-hook 'exwm-update-title-hook
    ;;         (lambda ()
    ;;             (when (or (not exwm-instance-name)
    ;;                     (string-prefix-p "sun-awt-X11-" exwm-instance-name)
    ;;                     (string= "gimp" exwm-instance-name))
    ;;             (exwm-workspace-rename-buffer exwm-title))))

    (defun exwm-rename-buffer ()
    (interactive)
    (exwm-workspace-rename-buffer
    (concat exwm-class-name ":"
            (if (<= (length exwm-title) 50) exwm-title
                (concat (substring exwm-title 0 49) "...")))))

    ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
    (add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
    (add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
    
    (exwm-config-ido)
    ;; `exwm-input-set-key' allows you to set a global key binding (available in
    ;; any case). Following are a few examples.
    ;; + We always need a way to go back to line-mode from char-mode
    ;; 's-r': Reset
    (exwm-input-set-key (kbd "s-/") #'exwm-reset) ;Switch to line-mode exit fullscreen mode refresh layout
    (exwm-input-set-key (kbd "s-b") #'exwm-workspace-switch-to-buffer)
    ;; (exwm-input-set-key (kbd "C-=") #'exwm-input-toggle-keyboard)
    (exwm-input-set-key (kbd "M-<tab>") #'exwm-workspace-switch)
    (exwm-input-set-key (kbd "C-M-<return>") #'exwm-toggle-fullscreen)
    (exwm-input-set-key (kbd "s-\\") #'toggle-input-method) 
    (exwm-input-set-key (kbd "s-k") #'my/kill-buffer)
    (exwm-input-set-key (kbd "s-0") #'plain-org-wiki)
    (exwm-input-set-key (kbd "s-1") #'sane-term)
    (exwm-input-set-key (kbd "s-7") #'switch-to-scratch-and-back)
    (exwm-input-set-key (kbd "s-8") #'winner-undo)
    (exwm-input-set-key (kbd "s-9") #'winner-redo)
    ;; (exwm-input-set-key (kbd "C-'") #'fm-bookmarks)
    (exwm-input-set-key (kbd "s-&") ;; lauch command
                      (lambda (command)
                        (interactive (list (read-shell-command "$ ")))
                        (start-process-shell-command command nil command)))

    ;; (use-package exwm-systemtray
    ;;     :config
    ;;     (setq exwm-systemtray-height 16)
    ;;     (exwm-systemtray-enable)
    ;;     ;; (add-hook 'exwm-init-hook 'exwm-x/volit t)
    ;;     ;; (add-hook 'exwm-init-hook 'exwm-x/power-manager t)
    ;;     ;; (add-hook 'exwm-init-hook 'exwm-x/lock-screen t)
    ;; )

    (require 'exwm-input)
    (push ?\M-c exwm-input-prefix-keys) ; Variable Not available
    (push ?\s-c exwm-input-prefix-keys) ; Variable Not available
    ;; (push ?\M-c exwm-input--global-prefix-keys) Variable Not available
)

(use-package ace-window
 :ensure t
 :defer 1
 :bind (("C-x o" . ace-window))
 :config
   (set-face-attribute 'aw-leading-char-face nil :foreground "white" :weight 'bold :height 3.0)
   (set-face-attribute 'aw-mode-line-face nil :inherit 'mode-line-buffer-id :foreground "red" :weight 'bold)
   (setq aw-keys   '(?a ?s ?d ?f ?j ?k ?l)
         aw-dispatch-always t
         aw-dispatch-alist
         '((?x aw-delete-window     "Ace - Delete Window")
           (?c aw-swap-window       "Ace - Swap Window")
           (?n aw-flip-window)
           (?v aw-split-window-vert "Ace - Split Vert Window")
           (?h aw-split-window-horz "Ace - Split Horz Window")
           (?m delete-other-windows "Ace - Maximize Window")
           (?g delete-other-windows)
           (?b balance-windows)
           (?u winner-undo)
           (?r winner-redo)))

   (when (package-installed-p 'hydra)
     (defhydra hydra-window-size (:color red)
       "Windows size"
       ("h" shrink-window-horizontally "shrink horizontal")
       ("j" shrink-window "shrink vertical")
       ("k" enlarge-window "enlarge vertical")
       ("l" enlarge-window-horizontally "enlarge horizontal"))
     (defhydra hydra-window-scroll (:color red)
       "Scroll other window"
       ("n" joe-scroll-other-window "scroll")
       ("p" joe-scroll-other-window-down "scroll down"))
     (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
     (add-to-list 'aw-dispatch-alist '(?o hydra-window-scroll/body) t))
   (ace-window-display-mode t)
)
; * time
(setq display-time-24hr-format t)
(setq display-time-mail-string "")

; * mode line stat
;; (use-package mode-line-stats
;;   :load-path "extra/mode-line-stats/"
;;   :init
;;     (require 'mls-cpu)
;;     (mls-cpu-start)
;;     (mode-line-stats-mode)
;;   :config
;;     (setq mls-modules '(cpu memory)
;;        mls-cpu-format "%A"
;;        mls-cpu-update-interval 1
;;        mls-memory-format "%t"
;;        mls-memory-update-interval 1
;;        mls-position :right) ; left right global-mode-string
;; )
; * symon
;; (use-package symon
;;   :ensure t
;;   :init
;;     (symon-mode)
;;   :config
;; )

; ibus-mode broken after ibus 1.5
;; (use-package ibus
;;   :load-path "extra/"
;;   :init
;; 	:preface
;;     ;; Turn on ibus-mode automatically after loading .emacs
;;     (add-hook 'after-init-hook 'ibus-mode-on)
;;   :config
;;     ;; Use C-SPC for Set Mark command
;;     ;;(ibus-define-common-key ?\C-\s nil)

;;     ;; Use C-/ for Undo command
;;     ;;(ibus-define-common-key ?\C-/ nil)

;;     ;; Change cursor color depending on IBus status
;;     (setq ibus-cursor-color '("limegreen" "white" "blue"))
;;     (global-set-key "\C-\\" 'ibus-toggle)
;;     ;; 全てのバッファで状態を共有
;;     (setq ibus-mode-local nil)

;;     (add-hook 'after-make-frame-functions
;;         (lambda (new-frame)
;;         (select-frame new-frame)
;;         (or ibus-mode (ibus-mode-on))))
;; )



; * exwm-x

;; (use-package dmenu
;;   :ensure t ; required by exwm-x
;;   :config
;;   (setq dmenu-prompt-string "dmenu: "))
; modeline
;; [-]: h-split
;; [|]: v-split
;; [+]: max-window
;; [_]: hide floating window
;; [X]: kill buffer

; * finaliser
;(setq debug-on-error nil)
;; Local Variables:
;; time-stamp-start: "Updated: +"
;; time-stamp-end: "$"
;; End:

; * w3m
; ** tips
; browse-url open link at point
; * system
(use-package volume
  :ensure t
  :bind
    (:map volume-mode-map
    ("<escape>" . volume-quit))
  :config
    (setq volume-backend 'volume-amixer-backend
          volume-electric-mode nil)
)
; * abo abo xmodemap
;; The config comes with its own .Xmodmap that makes ; into an additional modifier. RSI savers:
;-u CapsLock.
;; And obviously the replacements for the two keys that the mod takes away:
;-c instead of ;. can be used with other modifiers
;-g instead of Shift-;.

; * todo
;; imenu defun add init package heading
;; add all my heading to   (add-to-list 'imenu-generic-expression `("Sections"
;; level is determined by the match, the less the higher
;; make tab-width 4 for all mode
;; (evil-define-key 'normal org-mode-map ";a" 'org-agenda) ; access agenda buffer
;; fix ipa ^L, edit in global file change annotation and bidirection go to
;; evil-emacs-mode doesn't work well with term-mode?
; * tips
; prefix + ? or "C-h" list keybinding with that prefix
; after dabbrev-expand (M-/), <space> then dabbrev-expand again:
;; input another word by expanding from the point that the expansion was found.
;; C-c h, C-x h mark-whole-buffer
;; C-M-\ indent-region
;; M-\ Delete spaces and tabs around point
;; M-<SPC> reduce spaces and tabs to one space
;; C-x C-o reduce blank lines to one
;; M-^ Join two lines
;; C-j    return indent
; M-x man view man page
;;     Within a manpage buffer:
;;     SPC -- Scrolls forward (or type C-M-v or M-Page Down)
;;     DEL -- Scrolls backward (or type M-Page Up)
;;     m -- Prompt to retrieve a new manpage (i.e. a shortcut for "M-x man").
;;     r -- Retrieve reference in SEE ALSO section.
;;     M-n -- Jump to next manpage in circular list.
;;     M-p -- Jump to previous manpage in circular list.
;;     n -- Jump to next manpage section.
;;     p -- Jump to previous manpage section.
;;     g -- Go to a manpage section.
;;     s -- Jumps to the SEE ALSO manpage section.
;;     q -- Deletes the manpage window, bury its buffer.
;;     k -- Deletes the manpage window, kill its buffer.

;; "^$" matches empty line
; ** reload
;; (remove-advice 'function-name #'my-function-name)
; to remove: ad-disable-advice
;to reset advices : ad-deactivate
; * keyboard
;; (global-set-key [S-home] 'beginning-of-buffer)   ;; Go to beginning of buffer
;; [end] [S-end] [select] [S-select] [f1] [S-f1] [menu] [print] [S-print] 
