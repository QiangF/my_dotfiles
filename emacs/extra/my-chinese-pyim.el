; * functions for processing personal file (user dictionary)

; create it if it doesn't exist
(defvar my-pyim-buffer-cache "~/.emacs.d/pyim-buffer-cache.el")
(unless (file-exists-p my-pyim-buffer-cache)
  ;; 如果 `pyim-personal-file' 对应的文件不存在，
  ;; 创建一个模版文件。
  (my-pyim-create-file my-pyim-buffer-cache))
(defvar my-pyim-buffer-create-cache-p nil)
(defvar my-pyim-buffer-name " *pyim-personal*")
(defvar my-pyim-personal "~/Nutstore/pyim/personal_file")
(defvar my-pyim-property-file "~/Nutstore/pyim/property_file")
(defvar my-pyim-buffer-list nil)
; change all function name pyim to my-pyim

(defun my-pyim-create-file (file &optional contents)
  (condition-case error
      (unless (file-exists-p file)
        (with-temp-buffer
          (erase-buffer)
          (insert contents)
          (make-directory (file-name-directory file) t)
          (write-file (expand-file-name file))
          (message "自动创建 Chinese-pyim 文件: %s" file)))
    (error
     (warn "`Chinese-pyim' 文件创建失败！" ))))


(defun my-pyim-read-file (file name &optional coding dict-type)
  (with-current-buffer (generate-new-buffer name)
    (if coding
        (let ((coding-system-for-read coding))
          (insert-file-contents file))
      (insert-file-contents file))
    `(("buffer" . ,(current-buffer))
      ("file" . ,file)
      ("coding" . ,coding)
      ("dict-type" . ,dict-type))))

(defun my-pyim-load-file ()
  "为 `pyim-personal-file'创建一个buffer(这些buffer用户不可见), 最后返回一个包含所有buffer对象以及词库文件名的alist。"
  (let ((bufname my-pyim-buffer-name)
        buflist buf file coding disable dict-type)
    (save-excursion
      (unless (file-exists-p my-pyim-personal-file)
        ;; 如果 `pyim-personal-file' 对应的文件不存在，
        ;; 创建一个模版文件。
        (my-pyim-create-file my-pyim-personal-file ";; -*- coding: utf-8 -*-\n"))
      (unless (file-exists-p my-pyim-property-file)
        (my-pyim-create-file my-pyim-property-file ";; -*- coding: utf-8 -*-\n"))

      (unless (get-buffer name)
        (setq buf (my-pyim-read-file my-pyim-personal-file bufname nil 'my-pyim-personal-file))
        (setq buflist (append buflist (list buf)))

      (setq buf (my-pyim-read-file my-pyim-property-file bufname nil 'my-pyim-property-file))
      (setq buflist (append buflist (list buf)))))
    buflist))

(defun my-pyim-delete-word-from-personal-buffer (word)
  "将中文词条 `word' 从 my-pyim-personal-file 对应的 buffer 中删除"
  (let* ((pinyins (pyim-hanzi2pinyin word nil "-" t))
         (pinyins-szm (mapcar
                       #'(lambda (pinyin)
                           (mapconcat #'(lambda (x)
                                          (substring x 0 1))
                                      (split-string pinyin "-") "-"))
                       pinyins)))
    (dolist (pinyin pinyins)
      (unless (pyim-string-match-p "[^ a-z-]" pinyin)
        (my-pyim-intern-file
          'my-pyim-personal-file word
          (remove word orig-value))))
    (dolist (pinyin pinyins-szm)
      (unless (pyim-string-match-p "[^ a-z-]" pinyin)
        (my-pyim-intern-file
         'my-pyim-personal-file word
         (remove word orig-value))))))

(defun my-pyim-delete-selection-from-personal-buffer ()
  "将高亮选择的字符从 personel-file 对应的 buffer 中删除。"
  (interactive)
  (if mark-active
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (when (and (< (length string) 6)
                   (> (length string) 0))
          (my-pyim-delete-word-from-personal-buffer string)
          (message "将词条: \"%s\" 从 personal file中删除。" string)))
    (message "请首先高亮选择需要删除的词条。")))

(defmacro my-pyim-intern-file (dict-type word &rest body)
  "用于更新 my-pyim-personal-file 或者 my-pyim-property-file 的宏，
运行 `body' 并用其返回值覆盖 buffer 变量 `my-pyim-buffer-cache'
中 `word' 原来的取值，表达式 `bode' 中，可以使用 `orig-value'
引用 `word' 原来的取值。"
  (declare (indent 0) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (pyim-get-buffer ,dict-type)))
       (with-current-buffer ,buffer
         (unwind-protect
             (let ((orig-value (gethash ,word my-pyim-buffer-cache)))
               (puthash ,word (or (progn ,@body) orig-value) my-pyim-buffer-cache)))))))


(defun my-pyim-create-or-rearrange-word (word &optional rearrange-word)
  "将中文词条 `word' 添加拼音后，保存到 my-pyim-personal-file 对应的
buffer中，当前词条追加到已有词条之后。`my-pyim-create-or-rearrange-word'
会调用 `pyim-hanzi2pinyin' 来获取中文词条的拼音code。
BUG：无法有效的处理多音字。"
  (when (> (length word) 0)
    (let* ((pinyins (pyim-hanzi2pinyin word nil "-" t nil t))) ;使用了多音字校正
      ;; 记录 word 的其他属性（比如：精确词频），用于词条联想和排序。
      (when (and (> (length word) 1)
                 (cl-some #'(lambda (py)
                              (member word (pyim-get py '(my-pyim-personal-file))))
                          pinyins))
        (my-pyim-intern-file
         'my-pyim-property-file word
         (let* ((plist (cdr orig-value))
                (count (pyim-string-to-number
                        (or (plist-get plist :count) "1"))))
           `(,word ,@(plist-put plist :count (+ count 1))))))

      (dolist (py pinyins)
        (unless (pyim-string-match-p "[^ a-z-]" py)
          ;; 添加词库： ”拼音“ - ”中文词条“
          (my-pyim-intern-file
            'my-pyim-personal-file py
            (delete-dups
             (if rearrange-word
                 `(,(car orig-value) ,word ,@(cdr orig-value))
               `(,@orig-value ,word))))
          ;; 添加词库： ”拼音首字母“ - ”中文词条“
          (my-pyim-intern-file
            'my-pyim-personal-file
            (mapconcat #'(lambda (x)
                           (substring x 0 1))
                       (split-string py "-") "-")
            (delete-dups
             (if rearrange-word
                 `(,(car orig-value) ,word ,@(cdr orig-value))
               `(,@orig-value ,word)))))))))

(defun my-pyim-create-word-from-selection ()
  "Add the selected text as a Chinese word into the personal dictionary."
  (interactive)
  (when (region-active-p)
    (let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
      (if (> (length string) 6)
          (error "词条太长")
        (if (not (string-match-p "^\\cc+\\'" string))
            (error "不是纯中文字符串")
          (my-pyim-create-or-rearrange-word string)
          (message "将词条: \"%s\" 插入 personal file。" string))))))


(defun my-pyim-filter-buffer-list (dict-types)
  "从 `pyim-buffer-list' 中挑选符合的 buffer."
  (delq nil
        (mapcar
         #'(lambda (buf)
             (when (member (cdr (assoc "dict-type" buf)) dict-types)
               buf))
         pyim-buffer-list)))

(defun my-pyim-save-files ()
  "将下面几个文件更新后内容保存。
1. `pyim-personal-file'
2. `pyim-my-pyim-property-file'
这个函数默认作为`kill-emacs-hook'使用。"
  (interactive)
  (let ((buffers-list (my-pyim-filter-buffer-list '(my-pyim-personal-file my-pyim-property-file))))
    (dolist (buffer-list buffers-list)
      (let* ((buffer (cdr (assoc "buffer" buffer-list)))
             (file (cdr (assoc "file" buffer-list)))
             (dict-type (cdr (assoc "dict-type" buffer-list))))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (save-restriction
              (if (file-exists-p file)
                  (progn (erase-buffer)
                         (goto-char (point-min))
                         (insert ";; -*- coding: utf-8 -*-\n")
                         (maphash
                          #'(lambda (key value)
                              (insert (mapconcat
                                       #'(lambda (x)
                                           (format "%s" x)) value " "))
                              (insert "\n"))
                          my-pyim-buffer-cache)
                         (write-region (point-min) (point-max) file)
                         (message "更新 Chinese-pyim 文件：%s。" file))
                (message "Chinese-pyim 文件：%s 不存在。" file)))))))))


(defun my-pyim-buffer-create-cache (&optional force)
  (interactive)
  (when (or force (not my-pyim-buffer-create-cache-p))
    (setq my-pyim-buffer-create-cache-p t)
    (dolist (list my-pyim-buffer-list)
      (let* ((buffer (cdr (assoc "buffer" list)))
             (file (cdr (assoc "file" list)))
             (coding (cdr (assoc "coding" list)))
             (dict-type (cdr (assoc "dict-type" list)))
             (plist-cdr (if (eq dict-type 'my-pyim-property-file) t nil)))
        (async-start
         `(lambda ()
            (when (or (not (file-exists-p ,my-pyim-buffer-cache))
                      (file-newer-than-file-p ,file ,my-pyim-buffer-cache)
                      (memq (quote ,dict-type) '(my-pyim-personal-file my-pyim-property-file)))
              ,(async-inject-variables "^load-path$")
              (require 'chinese-pyim-core)
              (if (quote ,coding)
                  (let ((coding-system-for-read (quote ,coding)))
                    (insert-file-contents ,file))
                (insert-file-contents ,file))
              (let ((hastable (make-hash-table :size 1000000 :test #'equal)))
                (goto-char (point-min))
                (forward-line 1)
                (while (not (eobp))
                  (let* ((content (pyim-line-content nil nil ,plist-cdr))
                         (key (car content)))
                    (puthash key content hastable))
                  (forward-line 1))
                (with-temp-buffer
                  (insert ";; Auto generated by `my-pyim-buffer-create-cache', don't edit it by hand!\n\n")
                  (insert "(setq my-pyim-buffer-cache\n")
                  (insert (prin1-to-string hastable))
                  (insert ")")
                  (make-directory (file-name-directory ,my-pyim-buffer-cache) t)
                  (write-file ,my-pyim-buffer-cache)))))
         `(lambda (result)
            (with-current-buffer ,buffer
              (when (file-exists-p ,my-pyim-buffer-cache)
                ;; my-pyim-buffer-cache is a elisp file, which include
                ;; a expression: (setq my-pyim-buffer-cache XXX)
                (load ,my-pyim-buffer-cache t t)
                (message "加载 pyim 词库缓存文件：%S！" ,my-pyim-buffer-cache)))))))))

(defun my-pyim-delete-line ()
  (delete-region (line-beginning-position) (min (+ (line-end-position) 1)
                                                (point-max))))

(defun my-pyim-sort-and-remove-duplicates (words-list)
  "使用分词后的文章来制作拼音词库时，首先按照词条在文章中
出现的频率对词条排序，然后再删除重复词条。"
  (let ((list (cl-remove-duplicates words-list :test #'equal))
        (count-table (make-hash-table :test #'equal)))
    (dolist (x words-list)
      (let ((value (gethash x count-table)))
        (if value
            (puthash x (1+ value) count-table)
          (puthash x 1 count-table))))
    (sort list (lambda (a b) (> (gethash a count-table)
                                (gethash b count-table))))))

(defun my-pyim-remove-duplicates-word (&optional sort-by-freq)
  "制作拼音词库时，删除当前行重复出现的词条，
当 `sort-by-freq' 为 t 时，首先按照当前行词条出现频率对词条排序，
然后再删除重复词条，用于：从中文文章构建词库。"
  (interactive)
  (let* (words-list length)
    (setq words-list (pyim-line-content " "))
    (setq length (length words-list))
    (setq words-list
          (if sort-by-freq
              (cons (car words-list) ;; 拼音必须排在第一位
                    (my-pyim-sort-and-remove-duplicates (cdr words-list)))
            (cl-remove-duplicates words-list :test #'equal)))
    (when (> length (length words-list))
      (my-pyim-delete-line)
      (insert (mapconcat 'identity words-list " "))
      (insert "\n")
      (goto-char (line-beginning-position)))))

(defun my-pyim-update-dict-file (&optional force sort-by-freq)
  "手动调整 Chinese-pyim 词库文件后，执行此命令可以：
1. 按照每行拼音对文件进行排序。
2. 删除重复的词条。
当我们明确无误的知道此命令的使用条件已经符合时。可以将 `force' 设置
为 t ，此时，就不需要用户进一步确认是否执行此命令。
当 `sort-by-freq' 设置位 t 时，删除每一行的重复词条之前，首先将词条按照
词条出现的频率大小排序，这个选项适用于：从文章构建词库，文章中词条出现
频率可以代表此词条的使用频率。"
  (interactive)
  (when (or force
            (yes-or-no-p "注意：当前 buffer *必须* 为词库文件 buffer，是否继续？"))
    (save-restriction
      (let ((lastw "")
            first-char total-char currw)
        (goto-char (point-min))
        (perform-replace "[ \t]+$" "" nil t nil nil nil (point-min) (point-max))
        (my-pyim-sort-dict-region (point-min)
                               (point-max))

        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-content (pyim-line-content))
                 (length (length line-content)))
            (if (or (> length 1) ;; 删除只包含 code，但没有词条的行
                    (pyim-string-match-p " *^;+" (car line-content)))
                (forward-line 1)
              (my-pyim-delete-line))))

        (goto-char (point-min))
        (while (not (eobp))
          (if (looking-at "^[ \t]*$")     ; 如果有空行，删除
              (my-pyim-delete-line)
            (setq currw (pyim-code-at-point))
            (if (equal currw lastw)
                (delete-region (1- (point)) (+ (point) (length currw))))
            (setq lastw currw)
            (forward-line 1)))

        (goto-char (point-min))
        (while (not (eobp))
          (my-pyim-remove-duplicates-word sort-by-freq)
          (forward-line 1))
        (if (looking-at "^$")
            (delete-char -1))))))

(defun my-pyim-sort-dict-region (start end)
  "将词库 buffer 中 `start' 和 `end' 范围内的词条信息按照拼音code排序
当 unix 工具 sort 存在时，优先使用这个工具，否则使用emacs自带函数
`sort-regexp-fields'。"
  (if (and (eq system-type 'gnu/linux)
           (executable-find "sort")
           (executable-find "env"))
      (call-process-region start end
                           "env" t t nil "LC_ALL=C"
                           "sort" "-k1,1" "-s")
    (sort-regexp-fields nil "^.*$" "[a-z-]+[ ]+" start end)))

; * setup personal file
; how to avoid set my-pyim-buffer-list twice if init.el is reloaded
(unless (not my-pyim-buffer-list)
  (setq my-pyim-buffer-list (my-pyim-load-file))) ; run once at init.el loading
; add save files hook to emacs kill
(unless (member 'my-pyim-save-files kill-emacs-hook)
  (add-to-list 'kill-emacs-hook 'my-pyim-save-files))
(my-pyim-buffer-create-cache t)
