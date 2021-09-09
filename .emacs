;; コマンドキーを Ctrl キーにする
(setq mac-command-modifier 'control)

;; パスを通す
(setq load-path(cons"~/.emacs.d/lisp" load-path))

;; 言語の設定
(set-language-environment 'Japanese)
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment  'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; パッケージ管理
(package-initialize)
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; キーバインディングの設定
(global-set-key "\C-q" 'Control-X-prefix)
(global-set-key "\C-j" 'mode-specific-command-prefix)
(global-set-key "\C-q\C-o" 'save-buffer)
(global-set-key "\C-q\C-j" 'save-buffers-kill-emacs)
(global-set-key "\C-q\C-a" 'write-file)
(global-set-key "\C-l" 'set-mark-command)
(global-set-key "\C-t" 'kill-line)
(global-set-key "\C-z" 'kill-region)
(global-set-key "\C-u" 'forward-char)
(global-set-key "\C-x" 'backward-char)
(global-set-key "\C-qx" 'switch-to-buffer)
(global-set-key "\C-q\C-u" 'find-file)
(global-set-key "\C-r" 'isearch-backward)
(global-set-key "\C-p" 'toggle-frame-maximized)
(global-set-key "\M-u" 'forward-word)
(global-set-key "\M-x" 'backward-word)
(global-set-key "\M-q" 'execute-extended-command)
(global-set-key "\C-j\C-l" 'comment-region)
(global-set-key "\C-j\C-r" 'uncomment-region)
(global-set-key (kbd "C-q g") 'magit-status)

;; ウィンドウの色の設定
(if window-system (progn
    (set-background-color "Black")
    (set-foreground-color "LightGray")
    (set-cursor-color "Gray")
    (set-frame-parameter nil 'alpha 85) ;透明度
    ))

;; (load-theme 'underwater t)
;; (load-theme 'monokai t)

;; ¥の代わりにバックスラッシュを入力する
(define-key global-map [?¥] [?\\])

;; 対応する括弧をハイライトする
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; フレームの設定
(setq default-frame-alist
      (append (list
               ;; サイズ・位置
               '(width . 170)  ; 横幅(文字数)
               '(height . 59) ; 高さ(行数)
               '(top . 90)    ; フレーム左上角 y 座標
               '(left . 2500)   ; フレーム左上角 x 座標
               )
              default-frame-alist))

;; スクロールは1行ごとに
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;; スクロールの加速をやめる
(setq mouse-wheel-progressive-speed nil)

;; 行番号の表示
(progn
  (global-display-line-numbers-mode)
  (set-face-attribute 'line-number nil
                      :foreground "#808080"
                      :background "#0f0f0f")
  (set-face-attribute 'line-number-current-line nil
                      :foreground "gold"))

;; ターミナルで起動したときにメニューを表示しない
(if (eq window-system 'x)
    (menu-bar-mode 1) (menu-bar-mode 0))
(menu-bar-mode nil)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; Tabキーを無効化する
(setq-default indent-tabs-mode nil)

;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;; ツールバーを消す
(tool-bar-mode 0)

;; ニョロファイルを作らない
(setq make-backup-files nil)

;; 画像ファイルを表示する
(auto-image-file-mode t)

;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; オートセーブしない＆バックアップファイルを作らない
(setq auto-save-default nil)
(setq make-backup-files nil)

;; yes-or-no を y-or-n で応えるようにする．
(fset 'yes-or-no-p 'y-or-n-p)

;; タイトルバーにファイル名をフルパスで表示する
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; 時刻表示
(display-time)

;; C-hv とか ファイル名補完時のウィンドウを自動的にリサイズする。
(temp-buffer-resize-mode t)

;; bufferの最後でカーソルを動かそうとしても音をならなくする
(defun next-line (arg)
  (interactive "p")
  (condition-case nil
      (line-move arg)
    (end-of-buffer)))

;; エラー音をならなくする
;;(setq ring-bell-function 'ignore)

(defun compile-newer-file (file)
  ".elcがない、あるいは.elより新しい時にcompileする"
  (let* ((el (expand-file-name file))
	 (elc (concat el (if (string-match "\\.el$" el) "c" ".elc"))))
    (when (and (file-exists-p el)
	       (or (not (file-exists-p elc))
		   (file-newer-than-file-p el elc)))
      (save-window-excursion
	(message "Byte-compile %s" el)
	(sit-for 1)
	(byte-compile-file el)))))

;; スクロールバーは右側とする
(set-scroll-bar-mode 'right)

;; フォント設定
(set-face-attribute 'default nil
                    :family "Ricty Diminished Discord"
                    :height 200)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (cons "Ricty Diminished Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0212
                  (cons "Ricty Diminished Discord" "iso10646-1"))
(set-fontset-font (frame-parameter nil 'font)
                  'katakana-jisx0201
                  (cons "Ricty Diminished Discord" "iso10646-1"))

;; Cycle-buffer
(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
(autoload 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
(autoload 'cycle-buffer-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
(autoload 'cycle-buffer-toggle-interesting "cycle-buffer" "Toggle if this buffer will be considered." t)
(global-set-key "\M-p" 'cycle-buffer-backward)
(global-set-key "\M-k" 'cycle-buffer)

;; 括弧自動補完
(require 'smartparens)
(smartparens-global-mode t)
;; (setq-default sp-highlight-pair-overlay nil)  ;ハイライト機能削除

;; オートコンプリートの有効化
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルト0.5
(setq company-minimum-prefix-length 2) ; デフォルト4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
;; (define-key company-active-map (kbd "M-n") nil)
;; (define-key company-active-map (kbd "M-p") nil)
;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; (define-key company-active-map (kbd "C-h") nil)

;; neotree（サイドバー）
(require 'neotree)
(global-set-key "\C-o" 'neotree-toggle)

;; elscreen（上部タブ）
(require 'elscreen)
(setq elscreen-prefix-key (kbd "C-v"))
(elscreen-start)
(global-set-key "\C-qt" 'elscreen-create)
(global-set-key "\C-w" 'elscreen-kill)
;; (global-set-key (kbd "s-t") 'elscreen-create)
(global-set-key (kbd "C-<down>") 'elscreen-next)
(global-set-key (kbd "C-<up>") 'elscreen-previous)
;; (global-set-key (kbd "s-d") 'elscreen-kill)
(set-face-attribute 'elscreen-tab-background-face nil
                    :background "grey10"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-control-face nil
                    :background "grey20"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-current-screen-face nil
                    :background "grey20"
                    :foreground "grey90")
(set-face-attribute 'elscreen-tab-other-screen-face nil
                    :background "grey30"
                    :foreground "grey60")
;;; [X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;;; [<->]を表示しない
(setq elscreen-tab-display-control nil)

;; golden ratio
(golden-ratio-mode 1)
(add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")

;; YAML-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(autoload 'markdown-preview-mode "markdown-preview-mode.el" t)
(setq markdown-preview-stylesheets (list "github.css"))

;; PDF出力
(setq my-pdfout-command-format "nkf -e | e2ps -a4 -p -nh | ps2pdf - %s")
(defun my-pdfout-region (begin end)
    (interactive "r")
    (shell-command-on-region begin end (format my-pdfout-command-format (read-from-minibuffer "File name:"))))
(defun my-pdfout-buffer ()
    (interactive)
    (my-pdfout-region (point-min) (point-max)))

;; Vue-mode
(setq vue-mode-packages
  '(vue-mode))

(setq vue-mode-excluded-packages '())

(defun vue-mode/init-vue-mode ()
  "Initialize my package"
  (use-package vue-mode))

(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face "#000000")))






;;;; 以下は整理していない

;; ;; (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
;; (setq vue-mode-packages
;;   '(vue-mode))

;; (setq vue-mode-excluded-packages '())

;; (defun vue-mode/init-vue-mode ()
;;   "Initialize my package"
;;   (use-package vue-mode))

;; ;; vue-mode時、node_modulesのパスを登録
;; (add-hook 'vue-mode-hook 'add-node-modules-path)

;; eslintによるflycheck
;; (require 'flycheck)
;; (flycheck-add-mode 'javascript-eslint 'vue-mode)
;; (flycheck-add-mode 'javascript-eslint 'vue-html-mode)
;; (flycheck-add-mode 'javascript-eslint 'css-mode)

;; ;; tern-mode有効
;; (add-hook 'vue-mode-hook 'tern-mode)

;; (defun js-company-tern-hook ()
;;   (when (locate-library "tern")
;;     ;; .tern-port を作らない
;;     (setq tern-command '("tern" "--no-port-file"))
;;     (tern-mode t)))

;; vue-modeでflycheck有効にしたら、
;; 保存時にfixする
;; (add-hook 'vue-mode-hook
;;           (lambda ()
;;             (add-hook 'flycheck-mode-hook
;;                       (lambda()
;;                         (add-hook 'after-save-hook #'eslint-fix)))))

;; (provide 'vue-config)


;; ;; 拡張子jsのファイルを開いたときjs-modeに
;; (add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; ;; node_modulesのパスを通す
;; (add-hook 'js-mode-hook 'add-node-modules-path)

;; (provide 'js-config)
;; (custom-set-variables
;;  '(package-selected-packages
;;    '(magit elpy smart-tabs-mode multi-term underwater-theme twilight-theme ubuntu-theme monokai-theme company vue-mode uuidgen smartparens mmm-jinja2 markdown-preview-mode markdown-mode+ flycheck)))
;; (custom-set-faces
;;  )

;; (require 'multi-term)
;; (setq multi-term-program shell-file-name)
;; (add-to-list 'term-unbind-key-list '"C-q")
;; (add-to-list 'term-unbind-key-list '"C-x")


;; ; shell の存在を確認
;; (defun skt:shell ()
;;   (or (executable-find "zsh")
;;       (executable-find "bash")
;;       (executable-find "cmdproxy")
;;       (error "can't find 'shell' command in PATH!!")))

;; ;; Shell 名の設定
;; (setq shell-file-name (skt:shell))
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)

(require 'xterm-color)

;; (when (and (require 'python nil t) (require 'elpy nil t))
;;    (elpy-enable))
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(vue-mode uuidgen underwater-theme ubuntu-theme twilight-theme smartparens smart-tabs-mode python-mode multi-term monokai-theme mmm-jinja2 markdown-preview-mode markdown-mode+ magit flycheck elpy)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages
;;    '(vterm vue-mode uuidgen underwater-theme ubuntu-theme twilight-theme tern smartparens smart-tabs-mode python-mode neotree multi-term monokai-theme mmm-jinja2 markdown-preview-mode markdown-mode+ magit golden-ratio flycheck elscreen elpy)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
