;; コマンドキーを Ctrl キーにする
(setq mac-command-modifier 'control)
;; (setq mac-option-modifier 'meta)

;; パスを通す
(setq load-path(cons"~/.emacs.d/lisp" load-path))

;; 言語の設定
(set-language-environment 'Japanese)
;; (setenv "LANG" "ja_JP.UTF-8")
;; (set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; パッケージ管理
(package-initialize)
;; (require 'package)
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

;; ;; ウィンドウの色の設定
;; (if window-system (progn
;;     ;; (set-background-color "Black")
;;     ;; (set-foreground-color "LightGray")
;;     ;; (set-cursor-color "Gray")
;;     (set-frame-parameter nil 'alpha 93) ;透明度
;;     ))

;; ;; (load-theme 'underwater t)
;; ;; (load-theme 'monokai t)
;; ;; (load-theme 'doom-dracula t)
;; (load-theme 'doom-dark+ t)

;; ウィンドウの色の設定
(load-theme 'doom-dark+ t)
;; (load-theme 'doom-homage-black t)
(if window-system (progn
    (set-frame-parameter nil 'alpha 93) ;透明度
    ))

;; ¥の代わりにバックスラッシュを入力する
(define-key global-map [?¥] [?\\])

;; フレームの設定
(setq default-frame-alist
      (append (list
               ;; サイズ・位置
               '(width . 170)  ; 横幅(文字数)
               '(height . 59)  ; 高さ(行数)
               '(top . 120)    ; フレーム左上角 y 座標
               '(left . 2500)  ; フレーム左上角 x 座標
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

;; 対応する括弧をハイライトする
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; メニューバーとツールバーを表示しない
(menu-bar-mode 0)
(tool-bar-mode 0)

;; scratch の初期メッセージを表示しない
(setq initial-scratch-message "")

;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;; Tabキーを無効化する
(setq-default indent-tabs-mode nil)

;; 自動再読み込み
(global-auto-revert-mode t)

;; 画像ファイルを表示する
(auto-image-file-mode t)

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
(cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
`((".*" ,(expand-file-name "~/.emacs.d/auto-save-list/") t)))

;; オートセーブファイル作成までの秒間隔
(setq auto-save-timeout 15)
;; オートセーブファイル作成までのタイプ間隔
(setq auto-save-interval 60)
;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; ニョロファイルを作らない
(setq make-backup-files nil)

;; yes-or-no を y-or-n で応えるようにする．
(fset 'yes-or-no-p 'y-or-n-p)

;; 行の先頭で C-k を一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; 最後に改行を入れる。
(setq require-final-newline t)

;; タイトルバーにファイル名をフルパスで表示する
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; 時刻表示
(display-time)

;; 以前編集したファイルのカーソル位置を覚える設定
(require 'saveplace)
(save-place-mode 1)

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
;; (set-scroll-bar-mode 'right)

;; default scroll bar消去
(scroll-bar-mode 0)

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

;;; フォントサイズ変更の設定
; C-+ で拡大、C--で縮小、\C-0 でデフォルトに戻す
(global-set-key [(control +)] (lambda () (interactive) (text-scale-increase 1)))
(global-set-key [(control -)] (lambda () (interactive) (text-scale-decrease 1)))
(global-set-key [(control ?0)] (lambda () (interactive) (text-scale-increase 0)))

;; Cycle-buffer
(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
;; (autoload 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
;; (autoload 'cycle-buffer-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
;; (autoload 'cycle-buffer-toggle-interesting "cycle-buffer" "Toggle if this buffer will be considered." t)
(global-set-key "\M-p" 'cycle-buffer-backward)
(global-set-key "\M-k" 'cycle-buffer)

;; 括弧自動補完
(require 'smartparens)
(smartparens-global-mode t)
;; (setq-default sp-highlight-pair-overlay nil)  ;ハイライト機能削除

;; オートコンプリートの有効化
(require 'auto-complete-config)
(global-auto-complete-mode 0.5)

;; (require 'company)
;; (global-company-mode) ; 全バッファで有効にする
;; (setq company-idle-delay 0) ; デフォルト0.5
;; (setq company-minimum-prefix-length 2) ; デフォルト4
;; (setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
;; ;; (define-key company-active-map (kbd "M-n") nil)
;; ;; (define-key company-active-map (kbd "M-p") nil)
;; ;; (define-key company-active-map (kbd "C-n") 'company-select-next)
;; ;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
;; ;; (define-key company-active-map (kbd "C-h") nil)
;; (setq company-require-match 'never)


;; migemo
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(migemo-init)


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

;; (require 'xterm-color)

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


;; ;;; elpy (melpa-stable)
;; (use-package elpy
;;   :ensure t
;;   :pin melpa-stable
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   :config
;;   (setq elpy-rpc-python-command "python3")
;;   (setq python-shell-interpreter "python3")
;; ;;  (setq elpy-rpc-virtualenv-path 'current)
;;   (when (load "flycheck" t t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(doom-themes all-the-icons doom-modeline migemo hiwin go-eldoc go-mode go-autocomplete uuidgen underwater-theme ubuntu-theme twilight-theme smartparens smart-tabs-mode python-mode neotree multi-term monokai-theme markdown-preview-mode markdown-mode+ magit golden-ratio flycheck elscreen)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(require 'doom-modeline)
(doom-modeline-mode 1)
(require 'all-the-icons)

;; (setq doom-modeline-support-imenu t)
;; (setq doom-modeline-height 25)
;; (setq doom-modeline-bar-width 4)
;; (setq doom-modeline-hud nil)
;; (setq doom-modeline-icon t)
;; (setq doom-modeline-window-width-limit 0.25)
;; (setq doom-modeline-project-detection 'auto)
;; (setq doom-modeline-buffer-file-name-style 'auto)
;; (setq doom-modeline-major-mode-icon t)
;; (setq doom-modeline-major-mode-color-icon t)
;; (setq doom-modeline-buffer-state-icon t)
;; (setq doom-modeline-buffer-modification-icon t)
;; (setq doom-modeline-unicode-fallback nil)
;; (setq doom-modeline-buffer-name t)
;; (setq doom-modeline-minor-modes nil)
;; (setq doom-modeline-enable-word-count nil)
;; (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))
;; (setq doom-modeline-buffer-encoding t)
;; (setq doom-modeline-indent-info nil)
;; (setq doom-modeline-checker-simple-format t)
;; (setq doom-modeline-number-limit 99)
;; (setq doom-modeline-vcs-max-length 12)
;; (setq doom-modeline-workspace-name t)
;; (setq doom-modeline-persp-name t)
;; (setq doom-modeline-display-default-persp-name nil)
;; (setq doom-modeline-persp-icon t)
;; (setq doom-modeline-lsp t)
;; (setq doom-modeline-github nil)
;; (setq doom-modeline-github-interval (* 30 60))
;; (setq doom-modeline-modal-icon t)
;; (setq doom-modeline-mu4e nil)
;; (setq doom-modeline-gnus t)
;; (setq doom-modeline-gnus-timer 2)
;; (setq doom-modeline-gnus-excluded-groups '("dummy.group"))
;; (setq doom-modeline-irc t)
;; (setq doom-modeline-irc-stylize 'identity)
;; (setq doom-modeline-env-version t)
;; (setq doom-modeline-env-enable-python t)
;; (setq doom-modeline-env-enable-ruby t)
;; (setq doom-modeline-env-enable-perl t)
;; (setq doom-modeline-env-enable-go t)
;; (setq doom-modeline-env-enable-elixir t)
;; (setq doom-modeline-env-enable-rust t)
;; (setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
;; (setq doom-modeline-env-ruby-executable "ruby")
;; (setq doom-modeline-env-perl-executable "perl")
;; (setq doom-modeline-env-go-executable "go")
;; (setq doom-modeline-env-elixir-executable "iex")
;; (setq doom-modeline-env-rust-executable "rustc")
;; (setq doom-modeline-env-load-string "...")
;; (setq doom-modeline-before-update-env-hook nil)
;; (setq doom-modeline-after-update-env-hook nil)

;; (require 'use-package)
;; (use-package doom-modeline
;;       :custom
;;       (doom-modeline-buffer-file-name-style 'truncate-with-project)
;;       (doom-modeline-icon t)
;;       (doom-modeline-major-mode-icon nil)
;;       (doom-modeline-minor-modes nil)
;;       :hook
;;       (after-init . doom-modeline-mode)
;;       :config
;;       (line-number-mode 0)
;;       (column-number-mode 0)
;;       (doom-modeline-def-modeline 'main
;;     '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker)))

