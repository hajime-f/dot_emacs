(setq mac-command-modifier 'control)
(setq load-path(cons"~/.emacs.d/lisp" load-path))

(set-language-environment 'Japanese)
(setenv "LANG" "ja_JP.UTF-8")
(set-language-environment  'utf-8)
(prefer-coding-system 'utf-8)

(package-initialize)
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(global-set-key "\C-q" 'Control-X-prefix)
(global-set-key "\C-j" 'mode-specific-command-prefix)

(global-set-key "\C-q\C-o" 'save-buffer)
(global-set-key "\C-q\C-j" 'save-buffers-kill-emacs)
(global-set-key "\C-q\C-a" 'write-file)
(global-set-key "\C-l" 'set-mark-command)
(global-set-key "\C-t" 'kill-line)
(global-set-key "\C-k" 'multi-term)
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

(global-set-key (kbd "C-q t") '(lambda ()
                                (interactive)
                                (if (get-buffer "*terminal<1>*")
                                    (switch-to-buffer "*terminal<1>*")
                                  (multi-term))))
(global-set-key (kbd "C-q n") 'multi-term-next)
(global-set-key (kbd "C-q p") 'multi-term-prev)

(setq-default indent-tabs-mode nil)

;; Color
(if window-system (progn
    (set-background-color "Black")
    (set-foreground-color "LightGray")
    (set-cursor-color "Gray")
    (set-frame-parameter nil 'alpha 85) ;透明度
    ))

;; ¥の代わりにバックスラッシュを入力する
(define-key global-map [?¥] [?\\])

;; 対応する括弧をハイライトする
(show-paren-mode t)
(setq show-paren-style 'mixed)

;; 括弧自動補完
(require 'smartparens)
(smartparens-global-mode t)
(setq-default sp-highlight-pair-overlay nil)  ;ハイライト機能削除

;; (load-theme 'underwater t)

;; ;; オートコンプリートの有効化
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

;; フレームの設定
(setq default-frame-alist
      (append (list
               ;; サイズ・位置
               '(width . 120)  ; 横幅(文字数)
               '(height . 59) ; 高さ(行数)
               '(top . 80)    ; フレーム左上角 y 座標
               '(left . 3000)   ; フレーム左上角 x 座標
               )
              default-frame-alist))

;; 起動時のメッセージを表示しない
(setq inhibit-startup-message t)

;; デフォルトの文字コードは utf-8
(set-default-coding-systems 'utf-8)

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

;; タイトルバーにファイル名を表示する
(setq frame-title-format (format "emacs@%s : %%f" (system-name)))

;; 時刻表示
(display-time)

;; C-hv とか ファイル名補完時のウィンドウを自動的にリサイズする。
(temp-buffer-resize-mode t)

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
(global-set-key "\M-p"        'cycle-buffer-backward)
(global-set-key "\M-k"       'cycle-buffer)

;; YAML-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(autoload 'markdown-preview-mode "markdown-preview-mode.el" t)
(setq markdown-preview-stylesheets (list "github.css"))


(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

;; vue-mode時、node_modulesのパスを登録
(add-hook 'vue-mode-hook 'add-node-modules-path)

;; eslintによるflycheck
(require 'flycheck)
(flycheck-add-mode 'javascript-eslint 'vue-mode)
(flycheck-add-mode 'javascript-eslint 'vue-html-mode)
(flycheck-add-mode 'javascript-eslint 'css-mode)

;; tern-mode有効
(add-hook 'vue-mode-hook 'tern-mode)

;; vue-modeでflycheck有効にしたら、
;; 保存時にfixする
(add-hook 'vue-mode-hook
          (lambda ()
            (add-hook 'flycheck-mode-hook
                      (lambda()
                        (add-hook 'after-save-hook #'eslint-fix)))))

(provide 'vue-config)


;; 拡張子jsのファイルを開いたときjs-modeに
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))

;; node_modulesのパスを通す
(add-hook 'js-mode-hook 'add-node-modules-path)

(provide 'js-config)
(custom-set-variables
 '(package-selected-packages
   '(magit elpy smart-tabs-mode multi-term underwater-theme twilight-theme ubuntu-theme monokai-theme company vue-mode uuidgen smartparens mmm-jinja2 markdown-preview-mode markdown-mode+ flycheck)))
(custom-set-faces
 )

(require 'multi-term)
(setq multi-term-program shell-file-name)
(add-to-list 'term-unbind-key-list '"C-q")
(add-to-list 'term-unbind-key-list '"C-x")


; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))

;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)

(require 'xterm-color)

(when (and (require 'python nil t) (require 'elpy nil t))
   (elpy-enable))
