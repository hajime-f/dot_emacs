(setq mac-command-modifier 'control)

(global-unset-key "\C-q")
(global-set-key "\C-q" 'Control-X-prefix)
(global-unset-key "\C-j")
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

(global-set-key "\M-u" 'forward-word)
(global-set-key "\M-x" 'backward-word)
(global-set-key "\M-q" 'execute-extended-command)

(global-set-key "\C-j\C-l" 'comment-region)
(global-set-key "\C-j\C-r" 'uncomment-region)

;; ¥の代わりにバックスラッシュを入力する
(define-key global-map [?¥] [?\\])

;; フレームの設定
(setq default-frame-alist
      (append (list
               ;; サイズ・位置
               '(width . 120)  ; 横幅(文字数)
               '(height . 60) ; 高さ(行数)
               '(top . 120)    ; フレーム左上角 y 座標
               '(left . 1275)   ; フレーム左上角 x 座標
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
(when (and (>= emacs-major-version 24) (not (null window-system)))
  (let* ((font-family "Menlo")
         (font-size 12)
         (font-height (* font-size 13))
         (jp-font-family "ヒラギノ角ゴ ProN"))
    (set-face-attribute 'default nil :family font-family :height font-height)
    (let ((name (frame-parameter nil 'font))
          (jp-font-spec (font-spec :family jp-font-family))
          (jp-characters '(katakana-jisx0201
                           cp932-2-byte
                           japanese-jisx0212
                           japanese-jisx0213-2
                           japanese-jisx0213.2004-1))
          (font-spec (font-spec :family font-family))
          (characters '((?\u00A0 . ?\u00FF)    ; Latin-1
                        (?\u0100 . ?\u017F)    ; Latin Extended-A
                        (?\u0180 . ?\u024F)    ; Latin Extended-B
                        (?\u0250 . ?\u02AF)    ; IPA Extensions
                        (?\u0370 . ?\u03FF)))) ; Greek and Coptic
      (dolist (jp-character jp-characters)
        (set-fontset-font name jp-character jp-font-spec))
      (dolist (character characters)
        (set-fontset-font name character font-spec))
      (add-to-list 'face-font-rescale-alist (cons jp-font-family 1.2)))))
