(add-to-list 'load-path "~/.emacs.d/lisp")

(setq org-log-done t)

;; save-history
;; (load "save-history")
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

(require 'recentf)
(recentf-mode 1)

;; default fill and tab width 
(setq-default fill-column 80)
(setq-default tab-width 4)

(add-hook 'c-mode-hook '(lambda ()
  (setq ac-sources (append '(ac-source-semantic) ac-sources))
  (local-set-key (kbd "RET") 'newline-and-indent)
  (semantic-mode t)
  (global-semantic-decoration-mode nil)
  (semantic-add-system-include "/usr/lib/include" 'c-mode)))

(add-hook 'c++-mode-hook '(lambda ()
  (setq ac-sources (append '(ac-source-semantic) ac-sources))
  (local-set-key (kbd "RET") 'newline-and-indent)
  (semantic-mode t)
  (global-semantic-decoration-mode nil)
  (semantic-add-system-include "/usr/lib/include" 'c++-mode)))

;; c-mode
(defun linux-c-mode ()
  "C mode with adjusted defaults"
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8 )
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 8 ))

;; c++-mode
(defun c++-mode-4s ()
  "C++ mode with adjusted defaults"
  (interactive)
  (c++-mode)
  (setq tab-width 4 )
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4 ))

  (setq auto-mode-alist (append '(("\\.cpp$" . c++-mode-4s)) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.cp$" . c++-mode-4s)) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.cc$" . c++-mode-4s))  auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.cxx$" . c++-mode-4s)) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.hpp$" . c++-mode-4s)) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.hxx$" . c++-mode-4s)) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.h$" . c++-mode-4s)) auto-mode-alist))
  (setq auto-mode-alist (append '(("\\.H$" . c++-mode-4s)) auto-mode-alist))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;(require 'sr-speedbar)
;;(global-set-key (kbd "M-s") 'sr-speedbar-toggle)

(column-number-mode 1)
(line-number-mode 1)
(cua-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Linum-format "%7i ")
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(ansi-term-color-vector
   [unspecified "#14191f" "#d15120" "#81af34" "#deae3e" "#7e9fc9" "#a878b5" "#7e9fc9" "#dcdddd"] t)
 '(auto-save-default nil)
 '(background-color "#202020")
 '(background-mode dark)
 '(backup-inhibited t t)
 '(battery-mode-line-format "[%p]")
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   (quote
    ("7feeed063855b06836e0262f77f5c6d3f415159a98a9676d549bfeb6c49637c4" "c1fb68aa00235766461c7e31ecfc759aa2dd905899ae6d95097061faeb72f9ee" default)))
 '(fci-rule-character-color "#192028")
 '(fci-rule-color "#073642")
 '(fill-column 78)
 '(foreground-color "#cccccc")
 '(frame-brackground-mode (quote dark))
 '(fringe-mode nil nil (fringe))
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (quote
    ("#EFFF00" "#73CD4F" "#83DDFF" "MediumPurple1" "#66CDAA" "DarkOrange" "HotPink1" "#809FFF" "#ADFF2F")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-background-colors
   (quote
    ("#00FF99" "#CCFF99" "#FFCC99" "#FF9999" "#FF99CC" "#CC99FF" "#9999FF" "#99CCFF" "#99FFCC" "#7FFF00")))
 '(hl-paren-colors (quote ("#326B6B")))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries (quote left))
 '(indicate-empty-lines t)
 '(less-css-compile-at-save t)
 '(less-css-lessc-options (quote ("--no-color -x --yui-compress")))
 '(line-number-mode nil)
 '(linum-format " %4d ")
 '(magit-diff-use-overlays nil)
 '(magit-use-overlays nil)
 '(main-line-color1 "#1E1E1E")
 '(main-line-color2 "#111111")
 '(main-line-separator-style (quote chamfer))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("marmalade" . "http://marmalade-repo.org/packages/")
     ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(powerline-buffer-size-suffix t)
 '(powerline-color1 "#1E1E1E")
 '(powerline-color2 "#111111")
 '(powerline-default-separator (quote arrow))
 '(powerline-default-separator-dir (quote (left . right)))
 '(powerline-height 16)
 '(puppet-indent-level tab-width)
 '(recentf-max-saved-items 75)
 '(require-final-newline t)
 '(ruby-indent-level tab-width)
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby"))))
 '(scroll-bar-mode nil)
 '(scss-sass-options (quote ("--style" "compressed")))
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#2aa198" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#dc322f" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#859900"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#268bd2" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#b58900"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#d33682"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#cb4b16" :weight bold))))
 '(tab-width 4)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(tool-bar-position (quote top))
 '(tooltip-mode nil)
 '(vc-annotate-background "#222222")
 '(vc-annotate-color-map
   (quote
    ((20 . "#fa5151")
     (40 . "#e43838")
     (60 . "#f8ffa0")
     (80 . "#e8e815")
     (100 . "#fe8b04")
     (120 . "#e5c900")
     (140 . "#32cd32")
     (160 . "#8ce096")
     (180 . "#7fb07f")
     (200 . "#3cb370")
     (220 . "#099709")
     (240 . "#2fdbde")
     (260 . "#1fb3b3")
     (280 . "#8cf1f1")
     (300 . "#94bff3")
     (320 . "#62b6ea")
     (340 . "#00aff5")
     (360 . "#e81ee7"))))
 '(vc-annotate-very-old-color "#e81ee7")
 '(web-mode-code-indent-offset 4)
 '(web-mode-css-indent-offset 4)
 '(web-mode-indent-style 4)
 '(web-mode-markup-indent-offset 4)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (global-hl-line-mode nil)
(setq-default cursor-type 'box)

;; (require 'rfringe)
;;(set-frame-parameter (selected-frame) 'internal-border-width 2)

(require 'ido)
(ido-mode t)

(defun pwd-repl-home (pwd)
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
   (home-len (length home)))
    (if (and
   (>= (length pwd) home-len)
   (equal home (substring pwd 0 home-len)))
  (concat "~" (substring pwd home-len))
      pwd)))

(defun curr-dir-git-branch-string (pwd)
  "Returns current git branch as a string, or the empty string if
   PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
      (propertize (concat " ["
              (if (> (length git-output) 0)
                  (substring git-output 0 -1)
                "(no branch)")
              "]") 'face `(:foreground "#cc3333"))
      )))

(setq eshell-prompt-function
      (lambda nil
        (concat
         (propertize (pwd-repl-home (eshell/pwd)) 'face `(:foreground "#90fa90"))
         (or (curr-dir-git-branch-string (eshell/pwd)))
         (propertize " $ " 'face `(:foreground "#ff9900")))))
(setq eshell-highlight-prompt nil)

(toggle-truncate-lines 1)
(set-default 'truncate-lines t)



(defun post-init ()
  (require 'frame-fns)
  (defun set-selected-frame-dark ()
    (interactive)
    (let ((frame-name (get-frame-name (selected-frame))))
      (call-process-shell-command (concat "xprop -f _GTK_THEME_VARIANT 8u -set _GTK_THEME_VARIANT \"dark\" -name \""
                                          frame-name
                                          "\""))))

  (require 'powerline)
  (powerline-default-theme)
  ;; (setq sml/theme 'dark)
  ;; (sml/setup)

  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
  (require 'helm-css-scss)

  (show-smartparens-global-mode +1)

  (require 'js2-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

  ;; go-mode
  (require 'go-mode-autoloads)
  (defun go-hook ()
    ;; Call Gofmt before saving                                                    
    (add-hook 'before-save-hook 'gofmt-before-save)
	;; Godef jump key binding                                                      
    (local-set-key (kbd "M-.") 'godef-jump))
  (add-hook 'go-mode-hook 'go-hook)

  (require 'auto-complete)
  (require 'go-autocomplete)
  (require 'auto-complete-config)
 
  (defun resize-frame ()
    (interactive)
    (set-frame-size (selected-frame) 82 46))


  (if (display-graphic-p)
    (set-selected-frame-dark)
    (resize-frame))
)
(add-hook 'after-init-hook 'post-init)

;;
;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes" t)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/mmolokai" t)

(defun theme-init ()
  ;; (load-theme 'mmolokai)
  (if (display-graphic-p)
      (load-theme 'hemisu-dark))
  
  ;; (load-theme 'hemisu-light)
  ;; (load-theme 'hemisu-dark)
  ;; (load-theme 'bliss)

  ;; (set-default-font "Monaco-8.5")
  ;; (set-default-font "Ubuntu Mono-10")
  ;; (set-default-font "Meslo LG S-8.5")
  ;; (set-default-font "Source Code Pro-9")
)
(add-hook 'after-init-hook 'theme-init)
