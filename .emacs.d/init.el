(if (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (progn
    (global-linum-mode t)
    (setq linum-format "%4d \u2502 ")))

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file"))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives(cons "melpa-stable" (concat proto "://melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/pachages/")))))
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default default default italic underline success warning error])
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "f6f3e8"])
 '(background-color "#202020")
 '(background-mode dark)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cursor-color "#cccccc")
 '(custom-emabled-themes (quote (wombat)))
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(face-font-family-alternatives (quote (("Monospace" "courier" "fixed" "IBM Plex Mono") ("courier" "CMU Typewriter Text" "fixed") ("Sans Serif" "helv" "helvetica" "arial" "fixed") ("helv" "helvetica" "arial" "fixed"))))
 '(fci-rule-color "#383838")
 '(foreground-color "#cccccc")
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors (quote (("#323342" . 0) ("#63de5d" . 20) ("#4BBEAE" . 30) ("#1DB4D0" . 50) ("#9A8F21" . 60) ("#A75B00" . 70) ("#F309DF" . 85) ("#323342" . 100))))
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(hl-sexp-background-color "#121212")
 '(hl-todo-keyword-faces (quote (("TODO" . "#dc752f") ("NEXT" . "#dc752f") ("THEM" . "#2aa198") ("PROG" . "#268bd2") ("OKAY" . "#268bd2") ("DONT" . "#d70000") ("FAIL" . "#d70000") ("DONE" . "#86dc2f") ("NOTE" . "#875f00") ("KLUDGE" . "#875f00") ("HACK" . "#875f00") ("TEMP" . "#875f00") ("FIXME" . "#dc752f") ("XXX+" . "#dc752f") ("\\?\\?\\?+" . "#dc752f"))))
 '(inhibit-startup-screen t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors (quote ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages (quote (god-mode fill-column-indicator helm org)))
 '(pdf-view-midnight-colors (quote ("#282828" . "#f2e5bc")))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
 '(scroll-bar-mode nil)
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(tetris-x-colors [[229 192 123] [97 175 239] [209 154 102] [224 108 117] [152 195 121] [198 120 221] [86 182 194]])
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#bf616a") (40 . "#DCA432") (60 . "#ebcb8b") (80 . "#B4EB89") (100 . "#89EBCA") (120 . "#89AAEB") (140 . "#C189EB") (160 . "#bf616a") (180 . "#DCA432") (200 . "#ebcb8b") (220 . "#B4EB89") (240 . "#89EBCA") (260 . "#89AAEB") (280 . "#C189EB") (300 . "#bf616a") (320 . "#DCA432") (340 . "#ebcb8b") (360 . "#B4EB89"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list (unspecified "#242728" "#323342" "#F70057" "#ff0066" "#86C30D" "#63de5d" "#BEB244" "#E6DB74" "#40CAE4" "#06d8ff" "#FF61FF" "#ff8eff" "#00b2ac" "#53f2dc" "#f8fbfc" "#ffffff"))
 '(xterm-mouse-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'rainbow-delimiters)
(add-hook 'prog-mode #'rainbow-delimiters-mode)
(rainbow-delimiters-mode t)
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-08"))
(set-face-attribute 'default t :font "IBM Plex Mono-08")

;; GOD MODE
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
			'box
		      'bar)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; GARBAGE
(menu-bar-mode -1)
