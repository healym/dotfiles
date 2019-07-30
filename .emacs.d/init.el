(if (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode)
  (progn
    (global-linum-mode t)
    (setq linum-format "%4d \u2502 ")))

(require 'package)
(setq package-archives
      '(("MELPA Stable" . "https://stable.melpa.org/packages/")
	("GNU ELPA"     . "https://elpa.gnu.org/packages/")
	("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
	("MELPA Stable" . 5)
	("MELPA"        . 0)))
(package-initialize)


;; LINE MAGIC
(defun duplicate-line ()
  (interactive)
  (save-mark-and-excursion
    (beginning-of-line)
    (insert (thing-at-point 'line t))))

(global-set-key (kbd "C-S-d") 'duplicate-line)

(defun move-line-down ()
   (interactive)
   (let ((col (current-column)))
     (save-excursion
       (forward-line)
       (transpose-lines 1))
     (forward-line)
     (move-to-column col)))

 (defun move-line-up ()
   (interactive)
   (let ((col (current-column)))
     (save-excursion
       (forward-line)
       (transpose-lines -1))
     (forward-line -1)
     (move-to-column col)))

 (global-set-key (kbd "C-S-j") 'move-line-down)
 (global-set-key (kbd "C-S-k") 'move-line-up)


;; MULTIPLE CURSORS
(require 'multiple-cursors)
(global-set-key (kbd "C-|") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(define-key mc/keymap (kbd "<return>") nil)


;; AUTO-COMPLETION
(add-hook 'after-init-hook 'global-company-mode)


;; THE FUZZ
(require 'projectile)
(setq projectile-indexing-method 'alien)
(setq projectile-enable-caching t)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-global-mode)

(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-M-x-fuzzyj-match t)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-mode 1)

;; SPEEDBAR
(global-set-key (kbd "<f8>") 'speedbar)

;; IDE
;;(dumb-jump-mode) ;; C-M-g => jump to definition
;; C-M-p => jump to function call


;; GOD MODE
(require 'god-mode)
(global-set-key (kbd "<escape>") 'god-mode)
(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
			'box
		      'bar)))
(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
			    (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
			    (set-face-background 'mode-line-inactive (if limited-colors "white" "#e9e2cb"))))
	  (t (progn
	       (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
	       (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)


;; QoL DEFAULT CHANGES
(load-theme 'atom-one-dark t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(setq-default cursor-type 'bar)
(set-default 'truncate-lines t)
(global-hl-line-mode 1)

(add-hook 'prog-mode-hook (lambda ()
			    (font-lock-add-keywords nil
						    '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))))
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(rainbow-delimiters-mode t)
(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-08"))
(set-face-attribute 'default t :font "IBM Plex Mono-08")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (sublimity ag helm-ag zenburn-theme swiper spacemacs-theme spacegray-theme rainbow-delimiters projectile powerline planet-theme plan9-theme org northcode-theme noctilux-theme multiple-cursors minimap material-theme magit lab-themes inverse-acme-theme inkpot-theme helm gruvbox-theme green-screen-theme gotham-theme god-mode flatland-theme flatland-black-theme fill-column-indicator darkokai-theme darkburn-theme cyberpunk-theme company-anaconda cheatsheet atom-one-dark-theme ample-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
