(require 'org)
(org-babel-load-file
 (expand-file-name "config.org"
		   user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" default)))
 '(global-whitespace-mode nil)
 '(package-selected-packages
   (quote
    (rjsx-mode multi-web-mode web-mode mmm-mode treemacs-magit treemacs-icons-dired treemacs-projectile magit rainbow-delimiters atom-dark-theme cmake-font-lock font-lock+ use-package projectile org multiple-cursors helm god-mode doom-modeline company atom-one-dark-theme)))
 '(projectile-completion-system (quote ivy))
 '(projectile-enable-caching t)
 '(projectile-indexing-method (quote alien)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
