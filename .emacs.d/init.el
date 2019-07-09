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
 '(compilation-message-face (quote default))
 '(cursor-color "#cccccc")
 '(custom-emabled-themes (quote (wombat)))
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes (quote ("05a4b82c39107308b5c3720fd0c9792c2076e1ff3ebb6670c6f1c98d44227689" "54449a089fc2f95f99ebc9b9b6067c802532fd50097cf44c46a53b4437d5c6cc" "9b35c097a5025d5da1c97dba45fed027e4fb92faecbd2f89c2a79d2d80975181" "c7f10959cb1bc7a36ee355c765a1768d48929ec55dde137da51077ac7f899521" "8d805143f2c71cfad5207155234089729bb742a1cb67b7f60357fdd952044315" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "37ba833442e0c5155a46df21446cadbe623440ccb6bbd61382eb869a2b9e9bf9" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "8ffdc8c66ceeaf7921f4510a70d808f01b303e6b4d177c947b442e80d4228678" "30289fa8d502f71a392f40a0941a83842152a68c54ad69e0638ef52f04777a4c" "10a31b6c251640d04b2fa74bd2c05aaaee915cbca6501bcc82820cdc177f5a93" "8885761700542f5d0ea63436874bf3f9e279211707d4b1ca9ed6f53522f21934" "669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "39546362fed4d5201b2b386dc21f21439497c9eec5fee323d953b3e230e4083e" "152c9642180cb0907bfe7c343ed07d0586c0d84fd8e7279d90566088989a13bb" "1436d643b98844555d56c59c74004eb158dc85fc55d2e7205f8d9b8c860e177f" "8e797edd9fa9afec181efbfeeebf96aeafbd11b69c4c85fa229bb5b9f7f7e66c" "2b9dc43b786e36f68a9fd4b36dd050509a0e32fe3b0a803310661edb7402b8b6" "b583823b9ee1573074e7cbfd63623fe844030d911e9279a7c8a5d16de7df0ed0" "59e82a683db7129c0142b4b5a35dbbeaf8e01a4b81588f8c163bd255b76f4d21" "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0" "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739" "725a0ac226fc6a7372074c8924c18394448bb011916c05a87518ad4563738668" "08a89acffece58825e75479333109e01438650d27661b29212e6560070b156cf" "5e515425f8a5ce00097d707742eb5eee09b27cebc693b8998c734305cbdce1f5" "d14d421ff49120d2c2e0188bcef76008407b3ceff2cfb1d4bdf3684cf3190172" "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a" default)))
 '(fci-rule-color "#383838")
 '(foreground-color "#cccccc")
 '(highlight-changes-colors (quote ("#ff8eff" "#ab7eff")))
 '(highlight-tail-colors (quote (("#323342" . 0) ("#63de5d" . 20) ("#4BBEAE" . 30) ("#1DB4D0" . 50) ("#9A8F21" . 60) ("#A75B00" . 70) ("#F309DF" . 85) ("#323342" . 100))))
 '(hl-paren-background-colors (quote ("#e8fce8" "#c1e7f8" "#f8e8e8")))
 '(hl-paren-colors (quote ("#40883f" "#0287c8" "#b85c57")))
 '(hl-sexp-background-color "#121212")
 '(hl-todo-keyword-faces (quote (("TODO" . "#dc752f") ("NEXT" . "#dc752f") ("THEM" . "#2aa198") ("PROG" . "#268bd2") ("OKAY" . "#268bd2") ("DONT" . "#d70000") ("FAIL" . "#d70000") ("DONE" . "#86dc2f") ("NOTE" . "#875f00") ("KLUDGE" . "#875f00") ("HACK" . "#875f00") ("TEMP" . "#875f00") ("FIXME" . "#dc752f") ("XXX+" . "#dc752f") ("\\?\\?\\?+" . "#dc752f"))))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors (quote ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages (quote (god-mode fill-column-indicator helm org)))
 '(pdf-view-midnight-colors (quote ("#282828" . "#f2e5bc")))
 '(pos-tip-background-color "#E6DB74")
 '(pos-tip-foreground-color "#242728")
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
