(load "~/scame/base.el" nil t)
;; (load "~/Documents/scame/extended/org-mode.el" nil t)
;; (load "~/Documents/scame/extended/editing-config.el" nil t)
;; (load "~/Documents/scame/extended/multiple-cursors.el" nil t)
;; (load "~/Documents/scame/extended/spell-check.el" nil t)
;; (load "~/Documents/scame/extended/dired-setting.el" nil t)
;; (load "~/Documents/scame/extended/mode-line.el" nil t)
;; (load "~/Documents/scame/extended/git.el" nil t)
;; (load "~/Documents/scame/extended/buffer-completion-system.el" nil t)
;; (load "~/Documents/scame/extended/minibuffer-completion-system.el" nil t)
;; (load "~/Documents/scame/extended/more-packages.el" nil t)
(load "~/scame/extended.el" nil t)

(use-package ef-themes)
(use-package doric-themes)
(when (display-graphic-p)
  ;; (set-frame-font "Comic Shanns Mono 14" nil t)
  (set-frame-font "Comic Code Ligatures 14" nil t)
  (setq-default line-spacing 0.1)
  ;; (load-theme 'modus-operandi-tinted)
  ;; (load-theme 'modus-operandi-tinted)
  ;; (load-theme 'doric-wind)
  ;; (load-theme 'doric-earth)
  ;; (load-theme 'doric-oak)
  ;; (load-theme 'doric-light)
  ;; (load-theme 'doric-beach)
  (load-theme 'doric-cherry)
  ;; (load-theme 'doric-marble) ;obsidian
  ;; (load-theme 'ef-autumn)
  ;; (load-theme 'ef-fire)
  )
;; ;; (set-face-attribute hl-line-face nil :underline t)

(setq modus-themes-fringes nil)
(add-hook 'enable-theme-functions
          (lambda (&rest _) (set-face-foreground
                             'vertical-border (face-background 'default))))

;; user info
(setq user-full-name "Felix Dumbeck"
      user-mail-address "felix@dumbeck.net")

(setq org-agenda-files
      '("~/uni/notes/uni.org"
        "~/uni/notes/personal.org"))

(setq completion-in-region-function #'completion--in-region)
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/Notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
	  "https://nixos.org/blog/announcements-rss.xml"
	  ;; "https://www.schneier.com/feed/atom/"
	  "https://systemcrafters.net/rss/"
	  "http://www.polyomica.com/feed/"
          "https://mccd.space/feed.xml"
          "https://dthompson.us/feed.xml"
	  "https://smallcultfollowing.com/babysteps//atom.xml"
          ;; "https://planet.emacslife.com/atom.xml"
	  "https://guix.gnu.org/feeds/blog.atom"
	  "https://chrismaiorana.com/feed/"
	  "https://blog.hansenpartnership.com/feed/"
          "https://news.opensuse.org/feed.xml"
          "https://irreal.org/blog/?feed=rss2"
          "https://protesilaos.com/keeb.xml"
          "https://protesilaos.com/codelog.xml"
          "https://protesilaos.com/news.xml"
          "https://drewdevault.com/blog/index.xml"
          "http://dominique.leuenberger.net/blog/feed"
          "https://lorendb.dev/index.xml"
          "https://0pointer.net/blog/index.rss20"
          "https://robert.kra.hn/feed.xml"
          "https://lambdaland.org/index.xml"
	  "https://cjohansen.no/atom.xml"
          ;; "https://archlinux.org/feeds/news/"
          "https://blogs.kde.org/index.xml")))

(bind-key "C-c c c" #'compile)
(bind-key "C-c c r" #'recompile)

(bind-key "C-z" #'yank)

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode))

(use-package eglot-java)

(require 'tramp)
(setq tramp-remote-process-environment
      (append
       (list (concat "PATH="
                     "/run/wrappers/bin" ":"
                     "/home/admin/.nix-profile/bin" ":"
                     "/nix/profile/bin" ":"
                     "/home/admin/.local/state/nix/profile/bin" ":"
                     "/etc/profiles/per-user/admin/bin" ":"
                     "/nix/var/nix/profiles/default/bin" ":"
                     "/run/current-system/sw/bin" ":"
                     "/bin" ":" "/usr/bin")) ; Match terminal PATH
       tramp-remote-process-environment))

(defun felix/hexl-hex-string-to-integer (hex-string)
  "Return decimal integer for HEX-STRING.
 Accepts optional 0x or 0X prefix."
  (interactive "sHex number: ")
  (when (string-match "\\`0[xX]\\(.+\\)" hex-string)
    (setq hex-string (match-string 1 hex-string)))
  (let ((hex-num 0))
    (while (not (equal hex-string ""))
      (setq hex-num (+ (* hex-num 16)
                       (hexl-hex-char-to-integer (string-to-char hex-string))))
      (setq hex-string (substring hex-string 1)))
    hex-num))

(require 'calc)

(defun felix/my-hexl-goto-hex-address (expr)
  "Goto address in hexl-mode.
 - Accepts hex literals (0x...).
 - Accepts arithmetic (e.g. 0x20 + 10).
 - Supports relative jumps with +N / -N.

 Examples:
   0x20       → absolute 0x20
   0x100 + 7  → absolute 0x107
   +0x10      → move forward 0x10 bytes
   -32        → move back 32 bytes"
  (interactive "sHex Address (expression): ")
  (let* ((cur (hexl-current-address))
         (relative (string-match-p "\\`[+-]" expr))
         ;; turn 0x... into decimal literals for calc
         (expr (replace-regexp-in-string
		"0x[0-9A-Fa-f]+"
		(lambda (s) (format "(%d)" (string-to-number (substring s 2) 16)))
		;; "0x[0-9A-Fa-f]+\\>"
		;; (lambda (s)
		;;   (format "(%d)"(string-to-number (replace-regexp-in-string "\\`0[xX]" "" s) 16)))
		expr))
         (val (string-to-number (calc-eval expr)))
         (addr (if relative (+ cur val) val)))
    (hexl-goto-address addr)))

(defun thanos/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype -s 350 \"%s\"" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype -s 350 \"%s\" && wtype -k Return" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(defun thanos/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (erase-buffer)
    (org-mode)
    (setq-local header-line-format
                (format " %s to insert text or %s to cancel."
                        (propertize "C-c C-c" 'face 'help-key-binding)
			(propertize "C-c C-k" 'face 'help-key-binding)))
    (local-set-key (kbd "C-c C-k")
		   (lambda () (interactive)
		     (kill-new (buffer-string))
		     (delete-frame)))
    (local-set-key (kbd "C-c C-c")
		   (lambda () (interactive)
		     (start-process-shell-command
		      "wtype" nil
		      (thanos/wtype-text (buffer-string)))
		     (delete-frame)))))

(setq auth-sources '("secrets:default" default))

(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.mailbox.org"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "felix@dumbeck.net")

;; (setq auth-nsource-debug t)
;; (setq smtpmail-debug-info t
;;       smtpmail-debug-verb t)
