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
  (set-frame-font "Comic Shanns Mono 14" nil t)
  ;; (load-theme 'modus-operandi-tinted)
  ;; (load-theme 'modus-operandi-tinted)
  ;; (load-theme 'doric-wind)
  ;; (load-theme 'doric-earth)
  ;; (load-theme 'doric-light)
  (load-theme 'doric-cherry)
  ;; (load-theme 'doric-marble)
  ;; (load-theme 'ef-autumn)
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
	  "https://www.schneier.com/feed/atom/"
          "https://mccd.space/feed.xml"
          "https://dthompson.us/feed.xml"
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
  :mode ("\\.toml\\'" . nix-ts-mode))

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
