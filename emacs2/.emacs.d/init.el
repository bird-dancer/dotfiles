(defmacro my/time-it (name &rest body)
  "Measure execution time of BODY and display with NAME."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06f seconds" ,name
              (float-time (time-since time)))))

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

(when (display-graphic-p)
  ;; (set-frame-font "Comic Shanns Mono 14" nil t)
  (set-frame-font "Comic Code Ligatures 13" nil t)
  ;;(setq-default line-spacing 0.08)
  (load-theme 'modus-operandi-tinted)
  ;; (use-package ef-themes)
  ;; (use-package doric-themes)
  ;; (load-theme 'modus-operandi-tinted)
  ;; (load-theme 'modus-operandi-tinted)
  ;; (load-theme 'doric-wind)
  ;; (load-theme 'doric-earth)
  ;; (load-theme 'doric-oak)
  ;; (load-theme 'doric-light)
  ;; (load-theme 'doric-beach)
  ;; (load-theme 'doric-cherry)
  ;; (load-theme 'alect-light-alt)
  ;; (load-theme 'alect-black)
  ;; (load-theme 'doric-marble) ;obsidian
  ;; (load-theme 'ef-autumn)
  ;; (load-theme 'ef-fire)
  )
;; (set-face-attribute hl-line-face nil :underline t)

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

(setq org-export-in-background t)

;; (setq org-highlight-latex-and-related '(latex script entities))
(with-eval-after-load 'org
  (setq org-cite-global-bibliography '("~/Zotero/better-bibtex/My Library.bib")))

;; (setq org-cite-export-processors '((latex biblatex)))
;; (setq org-latex-pdf-process
;;     '("latexmk -pdf -interaction=nonstopmode -output-directory=%o %f"))

(setq completion-in-region-function #'completion--in-region)
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/Notes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
	 ("C-c n a" . org-roam-alias-add)
	 ("C-c n b" . orb-insert-link)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))
(use-package org-roam-ui)

(use-package org-roam-bibtex
:after org-roam
:config
(setq bibtex-completion-bibliography org-cite-global-bibliography)
(setq orb-roam-ref-format 'org-cite)
(org-roam-bibtex-mode))

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
	  "https://grapheneos.org/releases.atom"
	  "https://mjg59.dreamwidth.org/data/rss"
	  "https://nixos.org/blog/announcements-rss.xml"
	  "https://rosenzweig.io/feed.xml"
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

(use-package gleam-ts-mode
  :mode ("\\.gleam\\'" . gleam-ts-mode)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
  		 '(gleam-ts-mode . ("gleam" "lsp")))))

(use-package nix-ts-mode
  :mode ("\\.nix\\'" . nix-ts-mode))

(use-package eglot-java
  :defer t)

(use-package indent-bars
  :hook (python-ts-mode . indent-bars-mode))

;;;###autoload
(defun felix/rust-ts--apply-rustfmt-config (rustfmt-data)
  "Apply settings from RUSTFMT-DATA to the current buffer.
  RUSTFMT-DATA is an alist parsed from rustfmt.toml."
  (let ((hard-tabs (alist-get "hard_tabs" rustfmt-data nil nil #'equal))
        (tab-spaces (alist-get "tab_spaces" rustfmt-data nil nil #'equal)))
    (message "rustfmt config: hard_tabs: %s; tab_spaces: %s" hard-tabs tab-spaces)
    (when (eq hard-tabs t)
      (setq-local indent-tabs-mode t))
    (if tab-spaces
        (setq-local tab-width tab-spaces)
      (setq-local tab-width 4))))
;;;###autoload
(defun felix/rust-ts--find-and-apply-rustfmt-config ()
  (interactive)
  "Look for a rustfmt.toml file in the current project tree and apply its settings."
  (let ((root (locate-dominating-file default-directory "rustfmt.toml")))
    (if (not(eql root nil))
  	(let ((rustfmt-file (expand-file-name "rustfmt.toml" root)))
  	  (message "using rustfmt.toml file: %s" rustfmt-file)
  	  (use-package toml)
  	  (condition-case err
  	      (let ((data (toml:read-from-file rustfmt-file)))
  		(message "data: %s" data)
  		(felix/rust-ts--apply-rustfmt-config data))
  	    (error (message "error: %s" err))))
      (message "no rustfmt-file found"))))

(add-hook 'rust-ts-mode-hook #'felix/rust-ts--find-and-apply-rustfmt-config)

(use-package gptel)

(setq auth-sources '("secrets:default" default))

;; (use-package smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-server "smtp.mailbox.org"
      smtpmail-smtp-service 465
      smtpmail-stream-type 'ssl
      smtpmail-smtp-user "felix@dumbeck.net")

;; (setq auth-nsource-debug t)
;; (setq smtpmail-debug-info t
;;       smtpmail-debug-verb t)
