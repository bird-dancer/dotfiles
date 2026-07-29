;; -*- lexical-binding: t; -*-

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

(setq-default cursor-type 'bar)	 ;use bar as cursor
;; (when (display-graphic-p)
;; (set-frame-font "Comic Shanns Mono 14" nil t)
(set-frame-font "Comic Code Ligatures 13" nil t)
;;(setq-default line-spacing 0.08)
;; (use-package ef-themes)
;; (use-package doric-themes)
;; (load-theme 'modus-operandi)
;; (load-theme 'modus-operandi-tinted)
;; (load-theme 'ef-day)
;; (load-theme 'doric-wind)
;; (load-theme 'doric-earth)
;; (load-theme 'doric-oak)
;; (load-theme 'doric-light)
;; (load-theme 'doric-beach)
;; (load-theme 'doric-cherry)
;; (load-theme 'alect-light-alt)
;; (load-theme 'alect-black)
;; (load-theme 'doric-marble) ;obsidian

;; dark
;; (load-theme 'ef-autumn)
;; )
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
  :defer t
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
(use-package org-roam-ui
  :after org-roam)

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
          ;; "https://news.opensuse.org/feed.xml"
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

(bind-key "C-x C-b" #'switch-to-buffer)

(bind-key "C-z" #'yank)

(use-package gleam-ts-mode
  :load-path "/home/felix/gleam-mode/"
  :mode ("\\.gleam\\'" . gleam-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
       	       '(gleam-ts-mode . ("gleam" "lsp"))))

(setq eglot-prefer-plaintext t)
;;(setq eldoc-documentation-strategy #'eldoc-documentation-compose)
(setq eldoc-echo-area-use-multiline-p t)
(advice-add 'flymake-diagnostic-oneliner :around
            (lambda (_orig-fun diag)
              (flymake-diagnostic-text diag)))

(defun my-locate-python-virtualenv ()
  "Find the Python executable based on the VIRTUAL_ENV environment variable."
  (when-let ((venv (getenv "VIRTUAL_ENV")))
    (let ((python-path (expand-file-name "bin/python" venv)))
      (when (file-executable-p python-path)
        python-path))))

(with-eval-after-load 'lsp-pyright
  (add-to-list 'lsp-pyright-python-search-functions
               #'my-locate-python-virtualenv))

(defun setup-python-environment ()
  "Setup a Python development environment in the current buffer."
  (yas-minor-mode 1)

  ;; Use the Python binary from the virtualenv
  (let ((python-bin (executable-find "python")))
    (setq-local python-shell-interpreter python-bin))

  ;; If IPython is installed in this venv, use it with the correct flags
  (when (executable-find "ipython")
    (setq-local python-shell-interpreter "ipython"
		python-shell-interpreter-args "-i --simple-prompt")))

;; (with-eval-after-load 'python
;;   (defun python-shell-completion-at-point ()
;;     nil))

(setenv "PYTHON_BASIC_REPL" "1")
(with-eval-after-load 'python
  ;; Disable native completion to prevent dummy_completion leaks
  (setq python-shell-completion-native-enable nil)

  ;; Force classic REPL for Python 3.13+
  (setq python-shell-process-environment
  	(cons "PYTHON_BASIC_REPL=1" python-shell-process-environment)))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            ;; 1. Hide terminal echo to catch any remaining stray strings
            (setq comint-process-echoes t)

            ;; 2. Stop eldoc from checking function arguments in the background
            (eldoc-mode -1)
            
            ;; 3. IF USING COMPANY: Disable the idle timer so it never auto-pops up
            (when (boundp 'company-idle-delay)
              (setq-local company-idle-delay nil))
            
            ;; 4. IF USING CORFU: Disable auto-triggering on typing
            (when (boundp 'corfu-auto)
              (setq-local corfu-auto nil))))

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

(defun my/copy-current-path ()
  "Copy the current buffer file path or Dired path to the kill ring.
Works in normal buffers and in Dired."
  (interactive)
  (let ((path
         (cond
          ;; Dired: use current directory or marked file
          ((derived-mode-p 'dired-mode)
           (expand-file-name
            (or (dired-get-filename nil t)
                default-directory)))

          ;; Regular file-visiting buffer
          (buffer-file-name
           (expand-file-name (buffer-file-name)))

          ;; Fallback
          (t
           (error "No file associated with this buffer")))))
    (kill-new path)
    (message "Copied path: %s" path)))

(use-package vterm
  :defer t
  :hook (vterm-mode . (lambda ()
			(setq-local global-hl-line-mode nil)
			(hl-line-mode -1))))

(use-package ghostel
  :bind(("C-c c c" . ghostel-compile)
      	("C-c C-c c" . ghostel-compile)
      	("C-c C-c r" . ghostel-recompile)
      	("C-c C-c d" . ghostel-compile-debug)
	("C-M-<return>" . ghostel))
  :init
  (setq ghostel-module-directory "~/.emacs.d/ghostel/")
  :config
  (setq ghostel-progress-function #'ghostel-spinner-progress)
  ;; (setq ghostel-progress-function #'ghostel-default-progress)
  (setq ghostel-spinner-type 'horizontal-moving)
  ;; make cursor bar
  (setq ghostel-ignore-cursor-change t)
  ;; (ghostel--set-cursor-style 0 t)
  )

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
