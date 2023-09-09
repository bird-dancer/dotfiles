;; set user info
(setq user-full-name "Felix"
      user-mail-address "f.dumbeck@campus.tu-berlin.de")

;; set repos
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; make all use-package :ensure t
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq use-package-verbose t)

;; open config with C-c f P
(defun open-config ()
  "Opens emacs config file"
  (interactive)
  (setq find-file-visit-truename t)
  (find-file (locate-user-emacs-file "init.el")))
(global-set-key (kbd "C-c f P") 'open-config)

;; kill buffer and close window simultaniously
(defun kill-buffer-and-close-window ()
  "Kill the current buffer and close its window."
  (interactive)
  (kill-buffer)
  (delete-window))
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-close-window)			  

(defun kill-line-backward ()
  "Kill line backwards from the position of the pointer to the beginning of the line."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "C-S-k") 'kill-line-backward)

;; show recently opened files
(recentf-mode t)
;; save location in file
(save-place-mode t)
;; Revert buffers when the underlying file has changed
(global-auto-revert-mode t)

;; Move customization variables to a sepereate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)
      
;; keep folders clean from ...
;; backup files
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))
;; auto-save files
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*", (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))
(use-package no-littering)

;; improve looks
;;
(global-hl-line-mode t) ;; enable line highlighting
(global-prettify-symbols-mode t) ;; prettify symbols
(global-visual-line-mode t) ;; enable line wrapping
;; line numbers
(global-display-line-numbers-mode t) ;; enable line numbers
(setq display-line-numbers-type 'relative) ;; make line numbers relative

(setq inhibit-startup-screen t ;; disable splash screen
      visible-bell t) ;; flash when the bell rings
;; remove GUI elements
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; make theme depend on sunrise/sunset
(use-package gruvbox-theme
  :init (load-theme 'gruvbox-dark-soft))
;(use-package circadian
;  :config
;  (setq calendar-latitude 52.5)
;  (setq calendar-longitude 13.4)
;  (setq circadian-themes '((:sunrise . gruvbox-light-soft)
;                           (:sunset  . gruvbox-dark-soft)))
;  (circadian-setup))
;; display current buffer as html
(use-package htmlize
  :defer t)
;; cursor flashes after big jumps
(use-package beacon
  :init (beacon-mode 1))
;; emoji stuff
;; enable emojis
(use-package emojify
  :init (emojify-mode))

;; enable autocompletion for emoji
(use-package company-emoji
  :init (company-emoji-init))
;; various icon fonts
(use-package all-the-icons
  :if (display-graphic-p))

;(use-package doom-modeline
;  :init (doom-modeline-mode t))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))


;; projectile
;(use-package projectile
;  :config (projectile-mode)
;  :bind-keymap ("C-c p" . projectile-command-map))
;;; treemacs
;(use-package treemacs)
;(use-package treemacs-projectile)



;;; autocompletion
;; buffer autocompletion with vertico
(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))
;; information about vertico completions
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))
;; make vertico completions save history
(setq history-length 30)
(savehist-mode t)
;; improve vertico completions
(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))
;; icons for completion
(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :init (all-the-icons-completion-mode)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup))

;; which-key is great for getting an overview of what keybindings are available based on the prefix keys you entered. Learned about this one from Spacemacs.
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;(use-package embark)
;;(load-file "./counlt.el")

;; enable autocompletion in code with company
(use-package company
  :config
  (global-company-mode t))

;; multiple cursors
(use-package multiple-cursors
  :bind ("M-SPC" . set-rectangular-region-anchor))
;; treesit-auto
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))
(setq treesit-auto-install 'prompt)


;;
;; org config
(use-package org
  :defer t
  :commands (org-mode)
  )
;; make it pretty
;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))
;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))
;; Nice bullets
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-special-todo-items t))

;; change headings
(custom-set-faces
 '(org-level-1 ((t (:height 1.75))))
 '(org-level-2 ((t (:height 1.5))))
 '(org-level-3 ((t (:height 1.25))))
 '(org-level-4 ((t (:height 1.1))))
 '(org-document-title ((t (:height 1.5)))))
;; quickly insert structual blocks
;(with-eval-after-load 'org

;; give pasted links the title provided by the website
(use-package org-cliplink
  :bind ("C-x p i" . org-cliplink)
  :hook (org-mode . org-cliplink))

;; idk
(use-package org-contrib
  :init (require 'org-tempo)
  :after org
)

(use-package ox-hugo
  :after org
  :commands org-hugo-auto-export-mode)
(setq org-imenu-depth 7)
;; languages in org mode
(use-package ob-rust
  :after org)

;; org-roam
(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Desktop/Notes/"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "\n* See also\n\n* Reference\n%?\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n\n")
      :unnarrowed t)
     ("b" "book notes" plain
      "\n* Source\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n%?\n\n* See also\n\n* References\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n\n")
      :unnarrowed t)))
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

;; vterm
(use-package vterm
 :bind ("M-RET" . vterm))

(use-package magit
  :commands magit)

;;
;; to install
;;
;; flycheck
;; highlighting for TODO etc.
;; ctlr + RET adds comments/indentation
;; make case insesetive for at least vertico
;; pdf stuff
;; look at my doom config



;; (use-package rust-mode
;;   :ensure t)
;; ;; rust
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; ;; (treesit-install-language-grammar 'rust)
;; (use-package rust-ts-mode
;;   :ensure t
;;   :hook ((rust-ts-mode . eglot-ensure)
;; 	 (rust-ts-mode . company-mode))
;;   :config
;;   (add-to-list 'exec-path "/usr/bin/cargo")
;;   (setenv "PATH" (concat (getenv "PATH") ":/usr/bin/cargo"))
;;  )
