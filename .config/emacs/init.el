(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; make all use-package :ensure t
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package no-littering
  :init
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*", (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(defun open-config ()
  "Opens emacs config file"
  (interactive)
  (setq find-file-visit-truename t)
  (find-file (locate-user-emacs-file "Emacs.org")))
(global-set-key (kbd "C-c f P") 'open-config)

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

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(add-hook 'prog-mode-hook 'hs-minor-mode)
(add-hook 'rust-ts-mode 'hs-minor-mode)
(global-set-key (kbd "C-c f h") 'hs-hide-block)
(global-set-key (kbd "C-c f s") 'hs-show-block)
(global-set-key (kbd "C-c f t") 'hs-toggle-hiding)

(defun copy-line-to-kill-ring ()
  "Copy the current line to the kill ring without killing it."
  (interactive)
  (let ((line-text (buffer-substring (line-beginning-position) (line-end-position))))
    (kill-new line-text)))
(global-set-key (kbd "C-S-c") 'copy-line)

(setq user-full-name "Felix"
      user-mail-address "f.dumbeck@campus.tu-berlin.de")

(setq inhibit-startup-screen t)

(setq visible-bell t)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-hl-line-mode t)

(global-prettify-symbols-mode t)

(global-visual-line-mode t)

(global-display-line-numbers-mode t) ;; enable line numbers
(setq display-line-numbers-type 'relative) ;; make line numbers relative

(use-package gruvbox-theme)
(use-package circadian
  :config
  (setq calendar-latitude 52.5)
  (setq calendar-longitude 13.4)
  (setq circadian-themes '((:sunrise . gruvbox-light-soft)
                           (:sunset  . gruvbox-dark-soft)))
  (circadian-setup))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package emojify
  :init (emojify-mode))

(use-package doom-modeline
  :init (doom-modeline-mode t))

(recentf-mode t)

(save-place-mode t)

(global-auto-revert-mode t)

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (all-the-icons marginalia)
  :init (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(setq history-length 30)
(savehist-mode t)

(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package company
  :config
  (global-company-mode t))

(use-package company-emoji
  :after company
  :init (company-emoji-init))

(use-package org
  :defer t
  :commands (org-mode)
  )

(use-package org-contrib
  :init (require 'org-tempo)
  :after org
  )

(use-package ox-hugo
  :after org
  :after ox
  :commands org-hugo-auto-export-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'org-up-element)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") 'org-down-element)))

(setq org-startup-indented t
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(custom-set-faces
 '(org-level-1 ((t (:height 1.75))))
 '(org-level-2 ((t (:height 1.5))))
 '(org-level-3 ((t (:height 1.25))))
 '(org-level-4 ((t (:height 1.1))))
 '(org-document-title ((t (:height 1.5)))))

(use-package org-cliplink
  :after org
  :bind ("C-x p i" . org-cliplink))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("ba" . "src bash"))
(add-to-list 'org-structure-template-alist '("zs" . "src zsh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("li" . "src lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("go" . "src go"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("tex" . "src latex"))
(add-to-list 'org-structure-template-alist '("rs" . "src rust"))

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(use-package org-download
  :hook (dired-mode . org-download-enable))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Desktop/Notes"))
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

(global-set-key (kbd "C-c a") #'org-agenda)

(setq org-agenda-start-day "0d")
(setq org-agenda-span 7)
(setq org-agenda-start-on-weekday nil)

(setq org-agenda-files
      '("~/Desktop/Uni/uni.org"
        "~/personal.org" ))

(use-package multiple-cursors
  :bind ("M-SPC" . set-rectangular-region-anchor))

(global-set-key (kbd "C-;") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")))
  (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
  (keymap-set hl-todo-mode-map "C-c P" #'hl-todo-next)
  (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur)
  (keymap-set hl-todo-mode-map "C-c i" #'hl-todo-insert))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(setq electric-pair-mode t)

(use-package magit
  :commands magit)

(use-package keychain-environment)

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))
(setq treesit-auto-install 'prompt)

(use-package format-all)

(use-package geiser-guile)

(use-package highlight-indent-guides
  :hook (python-ts-mode . highlight-indent-guides-mode))

(use-package flymake-shellcheck
  :hook (bash-ts-mode . flymake-shellcheck-mode))

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-hook 'rust-ts-mode-hook #'rainbow-delimiters-mode)
(add-hook 'rust-ts-mode-hook #'hs-minor-mode)
(add-hook 'rust-ts-mode-hook #'hl-todo-mode)
(add-hook 'rust-ts-mode-hook #'eglot)

(use-package eglot
  :defer t
  :config
  (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c c o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c c h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c c f") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package vterm
  :commands vterm
  :bind ("M-RET" . vterm))

(use-package htmlize
  :defer t)

(use-package pdf-tools)

(setq gc-cons-threshold (* 2 1000 1000))
