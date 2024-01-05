(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; make all use-package :ensure t
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*", (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq user-full-name "Felix"
      user-mail-address "f.dumbeck@campus.tu-berlin.de")

(setq inhibit-startup-screen t)
(setq initial-major-mode 'org-mode ;; Major mode of new buffers
      initial-scratch-message ""
      initial-buffer-choice t) ;; Blank scratch buffer

(setq visible-bell t)

(setq use-dialog-box nil)

(setq use-dialog-box nil)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-hl-line-mode t)

(global-prettify-symbols-mode t)

(global-visual-line-mode t)

;(global-display-line-numbers-mode t) ;; enable line numbers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative) ;; make line numbers relative

(use-package ef-themes)
(load-theme 'ef-autumn t)

(set-frame-font "Fantasque Sans Mono 12" nil t)

(use-package all-the-icons
  :if (display-graphic-p))

(use-package emojify
  :init (global-emojify-mode))

(use-package doom-modeline
  :init (doom-modeline-mode t))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(recentf-mode t)

(save-place-mode t)

(delete-selection-mode)

(global-auto-revert-mode t)
;; revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(defun kill-buffer-and-close-window ()
  "Kill the current buffer and close its window."
  (interactive)
  (kill-buffer)
  (delete-window))
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-close-window)

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

(setq history-length 50)
(savehist-mode t)

(use-package orderless
  :after vertico
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package corfu
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-auto-prefix 2) ;; show completions after two letters
  (corfu-auto-delay 0) ;; show completions immediatly
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  (setq corfu-popupinfo-delay nil))

(use-package nerd-icons-corfu
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package consult
  :bind ("C-c r" . 'consult-ripgrep)
  :config
  (keymap-global-set "C-s" 'consult-line)
  (keymap-set minibuffer-local-map "C-r" 'consult-history)
  (setq completion-in-region-function #'consult-completion-in-region)
  )

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-emoji)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package org
  :defer t
  :commands (org-mode))

(use-package org-contrib
  :init (require 'org-tempo)
  :after org-mode)

(use-package org-cliplink
  :bind ("C-x p i" . org-cliplink))

(use-package ox-reveal
  :after org-mode
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js"))

(use-package ox-hugo
  :after org-mode
  :commands org-hugo-auto-export-mode)

(use-package org-download
  :after org-mode
  :config (add-hook 'dired-mode-hook #'org-download-enable))

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

(custom-set-faces
 '(org-level-1 ((t (:height 1.75))))
 '(org-level-2 ((t (:height 1.5))))
 '(org-level-3 ((t (:height 1.25))))
 '(org-level-4 ((t (:height 1.1))))
 '(org-document-title ((t (:height 1.5)))))

(use-package org-make-toc
  :after org-mode
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

(use-package ob-rust
  :after org-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

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
(setq org-agenda-span 14)
(setq org-agenda-start-on-weekday nil)

(setq org-agenda-files
      '("~/uni/uni.org"
        "~/uni/personal.org"))

(global-set-key (kbd "C-c C-h") 'hs-hide-block)
(global-set-key (kbd "C-c C-s") 'hs-show-block)
(global-set-key (kbd "C-c C-t") 'hs-toggle-hiding)
(global-set-key (kbd "C-c C-a") 'hs-show-all)
(global-set-key (kbd "C-c C-l") 'hs-hide-all)

(defun kill-line-backward ()
  "Kill line backwards from the position of the pointer to the beginning of the line."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "C-S-k") 'kill-line-backward)

(use-package multiple-cursors
  :defer t
  :bind(("C-;" . mc/edit-lines)
        ("C->" . mc/mark-next-like-this)
        ("C-<" . mc/mark-previous-like-this)
        ("C-c C-<" . mc/mark-all-like-this)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(electric-pair-mode t)

(use-package magit
  :commands magit)

(use-package keychain-environment
  :after magit)

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package geiser-guile
  :hook (scheme-mode . geiser-guile))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-view-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package flymake-shellcheck
  :hook (bash-ts-mode . flymake-shellcheck-mode))

(add-to-list 'auto-mode-alist '("Makefile" . makefile-mode))

(use-package eglot
  :defer t
  :config
  (define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c c o") 'eglot-code-action-organize-imports)
  (define-key eglot-mode-map (kbd "C-c c h") 'eldoc)
  (define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c c f") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)
  (define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)
  )

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(setq dired-listing-switches "-alh")

(setq dired-auto-revert-buffer t)

(setq dired-dwim-target t)

(use-package vterm
  :commands vterm)

(global-set-key (kbd "M-RET") 'eshell)

(use-package htmlize
  :defer t)

(use-package pdf-tools
  :defer t
  :mode ("\\.pdf\\'" . pdf-view-mode))

(setq gc-cons-threshold (* 2 1000 1000))
