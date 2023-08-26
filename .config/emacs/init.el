;; set user info
(setq user-full-name "Felix"
      user-mail-address "f.dumbeck@campus.tu-berlin.de")

;; open config with C-c f P
(global-set-key (kbd "C-c f P") (lambda ()
  (interactive)
  (find-file "~/.config/emacs/init.el")))

;; load auto-ts-mode
;;(load-file "~/Desktop/treesit-auto/treesit-auto.el")
;;(add-to-list 'auto-mode-alist '("\\.rs'" . rust-ts-mode))
;;(add-to-list 'auto-mode-alist '("\\.hs'" . haskell-ts-mode))

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
;;(load-theme 'wombat) ;; TODO: load dark theme at night
(use-package circadian
  :config
  (setq calendar-latitude 52.5)
  (setq calendar-longitude 13.4)
  (setq circadian-themes '((:sunrise . modus-operandi)
                           (:sunset  . modus-vivendi)))
  (circadian-setup))
;; display current buffer as html
(use-package htmlize)



;; set repos
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; make all use-package :ensure t
(require 'use-package-ensure)
(setq use-package-always-ensure t)


;; autocompletion
;;
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
(use-package savehist
  :init
  (savehist-mode))
;; improve vertico completions
(use-package orderless
  :after vertico
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; enable autocompletion in code with company
(use-package company
  :config
  (global-company-mode t))

;; treesit-auto
(use-package treesit-auto
  :config
  (global-treesit-auto-mode))
(setq treesit-auto-install 'prompt)
;; TODO add extensions for programming languages that don't have built in major-modes for auto-ts-mode

;; 

;;
;; org config
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
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))
;; give pasted links the title provided by the website
(use-package org-cliplink)
(global-set-key (kbd "C-x p i") 'org-cliplink)
;; idk
(use-package org-contrib)
(use-package ox-hugo)
;; languages in org-mode
(use-package ob-rust) ; rust

;; Edit header size and color
(custom-set-faces! (org-document-title :weight bold :height 1.4))
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
;;(use-package vterm
;;  :bind ("M-RET" . vterm))

(use-package magit)



;;
;; to install
;;
;; flycheck
;; highlighting for TODO etc.
;; ctlr + RET adds comments/indentation
;; make case insesetive for at least vertico
;; vterm M+RET
;; pdf stuff
;; emojies
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit ox-hugo vterm vertico treesit-auto toc-org rust-mode org-superstar org-roam org-contrib org-cliplink org-appear orderless ob-rust marginalia julia-mode htmlize eglot company circadian)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title :weight nil :height))
