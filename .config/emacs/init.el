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
;;(load-theme 'wombat) ;; TODO: load dark theme at night
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


;; set repos
;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

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
;;(use-package treesit-auto
;;  :config
;;  (global-treesit-auto-mode))
;;(setq treesit-auto-install 'prompt)
;; TODO add extensions for programming languages that don't have built in major-modes for auto-ts-mode


;;
;; to install
;;
;; flycheck
;; highlighting for TODO etc.
;; ctlr + RET adds comments/indentation
;; make case insesetive for at least vertico
;; vterm M+RET
;; look at my doom config



;;(use-package rust-mode
;;  :ensure t)
;; rust
;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
;; (treesit-install-language-grammar 'rust)
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
 '(package-selected-packages '(vertico orderless marginalia company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
