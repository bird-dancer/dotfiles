(setq gc-cons-threshold (* 50 1000 1000))      ;500mb
(setq read-process-output-max (* 2 1024 1024)) ; 2mb

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(require 'use-package-ensure) ;; make all use-package :ensure t
(setq use-package-always-ensure t)

(setq package-install-upgrade-built-in t)

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

(setq backup-dir (expand-file-name "tmp/backups/" user-emacs-directory))
(setq backup-directory-alist `(("." . , backup-dir)))
(setq delete-old-versions t)

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*", (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(setq user-full-name "Felix Dumbeck"
      user-mail-address "felix@dumbeck.net")

(setq inhibit-startup-screen t)

(setq visible-bell t)

(setq use-dialog-box nil)

(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-hl-line-mode t)

(global-visual-line-mode t)

;; (global-display-line-numbers-mode t) ;; enable line numbers globally
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative) ;; make line numbers relative

(global-prettify-symbols-mode t)

(use-package ef-themes)

(setq modus-themes-mode-line '(borderless))

(setq modus-themes-common-palette-overrides
      '((border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))

(setq modus-themes-fringes nil)
(add-hook 'enable-theme-functions
          (lambda (&rest _) (set-face-foreground
                             'vertical-border (face-background 'default))))

(require 'ef-themes)
(use-package circadian
  :if (display-graphic-p)
  :config
  (setq calendar-latitude 52.5)
  (setq calendar-longitude 13.4)
  (setq circadian-themes '(
                           ;; (:sunrise . modus-operandi-tinted) ;emacs 30
                           (:sunrise . modus-operandi)
                           ;; (:sunrise  . ef-day)
                           ;; (:sunset  . ef-night)
                           ;; (:sunset  . ef-autumn)
                           ;; (:sunset . tango-dark)
                           (:sunset . modus-vivendi)
                           ;; (:sunset . ef-owl)
                           ;; (:sunrise . tsdh-light)
                           ;; (:sunset . gruber-darker)
                           ;; (:sunrise . tango)
                           ;; (:sunset . deeper-blue)
                           ;; (:sunset . wheatgrass)
                           ;; (:sunset . manoj-dark)
                           ))
  (circadian-setup))

;; (set-frame-font "Comic Shanns Mono 13" nil t)
(set-frame-font "Serious Shanns NerdFont 14" nil t)
;; (set-frame-font "Fantasque Sans Mono 12" nil t)
(add-to-list 'default-frame-alist '(font . "Serious Shanns NerdFont 14"))

(use-package all-the-icons
  :if (display-graphic-p))

(setq-default cursor-type 'bar)

(use-package doom-modeline
  :init (doom-modeline-mode t)
  :config
  (display-battery-mode)
  (setq display-time-24hr-format t)
  (display-time))

(use-package which-key
  ;; :ensure nil				;included in emacs 30+
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(global-set-key (kbd "C-x C-j") 'join-line)

(setq delete-by-moving-to-trash t)

(defun move-current-file-to-trash ()
  (interactive)
  (when (eq major-mode 'dired-mode)
    (user-error "%s: In dired. Nothing is done." real-this-command))
  (move-file-to-trash buffer-file-name))
(global-set-key (kbd "C-x x x") 'move-current-file-to-trash)

(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

(recentf-mode t)

(save-place-mode t)

(setq dired-recursive-deletes 'always)

(delete-selection-mode)

(global-auto-revert-mode t)
;; revert dired and other buffers
(setq global-auto-revert-non-file-buffers t)

(setq save-interprogram-paste-before-kill t)

(global-set-key (kbd "C-z") 'yank)

(defun kill-buffer-and-close-window ()
  "Kill the current buffer and close its window."
  (interactive)
  (kill-buffer)
  (delete-window))
(global-set-key (kbd "C-x C-k") 'kill-buffer-and-close-window)

(defun convert-region-decimal-to-hexadecimal (start end)
  "Convert a region from decimal to hexadecimal."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((num (thing-at-point 'word)))
        (when (string-match-p "^[0-9]+$" num)
          (delete-region (point) (+ (point) (length num)))
          (insert (format "0x%x" (string-to-number num)))))
      (forward-word))))

(defun convert-region-hexadecimal-to-decimal (start end)
  "Convert a region from hexadecimal to decimal."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((num (thing-at-point 'word)))
        (when (string-match-p "^0x[0-9a-fA-F]+$" num)
          (delete-region (point) (+ (point) (length num)))
          (insert (format "%d" (string-to-number (substring num 2) 16)))))
      (forward-word))))

(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  (vertico-count 10) ;; Show more candidatesm
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)	;Support opening new minibuffers from inside existing minibuffers.
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(setq history-length 50)
(savehist-mode t)

(use-package consult
  :bind (("C-c r" . consult-ripgrep)
         ("C-s" . consult-line))
  :config
  (keymap-set minibuffer-local-map "C-r" 'consult-history)
  (setq completion-in-region-function #'consult-completion-in-region))

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
  (corfu-history-mode))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.6 . 0.4))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :init
  (corfu-terminal-mode t))

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(add-hook 'eshell-mode-hook
          (lambda ()
            (setq-local corfu-auto nil)
            (corfu-mode)))

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
         ("C-c p :" . cape-emoji);;; might be deleted
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
  (add-to-list 'completion-at-point-functions #'cape-abbrev) ;Complete abbreviation (add-global-abbrev, add-mode-abbrev).
  (add-to-list 'completion-at-point-functions #'cape-dabbrev) ;Complete word from current buffers. See also dabbrev-capf on Emacs 29
  (add-to-list 'completion-at-point-functions #'cape-file)    ;Complete file name.
  (add-to-list 'completion-at-point-functions #'cape-elisp-block) ;Complete Elisp in Org or Markdown code block.
  (add-to-list 'completion-at-point-functions #'cape-history)	  ;Complete from Eshell, Comint or minibuffer history.
  (add-to-list 'completion-at-point-functions #'cape-keyword)	  ;Complete programming language keyword.
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-dict) ;Complete word from dictionary file.
  ;; (add-to-list 'completion-at-point-functions #'cape-emoji)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package org
  :ensure nil				;load built in org-mode
  :commands (org-mode))

(defun unpropertize (string)
  "Removes all text properties from STRING."
  (set-text-properties 0 (length string) nil string) string)
(defun org-get-headings ()
  "Return a list of an org document's headings."
  (org-map-entries (lambda () (unpropertize (org-get-heading t t t t)))))
(defun org-insert-link-headline (header)
  "Insert internal link to HEADER entry in current file."
  (interactive (list (completing-read "Link: " (org-get-headings) nil nil)))
  (org-insert-link nil header))
;; (define-key org-mode-map (kbd "C-c h") 'org-insert-link-headline)

(defun transform-comments (backend)
  (while (re-search-forward "[:blank:]*# " nil t)
    (replace-match "#+LATEX: % ")))
(add-hook 'org-export-before-parsing-hook #'transform-comments)

(setq org-startup-folded t)

(use-package org-cliplink
  :bind ("C-x p i" . org-cliplink))

(use-package ox-hugo
  :after org-mode
  :commands org-hugo-auto-export-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-F") 'org-shiftmetaright)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-B") 'org-shiftmetaleft)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-P") 'org-move-subtree-up)))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-N") 'org-move-subtree-down)))

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

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("ba" . "src bash"))
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

(setq org-confirm-babel-evaluate nil)

(use-package org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Notes"))
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
(setq org-agenda-span 20)
(setq org-agenda-start-on-weekday nil)

(setq org-agenda-files
      '("~/uni/uni.org"
        "~/uni/personal.org"))

(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c C-h") 'hs-hide-block)
(global-set-key (kbd "C-c C-s") 'hs-show-block)
(global-set-key (kbd "C-c C-t") 'hs-toggle-hiding)
(global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)
(global-set-key (kbd "C-c C-a") 'hs-show-all)
(global-set-key (kbd "C-c C-l") 'hs-hide-all)

(defun kill-line-backward ()
  "Kill line backwards from the position of the pointer to the beginning of the line."
  (interactive)
  (kill-line 0))
(global-set-key (kbd "C-S-k") 'kill-line-backward)

(use-package multiple-cursors
  :bind (("C-;" . mc/edit-lines)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :init
  (setq  mc/match-cursor-style nil))

;; use on normal systems
(use-package jinx
  :if (not (file-directory-p "~/.guix-profile/share/emacs/site-lisp")) ;only install on non guix system
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; use emacs-jinx package from guix if available
(use-package jinx
  :if (file-directory-p "~/.guix-profile/share/emacs/site-lisp") ;only install on guix system
  :ensure nil
  :load-path "~/.guix-profile/share/emacs/site-lisp/jinx-1.9/"
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;; (add-hook 'emacs-startup-hook #'global-jinx-mode)
;; (keymap-global-set "M-$" #'jinx-correct)
;; (keymap-global-set "C-M-$" #'jinx-languages)
;; (keymap-global-set "M-p" #'jinx-previous)
;; (keymap-global-set "M-n" #'jinx-next)

(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (org-mode . hl-todo-mode))
  :config
  ;; (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
  ;; (keymap-set hl-todo-mode-map "C-c n" #'hl-todo-next)
  ;; (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur)
  ;; (keymap-set hl-todo-mode-map "C-c i" #'hl-todo-insert)
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF"))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens-mode
  :ensure smartparens
  :hook (prog-mode text-mode markdown-mode)
  :config
  (require 'smartparens-config))

(electric-indent-mode t)
;; (electric-quote-mode t)
(setq minibuffer-default-prompt-format " [%s]") ; Emacs 29
(minibuffer-electric-default-mode 1)

(use-package magit
  :bind (("C-x g" . magit)
         ("C-x c" . magit-clone-shallow)))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package diff-hl
  :hook ((text-mode . diff-hl-mode)
         (org-mode . diff-hl-mode)
         (prog-mode . diff-hl-mode)
         ;; (dired-mode . diff-hl-dired-mode)
         ))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :config (setq markdown-command "multimarkdown"))

(use-package php-mode)

(use-package web-mode
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))

(add-to-list 'auto-mode-alist '("\\.c\\'" . c-ts-mode))
(add-hook 'c-ts-mode-hook (lambda () (c-ts-mode-set-global-style 'linux)
                            (when (eq c-ts-mode-indent-style 'linux)
                              (setq c-ts-mode-indent-offset 8)
                              (setq comment-style 'extra-line))))

;; (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
;; (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)

(require 'eglot)
(setq eglot-events-buffer-size 0) ;disable logging and improve perfomance
(define-key eglot-mode-map (kbd "C-c c r") 'eglot-rename)
(define-key eglot-mode-map (kbd "C-c c o") 'eglot-code-action-organize-imports)
(define-key eglot-mode-map (kbd "C-c c h") 'eldoc)
(define-key eglot-mode-map (kbd "C-c c a") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-c c f") 'eglot-format-buffer)
(define-key eglot-mode-map (kbd "C-c c q") 'eglot-code-action-quickfix)
(define-key eglot-mode-map (kbd "C-c c e") 'eglot-code-action-extract)
(define-key eglot-mode-map (kbd "<f6>") 'xref-find-definitions)
(define-key eglot-mode-map (kbd "M-.") 'xref-find-definitions)

(setq compilation-scroll-output 'first-error)

(use-package restclient
  :defer t)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(setq dired-listing-switches "-alh")

(setq dired-auto-revert-buffer t)

(setq dired-dwim-target t)

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds
        '("http://nullprogram.com/feed/"
          "https://mccd.space/feed.xml"
          "https://dthompson.us/feed.xml"
          "https://planet.emacslife.com/atom.xml"
          "https://archlinux.org/feeds/news/")))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))
;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;; :commands nov)

(global-set-key (kbd "M-RET") 'eshell)

(use-package pdf-tools
  :if (not (file-directory-p "~/.guix-profile/share/emacs/site-lisp")) ;only install on non guix system
  :mode ("\\.pdf\\'" . pdf-view-mode))
(use-package pdf-tools
  :if (file-directory-p "~/.guix-profile/share/emacs/site-lisp") ;only install on guix system
  :ensure nil
  :load-path "~/.guix-profile/share/emacs/site-lisp/pdf-tools-1.1.0"
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package notmuch
  :commands notmuch
  :bind (:map global-map ("C-c m" . notmuch)
              :map notmuch-hello-mode-map ("G" . mbsync)
              :map notmuch-search-mode-map ("G" . mbsync)))

(use-package mbsync
  :commands mbsync
  :config
  (add-hook 'mbsync-exit-hook 'notmuch-poll-and-refresh-this-buffer))
