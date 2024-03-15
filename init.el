(eval-when-compile
  (require 'use-package))

(require 'cl-lib)

(use-package bind-key
  :ensure t)

(use-package blackout
  :ensure t)

;; TODO: monaspace neon is missing the o symbol, so when we get fallback font in the
;; minibuffer it is slightly the wrong size and causes prompt to move
(set-face-attribute 'default nil :family "Monaspace Neon" :height 100 :weight 'normal)

(defun set-bigger-spacing ()
  (setq-local default-text-properties '(line-spacing 0.25)))
(dolist (hook '(text-mode-hook prog-mode-hook)) (add-hook hook 'set-bigger-spacing))

;; UI
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; Line numbers
(require 'display-line-numbers)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)
(global-display-line-numbers-mode)

;; fill column

(dolist (hook '(prog-mode-hook
                text-mode-hook))
  (add-hook hook #'display-fill-column-indicator-mode t))

;; Icons

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :after (all-the-icons)
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; Whitespace
(dolist (hook '(prog-mode-hook
                text-mode-hook))
  (add-hook hook (lambda () (setq-local show-trailing-whitespace t))))

(use-package whitespace-cleanup-mode
  :ensure t
  :init
  (global-whitespace-cleanup-mode))

;; Backups

;; TODO: backup on save versioned
(setq backup-directory-alist '(("." . "~/.emacs.d/backup/per-save")))

;; TODO: autosave

;; Repeat mode

(repeat-mode t)

;; Tabs
(setq-default indent-tabs-mode nil)

;; themes
(use-package modus-themes
  :ensure t
  :after hl-todo
  :config
  (defun modus-theme-hl-undone ()
    (modus-themes-with-colors
      (add-to-list 'hl-todo-keyword-faces (cons "UNDONE" err))))
  (add-hook 'modus-themes-after-load-theme-hook #'modus-theme-hl-undone)
  ;; (setq modus-themes-mixed-fonts t)
  (modus-themes-load-theme 'modus-operandi))

;; Todo highlighting
(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))

;; Scrolling
(setq scroll-margin 10
      scroll-conservatively 10
      ;; aggressively doesn't get set in any buffers anyway :(
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; recent file list
(recentf-mode 1)

;; Persist history
(use-package savehist
  :init
  (savehist-mode))

;; Transient mark mode
;; https://emacsdocs.org/docs/emacs/Mark
(transient-mark-mode 1)

;; spell checking
(use-package jinx
  :ensure t
  :hook ((prog-mode . jinx-mode)
         (text-mode . jinx-mode)
         (conf-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  )

;; TODO: org & switch init.el to org file
(use-package org
  :ensure t
  :init
  (org-mode))

;; nested markup
;; https://list.orgmode.org/87zgrq5wi8.fsf@localhost/t/#mf75fdf28957e4c7df397bf7911099082a9d7eafd

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'org-babel-tangle
                      :append :local)))

;; Bash aliases from
;; https://emacs.stackexchange.com/questions/74385/is-there-any-way-of-making-eshell-aliases-using-bash-and-zsh-aliases-syntax
(defun eshell-load-bash-aliases ()
  "Read Bash aliases and add them to the list of eshell aliases."
  ;; Bash needs to be run - temporarily - interactively
  ;; in order to get the list of aliases.
  (with-temp-buffer
    (call-process "bash" nil '(t nil) nil "-ci" "alias")
    (goto-char (point-min))
    (cl-letf (((symbol-function 'eshell-write-aliases-list) #'ignore))
      (while (re-search-forward "alias \\(.+\\)='\\(.+\\)'$" nil t)
        (eshell/alias (match-string 1) (format "%s $*" (match-string 2)))))
    (eshell-write-aliases-list)))

;; We only want Bash aliases to be loaded when Eshell loads its own aliases,
;; rather than every time `eshell-mode' is enabled.
(add-hook 'eshell-alias-load-hook 'eshell-load-bash-aliases)

;; Windows

;; undo+redo window changes
(use-package winner
  :init
  (winner-mode t))

;; avy style winodw navigation + editing
(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; Undo

(use-package vundo
  :ensure t)

;; Key help

(use-package which-key
   :ensure t
   :config
   (which-key-mode)
   :blackout)

;; Git

(use-package git-timemachine
  :ensure t)

;; Forges

;; (use-package consult-gh
;;   ;; Installed via flake
;;   :after consult

;; (use-package forge
;;  :after magit)

(use-package browse-at-remote
  :ensure t)

;; Magit
(use-package magit-delta
  :ensure t
  :hook (magit-mode . magit-delta-mode))

(use-package magit
  :ensure t)

;; diff highlighting

;; TODO: don't use margin mode, instead hack fringe ala doom
;; https://github.com/doomemacs/doomemacs/blob/98d753e1036f76551ccaa61f5c810782cda3b48a/modules/ui/vc-gutter/config.el#L34
(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode)
  ;; (diff-hl-dired-mode)
  (diff-hl-margin-mode))

;; Operate on grep buffer

(use-package wgrep
  :ensure t)

;; Dired

(use-package wdired)

(use-package diredfl
  :ensure t
  :init
  (diredfl-global-mode))

;; Keymap Actions

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good alternative: M-.
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary for hook below
  :hook
  ;; if you want to have consult previews as you move around
  ;; an auto-updating embark collect buffer
  (embark-collect-mode . consult-preview-at-point-mode))


;; Completion style

(use-package orderless
  :ensure t
  :custom
  ;; Tune the global completion stle settings to your liking
  ;; This affects the minibuffer and non-lsp completion at point
  (completion-styles '(orderless partial-completion basic)
                     (completion-category-overrides nil)
                     (completion-category-defaults nil)))

;; Snippets

(use-package tempel
  :ensure t
  :hook
  (conf-mode . tempel-setup-capf)
  (prog-mode . tempel-setup-capf)
  (text-mode . tempel-setup-capf)
  :init
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions`
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions))))

(use-package lsp-snippet-tempel
  ;; Installed through nix flake as git repo
  :after lsp-mode
  :config
  (when (featurep 'lsp-mode)
    (lsp-snippet-tempel-lsp-mode-init))
  (when (featurep 'eglot)
    (lsp-snippet-tempel-eglot-init))
  )

;; Completion at point functions + capf UI

;; https://kristofferbalintona.me/posts/202203130102/
(use-package cape
  :init
  :custom
  :bind ("C-c p p" . completion-at-point) ;; capf
  ("C-c p t" . complete-tag)		  ;; etags
  ("C-c p d" . cape-dabbrev)		  ;; or dabbrev-completion
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
  ("C-c p r" . cape-rfc1345)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-file)
  :ensure t
  :config
  ;; https://old.reddit.com/r/emacs/comments/19b8a83/capefile_fails_when_called_as_a_capf_but_works/
  (setq cape-file-directory-must-exit nil))


;; stuff for completion in region + corfu
(setq tab-always-indent 'complete)
(setq completion-cycle-threshold nil)

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-preselect 'first)
  (corfu-quit-no-match 'separator)
  (corfu-auto nil)
  (corfu-min-width 60)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-echo-mode nil) ; Using corfu-popupinfo
  (lsp-completion-provider :none) ; use corfu intsead for lsp completion
  :bind
  (:map corfu-map ;; use TAB for cycling, default is `corfu-complete`
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous)
        ;; configure M-SPC for seprator insertion
        ("M-SPC" . corfu-insert-separator)
        ("S-<return>" . corfu-insert)
        ("RET" . nil) ;; leave enter alone
        )

  :init
  (global-corfu-mode)
  :config
    (defun my/corfu-setup-lsp ()
      "Use orderless completion style with lsp-capf intsead of the default lsp-passthrough."
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
    ;; https://old.reddit.com/r/emacs/comments/u8szz6/help_me_get_c_tab_completion_working/
    (with-eval-after-load 'cc-mode
      (defun c-indent-then-complete ()
        (interactive)
        (if (= 0 (c-indent-line-or-region))
            (completion-at-point)))
      (when (equal tab-always-indent 'complete)
        (dolist (map (list c-mode-map c++-mode-map))
          (define-key map (kbd "<tab>") #'c-indent-then-complete))))
    :hook (lsp-completion-mode . my/corfu-setup-lsp) ;; use corfu for lsp
    )

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :custom
  (corfu-popupinfo-delay '(0.2 . 1.0))
  :init
  (corfu-popupinfo-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  ;;(svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache"))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'my/themes-hooks #'(lambda () (interactive) (kind-icon-reset-cache))))

;; minibuffer

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-reverse-mode t)
  (vertico-resize t)
  :init
  (vertico-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c K" . consult-kmacro) ;; C-c k is used by meow
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flycheck)               ;; Alternative: consult-flymake
         ("M-g F" . consult-lsp-diagnostics)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g s" . consult-lsp-file-symbols)
         ("M-g S" . consult-lsp-symbols)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)                  ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element
  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config
  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"
  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)
  )

(use-package consult-todo
  :ensure t
  :after (consult hl-todo))

(use-package consult-lsp
  :ensure t
  :after consult)

(use-package consult-flycheck
  :ensure t
  :after (consult flycheck))

;; Text Motions

(use-package avy
  :ensure t
  :config
  (avy-setup-default))

;; In order for the bindings in this prefix to remain active until you
;; press ESC (or some other key not bound under the prefix), you must
;; have `repeat-mode' enabled.
;; https://github.com/meow-edit/meow/discussions/368#discussioncomment-4219587
(defvar my-view-prefix)
(define-prefix-command 'my-view-prefix)
(define-key mode-specific-map (kbd "v") 'my-view-prefix)
(defvar my-view-rep-map (make-sparse-keymap))
(dolist (kb '(("@"    . View-back-to-mark)
              ("%"    . View-goto-percent)
              ("G"    . View-goto-line-last)
              ("g"    . View-goto-line)
              ("F"    . View-revert-buffer-scroll-page-forward)
              ("k"    . View-scroll-line-backward)
              ("j"    . View-scroll-line-forward)
              ("u"    . View-scroll-half-page-backward)
              ("d"    . View-scroll-half-page-forward)
              ("z"    . View-scroll-page-forward-set-page-size)
              ("w"    . View-scroll-page-backward-set-page-size)
              ("b"    . View-scroll-page-backward)
              ("f"    . View-scroll-page-forward)
              ("o"    . View-scroll-to-buffer-end)))
  (define-key my-view-prefix (kbd (car kb)) (cdr kb))
  (define-key my-view-rep-map (kbd (car kb)) (cdr kb))
  (put (cdr kb) 'repeat-map my-view-rep-map)
  (autoload (cdr kb) "view" nil 'interactive))

(defvar meow--last-avy-char)

(defun meow-avy-goto-char (char &optional arg expand)
  "Goto using avy"
  (interactive (list (read-char "goto: " t)
                     current-prefix-arg))
  (let* ((beg (point))
         (end (save-mark-and-excursion
                (avy-goto-char char arg)
                (point))))
    (thread-first
      (meow--make-selection '(select . avy)
                            beg end expand)
      (meow--select)))
    (setq meow--last-avy-char char))

(defun meow-avy-goto-char-expand (char &optional arg)
  "Goto using avy expand"
  (interactive (list (read-char "Expand goto: " t)
                     current-prefix-arg))
  (meow-avy-goto-char char arg t))

(defun meow--add-beacons-for-avy ()
  "Add beacon for avy movement"
  (let ((ch-str (if (eq meow--last-avy-char 13)
                    "\n"
                  (char-to-string meow--last-avy-char))))
    (save-restriction
      (meow--narrow-secondary-selection)
      (let ((orig (point))
            (case-fold-search t))
        (save-mark-and-excursion
          (goto-char (point-max))
          (while (search-backward ch-str nil t)
            (unless (= (point) orig)
              (meow--beacon-add-overlay-at-point (point)))))))
    (meow--beacon-shrink-selection)))

(defun meow--beacon-update-overlays-custom ()
    (when (meow--beacon-inside-secondary-selection)
      (let* ((ex (car (meow--selection-type)))
             (type (cdr (meow--selection-type))))
        (cl-case type
          ((avy) (meow--add-beacons-for-avy))))))

(advice-add 'meow--beacon-update-overlays :after #'meow--beacon-update-overlays-custom)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (setq meow-goto-line-function 'consult-goto-line)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-avy-goto-char)
   '("F" . meow-avy-goto-char-expand)
   '("t" . meow-till)
   '("T" . meow-till-expand)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :after avy
  :config
  (meow-setup)
  (meow-global-mode 1))


;; Diagnostics

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; (use-package flycheck-inline
;;   :ensure t
;;   :after flycheck
;;   :init (global-flycheck-inline-mode))


;; LSP

(setq lsp-keymap-prefix "C-c l")

(defun my/setup-lsp-mode ()
  (message "my/setup-lsp-mode called")
  (lsp-enable-which-key-integration)
  (lsp-diagnostics-mode t)
  ;; (when (lsp-feature? "textDocument/formatting")
  ;;  (setq my/format/buffer-function 'lsp-format-buffer))
  )

(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  :custom
  (lsp-enable-symbol-highlighting t)
  (lsp-modeline-code-actions-enable t)
  (lsp-signature-auto-activate  t)
  (lsp-signature-render-documentation t)
  (lsp-diagnostics-provider :flycheck)
  (lsp-enable-indentation nil) ;; disabled indentation
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-enable-imenu t)
  (lsp-inlay-hint-enable t)
  (lsp-enable-links t)
  (lsp-lens-enable t)
  (lsp-semantic-tokens-enable t)
  (lsp-enable-semantic-highlighting t)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (gc-cons-threshold (* 10 1024 1024))
  (lsp-log-io nil)
  :hook
  (lsp-mode . my/setup-lsp-mode)
  :config
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :ensure t
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-code-actions nil))

;; Python
(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package python-pytest
  :ensure t)

;; (use-package python-black
;;   :ensure t
;;   :after python
;;   :hook (python-mode . python-black-on-save-mode-enable-dwim)
;;   (python-mode . (lambda ()
;;                 (define-key python-mode-map (kbd "C-c f b") 'python-black-buffer)
;;                 (define-key python-mode-map (kbd "C-c f r") 'python-black-region))))

;; Nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook (nix-mode . (lambda () (lsp-deferred)))

;; C/C++
(use-package c
  :ensure nil
  :hook (c-mode . lsp-deferred))

(use-package cpp
  :ensure nil
  :hook (c++-mode . (lambda ()
                      (lsp-deferred))))

(use-package modern-cpp-font-lock
 :ensure t
 :hook (c++-mode . modern-c++-font-lock-mode))

;; Go

;; Move to aphaelia

(use-package go-mode
  :ensure t
  :hook (go-mode . (lambda ()
                     (lsp-deferred))))

;; Rust
(use-package rustic
  :ensure t
  :custom
  (rustic-lsp-client 'lsp-mode))

(use-package sh-script
  :hook (sh-mode . (lambda () (lsp-deferred))))

;; TODO: treesitter

;; (use-package tree-sitter
;;   :ensure t
;;   :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
;;   :config
;;   (global-tree-sitter-mode))
;; (use-package tree-sitter-langs
;;   :ensure t)

;; Misc. editor

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode t)
  :blackout)

;; Formatting

;; Hook LSP and custom functions into apheleia formatting
;; https://github.com/radian-software/apheleia/issues/153
;;
;; (defun my/lsp-go-save-hooks ()
;;  (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package apheleia
  :ensure t
  :hook ((go-mode . apheleia-mode)
         (python-mode . apheleia-mode))
  :blackout " Fmt")

;; singlestore
(setq-default compile-command "memsql-please make debug --skip-binplace memsql-server") ; set default command for M-x compile
(setq-default gdb-create-source-file-list nil)  ; gdb initialization takes a long time without this
(setq-default word-wrap t)                      ; wrap long lines at word boundaries for better readability

;; Adjust C++ style to more closely match the style we use in the MemSQL codebase
(c-add-style "memsql"
         '("linux"
           (c-basic-offset . 4)
               (c-offsets-alist
                (inline-open . 0)
                (innamespace . 0)       ; don't indent inside namespaces
                )
           ))
(add-to-list 'c-default-style '(c++-mode . "memsql"))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ; use c++-mode instead of c-mode for .h files

;; Default settings for sql-mysql
;; You can run a mysql/memsql client in Emacs with M-x sql-mysql

(setq sql-user "root")
(setq sql-password "")
(setq sql-server "127.0.0.1")
