;; #+property: header-args :tangle
;;   (concat (file-name-sans-extension (buffer-file-name)) ".el")
;;
(eval-when-compile
;; #+begin_src elisp :tangle
  (require 'use-package))
;; #+end_src;; Main typeface
(set-face-attribute 'default nil :family "Monaspace Neon")
;; #+begin_src elisp :tangle :noweb-ref Ensure installed
;; :ensure t ;; #+end_src
;;   ;; use nix provided org
;; #+begin_src elisp :tangle

(use-package org
  :ensure t
  :init
  (org-mode))
(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-mixed-fonts t)
  (load-theme 'modus-vivendi))

(tool-bar-mode -1)

(use-package bind-key
  :ensure t)

(require 'display-line-numbers)

(add-hook 'org-mode-hook
	  (lambda () (add-hook 'after-save-hook #'org-babel-tangle
	       		       :append :local)))
;; testing
(defun next-window-or-frame ()
  (interactive)
  (if current-prefix-arg
      (other-frame 1)
    (if (one-window-p)
	(other-frame 1)
      (other-window 1))))

(use-package magit
  :ensure t)

(global-set-key (kbd "C-c n")  'next-window-or-frame)

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package which-key
   :ensure t
   :config
   (which-key-mode))

(use-package corfu
  :ensure t
  :hook (lsp-completion-mode . my/corfu-setup-lsp) ; use corfu for lsp 
  :custom
  (corfu-cycle t)
  (corfu-preselect 'prompt)
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
	;; configure SPC for seprator insertion
	("SPC" . corfu-insert-separator))
  :init
  (global-corfu-mode)
  :config
    (defun my/corfu-setup-lsp ()
    "Use orderless completion style with lsp-capf intsead of the default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless)))

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

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-completion
  :ensure t
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  :init
  (vertico-mode))

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :custom
  (vertico-multiform-categories
   `((file reverse)
     (t reverse)
     ))
  :init
  (vertico-multiform-mode))

;; Example configuration for Consult
(use-package consult
  :ensure t
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro) ;; TODO: overrided by meow currentl
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
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-fd
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

(use-package orderless
  :ensure t
  :custom
  ;; Tune the global completion stle settings to your liking
  ;; This affects the minibuffer and non-lsp completion at point
  (completion-styles '(orderless partial-completion basic)
		     (completion-category-overrides nil)
		     (completion-category-defaults nil)))
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

(setq completion-cycle-threshold nil)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-current-absolute t)

(global-display-line-numbers-mode)
(setq tab-always-indent 'complete)
(icomplete-mode)
(setq inhibit-splash-screen t)
(transient-mark-mode 1)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
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
   '("f" . meow-find)
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
   '("t" . meow-till)
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
   :config
   (meow-setup)
   (meow-global-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-inline
  :ensure t
  :after flycheck
  :init (global-flycheck-inline-mode))

(use-package cpp
  :ensure nil
  :after lsp
  :hook (c++-.mode . lsp-deferred))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim)
  (python-mode . (lambda ()
		   (define-key python-mode-map (kbd "C-c f b") 'python-black-buffer)
		   (define-key python-mode-map (kbd "C-c f r") 'python-black-region))))

(setq lsp-keymap-prefix "C-c l")

(defun my/setup-lsp-mode ()
  (message "my/setup-lsp-mode called")
  (lsp-enable-which-key-integration)
  (lsp-diagnostics-mode 1)
  (lsp-completion-mode 1)
  ;; (when (lsp-feature? "textDocument/formatting")
  ;;  (setq my/format/buffer-function 'lsp-format-buffer))
  )
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init
  :custom
  (lsp-diagnostics-provider :flycheck)
  (lsp-enable-indentation t)
  (lsp-signature-auto-activate t)
  (lsp-enable-snippet t)
  (lsp-enable-xref t)
  (lsp-log-io t)
  (lsp-enable-imenu t)
  (read-process-output-max (* 1024 1024)) ;; 1mb
  (gc-cons-threshold (* 10 1024 1024))
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
)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")
;; (use-package tree-sitter
;;   :ensure t
;;   :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode)
;;   :config
;;   (global-tree-sitter-mode))
;; (use-package tree-sitter-langs
;;   :ensure t)
