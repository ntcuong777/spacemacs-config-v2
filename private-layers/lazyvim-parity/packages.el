;;; layers/lazyvim-parity/packages.el -*- lexical-binding: t; -*-

;; A private Spacemacs layer to minimize keybinding context switching between
;; LazyVim (leader: <space>, localleader: ',') and Spacemacs (leader: SPC,
;; major-mode leader: ',').
;;
;; Goals
;; - Provide consistent COMMON menus under SPC:
;;   f files/find, g git, b buffers, w windows, p projects, s search/symbols,
;;   c code(LSP/Eglot), x diagnostics/errors, t toggles.
;; - Provide a consistent “localleader” (',') menu for common per-language
;;   code operations (rename, actions, format, docs, def/refs) so it matches
;;   your LazyVim localleader muscle memory.
;; - Stay compatible with Helm (your completion backend).
;; - Work whether you use lsp-mode or eglot (wrappers pick what exists).
;;
;; Install
;; 1) Ensure this layer is discoverable:
;;    You already have: dotspacemacs-configuration-layer-path
;;      '("~/.config/spacemacs/private-layers/")
;;
;; 2) Create:
;;    ~/.config/spacemacs/private-layers/lazyvim-parity/layers/lazyvim-parity/
;;
;; 3) Put this file at:
;;    ~/.config/spacemacs/private-layers/lazyvim-parity/layers/lazyvim-parity/packages.el
;;
;; 4) Enable the layer in dotspacemacs/layers:
;;    (lazyvim-parity :variables lazyvim-parity-enable-which-key-labels t)
;;
;; 5) Reload Spacemacs: SPC f e R
;;
;; Notes
;; - This layer installs no packages. It expects you already have projectile,
;;   magit, helm, and optionally lsp/eglot + flycheck (your config does).
;; - The bindings call Spacemacs “smart” functions when possible so they work
;;   with Helm and your existing layers.

(defconst lazyvim-parity-packages '()
  "No packages are installed by this layer.")

(defgroup lazyvim-parity nil
  "LazyVim parity keybindings for Spacemacs."
  :group 'spacemacs)

(defcustom lazyvim-parity-enable-which-key-labels t
  "If non-nil, declare which-key labels for key prefixes."
  :type 'boolean
  :group 'lazyvim-parity)

(defun lazyvim-parity//wk (key label)
  (when (and lazyvim-parity-enable-which-key-labels
             (fboundp 'spacemacs/declare-prefix))
    (spacemacs/declare-prefix key label)))

(defun lazyvim-parity//have (sym)
  (and (fboundp sym) sym))

(defun lazyvim-parity//call (sym &rest args)
  (when (lazyvim-parity//have sym)
    (apply sym args)
    t))

;; -------------------- Backend-agnostic “code” helpers --------------------

(defun lazyvim-parity/code-rename ()
  "Rename symbol at point (eglot or lsp)."
  (interactive)
  (cond
   ((lazyvim-parity//have 'eglot-rename) (call-interactively 'eglot-rename))
   ((lazyvim-parity//have 'lsp-rename) (call-interactively 'lsp-rename))
   (t (user-error "No rename backend found (need eglot or lsp-mode)"))))

(defun lazyvim-parity/code-action ()
  "Code actions (eglot or lsp)."
  (interactive)
  (cond
   ;; eglot >= 29 has eglot-code-actions
   ((lazyvim-parity//have 'eglot-code-actions) (call-interactively 'eglot-code-actions))
   ((lazyvim-parity//have 'lsp-execute-code-action) (call-interactively 'lsp-execute-code-action))
   (t (user-error "No code action backend found"))))

(defun lazyvim-parity/code-format ()
  "Format region if active, otherwise format buffer."
  (interactive)
  (if (use-region-p)
      (lazyvim-parity/code-format-region)
    (lazyvim-parity/code-format-buffer)))

(defun lazyvim-parity/code-format-region ()
  "Format the active region."
  (interactive)
  (cond
   ((lazyvim-parity//have 'eglot-format) (eglot-format (region-beginning) (region-end)))
   ((lazyvim-parity//have 'lsp-format-region) (call-interactively 'lsp-format-region))
   ((lazyvim-parity//have 'format-all-region) (call-interactively 'format-all-region))
   (t (user-error "No formatter found (eglot/lsp/format-all)"))))

(defun lazyvim-parity/code-format-buffer ()
  "Format current buffer."
  (interactive)
  (cond
   ((lazyvim-parity//have 'eglot-format-buffer) (call-interactively 'eglot-format-buffer))
   ((lazyvim-parity//have 'lsp-format-buffer) (call-interactively 'lsp-format-buffer))
   ((lazyvim-parity//have 'format-all-buffer) (call-interactively 'format-all-buffer))
   (t (user-error "No formatter found (eglot/lsp/format-all)"))))

(defun lazyvim-parity/code-hover ()
  "Show documentation/hover at point."
  (interactive)
  (cond
   ((lazyvim-parity//have 'eglot-help-at-point) (call-interactively 'eglot-help-at-point))
   ((lazyvim-parity//have 'lsp-describe-thing-at-point) (call-interactively 'lsp-describe-thing-at-point))
   (t (call-interactively 'eldoc))))

(defun lazyvim-parity/code-definitions ()
  "Go to definition (xref)."
  (interactive)
  (call-interactively 'xref-find-definitions))

(defun lazyvim-parity/code-references ()
  "Find references (xref)."
  (interactive)
  (call-interactively 'xref-find-references))

(defun lazyvim-parity/code-implementations ()
  "Find implementations (lsp/eglot if available; else xref)."
  (interactive)
  (cond
   ((lazyvim-parity//have 'lsp-find-implementation) (call-interactively 'lsp-find-implementation))
   ;; eglot has xref integration; implementations may not be distinct
   (t (call-interactively 'xref-find-definitions))))

(defun lazyvim-parity/code-type-definition ()
  "Go to type definition (lsp if available)."
  (interactive)
  (cond
   ((lazyvim-parity//have 'lsp-find-type-definition) (call-interactively 'lsp-find-type-definition))
   (t (user-error "Type definition requires lsp-mode"))))

(defun lazyvim-parity/code-symbols-buffer ()
  "Jump to symbol in current buffer.
Prefers Helm if present."
  (interactive)
  (cond
   ((lazyvim-parity//have 'helm-imenu) (call-interactively 'helm-imenu))
   (t (call-interactively 'imenu))))

(defun lazyvim-parity/code-symbols-project ()
  "Project symbols.
In Spacemacs+Helm, try helm-lsp-workspace-symbol when available."
  (interactive)
  (cond
   ((lazyvim-parity//have 'helm-lsp-workspace-symbol) (call-interactively 'helm-lsp-workspace-symbol))
   ((lazyvim-parity//have 'lsp-workspace-symbol) (call-interactively 'lsp-workspace-symbol))
   (t (user-error "Project symbols require lsp-mode"))))

;; -------------------- Diagnostics helpers --------------------

(defun lazyvim-parity/diag-list ()
  "Show diagnostics list."
  (interactive)
  (cond
   ((lazyvim-parity//have 'flycheck-list-errors) (call-interactively 'flycheck-list-errors))
   ((lazyvim-parity//have 'lsp-treemacs-errors-list) (call-interactively 'lsp-treemacs-errors-list))
   (t (user-error "No diagnostics UI (flycheck or lsp-treemacs)"))))

(defun lazyvim-parity/diag-next ()
  "Next diagnostic."
  (interactive)
  (cond
   ((lazyvim-parity//have 'flycheck-next-error) (call-interactively 'flycheck-next-error))
   ((lazyvim-parity//have 'lsp-next-diagnostic) (call-interactively 'lsp-next-diagnostic))
   (t (call-interactively 'next-error))))

(defun lazyvim-parity/diag-prev ()
  "Previous diagnostic."
  (interactive)
  (cond
   ((lazyvim-parity//have 'flycheck-previous-error) (call-interactively 'flycheck-previous-error))
   ((lazyvim-parity//have 'lsp-previous-diagnostic) (call-interactively 'lsp-previous-diagnostic))
   (t (call-interactively 'previous-error))))

;; -------------------- Keybinding setup --------------------

(defun lazyvim-parity/init-lazyvim-parity ()
  "Initialize the layer."
  (lazyvim-parity//bind-leader)
  (lazyvim-parity//bind-major-mode-leader))

(defun lazyvim-parity//bind-leader ()
  "Bind common menus under SPC to match LazyVim muscle memory."
  ;; Prefix labels (for which-key)
  (lazyvim-parity//wk "f" "files/find")
  (lazyvim-parity//wk "g" "git")
  (lazyvim-parity//wk "b" "buffers")
  (lazyvim-parity//wk "w" "windows")
  (lazyvim-parity//wk "p" "project")
  (lazyvim-parity//wk "s" "search/symbols")
  (lazyvim-parity//wk "c" "code")
  (lazyvim-parity//wk "x" "diagnostics")
  (lazyvim-parity//wk "t" "toggles")

  ;; Files / Find (SPC f …)
  ;; Use Spacemacs smart wrappers so Helm is respected.
  (spacemacs/set-leader-keys
    "ff" #'spacemacs/find-files            ; fuzzy find files (helm/whatever)
    "fF" #'spacemacs/find-file             ; literal find file
    "fr" #'spacemacs/recentf               ; recent files
    "fp" #'projectile-find-file            ; files in project
    "fP" #'projectile-switch-project       ; switch project
    "fg" #'spacemacs/search-project-auto   ; ripgrep/ag depending on config
    "fG" #'spacemacs/grep
    "fb" #'spacemacs/switch-to-buffer
    "fs" #'save-buffer
    "fS" #'write-file)

  ;; Git (SPC g …)
  (spacemacs/set-leader-keys
    "gg" #'magit-status
    "gs" #'magit-status
    "gB" #'magit-blame-addition
    "gb" #'magit-branch
    "gc" #'magit-commit
    "gl" #'magit-log
    "gd" #'magit-diff
    "gD" #'magit-dispatch)

  ;; Buffers (SPC b …)
  (spacemacs/set-leader-keys
    "bb" #'spacemacs/switch-to-buffer
    "bB" #'spacemacs/switch-to-buffer-other-frame
    "bd" #'spacemacs/kill-this-buffer
    "bD" #'spacemacs/kill-buffer-and-window
    "bo" #'spacemacs/kill-other-buffers
    "bn" #'next-buffer
    "bp" #'previous-buffer
    "br" #'revert-buffer
    "bs" #'spacemacs/save-buffers-kill-terminal)

  ;; Windows (SPC w …)
  (spacemacs/set-leader-keys
    "wv" #'split-window-right
    "ws" #'split-window-below
    "wd" #'delete-window
    "wq" #'delete-window
    "wo" #'delete-other-windows
    "ww" #'other-window
    "wW" #'spacemacs/window-transient-state
    "w=" #'balance-windows)

  ;; Projects (SPC p …)
  (spacemacs/set-leader-keys
    "pp" #'projectile-switch-project
    "pf" #'projectile-find-file
    "pF" #'projectile-find-file-in-known-projects
    "pb" #'projectile-switch-to-buffer
    "ps" #'projectile-save-project-buffers
    "pk" #'projectile-kill-buffers
    "pg" #'projectile-grep)

  ;; Search / symbols (SPC s …)
  (spacemacs/set-leader-keys
    "ss" #'spacemacs/swiper-or-helm-swoop
    "sp" #'spacemacs/search-project-auto
    "si" #'lazyvim-parity/code-symbols-buffer
    "sI" #'lazyvim-parity/code-symbols-project
    "sd" #'lazyvim-parity/code-definitions
    "sr" #'lazyvim-parity/code-references)

  ;; Code (SPC c …)
  ;; Mirroring LazyVim “code” actions in one predictable place.
  (spacemacs/set-leader-keys
    "ca" #'lazyvim-parity/code-action
    "cr" #'lazyvim-parity/code-rename
    "cf" #'lazyvim-parity/code-format
    "cF" #'lazyvim-parity/code-format-buffer
    "ch" #'lazyvim-parity/code-hover
    "cd" #'lazyvim-parity/code-definitions
    "cR" #'lazyvim-parity/code-references
    "ci" #'lazyvim-parity/code-implementations
    "ct" #'lazyvim-parity/code-type-definition
    "cs" #'lazyvim-parity/code-symbols-buffer
    "cS" #'lazyvim-parity/code-symbols-project)

  ;; Diagnostics (SPC x …)
  (spacemacs/set-leader-keys
    "xx" #'lazyvim-parity/diag-list
    "xl" #'lazyvim-parity/diag-list
    "xn" #'lazyvim-parity/diag-next
    "xp" #'lazyvim-parity/diag-prev)

  ;; Toggles (SPC t …)
  (spacemacs/set-leader-keys
    "tn" #'display-line-numbers-mode
    "tw" #'whitespace-mode
    "ts" #'flyspell-mode
    "tf" #'auto-fill-mode
    "tt" #'spacemacs/toggle-truncate-lines
    "th" #'hl-line-mode))

(defun lazyvim-parity//bind-major-mode-leader ()
  "Bind common localleader (',') actions in every major mode.

Spacemacs uses `,' as a major-mode leader by default, which matches your
LazyVim localleader choice.

We only bind *safe/common* actions here so we don't fight language layers.
They’ll appear in `,` menus across modes.
"
  (when (fboundp 'spacemacs/set-leader-keys-for-major-mode)
    ;; Use `nil` as the “major mode” target to define a global major-mode leader
    ;; map. This is supported in modern Spacemacs.
    (spacemacs/set-leader-keys-for-major-mode 'nil
      "a"  #'lazyvim-parity/code-action
      "r"  #'lazyvim-parity/code-rename
      "f"  #'lazyvim-parity/code-format
      "F"  #'lazyvim-parity/code-format-buffer
      "h"  #'lazyvim-parity/code-hover
      "d"  #'lazyvim-parity/code-definitions
      "R"  #'lazyvim-parity/code-references
      "i"  #'lazyvim-parity/code-implementations
      "t"  #'lazyvim-parity/code-type-definition
      "s"  #'lazyvim-parity/code-symbols-buffer
      "S"  #'lazyvim-parity/code-symbols-project)

    ;; which-key group label for major-mode leader
    (when (and lazyvim-parity-enable-which-key-labels
               (fboundp 'spacemacs/declare-prefix-for-mode))
      ;; Applies to most modes that support which-key prefixes.
      (spacemacs/declare-prefix-for-mode 'prog-mode "," "local/code"))))

;;; End of layer
