;;; config.el --- TTY configuration layer configuration file

;; Terminal-specific window title updates
;; Keep window title up-to-date (should fail gracefully in non-xterm terminals)
(defun tty-config/update-window-title ()
  "Update window title in terminal."
  (unless (display-graphic-p)
    (setq frame-title-format
          (if (bound-and-true-p projectile-project-name)
              (format "%s - %s" (projectile-project-name) (buffer-name))
            (format "%s - %s" (system-name) (buffer-name))))))

(add-hook 'buffer-list-update-hook #'tty-config/update-window-title)


;; Terminal-specific cursor configuration
;; Fix cursor shape-changing in terminal for Evil mode
;; Supported in: XTerm, Gnome Terminal, iTerm, Konsole, mintty, Apple Terminal.app
;; For Apple Terminal.app, install: https://github.com/selwyn/evil-terminal-cursor-changer

;; Note: evil-terminal-cursor-changer is initialized in packages.el

;; Kitty keyboard protocol support
;; Note: kkp is initialized in packages.el

;; ;; Zellij terminal multiplexer key remapping
;; ;; Zellij doesn't support some shifted keys properly, so we remap them
;; (defun tty-config/remap-zellij-shifted-keys ()
;;   "Remap some shifted keys in Zellij terminal multiplexer."
;;   (when (and (not (display-graphic-p))
;;              (getenv "ZELLIJ"))
;;     ;; Remap common shifted keys that Zellij doesn't handle well
;;     (define-key input-decode-map (kbd "S-<return>") (kbd "C-j"))
;;     (define-key input-decode-map (kbd "S-<tab>") (kbd "C-i"))))


;; Keep window title up-to-date. Should fail gracefully in non-xterm terminals.
;; Only works in Emacs 27+.
(setq xterm-set-window-title t)

;; Some terminals offer two different cursors: a "visible" static cursor and a
;; "very visible" blinking one. By default, Emacs uses the very visible cursor
;; and will switch back to it when Emacs is started or resumed. A nil
;; `visible-cursor' prevents this.
(setq visible-cursor nil)

;; ============================================================
;; Region Selection Configuration
;; ============================================================

;; Enable transient mark mode for better region selection
;; This makes the region visible and allows Shift+Arrow selection
(setq transient-mark-mode t)

;; ;; Enable mouse selection mode for terminal
;; (setq mouse-sel-mode t)

;; ;; Enable mouse yank at point (paste at cursor position)
;; (setq mouse-yank-at-point t)

;; Enable the mouse in terminal Emacs
(when (not (display-graphic-p))
  (add-hook 'tty-setup-hook #'xterm-mouse-mode))

;; ;; Mouse keybindings for region selection
;; ;; Mouse button 1 (left click) sets point
;; (define-key global-map [mouse-1] #'mouse-set-point)
;; ;; Mouse button 1 drag selects region
;; (define-key global-map [drag-mouse-1] #'mouse-set-region)
;; ;; Mouse button 3 (right click) extends selection
;; (define-key global-map [mouse-3] #'mouse-save-then-kill)

;; ;; Mouse wheel scrolling
;; (global-set-key (kbd "<mouse-4>") #'scroll-down-line)
;; (global-set-key (kbd "<mouse-5>") #'scroll-up-line)

;; ;; Enable Shift+Arrow key selection (standard Emacs behavior)
;; ;; These are already enabled by default, but we ensure they work
;; (global-set-key (kbd "S-<left>") #'backward-char)
;; (global-set-key (kbd "S-<right>") #'forward-char)
;; (global-set-key (kbd "S-<up>") #'previous-line)
;; (global-set-key (kbd "S-<down>") #'next-line)

;; ;; Mouse support in Evil states
;; (with-eval-after-load 'evil
;;   (unless (display-graphic-p)
;;     ;; Mouse click sets point in normal and insert states
;;     (define-key evil-normal-state-map [mouse-1] #'mouse-set-point)
;;     (define-key evil-insert-state-map [mouse-1] #'mouse-set-point)
;;     (define-key evil-visual-state-map [mouse-1] #'mouse-set-point)
;;     ;; Mouse drag selects region
;;     (define-key evil-normal-state-map [drag-mouse-1] #'mouse-set-region)
;;     (define-key evil-insert-state-map [drag-mouse-1] #'mouse-set-region)
;;     ;; Right click extends/kills selection
;;     (define-key evil-normal-state-map [mouse-3] #'mouse-save-then-kill)
;;     (define-key evil-visual-state-map [mouse-3] #'mouse-save-then-kill)))

;; ;; Shift+Arrow keys work in Evil insert mode for region selection
;; ;; In normal mode, use visual mode (v key) or Shift+Arrow will work too
;; (with-eval-after-load 'evil
;;   (define-key evil-insert-state-map (kbd "S-<left>") #'backward-char)
;;   (define-key evil-insert-state-map (kbd "S-<right>") #'forward-char)
;;   (define-key evil-insert-state-map (kbd "S-<up>") #'previous-line)
;;   (define-key evil-insert-state-map (kbd "S-<down>") #'next-line))

;; ;; Enable visual feedback for region selection
;; (setq highlight-nonselected-windows t
;;       mark-even-if-inactive t)

;; Support for child frames in terminal frames was added in 31. Enable it, if it
;; is available.
(when (and (featurep 'tty-child-frames)
           (not (display-graphic-p)))
  (add-hook 'tty-setup-hook #'tty-tip-mode))

;; Windows terminals don't support what I'm about to do, but best not to wrap
;; this in an OS check, in case you're using WSL or Cygwin, which *might*
;; support it.
(when (not (display-graphic-p))
  (add-hook 'tty-setup-hook
    (defun chris-init-clipboard-in-tty-emacs-h ()
          (and (require 'clipetty nil t)
              (global-clipetty-mode +1)))))

(use-package emacs
  :config
  (when (not (display-graphic-p))
    (tty-config/remap-zellij-shifted-keys)))

(when (not (display-graphic-p))
  (add-hook 'tty-setup-hook #'tty-config/remap-zellij-shifted-keys))

