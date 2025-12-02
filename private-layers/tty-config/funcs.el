;;; funcs.el --- TTY configuration layer functions file

(defun tty-config/update-window-title ()
  "Update window title in terminal."
  (unless (display-graphic-p)
    (setq frame-title-format
          (if (bound-and-true-p projectile-project-name)
              (format "%s - %s" (projectile-project-name) (buffer-name))
            (format "%s - %s" (system-name) (buffer-name))))))

;; HACK: Remap some keys in Zellij terminal multiplexer, because it doesn't
;; send distinct keycodes for some modified keys.
;; See PR comments:
;; 1. https://github.com/zellij-org/zellij/issues/3789#issuecomment-2569862441
;; 2. https://github.com/zellij-org/zellij/issues/3789#issuecomment-2570032065
;;
;; The code below is adapted from the 2nd comment above. The explanation from
;; the original author is kept intact for reference.
;;
;; -----
;;
;; Zellij supports the "disambiguate escape codes" component of the Kitty
;; Keyboard protocol, but **not** "report alternate keys". (See
;; <https://github.com/zellij-org/zellij/issues/3789> for more details and
;; discussion.)
;;
;; An implication is explained in the kpp.el README:
;;
;; > Note that when you activate only `disambiguate-escape-codes`, the terminal
;; > reports shifted keypresses which involve another modifier by sending the
;; > modifiers with the base layout of the key.
;; >
;; > This means "M-S-." (Meta-Shift-.) is not translated to "M-:" (on a German
;; > keyboard) and Emacs will probably not find the proper keybinding.
;;
;; As a workaround, we simply add relevant entries to the key translation map.
;; For example, for an English layout, we map "M-S-." to "M->".
(defun tty-config/remap-zellij-shifted-keys ()
  "Remap some shifted keys in Zellij terminal multiplexer."
  (let ((key-remap-alist '(("M-S-`" . "M-~")
                           ("M-S-1" . "M-!")
                           ("M-S-2" . "M-@")
                           ("M-S-3" . "M-#")
                           ("M-S-4" . "M-$")
                           ("M-S-5" . "M-%")
                           ("M-S-6" . "M-^")
                           ("M-S-7" . "M-&")
                           ("M-S-8" . "M-*")
                           ("M-S-9" . "M-(")
                           ("M-S-0" . "M-)")
                           ("M-S--" . "M-_")
                           ("M-S-=" . "M-+")
                           ("M-S-[" . "M-{")
                           ("M-S-]" . "M-}")
                           ("M-S-\\" . "M-|")
                           ("M-S-;" . "M-:")
                           ("M-S-'" . "M-\"")
                           ("M-S-," . "M-<")
                           ("M-S-." . "M->")
                           ("M-S-/" . "M-?"))))
    (dolist (pair key-remap-alist)
      (let ((from (kbd (car pair)))
            (to (kbd (cdr pair))))
        (define-key key-translation-map from to)))))

(defun tty-config/enable-mouse-support ()
  "Enable mouse support in terminal."
  (unless (display-graphic-p)
    (xterm-mouse-mode 1)
    (setq mouse-sel-mode t
          mouse-yank-at-point t)
    (global-set-key (kbd "<mouse-4>") #'scroll-down-line)
    (global-set-key (kbd "<mouse-5>") #'scroll-up-line)
    (define-key global-map [mouse-1] #'mouse-set-point)))

(defun tty-config/enable-region-selection ()
  "Enable region selection with cursor/mouse in terminal."
  (setq transient-mark-mode t
        mouse-sel-mode t
        mouse-yank-at-point t
        highlight-nonselected-windows t
        mark-even-if-inactive t)
  ;; Mouse selection
  (define-key global-map [mouse-1] #'mouse-set-point)
  (define-key global-map [drag-mouse-1] #'mouse-set-region)
  (define-key global-map [mouse-3] #'mouse-save-then-kill)
  ;; Shift+Arrow selection
  (global-set-key (kbd "S-<left>") #'backward-char)
  (global-set-key (kbd "S-<right>") #'forward-char)
  (global-set-key (kbd "S-<up>") #'previous-line)
  (global-set-key (kbd "S-<down>") #'next-line))

