;; -*- no-byte-compile: t -*-
;;; packages.el --- TTY configuration layer packages file

(defconst tty-config-packages
  '(
    ;; Terminal clipboard support
    clipetty
    ;; Terminal cursor shape changer for Evil mode
    evil-terminal-cursor-changer
    ;; Kitty keyboard protocol support
    kkp
    ))

(defun tty-config/init-clipetty ()
  "Initialize clipetty for terminal clipboard support."
  (use-package clipetty
    :if (not (display-graphic-p))
    :hook (tty-setup . global-clipetty-mode)
    :config
    (when (not (display-graphic-p))
      (global-clipetty-mode))))

;; Fix cursor shape-changing in the terminal. Only supported in XTerm, Gnome
;; Terminal, iTerm, Konsole, dumb (etc. mintty), and Apple Terminal.app. If
;; using Apple Terminal.app, install
;; http://www.culater.net/software/SIMBL/SIMBL.php and
;; https://github.com/saitoha/mouseterm-plus/releases. That makes to support
;; VT's DECSCUSR sequence.
(defun tty-config/init-evil-terminal-cursor-changer ()
  "Initialize evil-terminal-cursor-changer."
  (use-package evil-terminal-cursor-changer
    :if (not (display-graphic-p))
    :hook (tty-setup . evil-terminal-cursor-changer-activate)
    :config
    (when (not (display-graphic-p))
      (evil-terminal-cursor-changer-activate))))

(defun tty-config/init-kkp ()
  "Initialize kkp (Kitty keyboard protocol)."
  (use-package kkp
    :if (not (display-graphic-p))
    :hook (tty-setup . global-kkp-mode)
    :config
    (when (not (display-graphic-p))
      (global-kkp-mode))))
