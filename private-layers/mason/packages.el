;; -*- no-byte-compile: t -*-
;;; packages.el --- TTY configuration layer packages file

(defconst mason-packages
  '(
    mason
    ))

(defun mason/init-mason ()
  "Initialize clipetty for terminal clipboard support."
  (use-package mason
    :hook
    (after-init-hook . mason-ensure)))
