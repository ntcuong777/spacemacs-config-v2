;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; ---------------------------------------
;; General Configuration changes
;; ---------------------------------------

;; ---------------------------------------
;; Line numbers
;; native line numbers taking up lots of space?
(setq-default display-line-numbers-width nil)
;; ---------------------------------------

;; ---------------------------------------
;; Searching
;; replace / search with helm-swoop in Evil normal state
(evil-global-set-key 'normal "/" 'helm-swoop)
;;
;; ---------------------------------------

;; ---------------------------------------
;; Helm Descbinds
;; Recent release of helm-descbinds package breaks which-key menu
;; Remove helm-discbinds-mode from helm mode hook to avoid activating
;; https://github.com/syl20bnr/spacemacs/issues/16276
(remove-hook 'helm-mode-hook 'helm-descbinds-mode)
;; ---------------------------------------

(setq projectile-project-search-path '("~/projects/"
                                       "~/personal-projects/"))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)

  (define-key smartparens-mode-map (kbd "M-(") 'sp-wrap-round)
  (define-key smartparens-mode-map (kbd "M-[") 'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-{") 'sp-wrap-curly)
  ;; Apply default smartparens bindings
  (dolist (binding sp-smartparens-bindings)
    (define-key smartparens-mode-map (kbd (car binding)) (cdr binding))))

;; Add auto save to several language mode
(setq lsp-enable-indentation nil
      ;; lsp-enable-on-type-formatting nil
      lsp-format-buffer-on-save nil)

(add-hook 'lisp-mode-hook #'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
(add-hook 'elisp-mode #'smartparens-mode)
(add-hook 'clojure-mode-hook #'smartparens-mode)

(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'go-mode-hook #'smartparens-mode)


;; Map ESC to quit several mode
(define-key minibuffer-local-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-ns-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-completion-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-must-match-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-isearch-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-filename-completion-map (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-shell-command-map (kbd "<escape>") 'keyboard-quit)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "<escape>") 'company-abort))
(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "<escape>") 'corfu-quit))
;; (with-eval-after-load 'helm
;;   (define-key helm-map (kbd "<escape>") 'helm-keyboard-quit))
;; (with-eval-after-load 'helm-descbinds
;;   (define-key helm-descbinds-map (kbd "<escape>") 'helm-keyboard-quit))
;; (define-key helm-M-x-map (kbd "<escape>") (kbd "C-g"))
;; (define-key key-translation-map (kbd "<escape>") (kbd "C-g"))

;; Map comment to `Cmd+/' on MacOS
(define-key global-map (kbd "H-/") 'spacemacs/comment-or-uncomment-lines)

;; 1) Bind ESC → C-g in the global keymap (lowest precedence)
(global-set-key (kbd "<escape>") 'keyboard-quit)

;; 2) After Evil loads, re-bind ESC in each state map
;;    (this will override the global binding in those Evil states)
(with-eval-after-load 'evil
  ;; Insert state: ESC → back to Normal
  (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

  ;; Visual state: ESC → exit Visual (same as default)
  (define-key evil-visual-state-map (kbd "<escape>") 'evil-exit-visual-state)

  ;; Replace state: ESC → Normal
  (define-key evil-replace-state-map (kbd "<escape>") 'evil-normal-state)

  ;; Operator-pending state: ESC → cancel
  (define-key evil-operator-state-map (kbd "<escape>") 'evil-normal-state)

  ;; Motion state: ESC → Normal
  (define-key evil-motion-state-map (kbd "<escape>") 'evil-normal-state)

  ;; Emacs state: ESC → Normal
  (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state))

(with-eval-after-load 'helm
  ;; Make <escape> quit any active Helm session
  (dolist (map (list
                'helm-map               ; main helm map
                'helm-generic-files-map ; some commands fall back here
                'helm-find-files-map    ; in M-x find-file
                'helm-read-file-map     ; in reading filenames
                'helm-grep-map          ; in grep/rgrep buffers
                'helm-M-x-map
                'helm-locate-map
                'helm-swoop-map
                ))
    (when (boundp map)
      (define-key (symbol-value map) (kbd "<escape>") #'helm-keyboard-quit)))

  ;; Also catch ESC sequences (two-step ESC ESC)
  (define-key helm-map (kbd "ESC ESC") #'helm-keyboard-quit))

;; Disable xterm mouse mode to avoid conflicts with terminal multiplexer
(xterm-mouse-mode -1)
