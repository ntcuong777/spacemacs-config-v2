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
;; (evil-global-set-key 'normal "/" 'helm-swoop)
;;
;; ---------------------------------------

;; ---------------------------------------
;; Helm Descbinds
;; Recent release of helm-descbinds package breaks which-key menu
;; Remove helm-discbinds-mode from helm mode hook to avoid activating
;; https://github.com/syl20bnr/spacemacs/issues/16276
;; (remove-hook 'helm-mode-hook 'helm-descbinds-mode)
;; ---------------------------------------

(setq projectile-project-search-path '("~/projects/"
                                       "~/personal-projects/"))

(with-eval-after-load 'smartparens
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (require 'smartparens-config)

  ;; (define-key smartparens-mode-map (kbd "M-(") 'sp-wrap-round)
  ;; (define-key smartparens-mode-map (kbd "M-[") 'sp-wrap-square)
  ;; (define-key smartparens-mode-map (kbd "M-{") 'sp-wrap-curly)
  ;; Apply default smartparens bindings
  ;; (dolist (binding sp-smartparens-bindings)
  ;;   (define-key smartparens-mode-map (kbd (car binding)) (cdr binding)))

  ;; Smartparens keybindings (Doom -> Spacemacs)
  ;; Put in dotspacemacs/user-config

  ;; Top-level under SPC k
  (spacemacs/set-leader-keys
    "ks"  '(sp-forward-slurp-sexp :which-key "Slurp forward")
    "kS"  '(sp-backward-slurp-sexp :which-key "Slurp backward")
    "k$"  '(sp-end-of-sexp :which-key "End of sexp")

    ;; Hybrid prefix: SPC k `
    "k`k" '(sp-kill-hybrid-sexp :which-key "Hybrid kill")
    "k`p" '(sp-push-hybrid-sexp :which-key "Hybrid push")
    "k`s" '(sp-slurp-hybrid-sexp :which-key "Hybrid slurp")
    "k`t" '(sp-transpose-hybrid-sexp :which-key "Hybrid transpose")

    "ka"  '(sp-absorb-sexp :which-key "Absorb")
    "kb"  '(sp-forward-barf-sexp :which-key "Barf forward")
    "kB"  '(sp-backward-barf-sexp :which-key "Barf backward")
    "kc"  '(sp-convolute-sexp :which-key "Convolute")

    ;; Delete prefix: SPC k d
    "kds" '(sp-kill-symbol (:which-key) "Delete symbol")
    "kdS" '(sp-backward-kill-symbol :which-key "Delete symbol backward")
    "kdw" '(sp-kill-word :which-key "Delete word")
    "kdW" '(sp-backward-kill-word :which-key "Delete word backward")
    "kdx" '(sp-kill-sexp :which-key "Kill sexp")
    "kdX" '(sp-backward-kill-sexp :which-key "Kill sexp backward")

    "k@" '(sp-splice-sexp :which-key "Splice")
    "ke"  '(sp-splice-sexp-killing-forward :which-key "Splice (kill fwd)")
    "kE"  '(sp-splice-sexp-killing-backward :which-key "Splice (kill back)")

    "kh"  '(sp-backward-symbol :which-key "Symbol backward")
    "kH"  '(sp-backward-sexp :which-key "Sexp backward")
    "kj"  '(sp-join-sexp :which-key "Join")

    "kl"  '(sp-forward-sexp :which-key "Sexp forward")
    ;; You had both l and L -> same command; keep L too:
    "kL"  '(sp-forward-sexp :which-key "Sexp forward")

    "kr"  '(sp-raise-sexp :which-key "Raise")
    "kt"  '(sp-transpose-sexp :which-key "Transpose")
    "kU"  '(sp-backward-up-sexp :which-key "Up backward")

    ;; Wrap prefix: SPC k w
    ;; "kw(" '(sp-wrap-round :which-key "Wrap ()")
    ;; "kw{" '(sp-wrap-curly :which-key "Wrap {}")
    ;; "kw[" '(sp-wrap-square :which-key "Wrap []")
    ;; "kww" '(sp-wrap-round :which-key "Wrap round")
    ;; "kwc" '(sp-wrap-curly :which-key "Wrap curly")
    ;; "kws" '(sp-wrap-square :which-key "Wrap square")
    ;; "kwu" '(sp-unwrap-sexp :which-key "Unwrap")

    "ky"  '(sp-copy-sexp :which-key "Copy sexp"))
  )

;; (add-hook 'lisp-mode-hook #'smartparens-mode)
;; (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
;; (add-hook 'elisp-mode #'smartparens-mode)
;; (add-hook 'clojure-mode-hook #'smartparens-mode)

;; (add-hook 'python-mode-hook #'smartparens-mode)
;; (add-hook 'go-mode-hook #'smartparens-mode)

(with-eval-after-load 'copilot
  (setq copilot-max-char-warning-disable t
        copilot-indent-offset-warning-disable t)
  (dolist (indent-config '((prog-mode . 4)
                           (org-mode . 2)
                           (text-mode . 2)
                           (clojure-mode . 2)
                           (python-mode . 4)
                           (python-ts-mode . 4)
                           (emacs-lisp-mode . 2)
                           (elisp-mode . 2)
                           (lisp-mode . 2)
                           (js-mode . 2)
                           (js-ts-mode . 2)
                           (typescript-mode . 2)
                           (typescript-ts-mode . 2)))
    (add-to-list 'copilot-indentation-alist indent-config))
  )


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

(defun my/escape-quit ()
  "A more consistent ESC quit."
  (interactive)
  (cond
   ;; If a minibuffer is active, close it.
   ((minibufferp) (abort-recursive-edit))
   ;; If region is active, deactivate it.
   ((region-active-p) (deactivate-mark))
   ;; Otherwise, run the standard quit.
   (t (keyboard-quit))))

(with-eval-after-load 'evil
  ;; Normal/visual/motion: ESC quits
  (define-key evil-normal-state-map [escape] #'my/escape-quit)
  (define-key evil-visual-state-map [escape] #'my/escape-quit)
  (define-key evil-motion-state-map [escape] #'my/escape-quit)
  (define-key evil-insert-state-map [escape] #'evil-normal-state))

;; Minibuffer maps: ESC aborts
(define-key minibuffer-local-map [escape] #'abort-recursive-edit)
(define-key minibuffer-local-ns-map [escape] #'abort-recursive-edit)
(define-key minibuffer-local-completion-map [escape] #'abort-recursive-edit)
(define-key minibuffer-local-must-match-map [escape] #'abort-recursive-edit)
(define-key minibuffer-local-isearch-map [escape] #'abort-recursive-edit)

;; Global fallback
(global-set-key [escape] #'my/escape-quit)

;; ;; 1) Bind ESC → C-g in the global keymap (lowest precedence)
;; (global-set-key (kbd "<escape>") 'keyboard-quit)


;; 2) After Evil loads, re-bind ESC in each state map
;;    (this will override the global binding in those Evil states)
;; (with-eval-after-load 'evil
;;   ;; Insert state: ESC → back to Normal
;;   (define-key evil-insert-state-map (kbd "<escape>") 'evil-normal-state)

;;   ;; Visual state: ESC → exit Visual (same as default)
;;   (define-key evil-visual-state-map (kbd "<escape>") 'evil-exit-visual-state)

;;   ;; Replace state: ESC → Normal
;;   (define-key evil-replace-state-map (kbd "<escape>") 'evil-normal-state)

;;   ;; Operator-pending state: ESC → cancel
;;   (define-key evil-operator-state-map (kbd "<escape>") 'evil-normal-state)

;;   ;; Motion state: ESC → Normal
;;   (define-key evil-motion-state-map (kbd "<escape>") 'evil-normal-state)

;;   ;; Emacs state: ESC → Normal
;;   (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state))

;; (with-eval-after-load 'helm
;;   ;; Make <escape> quit any active Helm session
;;   (dolist (map (list
;;                 'helm-map               ; main helm map
;;                 'helm-generic-files-map ; some commands fall back here
;;                 'helm-find-files-map    ; in M-x find-file
;;                 'helm-read-file-map     ; in reading filenames
;;                 'helm-grep-map          ; in grep/rgrep buffers
;;                 'helm-M-x-map
;;                 'helm-locate-map
;;                 'helm-swoop-map
;;                 ))
;;     (when (boundp map)
;;       (define-key (symbol-value map) (kbd "<escape>") #'helm-keyboard-quit)))

;;   ;; Also catch ESC sequences (two-step ESC ESC)
;;   (define-key helm-map (kbd "ESC ESC") #'helm-keyboard-quit))

(with-eval-after-load 'lsp-pyright
  (setq lsp-pyright-langserver-command "basedpyright")
  (add-hook 'python-mode-hook (lambda ()
                                (require 'lsp-pyright)
                                (lsp-deferred))))  ; or lsp-deferred


;; ---------------------------------------
;; LSP booster
;;

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
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

;; ---------------------------------------


;; ---------------------------------------
;; Evil
;;

(with-eval-after-load 'evil-cleverparens
  (setq evil-cp-move-beginning-of-defun-keys '("C-M-l")
        evil-cp-move-end-of-defun-keys '("C-M-h"))

  ;; (delete '("M-l" . evil-cp-beginning-of-defun) evil-cp-additional-movement-keys)
  ;; (delete '("M-h" . evil-cp-end-of-defun) evil-cp-additional-movement-keys)
  (dolist (key evil-cp-move-beginning-of-defun-keys)
    (add-to-list 'evil-cp-additional-movement-keys `(,key . evil-cp-beginning-of-defun)))
  (dolist (key evil-cp-move-end-of-defun-keys)
    (add-to-list 'evil-cp-additional-movement-keys `(,key . evil-cp-end-of-defun)))

  (evil-cp-set-additional-movement-keys)
  )
