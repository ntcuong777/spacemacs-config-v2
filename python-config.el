;; -*- mode: emacs-lisp; lexical-binding: t -*-

;; ---------------------------------------
;; Python configurations
(with-eval-after-load 'flycheck
  (setq flycheck-checker-error-threshold 500)

  (flycheck-define-checker
      python-mypy-pylint-flake8
    "Custom checker for Python using existing mypy, pylint, and flake8 checkers in sequence.
Inspired by the `python-mypy' checker from Flycheck."
    :command ("mypy"
              "--strict" "--ignore-missing-imports"
              "--disallow-incomplete-defs"
              "--disallow-any-explicit"
              "--disallow-any-generics"
              "--disallow-untyped-decorators"
              "--show-column-numbers"
              "--warn-return-any"
              "--warn-redundant-casts"
              "--warn-unused-ignores"
              "--check-untyped-defs"
              "--disallow-untyped-calls"
              "--no-color-output" "--hide-error-context"
              "--no-error-summary" "--show-column-numbers"
              "--no-pretty"
              (config-file "--config-file" flycheck-python-mypy-config)
              (option "--cache-dir" flycheck-python-mypy-cache-dir)
              (option "--python-executable" flycheck-python-mypy-python-executable)
              source-original)
    :error-patterns
    ((error line-start (file-name) ":" line (optional ":" column)
            ": error:" (message) line-end)
     (warning line-start (file-name) ":" line (optional ":" column)
              ": warning:" (message) line-end)
     (info line-start (file-name) ":" line (optional ":" column)
           ": note:" (message) line-end))
    :working-directory flycheck-python-find-project-root
    :modes (python-mode python-ts-mode)
    :next-checkers ((warning . lsp)
                    (warning . python-pylint)
                    (warning . python-flake8))
    ;; Ensure the file is saved, to work around
    ;; https://github.com/python/mypy/issues/4746.
    :predicate flycheck-buffer-saved-p)

  (add-to-list 'flycheck-checkers 'python-mypy-pylint-flake8 t)
  (add-hook! '(python-mode-hook python-ts-mode-hook)
             :append (lambda ()
                       (flycheck-select-checker 'python-mypy-pylint-flake8))))
