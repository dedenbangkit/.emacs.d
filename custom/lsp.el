;;; lsp.el --- Main LSP Configuration
;;; Commentary:

;;; Code:

(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
(setenv "JAVA_HOME"  "/usr/lib/jvm/java-19-openjdk-amd64")
(defvar lsp-java-java-path "/usr/lib/jvm/java-19-openjdk-amd64/bin/java")

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :ensure t)
(use-package lsp-ui
  :after lsp-mode
  :ensure t)
(use-package helm-lsp
  :commands helm-lsp-workspace-symbol
  :ensure t)
(use-package yasnippet
  :after lsp-mode
  :ensure t
  :config (yas-global-mode))

;; Java
(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook #'lsp))
(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))
(use-package dap-java
  :after lsp-mode
  :ensure nil)

;; Python
(use-package python-mode
  :ensure nil
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3"))

(use-package company-jedi
  :ensure t
  :config
  (progn
    (setq jedi:complete-on-dot t
          jedi:use-shortcuts t)
    (defun user/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi)
      (add-to-list 'write-file-functions 'delete-trailing-whitespace))
    (add-hook 'python-mode-hook 'user/python-mode-hook)
    (add-hook 'python-mode-hook 'flycheck-mode)))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(custom-set-variables
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3"))

;;; lsp.el ends here
