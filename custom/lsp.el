;; Main LSP Configuration
;; lsp-mode

(add-to-list 'load-path (expand-file-name "lib/lsp-mode" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lib/lsp-mode/clients" user-emacs-directory))
(setenv "JAVA_HOME"  "/Users/dedenbangkit/Library/Java/JavaVirtualMachines/openjdk-16.0.1/Contents/Home/")
(setq lsp-java-java-path "/Users/dedenbangkit/Library/Java/JavaVirtualMachines/openjdk-16.0.1/Contents/Home/bin/java")

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
(use-package company-jedi
  :ensure t
  :config
  (progn
    (setq jedi:complete-on-dot t
          jedi:use-shortcuts t)
    (defun user/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'user/python-mode-hook)))
