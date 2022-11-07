(use-package lsp-mode
    :init (setq lsp-keymap-prefix "C-l")
    :hook ((lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

(use-package helm-lsp
    :commands helm-lsp-workspace-symbol)
