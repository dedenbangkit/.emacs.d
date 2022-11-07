;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(setq auto-mode-alist
      (append '(("\\.el\\'" . lisp-mode)
		("\\.py\\'" . python-mode)
		("\\.org\\'" . org-mode)
		("\\.clj\\'" . clojure-mode)
		("\\.edn\\'" . clojure-mode))
		auto-mode-alist))

(use-package company-jedi
  :ensure t
  :config
  (progn
    (setq jedi:complete-on-dot t
          jedi:use-shortcuts t)
    (defun user/python-mode-hook ()
      (add-to-list 'company-backends 'company-jedi))
    (add-hook 'python-mode-hook 'user/python-mode-hook)))
