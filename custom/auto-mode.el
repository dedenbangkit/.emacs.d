;;; auto-mode.el --- auto-mode list
;;; Commentary:
;;; To create a file, visit it with C-x C-f and enter text in its buffer.

(setq auto-mode-alist
      (append '(("\\.el\\'" . lisp-mode)
		("\\.py\\'" . python-mode)
		("\\.java\\'" . java-mode)
		("\\.js\\'" . javascript-mode)
		("\\.org\\'" . org-mode)
		("\\.clj\\'" . clojure-mode)
		("\\.edn\\'" . clojure-mode))
		auto-mode-alist))

;;; auto-mode.el ends here
