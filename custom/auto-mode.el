;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(setq auto-mode-alist
      (append '(("\\.el\\'" . lisp-mode)
		("\\.py\\'" . python-mode)
		("\\.java\\'" . java-mode)
		("\\.js\\'" . javascript-mode)
		("\\.org\\'" . org-mode)
		("\\.clj\\'" . clojure-mode)
		("\\.edn\\'" . clojure-mode))
		auto-mode-alist))
