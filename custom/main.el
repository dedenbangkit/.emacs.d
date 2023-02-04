;;; main.el --- custom loads
;;; Commentary:
;;; Code:

(load "~/.emacs.d/custom/helm-fzf.el")
(load "~/.emacs.d/custom/auto-mode.el")
(load "~/.emacs.d/custom/lsp.el")

;;Linum Highlight
(load "~/.emacs.d/custom/linum-highlight.el")
(require 'linum-highlight-current-line-number)
(setq linum-format 'linum-highlight-current-line-number)

;;Simple Clip
(load "~/.emacs.d/custom/simpleclip.el")
(require 'simpleclip)
(simpleclip-mode 1)

;;Org to RST
(load "~/.emacs.d/custom/ox-rst.el")
(require 'ox-rst)

;;Command Log Mode
(load "~/.emacs.d/custom/command-log-mode.el")
(require 'command-log-mode)
(add-hook 'LaTeX-mode-hook 'command-log-mode)

;;Org Reveal
(load "~/.emacs.d/custom/ox-reveal.el")
(require 'ox-reveal)
(setq org-reveal-root "file:///home/dedenbangkit/.emacs.d/custom/reveal.js")

;; Org Gcal
(use-package org-gcal
  :ensure t
  :defer t
  :config
	(setq org-gcal-client-id (getenv "GCAL_CLIENT_ID")
				org-gcal-client-secret (getenv "GCAL_SECRET")
				org-gcal-fetch-file-alist '(("deden@akvo.org" .  "~/Orgs/gcal.org")))
  (org-gcal-reload-client-id-secret)
  (setq plstore-cache-passphrase-for-symmetric-encryption t)
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-save-all-org-buffers (lambda () (org-gcal-sync) ))
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal--sync-unlock)) 100) )

;; All the Icons
(when (display-graphic-p)
  (require 'all-the-icons))
;; or
(use-package all-the-icons
  :if (display-graphic-p))

;; Neotree
(setq neo-theme 'icons)
(setq neo-windo-width 55)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-python-flake8-executable "python3")
 '(flycheck-python-pycompile-executable "python3")
 '(flycheck-python-pylint-executable "python3")
 '(helm-minibuffer-history-key "M-p")
 '(package-selected-packages
   '(## dockerfile-mode json-mode all-the-icons page-break-lines projectile helm dashboard flycheck-clojure flycheck-clj-kondo flycheck company paredit cider clojure-mode zenburn-theme use-package evil))
 '(safe-local-variable-values
   '((eval customize-set-variable 'cider-path-translations
	   (let
	       ((m2
		 (concat
		  (getenv "HOME")
		  "/.m2")))
	     (list
	      (cons "/app"
		    (clojure-project-dir))
	      (cons "/home/akvo/.m2" m2)
	      (cons "/root/.m2" m2))))
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; main.el ends here
