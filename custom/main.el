(setq helm-fzf-file "~/.emacs.d/custom/helm-fzf.el")
(load helm-fzf-file)
(setq automode-file "~/.emacs.d/custom/auto-mode.el")
(load automode-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
