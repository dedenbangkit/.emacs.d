;;; splash.el --- Initialization my splash for Emacs
;;; Commentary: Deden Splash File --- initialization my splash for Emacs


(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(use-package page-break-lines :ensure t)
(use-package all-the-icons :ensure t)
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title nil)
  (setq dashboard-startup-banner '"/splash.txt")
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents  . 3)
			  (bookmarks . 10)
			  (projects . 5)
			  (agenda . 5)
			  (registers . 2)))
  (setq dashboard-footer-messages nil)
  :config
  (dashboard-modify-heading-icons '((recents. "file-text")
				    (bookmarks . "book")))
  (dashboard-setup-startup-hook))

;;This is for client mode
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; (setq dashboard-center-content t)
;; (setq dashboard-footer-messages '("Akvo Foundation"))
;; (provide 'init-startup)
