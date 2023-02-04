;;; dashboard.el --- Initialization my splash for Emacs
;;; Commentary:
;;; Code:

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))
;; Projectile Ends here

(use-package page-break-lines :ensure t)
(use-package all-the-icons :ensure t)
(use-package dashboard
  :ensure t
  :init
  :config
  (dashboard-setup-startup-hook))

(setq dashboard-week-agenda t)
(setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
(setq dashboard-item-names '(("Recent Files:" . "Recently opened files:")
			     ("Agenda for today:" . "Today's agenda:")
			     ("Agenda for the coming week:" . "Agenda:")))
(setq dashboard-items '((recents  . 5)
			(bookmarks . 5)
			(projects . 5)
			(agenda . 10)))
(setq dashboard-set-navigator nil)
(setq dashboard-set-file-icons t)
(setq dashboard-set-init-info t)
(setq dashboard-banner-logo-title nil)
(setq dashboard-startup-banner '"/home/dedenbangkit/Pictures/Logo/akvo-icon.png")
(setq dashboard-center-content nil)
(setq dashboard-footer-messages nil)

;;This is for client mode
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; (setq dashboard-center-content t)
;; (setq dashboard-footer-messages '("Akvo Foundation"))
(provide 'init-startup)

;;; dashboard.el ends here
