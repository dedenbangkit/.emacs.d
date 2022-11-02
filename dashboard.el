;;; splash.el --- Initialization my splash for Emacs
;;; Commentary: Deden Splash File --- initialization my splash for Emacs


;;; Projectile

(unless (package-installed-p 'projectile)
  (package-install 'projectile))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)

(unless (package-installed-p 'page-break-lines)
  (package-install 'page-break-lines))
(require 'page-break-lines)
(page-break-lines-mode)

(unless (package-installed-p 'all-the-icons)
  (package-install 'all-the-icons))
(when (display-graphic-p)
  (require 'all-the-icons))

(unless (package-installed-p 'dashboard)
  (package-install 'dashboard))
(require 'dashboard)
(dashboard-setup-startup-hook)

(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-footer-messages nil)
(setq dashboard-startup-banner '"/Users/dedenbangkit/.emacs.d/splash/splash.txt")

(setq dashboard-items '((recents  . 3)
                        (bookmarks . 10)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 2)))


;; (setq dashboard-center-content t)
;; (setq dashboard-footer-messages '("Akvo Foundation"))
;; (provide 'init-startup)
