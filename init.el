;;; init.el --- Initialization file for my emacs
;;; Commnetary:

;;; Code:
(require 'package)

;; add to list
;; (add-to-list 'package-archives
;; 	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Remove vertical border character "|"
;; Reverse colors for the border to have nicer line

(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))

;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

;; fonts
(set-frame-font
 "JetBrainsMono Nerd Font 10" nil t)
(add-to-list 'default-frame-alist
	     '(font . "JetBrainsmono Nerd Font 10"))
(set-face-attribute
 'default t
 :font "JetBrainsmono Nerd Font 10")


;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; No splash screen and backup file
(setq inhibit-splash-screen t
      inhibit-startup-message t
      backup-inhibited t)

(setq auto-save-default nil)

;; Default column
(setq-default fill-column 120)

;; Remove menu
(when (display-graphic-p)
  (scroll-bar-mode -1))
(tool-bar-mode -1)
(display-time-mode 1)
;; (menu-bar-mode -1)

;; my-packages
(defvar my-package-list
  '(clojure-mode
    json-mode
    yaml-mode
    dockerfile-mode
    helm
    helm-ag
    helm-ext
    cider
    paredit
    company
    hydra
    magit
    flycheck
    flycheck-color-mode-line
    flycheck-package
    flycheck-clj-kondo
    transpose-frame
    evil
    evil-leader
    evil-collection
    which-key
    undo-fu
    undo-fu-session
    simpleclip ;; copy paste
    org-bullets
		request
		request-deferred
		aio
		persist ;; for org gcal
    neotree
    treemacs
    use-package))

;; Scans the list in myPackages
;; install the missing packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Enable Evil
(setq-default evil-want-integration t)
(setq-default evil-want-keybinding nil)
(setq-default evil-want-C-u-scroll t)
(require 'undo-fu)
(setq-default evil-undo-system 'undo-fu)
(global-undo-fu-session-mode)
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init))
(require 'evil-collection)
(evil-mode 1)
(require 'evil-leader)
(global-evil-leader-mode)

;; Which key
(require 'which-key)
(which-key-mode)

;; Doom Mode Line
;; (require 'doom-modeline)
;; (doom-modeline-mode 1)

;; Transpose Frame
(require 'transpose-frame)

;; Search properties
(global-company-mode 1)
(global-flycheck-mode 1)
(eval-after-load 'flycheck
  '(flycheck-package-setup))
(show-paren-mode 1)
(setq show-paren-delay 0)

(require 'flycheck-color-mode-line)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(load-theme 'misterioso t)

;; lein, clj-kondo, etc
;; (setq exec-path (append exec-path '("/usr/local/bin"))) ;;change-me

;; CIDER
(add-hook 'clojure-mode-hook 'paredit-mode)
(setq-default cider-prompt-save-file-on-load 'always-save)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq-default cider-save-file-on-load t)
(setq-default cider-repl-display-help-banner nil)

;; ido
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)
(ido-mode 1)

;; Compojure
(require 'clojure-mode)

(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (context 2))

;; Paredit Lisp
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'emacs-lisp-mode)

;; ORG-MODE
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (shell . t)
   (python . t)
   (sql . t)
   (sqlite . t)))

;; ORG-AGENDA
(setq org-agenda-files
      '("/home/dedenbangkit/Orgs/gcal.org"
	"/home/dedenbangkit/Orgs/agenda.org"))

(eval-after-load "org"
  '(require 'ox-md nil t))
(require 'org-tempo)
(require 'org-bullets)
(add-hook
 'org-mode-hook
 (lambda ()
   (org-bullets-mode 1)
   (org-indent-mode 1)))

(setq org-confirm-babel-evaluate nil)

;; clj-kondo
(require 'flycheck-clj-kondo)
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

;; sort dired buffer
(defun sort-dired-buffers (buffers)
  "Sort BUFFERS by moving all Dired buffers to the end."
  (let (dired-buffers other-buffers)
    (dolist (buf buffers)
      (if (with-current-buffer buf
            (eq major-mode 'dired-mode))
          (push buf dired-buffers)
        (push buf other-buffers)))
    (nreverse (append dired-buffers other-buffers))))

(load "~/.emacs.d/custom/main.el")
(load "~/.emacs.d/keybindings.el")
(load "~/.emacs.d/dashboard.el")

(set-frame-parameter
        nil 'title (format-mode-line mode-line-format))

;; Transparent Background
(defun toggle-transparency ()
  "Toggle transparency."
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(95 . 50) '(100 . 100)))))
(global-set-key (kbd "C-c t") 'toggle-transparency)
(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

;;; init.el ends here
