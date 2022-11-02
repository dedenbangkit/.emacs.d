;;; init.el --- Initialization file for my emacs
;;; Commentary: Deden Startup File --- initialization file for Emacs

(require 'package)

;; add to list
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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
(setq-default fill-column 100)

;; Remove menu
(when (display-graphic-p)
  (scroll-bar-mode -1))
(tool-bar-mode -1)
(display-time-mode 1)
(menu-bar-mode -1)

;; my-packages
(defvar my-package-list
  '(clojure-mode
    python-mode
    json-mode
    yaml-mode
    dockerfile-mode
    helm
    helm-ag
    helm-ext
    cider
    paredit
    company
    doom-modeline
    flycheck
    flycheck-clj-kondo
    evil
    evil-leader
    evil-collection
    which-key
    use-package))

;; Scans the list in myPackages
;; install the missing packages
(dolist (package my-package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Enable Evil
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
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
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Search properties
(global-company-mode)
(global-flycheck-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(load-theme 'tango-dark t)

;; lein, clj-kondo, etc
;; (setq exec-path (append exec-path '("/usr/local/bin"))) ;;change-me

;; CIDER
(add-hook 'clojure-mode-hook 'paredit-mode)
(setq cider-prompt-save-file-on-load 'always-save)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-mode-hook #'eldoc-mode)
(setq cider-save-file-on-load t)
(setq cider-repl-display-help-banner nil)

;; ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
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

;; org-mode

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . nil)
   (shell . t)
   (python . t)
   (sql . t)
   (sqlite . t)))

(eval-after-load "org"
  '(require 'ox-md nil t))
(require 'org-tempo)

(setq org-confirm-babel-evaluate nil)

;; clj-kondo
(require 'flycheck-clj-kondo)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dashboard flycheck-clojure flycheck-clj-kondo flycheck company paredit cider clojure-mode zenburn-theme use-package evil))
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

;; custom files

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

(setq custom-file "~/.emacs.d/custom/main.el")
(load custom-file)
(setq keybindings-file "~/.emacs.d/keybindings.el")
(load keybindings-file)
(setq dashboard-file "~/.emacs.d/dashboard.el")
(load dashboard-file)

(set-frame-parameter
        nil 'title (format-mode-line mode-line-format))
