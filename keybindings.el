;;; keybindings.el --- Initialization keybindings for Emacs
;;; Commentary: Deden Custom Keybinding --- initialization keybinding for Emacs

(setq which-key-idle-delay 0.01)
(helm-ext-ff-enable-split-actions t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; For leader f
(defun edit-keybindings ()
  (interactive) (find-file "~/.emacs.d/keybindings.el"))
(defun edit-init-el ()
  (interactive) (find-file "~/.emacs.d/init.el"))
(defun edit-custom-el ()
  (interactive) (find-file "~/.emacs.d/custom/main.el"))
(defun modes ()
  (interactive) nil)

(define-key helm-map (kbd "<escape>") #'helm-keyboard-quit)

(define-key evil-normal-state-map (kbd "ff") #'helm-find-files)
(define-key evil-normal-state-map (kbd "fb") #'helm-bookmarks)
(define-key evil-normal-state-map (kbd "fl") #'helm-mini)
(define-key evil-normal-state-map (kbd "fj") #'helm-buffers-list)
(define-key evil-normal-state-map (kbd "fn") #'display-line-numbers-mode)
(define-key evil-normal-state-map (kbd "fp") #'find-file-literally-at-point)
(define-prefix-command 'edit-my-config)
(define-key evil-normal-state-map (kbd "fc") #'edit-my-config)
(define-key evil-normal-state-map (kbd "fck") #'edit-keybindings)
(define-key evil-normal-state-map (kbd "fci") #'edit-init-el)
(define-key evil-normal-state-map (kbd "fcc") #'edit-custom-el)
(define-prefix-command 'find-ag)
(define-key evil-normal-state-map (kbd "fs") #'find-ag)
(define-key evil-normal-state-map (kbd "fss") #'helm-do-ag)
(define-key evil-normal-state-map (kbd "fsh") #'helm-ag-pop-stack)
(define-key evil-normal-state-map (kbd "fsp") #'helm-ag-project-root)
(define-key evil-normal-state-map (kbd "fsb") #'helm-ag-buffers)

(define-key evil-normal-state-map (kbd "s") #'save-buffer)
(define-key evil-normal-state-map (kbd "f SPC") #'keyboard-escape-quit)
(define-key evil-normal-state-map (kbd "f <escape>") #'keyboard-escape-quit)

;; Open Files
(define-key evil-normal-state-map (kbd "SPC") #'helm-ag-this-file)

;; Windows
(define-key evil-normal-state-map (kbd "wl") #'windmove-right)
(define-key evil-normal-state-map (kbd "wh") #'windmove-left)
(define-key evil-normal-state-map (kbd "wj") #'windmove-down)
(define-key evil-normal-state-map (kbd "wk") #'windmove-up)
(define-key evil-normal-state-map (kbd "%") #'split-window-below)
(define-key evil-normal-state-map (kbd "|") #'split-window-right)

;; Modes
(define-prefix-command 'modes)
(define-key evil-normal-state-map (kbd "fm") #'modes)
(define-key evil-normal-state-map (kbd "fml") #'lisp-mode)
(define-key evil-normal-state-map (kbd "fmo") #'org-mode)
(define-key evil-normal-state-map (kbd "fmc") #'clojure-mode)
(define-key evil-normal-state-map (kbd "fm <escape>") #'keyboard-escape-quit)


;; Clojure Cider
(add-hook 'clojure-mode-hook '(lambda ()
    (define-key evil-normal-state-map (kbd "ec") #'cider-connect)
    (define-key evil-normal-state-map (kbd "eb") #'cider-eval-buffer)
    (define-key evil-normal-state-map (kbd "ee") #'cider-eval-list-at-point)
    (define-key evil-normal-state-map (kbd "ej") #'cider-find-var)
    (define-key evil-normal-state-map (kbd "ek") #'cider-pop-back)
    (define-key evil-normal-state-map (kbd "e <escape>") #'keyboard-escape-quit)))

;; Org Custom Evil
(add-hook 'org-mode-hook '(lambda ()
    (define-key evil-normal-state-map (kbd "L") #'org-cycle)
    (define-key evil-normal-state-map (kbd "+") #'org-toggle-checkbox)
    (define-key evil-normal-state-map (kbd "RET") #'org-babel-execute-src-block)
    (define-key evil-normal-state-map (kbd "#") #'org-insert-structure-template)))

;; (evil-ex-define-cmd "q[uit]" nil)
