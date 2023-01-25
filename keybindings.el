;;; keybindings.el --- Initialization keybindings for Emacs
;;; Commentary
;;; Code:

(defvar which-key-idle-delay 0.01)
(helm-ext-ff-enable-split-actions t)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; For leader f
(defun edit-keybindings ()
  "Shortcut to edit keybindings.el."
  (interactive) (find-file "~/.emacs.d/keybindings.el"))
(defun edit-init-el ()
  "Shortcut to edit init.el."
  (interactive) (find-file "~/.emacs.d/init.el"))
(defun edit-custom-el ()
  "Shortcut to edit custom/main.el."
  (interactive) (find-file "~/.emacs.d/custom/main.el"))
(defun edit-lsp-el ()
  "Shortcut to edit custom/lsp.el."
  (interactive) (find-file "~/.emacs.d/custom/lsp.el"))
(defun modes ()
  "Switch between modes."
  (interactive) nil)
(defun edit-my-config ()
  "Edit Emacs Config."
  (interactive) nil)

(global-set-key (kbd "<f5>") #'revert-buffer-quick)
(define-key helm-map (kbd "<escape>") 'helm-keyboard-quit)
(define-key helm-map (kbd "%") 'helm-ext-ff-execute-horizontal-split)
(define-key helm-map (kbd "|") 'helm-ext-ff-execute-vertical-split)

(define-key evil-normal-state-map (kbd "ff") #'helm-find-files)
(define-key evil-normal-state-map (kbd "fb") #'helm-bookmarks)
(define-key evil-normal-state-map (kbd "fl") #'helm-mini)
(define-key evil-normal-state-map (kbd "fj") #'helm-buffers-list)
(define-key evil-normal-state-map (kbd "fn") #'global-linum-mode)
(define-key evil-normal-state-map (kbd "fp") 'find-file-literally-at-point)
(define-prefix-command 'edit-my-config)
(define-key evil-normal-state-map (kbd "fc") #'edit-my-config)
(define-key evil-normal-state-map (kbd "fck") #'edit-keybindings)
(define-key evil-normal-state-map (kbd "fci") #'edit-init-el)
(define-key evil-normal-state-map (kbd "fcc") #'edit-custom-el)
(define-key evil-normal-state-map (kbd "fcl") 'edit-custom-lsp)
(define-key evil-normal-state-map (kbd "fss") 'find-ag)
(define-key evil-normal-state-map (kbd "fsd") #'helm-do-ag)
(define-key evil-normal-state-map (kbd "fsh") #'helm-ag-pop-stack)
(define-key evil-normal-state-map (kbd "fsp") #'helm-ag-project-root)
(define-key evil-normal-state-map (kbd "fsb") #'helm-ag-buffers)

(define-key evil-normal-state-map (kbd "s") #'save-buffer)
(define-key evil-normal-state-map (kbd "f SPC") #'keyboard-escape-quit)
(define-key evil-normal-state-map (kbd "f <escape>") #'keyboard-escape-quit)

;; Indentation
(define-key evil-visual-state-map (kbd "SPC") #'indent-region)

;; Simple Clip
(define-key evil-visual-state-map (kbd "Y") #'simpleclip-copy)
(define-key evil-normal-state-map (kbd "P") #'simpleclip-paste)

;; Magit
(define-key evil-normal-state-map (kbd "M") #'magit)

;; Open Files
(define-key evil-normal-state-map (kbd "SPC") #'helm-ag-this-file)

;; Global Moving
(define-key evil-normal-state-map (kbd "(")  'evil-previous-open-paren)
(define-key evil-normal-state-map (kbd ")")  'evil-next-close-paren)

;; Windows
(define-key evil-normal-state-map (kbd "wl") #'windmove-right)
(define-key evil-normal-state-map (kbd "wh") #'windmove-left)
(define-key evil-normal-state-map (kbd "wj") #'windmove-down)
(define-key evil-normal-state-map (kbd "wk") #'windmove-up)
(define-key evil-normal-state-map (kbd "wd") #'kill-current-buffer)
(define-key evil-normal-state-map (kbd "ww") #'transpose-frame)
(define-key evil-normal-state-map (kbd "wH") #'flop-frame)
(define-key evil-normal-state-map (kbd "wJ") #'flip-frame)
(define-key evil-normal-state-map (kbd "w+") 'evil-window-increase-height)
(define-key evil-normal-state-map (kbd "w-") 'evil-window-decrease-height)
(define-key evil-normal-state-map (kbd "w(") 'evil-window-increase-width)
(define-key evil-normal-state-map (kbd "w)") 'evil-window-decrease-width)
(define-key evil-normal-state-map (kbd "%") #'split-window-below)
(define-key evil-normal-state-map (kbd "|") #'split-window-right)

;; Modes
(define-prefix-command 'modes)
(define-key evil-normal-state-map (kbd "fm") #'modes)
(define-key evil-normal-state-map (kbd "fml") #'lisp-mode)
(define-key evil-normal-state-map (kbd "fmo") #'org-mode)
(define-key evil-normal-state-map (kbd "fmc") #'clojure-mode)
(define-key evil-normal-state-map (kbd "fm <escape>") #'keyboard-escape-quit)

;; Flycheck
(define-key evil-normal-state-map (kbd "er") 'flycheck-list-errors)

;; Lisp
(add-hook
 'emacs-lisp-mode-hook
 #'(lambda ()
     (linum-mode 1)
     (define-key evil-normal-state-map (kbd "eb") 'eval-buffer)
     (define-key evil-normal-state-map (kbd "eb") 'eval-buffer)))

;; Clojure Cider
(add-hook
 'clojure-mode-hook
 #'(lambda ()
     (linum-mode 1)
     (define-key evil-normal-state-map (kbd "ec") 'cider-connect)
     (define-key evil-normal-state-map (kbd "eb") 'cider-eval-buffer)
     (define-key evil-normal-state-map (kbd "ee") 'cider-eval-list-at-point)
     (define-key evil-normal-state-map (kbd "ew") 'cider-repl-clear-buffer)
     (define-key evil-normal-state-map (kbd "ej") 'cider-find-var)
     (define-key evil-normal-state-map (kbd "ek") 'cider-pop-back)
     (define-key evil-normal-state-map (kbd "et") 'cider-test-run-test)
     (define-key evil-normal-state-map (kbd "eq") 'cider-quit)
     (define-key evil-normal-state-map (kbd "ed") 'cider-debug-defun-at-point)
     (define-key evil-normal-state-map (kbd "e <escape>") 'keyboard-escape-quit)))

;; Org Custom Evil
(add-hook
 'org-mode-hook
 #'(lambda ()
     (define-key evil-normal-state-map (kbd "L") 'org-cycle)
     (define-key evil-normal-state-map (kbd "R") 'org-export-dispatch)
     (define-key evil-normal-state-map (kbd ">") 'org-table-wrap-region)
     (define-key evil-normal-state-map (kbd "+") 'org-toggle-checkbox)
     (define-key evil-normal-state-map (kbd "RET") 'org-babel-execute-src-block)
     (define-key evil-normal-state-map (kbd "#") 'org-insert-structure-template)))

;; Python Custom
(add-hook
 'python-mode-hook
 #'(lambda ()
     '(jedi-mode)
     (linum-mode 1)
     (define-key evil-normal-state-map (kbd "ef") 'lsp-goto-type-definition)
     (define-key evil-normal-state-map (kbd "ej") 'jedi:goto-definition)
     (define-key evil-normal-state-map (kbd "ee") 'elpy-yapf-fix-code)
     (define-key evil-normal-state-map (kbd "ed") 'jedi:show-doc)))

;; (evil-ex-define-cmd "q[uit]" nil)

;;; keybindings.el ends here

;; custom patch
(fset 'evil-visual-update-x-selection 'ignore)
