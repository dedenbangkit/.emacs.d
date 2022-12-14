;;; helm-fzf.el --- helm binding for FZF

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Ivan Buda Mandura (ivan.mandura93@gmail.com)

;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: helm fzf

;;; Commentary:

;;; Code:

(defvar custom-package-list-for-fzf
  '(helm
    s
    dash))

;; Scans the list in myPackages
;; install the missing packages
(dolist (package custom-package-list-for-fzf)
  (unless (package-installed-p package)
    (package-install package)))

(require 'helm)
(require 'helm-files)
(require 's)
(require 'dash)

(defcustom helm-fzf-executable "fzf"
  "Default executable for fzf"
  :type 'stringp
  :group 'helm-fzf)

(defun helm-fzf--project-root ()
  (cl-loop for dir in '(".git/" ".hg/" ".svn/" ".git")
           when (locate-dominating-file default-directory dir)
           return it))

(defvar helm-fzf-source
  (helm-build-async-source "fzf"
    :candidates-process 'helm-fzf--do-candidate-process
    :filter-one-by-one 'identity
    :requires-pattern 3
    :action 'helm-find-file-or-marked
    :candidate-number-limit 9999))

(defun helm-fzf--do-candidate-process ()
  (let* ((cmd-args (-filter 'identity (list helm-fzf-executable
                                            "--no-sort"
                                            "-f"
                                            helm-pattern)))
         (proc (apply 'start-file-process "helm-fzf" helm-buffer cmd-args)))
    (prog1 proc
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory)))))))

;;;###autoload
(defun helm-fzf (directory)
  (interactive "D")
  (let ((default-directory directory))
    (helm :sources '(helm-fzf-source)
          :buffer "*helm-fzf*")))

(defun helm-fzf-project-root ()
  (interactive)
  (let ((default-directory (helm-fzf--project-root)))
    (unless default-directory
      (error "Could not find the project root."))
    (helm :sources '(helm-fzf-source)
          :buffer "*helm-fzf*")))

(provide 'helm-fzf)

(defun custom-filter-buffers (buffer-list)
  (delq nil (mapcar
             (lambda (buffer)
               (cond
                    ((eq (with-current-buffer buffer major-mode)  'dired-mode) nil)
                    ((eq (with-current-buffer buffer major-mode)  'org-mode) nil)
                    ((eq (with-current-buffer buffer major-mode)  'org-agenda-mode) nil)
                    (t buffer)))
             buffer-list)))

(advice-add 'helm-skip-boring-buffers :filter-return 'custom-filter-buffers)

;;; helm-fzf.el ends here
