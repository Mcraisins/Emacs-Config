;; Package --- Summary

;;; Basic stuff
;;; -------------------------
(require 'package)
(setq package-enable-at-startup nil)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

(setq use-package-verbose t)
(setq use-package-always-ensure t)

(setq gc-cons-threshold 100000000)

;;; Backup
;;; ------------------------
(setq version-control t)
(setq vc-make-backup-files t)
(setq delete-old-versions -1)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist `(("." . "~/.saves")))

;;; Graphical
;;; -------------------------
(use-package moe-theme
  :config
  (progn
    (moe-dark)
    (moe-theme-set-color 'purple)))

(use-package powerline
  :config
  (progn
    (powerline-moe-theme)))

(use-package linum-relative
  :config
  (progn
    (linum-relative-toggle)))

(tool-bar-mode -1)
(display-time-mode 1)
(blink-cursor-mode -1)

;;; CMake
;;; -----------------------
(use-package cmake-mode)

;;; Dired
;;; ------------------------
(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-lX --group-directories-first")
  (use-package diredfl
    :config (diredfl-global-mode 1)))

(use-package dired-git-info
  :bind (:map dired-mode-map ("z" . dired-git-info-mode)))

;;; Helm
;;; ------------------------
(use-package helm
  :init
  (progn
    (require 'helm-config)
    (use-package helm-swoop)
    (setq helm-candidate-number-limit 100
          helm-idle-delay 0.0
          helm-input-idle-delay 0.01

          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t
          helm-buffers-fuzzy-matching t
          helm-move-to-line-cycle-in-source t
          helm-split-window-in-side-p t
      )
    (helm-mode))
  :bind (("M-x" . helm-M-x)
     ("M-y" . helm-show-kill-ring)
     ("C-x b" . helm-buffers-list)
     ("C-x C-b" . helm-mini)
     ("C-h a" . helm-apropos)
     ("C-x C-f" . helm-find-files)
     ("C-x c o" . helm-occur)
     ("C-x c s" . helm-swoop)
     ("C-x c S" . helm-multi-swoop-all)
     ("C-x c y" . helm-yas-complete)
     ("C-x c Y" . helm-yas-create-snippet-on-region)
     :map helm-map
     ("<tab>" . helm-execute-persistent-action)
     ("C-i" . helm-execute-persistent-action)
     ("C-z" . helm-select-action)
     )
  )
(ido-mode -1)

;;; Utility
;;; -------------------------
(use-package yasnippet
  :config
  (progn
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-global-mode 1)))

(use-package key-chord
  :config
  (progn (key-chord-mode 1)))

(use-package iedit)

(use-package ace-jump-mode
  :init
  (progn
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    (key-chord-define-global "s/" 'ace-jump-word-mode)
    (key-chord-define-global "s'" 'ace-jump-char-mode)))

(use-package smartparens
  :init
  (progn
    (require 'smartparens-config))
  :config
  (progn
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;;;Auto-include header guards
;;;---------------------------
(define-auto-insert
  (cons "\\.\\([Hh]\\|hh\\|hpp\\)\\'" "C/C++ header")
  '(nil
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat (upcase nopath) "_H_")))
      (concat "#ifndef " ident "\n"
              "#define " ident "\n\n\n"
              "\n\n#endif // " ident "\n"))
    ))

(add-hook 'find-file-hook 'auto-insert)

;;; Auto-completion
;;; -------------------------
(use-package company
  :config
  (setq company-idle-delay 0.2)
  :hook ((prog-mode) . (company-mode))
  :bind (("<C-return>" . company-complete-common)))

(use-package flycheck)
(use-package helm-flycheck)

(use-package lsp-mode :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp
  :config (push 'company-lsp company-backends))

(use-package ccls
  :config
  (setq ccls-executable "ccls")
  (setq lsp-prefer-flymake nil)
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

;;;org
;;;----
(use-package org
  :init
  (progn
    (setq org-log-done t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-startup-indented t
      org-agenda-files (list
                ))
    (org-babel-do-load-languages
     'org-babel-load-languages '((shell . C))))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)))

;;; Code:
;;;-------
;(toggle-frame-maximized)
(setq default-frame-alist '((font . "Bitstream Vera Sans Mono 9")))
(set-frame-font "Bitstream Vera Sans Mono 9")
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook
          (lambda()
            (c-set-offset 'substatement-open 0)))
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default tab-width 4
              indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM SET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bongo-enabled-backends '(vlc))
 '(bongo-mode-line-icon-color "black")
 '(c-basic-offset 4)
 '(c-default-style
   '((c-mode . "bsd")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(c-label-minimum-indentation 4)
 '(column-number-mode t)
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(org-agenda-custom-commands
   '(("i" "Index"
      ((tags "INDEXED" nil))
      nil nil)
     ("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("u" "Urgent Tasks & Appointments"
      ((tags "URGENT" nil)
       (tags "APPOINTMENT" nil))
      nil nil)
     ("h" "Homework & Assignments"
      ((agenda ""
               ((org-agenda-tag-filter-preset
                 '("+SCHOOL"))))
       (todo "HW" nil))
      nil nil)))
 '(org-capture-templates
   '(("n" "New Task" entry
      (file+headline "~/org/todo.org" "Scratch")
      (file "~/org/tpl-todo.org"))))
 '(org-log-into-drawer t)
 '(org-use-property-inheritance t)
 '(package-selected-packages
   '(company ccls helm-lsp magit lsp-ui company-lsp batch-mode color-theme-wombat ninja-mode fixmee flycheck-clangcheck auto-indent-mode rtags helm-projectile projectile company-c-headers volume bongo helm-emms emms yasnippet symon sr-speedbar relative-line-numbers powerline paredit org moe-theme linum-relative key-chord iedit helm-swoop helm-gtags helm-flymake helm-flycheck google-c-style ggtags flycheck-irony flycheck-cstyle flycheck-color-mode-line ecb company-quickhelp cmake-mode cmake-ide cider cedit buffer-stack auto-complete-clang auto-complete-c-headers aggressive-indent ace-jump-mode))
 '(ps-line-number nil)
 '(safe-local-variable-values
   '((eval setq cmake-ide-project-dir
           (concat patherino ""))
     (cmake-ide-project-dir concat patherino "")
     (eval setq cmake-ide-build-dir
           (concat patherino "build"))
     (cmake-ide-project-dir . patherino)
     (eval set
           (make-local-variable 'patherino)
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (eval setq cmake-ide-build-dir
           (concat 'my-project-path "build"))
     (cmake-ide-project-dir quote my-project-path)
     (eval set
           (make-local-variable 'my-project-path)
           (file-name-directory
            (dir-locals-find-file ".")))
     (eval setq cmake-ide-build-dir
           (concat my-project-path "build"))
     (cmake-ide-project-dir . my-project-path)
     (eval set
           (make-local-variable 'my-project-path)
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))))
 '(show-paren-delay 0)
 '(symon-delay 0)
 '(tool-bar-mode nil)
 '(transient-mark-mode 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; Commentary:
;; Certain graphical features do not work with the daemon on init
;; As such those features will be moved to init2.el which is ran after this file

(provide 'init)
;;; init.el ends here
