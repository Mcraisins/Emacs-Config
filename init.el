;; Package --- Summary

;;; Basic stuff
;;; -------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(setq gc-cons-threshold 100000000)

(setq backup-directory-alist `(("." . "~/.saves")))

;;; Graphical
;;; -------------------------
(require 'moe-theme)
(moe-dark)
(moe-theme-set-color 'purple)

(require 'powerline)
(powerline-moe-theme)

(require 'linum-relative)
(linum-relative-toggle)

;;; Utility
;;; -------------------------
(load-file "~/.emacs.d/setup-helm.el")

(require 'batch-mode)
(add-hook 'bat-mode-hook 'batch-mode)

(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

(require 'key-chord)
(key-chord-mode 1)

(require 'iedit)

(require 'button-lock)
(eval-after-load "button-lock" (require 'fixmee))
(eval-after-load "fixmee" (global-fixmee-mode 1))

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(key-chord-define-global "s/" 'ace-jump-word-mode)
(key-chord-define-global "s'" 'ace-jump-char-mode)

(require 'bongo)
(require 'volume)

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

(define-auto-insert
  (cons "\\.\\([Cc]\\|cc\\|cpp\\)\\'" "C++ documentation")
  '(nil
    "/* "(make-string 70 ?=)"\n"
    "   "(file-name-nondirectory buffer-file-name)"\n"
    "   Time-stamp: <> \n"
    "   Description:\n"
    "    \n"
    "   "(make-string 70 ?=)" */\n\n"
    (let* ((noext (substring buffer-file-name 0 (match-beginning 0)))
           (nopath (file-name-nondirectory noext))
           (ident (concat nopath ".h")))
      (if (file-exists-p ident)
          (concat "#include \""ident"\"\n")))
    ))

(add-hook 'find-file-hook 'auto-insert)

;;; Auto-completion
;;; -------------------------
(require 'irony)
(add-hook 'c-mode-common-hook 'irony-mode)

(defun my-irony-mode-hook () "Turn on Irony completion."
       (define-key irony-mode-map [remap completion-at-point]
         'irony-completion-at-point-async)
       (define-key irony-mode-map [remap complete-symbol]
         'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

(require 'company)
(require 'company-irony)
(require 'company-irony-c-headers)

(setq company-idle-delay 0.2
      company-backends '(company-irony-c-headers company-irony company-yasnippet company-files company-keywords))
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key [C-return] 'company-complete-common)

(require 'flycheck)
(require 'helm-flycheck)
;;(require 'flycheck-clangcheck)
(eval-after-load 'flycheck
  '(progn
     (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck)
     (add-hook 'after-init-hook #'global-flycheck-mode)
     (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;;(setq flycheck-clangcheck-analyze nil)

;;;org
;;;----
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t
      org-enforce-todo-dependencies t
      org-enforce-todo-checkbox-dependencies t
      org-startup-indented t
      org-agenda-files (list
                        "C:/Users/Ryan Lira/Dropbox/Orgzly/games.org"
                        "C:/Users/Ryan Lira/Dropbox/Orgzly/todo.org"
                        ))
(org-babel-do-load-languages
 'org-babel-load-languages '((sh . C)))

;;; Code:
;;;-------
(toggle-frame-maximized)
(setq default-frame-alist '((font . "Bitstream Vera Sans Mono 9")))
(global-set-key (kbd "<tab>") #'self-insert-command)
(global-set-key (kbd "RET") 'newline-and-indent)
(set-frame-font "Bitstream Vera Sans Mono 9")
(add-hook 'c-mode-common-hook 'hs-minor-mode)
(add-hook 'c-mode-common-hook
          (lambda()
            (c-set-offset 'substatement-open 0)))
(add-hook 'before-save-hook 'time-stamp)
(setq-default indent-tabs-mode nil
              tab-width 4
              indent-line-function 'insert-tab
              c-tab-always-indent nil
              next-line-add-newlines t)
(fset 'yes-or-no-p 'y-or-n-p)
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOM SET ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(bongo-enabled-backends (quote (vlc)))
 '(bongo-mode-line-icon-color "black")
 '(c-basic-offset (quote set-from-style))
 '(c-default-style
   (quote
    ((c-mode . "bsd")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(c-label-minimum-indentation 4)
 '(column-number-mode t)
 '(global-linum-mode t)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(org-agenda-custom-commands
   (quote
    (("i" "Index"
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
                 (quote
                  ("+SCHOOL")))))
       (todo "HW" nil))
      nil nil))))
 '(org-capture-templates
   (quote
    (("n" "New Task" entry
      (file+headline "~/org/todo.org" "Scratch")
      (file "~/org/tpl-todo.org")))))
 '(org-log-into-drawer t)
 '(org-use-property-inheritance t)
 '(package-selected-packages
   (quote
    (company-irony-c-headers company-irony flycheck-irony irony batch-mode color-theme-wombat ninja-mode fixmee flycheck-clangcheck rtags helm-projectile projectile company-c-headers volume bongo helm-emms emms yasnippet symon sr-speedbar relative-line-numbers powerline paredit org moe-theme linum-relative key-chord iedit helm-swoop helm-gtags helm-flymake helm-flycheck google-c-style ggtags flycheck-cstyle flycheck-color-mode-line ecb company-quickhelp cmake-mode cider cedit buffer-stack auto-complete-clang auto-complete-c-headers ace-jump-mode)))
 '(ps-line-number nil)
 '(safe-local-variable-values
   (quote
    ((eval setq flycheck-gcc-include-path
           (list
            (expand-file-name "D:inlibglewinclude" "D:inlibglfwinclude" "D:inlibglm" "D:inpitinclude")))
     (eval setq flycheck-clang-include-path
           (list
            (expand-file-name "D:inlibglewinclude" "D:inlibglfwinclude" "D:inlibglm" "D:inpitinclude")))
     (eval setq cmake-ide-project-dir
           (concat patherino ""))
     (cmake-ide-project-dir concat patherino "")
     (eval setq cmake-ide-build-dir
           (concat patherino "build"))
     (cmake-ide-project-dir . patherino)
     (eval set
           (make-local-variable
            (quote patherino))
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (eval setq cmake-ide-build-dir
           (concat
            (quote my-project-path)
            "build"))
     (cmake-ide-project-dir quote my-project-path)
     (eval set
           (make-local-variable
            (quote my-project-path))
           (file-name-directory
            (dir-locals-find-file ".")))
     (eval setq cmake-ide-build-dir
           (concat my-project-path "build"))
     (cmake-ide-project-dir . my-project-path)
     (eval set
           (make-local-variable
            (quote my-project-path))
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d))))))))
 '(show-paren-delay 0)
 '(show-paren-mode 1)
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
