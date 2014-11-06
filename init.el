;;; init.el --- Where all the magic begins
;;
;; Part of the Emacs Starter Kit
;;
;; This is the first thing to get loaded.
;;

;; load Org-mode from source when the ORG_HOME environment variable is set
(when (getenv "ORG_HOME")
  (let ((org-lisp-dir (expand-file-name "lisp" (getenv "ORG_HOME"))))
    (when (file-directory-p org-lisp-dir)
      (add-to-list 'load-path org-lisp-dir)
      (require 'org))))

;; load the starter kit from the `after-init-hook' so all packages are loaded
(add-hook 'after-init-hook
 `(lambda ()
    ;; remember this directory
    (setq starter-kit-dir
          ,(file-name-directory (or load-file-name (buffer-file-name))))
    ;; only load org-mode later if we didn't load it just now
    ,(unless (and (getenv "ORG_HOME")
                  (file-directory-p (expand-file-name "lisp"
                                                      (getenv "ORG_HOME"))))
       '(require 'org))
    ;; load up the starter kit
    (org-babel-load-file (expand-file-name "starter-kit.org" starter-kit-dir))))

;;; ForNeVeR configuration

(setq package-enable-at-startup nil)
(package-initialize)

(require 'cl)
(require 'package)

(server-start)

(setq package-list '(batch-mode
                     coffee-mode
                     graphviz-dot-mode
                     gruber-darker-theme
                     haskell-mode
                     multiple-cursors
                     paredit
                     powershell
                     rust-mode))

(setq package-archives '(("elpa" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(add-to-list 'process-coding-system-alist
             '("powershell.exe" . (cp866-dos . cp866-dos)))

;; Enable shift-arrow selection:
(setq shift-select-mode t)

;; Fix the auto-fill-mode starting in the starter kit:
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; linum-mode:
(global-linum-mode 1)

;; powershell-mode
;(let ((current-directory (file-name-directory load-file-name)))
;  (add-to-list 'load-path current-directory))
;(load "PowerShell-Mode.el")
;(add-to-list 'auto-mode-alist '("\\.ps1\\'" . powershell-mode))
;(add-to-list 'auto-mode-alist '("\\.psm1\\'" . powershell-mode))
(add-to-list 'auto-mode-alist '("\\.cmd\\'" . batch-mode))
(add-to-list 'auto-mode-alist '("\\.bat\\'" . batch-mode))

;; proof general
(load "~/.emacs.d/3rd-party/proofgeneral/generic/proof-site.el")

;; SLIME
;(add-to-list 'load-path (concat (file-name-directory load-file-name) "slime-2.9"))
;(load "slime-2.9/slime-autoloads.el")

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "M-C-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-C-<down>") 'mc/mark-next-like-this)

;; cua-mode
(cua-mode)

;; Theme and menus:
(load-theme 'gruber-darker t)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Global editor settings:
(setq default-file-name-coding-system 'cp1251)
(setq file-name-coding-system 'cp1251)

(setq-default indent-tabs-mode t)
(setq default-tab-width 4)

(setq default-input-method "russian-computer")
(set-face-attribute 'default t :font "Liberation Mono-10")
(add-to-list 'default-frame-alist '(font . "Liberation Mono-10"))
(set-fontset-font
 nil '(#x1d539 . #x1d539) (font-spec :family "DejaVu Sans"))

;;; ForNeVeR configuration ends here

;;; init.el ends here
