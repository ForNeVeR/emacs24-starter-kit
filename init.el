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

(add-to-list 'package-pinned-packages '(minimap . "gnu") t)

(setq package-list '(auto-complete
                     batch-mode
                     cask-package-toolset
                     coffee-mode
                     cil-mode
                     editorconfig
                     elixir-mode
                     fsharp-mode
                     gradle-mode
                     graphviz-dot-mode
                     gruber-darker-theme
                     haskell-mode
                     less-css-mode
                     markdown-mode
                     minimap
                     multiple-cursors
                     neotree
                     nix-mode
                     paredit
                     powershell
                     purescript-mode
                     rust-mode
                     tabbar
                     tss
                     yaml-mode))

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Encoding fix in PowerShell buffers:
(add-to-list 'process-coding-system-alist
             '("powershell.exe" . (cp866-dos . cp866-dos)))

;; Enable file modes:
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode) t)

;; Fix the auto-fill-mode starting in the starter kit:
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

;; neotree:
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; linum-mode:
(global-linum-mode 1)

;; proof general
(load "~/.emacs.d/3rd-party/proofgeneral/generic/proof-site.el")
(load "~/.emacs.d/3rd-party/pg-ssr.el")

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
(scroll-bar-mode -1)

;; Tabbar:
(require 'tabbar)

(global-set-key [C-S-tab] 'tabbar-backward-tab)
(global-set-key [C-tab] 'tabbar-forward-tab)

(set-face-foreground 'tabbar-default "LightSteelBlue")
(set-face-background 'tabbar-default "DarkSlateGray")
(set-face-foreground 'tabbar-selected "pale green")
(set-face-bold-p 'tabbar-selected t)
(set-face-attribute 'tabbar-button nil :box '(:line-width 1 :color "gray72"))

(setq tabbar-buffer-groups-function
      (lambda () 
        (list
         (cond
          ((find (aref (buffer-name (current-buffer)) 0) " *") "*")
          (t "All Buffers"))
         )))

(tabbar-mode)

;; Global editor settings:
(setq default-file-name-coding-system 'cp1251)
(setq file-name-coding-system 'cp1251)

(setq-default indent-tabs-mode t)
(setq default-tab-width 4)

(setq default-input-method "russian-computer")
(set-face-attribute 'default t :font "Liberation Mono-13")
(add-to-list 'default-frame-alist '(font . "Liberation Mono-13"))
(set-fontset-font
 nil '(#x1d539 . #x1d539) (font-spec :family "DejaVu Sans"))

;; auto-complete:
(require 'auto-complete-config)
(ac-config-default)
(global-set-key (kbd "C-SPC") 'auto-complete)

;;; ForNeVeR configuration ends here

;;; init.el ends here
