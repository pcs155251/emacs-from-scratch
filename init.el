;; PERSONAL configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you ;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.


(setq user-full-name "Julian Lee"
      user-mail-address "pcs155251@gmail.com")


;; Set relative line
(setq display-line-numbers-type `relative)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable scroll bar
(scroll-bar-mode -1)

;; Enable line numbering by default
(global-display-line-numbers-mode t)

;; Automatically pair parentheses
(electric-pair-mode t)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Maximize when start
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default line-spacing 0.2)

;; Better line warp setting without change line within same word
(global-visual-line-mode t)

;; elpaca package management
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t)
)

;; Block until current queue processed.
(elpaca-wait)

;; Set default font face
;; (set-face-attribute 'default nil :font "Ligamononoki Nerd Font" :height 140)
;; (set-face-attribute 'default nil :family "JetBrains Mono" :height 100)
(set-face-attribute 'default nil :family "Iosevka" :height 120 :width 'expanded :weight 'semi-light)

; (set-face-attribute 'fixed-pitch nil :family "JetBrains Mono" :height 100)
(set-face-attribute 'fixed-pitch nil :family "Iosevka" :height 120 :width 'expanded :weight 'semi-light)

(set-face-attribute 'variable-pitch nil :family "Helvetica" :weight 'light :height 160)
;; (set-face-attribute 'variable-pitch nil :family "NewComputerModern" :weight 'light :height 160)


;; Vim bindings
(use-package evil
  :demand t
  :init
  ;; allows for using cgn
  (setq evil-search-module 'evil-search)
  (setq evil-want-keybinding nil)
  ;; no vim insert bindings
  ;; (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
)
(use-package evil-collection
  :after evil
  :init (evil-collection-init)
)


; Vertical completion better than default
(use-package vertico
  :demand t
  :bind (:map vertico-map
           ("C-j" . vertico-next)
           ("C-k" . vertico-previous)
           :map minibuffer-local-map
        )
  :custom (vertico-cycle t) ;; Enable cycling for `vertico-next' and `vertico-previous'.
  :init (vertico-mode)
)

; consult
(use-package consult
  :ensure t
  ;:bind (("C-x b" . consult-buffer)  ;; example keybinding
  ;       ("C-x M-:" . consult-apropos))  ;; another example keybinding
  :config
  ;; Optionally configure preview. Note that the default value
  ;; of `consult-preview-key` is 'any, which means that as long
  ;; as any key is pressed, the preview is shown.
  ;; If you prefer to disable previews, set the variable to nil.
  ;(setq consult-vertico-cycle t)
  (setq consult-preview-key 'any)
)

;; Magit, git for emacs
(use-package magit
  :bind (("C-x g" . magit))
)

; Org mode settings
;; auto indent
(electric-indent-mode 0)
(setq evil-auto-indent nil)
(setq org-hide-leading-stars nil
      org-indent-mode-turns-on-hiding-stars nil
      org-adapt-indentation 0
      ;;org-indent-mode-turns-off-org-adapt-indentation nil
      org-preview-latex-default-process 'dvisvgm
      org-indent-indentation-per-level 2
      org-hide-emphasis-markers nil
      org-fontify-todo-headline t
      org-fontify-done-headline t
      ;; org image settings
      ;;org-display-inline-images t
      ;;org-redisplay-inline-images t
      ;;org-startup-with-inline-images "inlineimages"
      ;; org mode latex preview when start up
      ;;org-startup-with-latex-preview 't
      ;; org-format-latex-options (plist-put org-format-latex-options :scale 1.1 )
      ;; org-format-latex-options (plist-put org-format-latex-options :background "Transparent")
      org-startup-folded 'showall
      org-startup-indented t
      org-todo-keywords
      '((sequence "TODO(t)" "NOW(n)" "WAITING(w)" "SOMEDAY(s)" "CANCELED(c)" "|" "DONE(d)" ))
      org-todo-keyword-faces '(
               ("NOW"       . (:inherit variable-pitch :foreground "#d4a052" ))
               ("WAITING"   . (:inherit variable-pitch :foreground "#d4a052" ))
               ("SOMEDAY"   . (:inherit variable-pitch :foreground "#d4a052" ))
               ("CANCELED"  . (:inherit variable-pitch :foreground "#d4a052" ))
      )
      system-time-locale "en_US.UTF-8"
)

(defvar my-color "#d4a052")

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook (lambda () (setq-local line-spacing 0.8)))
(defun my-ef-themes-custom-faces ()
  "My customizations on top of the Ef themes.
This function is added to the `ef-themes-post-load-hook'."
(defvar org-heading-foreground (face-foreground 'font-lock-keyword-face))
(defface org-heading-face
  `((t (:inherit variable-pitch :family "NewComputerModern" :font "NewComputerModern" :foreground ,org-heading-foreground)))
  "My custom face that inherits from org-level-1 and uses the foreground color of org-level-2.")
(ef-themes-with-colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(line-number ((t (:inherit (shadow fixed-pitch)))))
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit fixed-pitch))))
 '(org-document-info ((t (:inherit variable-pitch :foreground "#eaedef"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-todo ((t (:inherit variable-pitch :height 1.0))))
 '(org-done ((t (:inherit variable-pitch :height 1.0))))
 '(org-drawer ((t (:inherit (shadow fixed-pitch)))))
 '(org-headline-done ((t (:height 1.0 :foreground "#eaedef" :strike-through t))))
 '(org-headline-todo ((t (:height 1.0 :foreground "#eaedef"))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-document-title ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 2.0  :foreground "#eaedef"))))
 '(org-level-1        ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 1.4  :foreground "#eaedef"))))
 '(org-level-2        ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 1.35 :foreground "#eaedef"))))
 '(org-level-3        ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 1.3  :foreground "#eaedef"))))
 '(org-level-4        ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 1.25 :foreground "#eaedef"))))
 '(org-level-5        ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 1.2  :foreground "#eaedef"))))
 '(org-level-6        ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 1.15 :foreground "#eaedef"))))
 '(org-level-7        ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 1.1  :foreground "#eaedef"))))
 '(org-level-8        ((t (:inherit (variable-pitch) :font "NewComputerModern" :height 1.05 :foreground "#eaedef"))))
 '(org-tag ((t (:inherit variable-pitch))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-table ((t (:inherit fixed-pitch :foreground "#eaedef"))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-code ((t (:inherit fixed-pitch :height 1.2 ))))
 '(org-verbatim ((t (:inherit fixed-pitch :height 1.2 ))))
 )
))

(setq org-hide-leading-stars nil
      org-indent-mode-turns-on-hiding-stars nil
      org-adapt-indentation 0
      ;;org-indent-mode-turns-off-org-adapt-indentation nil
      org-indent-indentation-per-level 0
      org-hide-emphasis-markers nil
      org-fontify-todo-headline t
      org-fontify-done-headline t
      ;; org image settings
      org-display-inline-images t
      org-redisplay-inline-images t
      org-startup-with-inline-images "inlineimages"
      ;; org mode latex preview when start up
      ;;; org latex repiew settings
      org-preview-latex-default-process 'dvisvgm
      org-startup-with-latex-preview 't
      ;org-format-latex-options (plist-put org-format-latex-options :background "Transparent")
      ;;
      ;; this is code is this
      org-startup-folded 'showall
      org-startup-indented t
      org-todo-keywords
      '((sequence "TODO(t)" "DOING(n)" "WAITING(w)"  "SOMEDAY(s)" "|" "CANCELED(c)" "DONE(d)" ))
      org-todo-keyword-faces '(
               ("DOING"    . (:inherit variable-pitch :foreground "#d4a052" ))
               ("WAITING"  . (:inherit variable-pitch :foreground "#d4a052" ))
               ("SOMEDAY"  . (:inherit variable-pitch :foreground "#d4a052" ))
               ("CANCELED" . (:inherit org-done))
      )
      system-time-locale "en_US.UTF-8"
)

(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (global-set-key (kbd "C-x q") 'org-latex-preview)
)


;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
;;(use-package evil :demand t)
;;
;;;;Turns off elpaca-use-package-mode current declartion
;;;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;;;Useful for configuring built-in emacs features.
;;(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))
;;
;;;; Don't install anything. Defer execution of BODY
;;(elpaca nil (message "deferred"))




;; (unless $package-archive-contents  (package-refresh-contents))
;;;; Add the NonGNU ELPA package archive
;;(require 'package)
;;(setq package-archives '(("gnu"    . "http://elpa.gnu.org/packages/")
;;                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;                         ("melpa"  . "http://melpa.org/packages/")))
;;
;;
;; Bootsrap use-package
;;(defvar emacs-dir			
;;  (eval-when-compile (file-truename user-emacs-directory))
;;  "The path to the .emacs.d directory. Must end with a slash.")
;;(push (concat emacs-dir "lib/use-package/") load-path)
;;(eval-when-compile
;;  (require 'use-package))
;;
;;;; load packages, currently use use-package, will change to elpaca




;; packages to try
;; 'magit
;; 'corfu
;; 'minions
;; 'diff-hl
;; 'vterm
;; 'ag
;; 'doom-modeline
;; 'nerd-icons
;; 'eglot

;; backup packages
;; bind-key
;; compat
;; elpaca-use-package
;; evil 
;; goto-chg 
;; use-package 
;; vertico 

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(setq confirm-kill-emacs #'yes-or-no-p)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(save-place-mode t)
(savehist-mode t)
(recentf-mode t)
(defalias 'yes-or-no-p #'y-or-n-p)

;;
(require 'recentf)
(setq recentf-max-menu-items 25)
(recentf-mode 1)


;; load ef-themes
(add-hook 'ef-themes-post-load-hook #'my-ef-themes-custom-faces)
(use-package ef-themes
  :init
  (setq ef-themes-to-toggle '(ef-maris-dark ef-summer))
  (setq ef-themes-mixed-fonts t)
  (ef-themes-select 'ef-maris-dark)
)
