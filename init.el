;; PERSONAL configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you ;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.


;; TODO Combine face settings with ef-theme
;; TODO Meta key wierd, vertico delete one word
;; TODO Org mode
;; TODO Font face
;; TODO Ivy completion with recentf
;; TODO Separate init.el
;; TODO Elpaca add repos, fix version
;; TODO Git graph?
;; TODO Key binding conflict? search all key bindings
;; TODO Shortcut to restart emacs


(setq user-full-name "Julian Lee"
      user-mail-address "pcs155251@gmail.com")


;; Set relative line
(setq display-line-numbers-type `relative)

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

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
(global-visual-line-mode nil)

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


;; load ef-themes
(use-package ef-themes
  :init
  (setq ef-themes-to-toggle '(ef-maris-dark ef-summer))
  (setq ef-themes-headings ; read the manual's entry or the doc string
    '(
       ; absence of weight means `bold'
       (0 variable-pitch regular 2.0)
       (1 variable-pitch regular 1.4)
       (2 variable-pitch regular 1.35)
       (3 variable-pitch regular 1.3)
       (4 variable-pitch regular 1.25)
       (5 variable-pitch regular 1.2) 
       (6 variable-pitch regular 1.15)
       (7 variable-pitch regular 1.1)
       (8 variable-pitch regular 1.05)
       (t variable-pitch regular 1.)
    )
  )
  (setq ef-themes-mixed-fonts t)
  (ef-themes-select 'ef-maris-dark)
)

;; Set default font face
;; (set-face-attribute 'default nil :font "Ligamononoki Nerd Font" :height 140)
(set-face-attribute 'default nil :family "Iosevka" :height 125 :width 'expanded)


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


;; Vertical completion better than default
(use-package vertico
  :demand t
  :bind (:map vertico-map
           ("C-j" . vertico-next)
           ("C-k" . vertico-previous)
           ("C-f" . vertico-exit)
           :map minibuffer-local-map
           ;;("M-h" . backward-kill-word)
        )
  :custom (vertico-cycle t) ;; Enable cycling for `vertico-next' and `vertico-previous'.
  :init (vertico-mode)
)


;; Magit, git for emacs
(use-package magit
  :bind (("C-x g" . magit))
)

;; Org mode settings
;; (add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook (lambda () (setq-local line-spacing 0.8)))


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
;; evil
;; goto-chg 
;; goto-chg 
;; use-package 
;; use-package 
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
(recentf-mode 1)
