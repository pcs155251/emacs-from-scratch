;; PERSONAL configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you ;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.


(setq user-full-name "Julian Lee"
      user-mail-address "pcs155251@gmail.com")

;; Load a custom theme
;; Requires Emacs 28
(load-theme 'modus-vivendi t)

;; Set default font face
;; (set-face-attribute 'default nil :font "Ligamononoki Nerd Font" :height 140)
(set-face-attribute 'default nil :family "Iosevka" :height 125 :width 'expanded)

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


;; (unless $package-archive-contents  (package-refresh-contents))
;;;; Add the NonGNU ELPA package archive
;;(require 'package)
;;(setq package-archives '(("gnu"    . "http://elpa.gnu.org/packages/")
;;                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
;;                         ("melpa"  . "http://melpa.org/packages/")))
;;
;;
;; Bootsrap use-package
(defvar emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the .emacs.d directory. Must end with a slash.")
(push (concat emacs-dir "lib/use-package/") load-path)
(eval-when-compile
  (require 'use-package))

;; load packages, currently use use-package, will change to elpaca
(use-package vertico
  :load-path "lib/vertico"
  :init
  (vertico-mode)
  ;; Enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  )

(use-package evil
  :load-path "lib/evil"
  :ensure t  ;; make sure the package is installed if not already
  :init  ;; code to execute before loading the package
  :config  ;; code to execute after loading the package
  (evil-mode 1)  ;; enable evil-mode
) 


;; packages
;; Completion framework
;; (unless (package-installed-p 'vertico)
;;   (package-install 'vertico))
;; 
;; ;; Enable completion by narrowing
;; (vertico-mode t)
;; 
;; ;; Improve directory navigation
;; (with-eval-after-load 'vertico
;;   (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
;;   (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
;;   (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char))
;; 
;; ;;; Extended completion utilities
;; (unless (package-installed-p 'consult)
;;   (package-install 'consult))
;; (global-set-key [rebind switch-to-buffer] #'consult-buffer)
;; (global-set-key (kbd "C-c j") #'consult-line)
;; (global-set-key (kbd "C-c i") #'consult-imenu)
;; (setq read-buffer-completion-ignore-case t
;;       read-file-name-completion-ignore-case t
;;       completion-ignore-case t)

;;;;; Vim Emulation
;;(unless (package-installed-p 'evil)
;;  (package-install 'evil))
;;
;;;; Enable Vim emulation
;;(evil-mode t)
;;
;;;; 
;;(unless (package-installed-p 'minion)
;;  (package-install 'minions))
;;
;;(unless (package-installed-p 'doom-modeline)
;;  (package-install 'doom-modeline))
;;(require 'doom-modeline)
;;(doom-modeline-mode 1)
;;
;;(unless (package-installed-p 'nerd-icons)
;;  (package-install 'nerd-icons))


;; Relative unimportant package
;;;;; LSP Support
;;(unless (package-installed-p 'eglot)
;;  (package-install 'eglot))
;;
;;;; Enable LSP support by default in programming buffers
;;(add-hook 'prog-mode-hook #'eglot-ensure)
;;
;;;;; Inline static analysis
;;;; Enabled inline static analysis
;;(add-hook 'prog-mode-hook #'flymake-mode)
;;
;;;;; Pop-up completion
;;(unless (package-installed-p 'corfu)
;;  (package-install 'corfu))
;;
;;;; Enable autocompletion by default in programming buffers
;;(add-hook 'prog-mode-hook #'corfu-mode)
;;
;;;;; Git client
;;(unless (package-installed-p 'magit)
;;  (package-install 'magit))
;;
;;;; Bind the `magit-status' command to a convenient key.
;;(global-set-key (kbd "C-c g") #'magit-status)
;;
;;;; Show word-granularity differences within diff hunks
;;(setq magit-diff-refine-hunk t)
;;
;;;;; Indication of local VCS changes
;;(unless (package-installed-p 'diff-hl)
;;  (package-install 'diff-hl))
;;
;;;; Enable `diff-hl' support by default in programming buffers
;;(add-hook 'prog-mode-hook #'diff-hl-mode)
;;
;;;;; In-Emacs Terminal Emulation
;;(unless (package-installed-p 'eat)
;;  (package-install 'eat))
;;
;;;; Close the terminal buffer when the shell terminates.
;;(setq eat-kill-buffer-on-exit t)
;;
;;;; Enable mouse-support.
;;(setq eat-enable-mouse t)
;;
;;
;;
;;
;;
;;
;;;; Miscellaneous options
;;(setq-default major-mode
;;              (lambda () ; guess major mode from file name
;;                (unless buffer-file-name
;;                  (let ((buffer-file-name (buffer-name)))
;;                    (set-auto-mode)))))
;;(setq confirm-kill-emacs #'yes-or-no-p)
;;(setq window-resize-pixelwise t)
;;(setq frame-resize-pixelwise t)
;;(save-place-mode t)
;;(savehist-mode t)
;;(recentf-mode t)
;;(defalias 'yes-or-no-p #'y-or-n-p)

