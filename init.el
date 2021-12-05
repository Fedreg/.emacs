; increase garbace collector threshold for startup
(setq gc-cons-threshold 800000000)
(setq inhibit-compacting-font-caches t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; List packages you want to install
(setq package-list '(ag
		     cider
		     company
		     evil
		     evil-collection
		     evil-commentary
		     evil-escape
		     evil-leader
		     fzf
		     magit
		     rainbow-delimiters
		     smartparens
		     typit
		     undo-fu
		     xclip))

;; Package Repos
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; Activate all the packages (in particular autoloads)
(package-initialize)

;; Update your local package index
(unless package-archive-contents
  (package-refresh-contents))

;;Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edit-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil Mode (vim)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq evil-undo-system 'undo-fu)
(setq evil-want-keybinding nil)

(require 'evil)
(evil-mode t)
(evil-collection-init)
(evil-commentary-mode)

(evil-global-set-key 'normal (kbd "TAB") 'evil-jump-item)
(evil-global-set-key 'visual (kbd "TAB") 'evil-jump-item)
(evil-global-set-key 'motion (kbd "TAB") 'evil-jump-item)
(evil-global-set-key 'normal (kbd "C-z") 'suspend-emacs)
(evil-global-set-key 'normal (kbd "M-h") 'help-command)
(evil-global-set-key 'normal (kbd "C-j") 'evil-window-down)
(evil-global-set-key 'normal (kbd "C-k") 'evil-window-up)
(evil-global-set-key 'normal (kbd "C-h") 'evil-window-left)
(evil-global-set-key 'normal (kbd "C-l") 'evil-window-right)
(evil-global-set-key 'normal (kbd "C-]") 'go-forward)
(evil-global-set-key 'normal (kbd "C-t") 'pop-back)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")

(require 'evil-escape)
(evil-escape-mode 1)
(setq-default evil-escape-key-sequence "jk")

(evil-leader/set-key
  "e v" 'edit-init-file
  "f"   'ag-project
  "b"   'switch-to-buffer
  "g s" 'magit-status
  "g b" 'magit-blame-echo
  "l"   'switch-to-previous-buffer
  "o"   'org-cycle
  "s"   'save-buffer
  "w x" 'evil-quit
  "w v" 'evil-window-vsplit
  "w h" 'evil-window-split
  "w =" 'balance-windows
  "v"   'ido-switch-buffer
  "z"   'fzf-git-files)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
  Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look & Feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defvar bg "#162230")
  (load-theme 'tsdh-dark t)
  (set-face-background 'mode-line bg)
  (set-face-foreground 'mode-line "#bbb")
  (set-face-background 'mode-line-inactive bg)
  (set-face-foreground 'mode-line-inactive bg)
  (set-face-background 'default bg)
  (set-cursor-color "#f00") 
  (set-face-attribute 'error nil :foreground "red")
  (set-face-attribute 'mode-line-buffer-id nil :foreground "DeepSkyBlue1")
  (set-face-background 'vertical-border "#222")
  (set-face-foreground 'vertical-border "#333")) ;"DeepSkyBlue1"))

;; y or n for prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Gui
(tool-bar-mode -1)
(menu-bar-mode -1)
;; (toggle-scroll-bar -1) 
(setq ns-auto-hide-menu-bar t)

;; No startup banner
(setq inhibit-startup-message t)

(setq initial-major-mode 'fundamental-mode)

;; Font
(add-to-list 'default-frame-alist '(font . "Menlo-16"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Ido Mode (default emacs narrowing; comes with Emacs)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; No Sounds
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ag-highlight-search t)
(setq ag-reuse-buffers 't)
(add-hook 'ag-mode-hook #'next-error-follow-minor-mode)

(setq magit-blame-styles
      '((margin
	 (margin-format " %s%f" " %C %a" " %H")
	 (margin-width . 42)
	 (margin-face . magit-blame-margin)
	 (margin-body-face magit-blame-dimmed))))

;; SO that customizations aren't loaaded into this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; No backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Auto Save on loss of focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(global-company-mode)

(xclip-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(setq cider-auto-jump-to-error             nil)
(setq cider-auto-select-error-buffer       nil)
(setq cider-auto-select-test-report-buffer t)
(setq cider-repl-display-help-banner       nil)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-pop-to-buffer-on-connect  nil)
(setq cider-repl-use-pretty-printing       t)
(setq cider-save-fileon-load               t)
(setq cider-show-error-buffer              :only-in-repl)
(setq cider-test-show-report               nil)
(setq nrepl-hide-special-buffers           t)
(setq cider-print-options '(("length" 1500)))

(defun my-cider-debug-setup ()
  (evil-make-overriding-map cider--debug-mode-map 'normal)
  (evil-normalize-keymaps))

(defun my-cider-nav-setup ()
  (evil-make-overriding-map cider-mode-map 'normal)
  (evil-normalize-keymaps))

(add-hook 'cider--debug-mode-hook 'my-cider-debug-setup 'my-cider-nav-setup)

(defun run-cider-debugger ()
  "Need to use this to work with evil mode"
  (interactive)
  (cider-debug-defun-at-point))

(defun go-forward ()
  (interactive)
  (cider-find-var))

(defun pop-back ()
  (interactive)
  (cider-pop-back))

(require 'smartparens-config)
(smartparens-global-mode 1)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(show-paren-mode t)

(defun clj-ns-align ()
  "Align ns requires."
  (interactive)
  (end-of-buffer)
  (when (re-search-backward "^\(ns.*\\(\n.*\\)*\(:require" nil t nil)
    (mark-sexp)
    (align-regexp (region-beginning)
                  (region-end)
                  "\\(\\s-*\\)\\s-:")))

(defun clear-repl-buffer ()
  (interactive)
  (cider-find-and-clear-repl-output)
  (cider-switch-to-repl-buffer)
  (evil-goto-first-line)
  (cider-switch-to-last-clojure-buffer))
  
(defun save-and-reload ()
  (interactive)
  (save-buffer)
  (cider-load-buffer))

(with-eval-after-load 'evil
  (evil-leader/set-key-for-mode 'clojure-mode
    "a"   'clj-ns-align
    "d"   'cider-doc
    "e '" 'cider-jack-in
    "e b" 'save-and-reload
    "e c" 'cider-connect
    "e d" 'run-cider-debugger
    "e e" 'cider-eval-last-sexp
    "e i" 'cider-inspect-last-result
    "e n" 'cider-enlighten-mode
    "e p" 'cider-pprint-eval-last-sexp ;; prints in repl
    "e P" 'cider-eval-print-last-sexp ;; prints in buffer
    "e r" 'cider-eval-defun-at-point
    "e x" 'cider-interrupt
    "]"   'sp-forward-barf-sexp
    "["   'sp-backwards-barf-sexp
    ">"   'sp-forward-slurp-sexp
    "<"   'sp-backwards-slurp-sexp
    "s c" 'clear-repl-buffer
    "t t" 'cider-test-run-test
    "t n" 'cider-test-run-ns-tests))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cleanup after load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold (* 2 1000 1000))
