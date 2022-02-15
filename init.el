; increase garbace collector threshold for startup
(setq gc-cons-threshold 800000000)
(setq inhibit-compacting-font-caches t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)

;; List packages you want to install
(setq package-list '(cider
		     company
		     deadgrep
		     fzf
		     god-mode
		     magit
		     puni
		     rainbow-delimiters
		     typit
		     xclip))

;; Activate all the packages (in particular autoloads)
(package-initialize)

;;Install all missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (progn
      (package-refresh-contents)
      (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
      (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
      (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
      (package-install package))))

(defun edit-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Look & Feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn
  (defvar bg "#062230")
  (load-theme 'tsdh-dark t)
  (set-face-background 'mode-line "#333")
  (set-face-foreground 'mode-line "#bbb")
  (set-face-background 'mode-line-inactive bg)
  (set-face-foreground 'mode-line-inactive bg)
  (set-face-background 'default bg)
  (set-cursor-color "#f00") 
  (set-face-attribute 'mode-line-buffer-id nil :foreground "deepskyblue1")
  (set-face-foreground 'vertical-border "#333"))

;; y or n for prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; gui
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq ns-auto-hide-menu-bar t)

;; no startup banner
(setq inhibit-startup-message t)

(setq initial-major-mode 'fundamental-mode)

;; font
(add-to-list 'default-frame-alist '(font . "menlo-16"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ido mode (default emacs narrowing; comes with emacs)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; no sounds
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq ag-highlight-search 't)
;; (setq ag-reuse-buffers 't)
;; (add-hook 'ag-mode-hook #'next-error-follow-minor-mode)

(setq magit-blame-styles
      '((margin
	 (margin-format " %s%f" " %c %a" " %h")
	 (margin-width . 42)
	 (margin-face . magit-blame-margin)
	 (margin-body-face magit-blame-dimmed))))

;; so that customizations aren't loaaded into this file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; no backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; auto save on loss of focus
(add-hook 'focus-out-hook (lambda () (save-some-buffers t)))

(global-company-mode)

(xclip-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clojure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(setq cider-auto-jump-to-error             nil)
(setq cider-auto-select-error-buffer       nil)
(setq cider-auto-select-test-report-buffer t)
(setq cider-repl-display-help-banner       nil)
(setq cider-repl-display-in-current-window t)
(setq cider-repl-pop-to-buffer-on-connect  nil)
(setq cider-repl-use-pretty-printing       t)
(setq cider-save-file-on-load              t)
(setq cider-show-error-buffer              :only-in-repl)
(setq cider-test-show-report               nil)
(setq nrepl-hide-special-buffers           t)
(setq cider-print-options '(("length" 1500)))

(require 'puni)
(puni-global-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(show-paren-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(require 'god-mode)
(god-mode-all)

(define-key god-local-mode-map (kbd "i") #'god-local-mode)
(global-set-key (kbd "C-o") 'god-local-mode)
(global-set-key (kbd "C-c %") 'forward-or-backward-sexp)
(global-set-key (kbd "C-c <") 'puni-slurp-backward)
(global-set-key (kbd "C-c >") 'puni-slurp-forward)
(global-set-key (kbd "C-c [") 'puni-barf-backward)
(global-set-key (kbd "C-c ]") 'puni-barf-forward)
(global-set-key (kbd "C-c a") 'avy-goto-char-2)
(global-set-key (kbd "C-c d") 'cider-debug-defun-at-point)
(global-set-key (kbd "C-c f") 'deadgrep)
(global-set-key (kbd "C-c i") 'edit-init-file)
(global-set-key (kbd "C-c l") 'switch-to-previous-buffer)
(global-set-key (kbd "C-c o") 'other-window)
(global-set-key (kbd "C-c p") 'list-packages)
(global-set-key (kbd "C-c s") 'occur)
(global-set-key (kbd "C-c z") 'fzf-git-files)
(global-set-key (kbd "C-c b") 'fzf-switch-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cleanup after load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold (* 2 1000 1000))
