;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$" "" (shell-command-to-string
                                          "$SHELL --login -c 'echo $PATH'"
                                          ))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(set-exec-path-from-shell-PATH)

(setq user-full-name "McCoy R. Becker"
      user-mail-address "mccoyb@mit.edu")

(setq default-frame-alist
      (append (list
               '(min-height . 1)
               '(height     . 45)
               '(min-width  . 1)
               '(width      . 81)
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(left-fringe    . 1)
               '(right-fringe   . 1)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)

(global-visual-line-mode t)

(setq display-line-numbers-type 'relative)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)
(setq initial-buffer-choice nil)
(setq frame-title-format nil)
(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq pop-up-windows nil)
(setq indicate-empty-lines nil)
(setq cursor-in-non-selected-windows nil)
(setq initial-major-mode 'text-mode)
(setq default-major-mode 'text-mode)
(setq font-lock-maximum-decoration nil)
(setq auto-fill-mode nil)
(setq fill-column 80)
(setq window-min-height 1)

(use-package citar
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  (citar-bibliography '("~/research/braindump/org/biblio.bib")))
(citar-refresh)

(setq org-directory "~/research/braindump/org/")
(setq org-roam-directory "~/research/braindump/org/")
(setq org-roam-db-location "~/research/braindump/org/org-roam.db")
(org-roam-db-autosync-mode)

(setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new
         (file+head "notes/%<%Y%m%d%H%M%S>-${slug}/${slug}.org"
                    "#+title: ${title}\n#+cite_export: csl ~/research/braindump/org/mrb-siggraph.csl\n")
         :immediate-finish t
         :unnarrowed t)))

(defun org-roam-node-from-cite (keys-entries)
  (interactive (list (citar-select-ref
                      :multiple nil
                      :rebuild-cache t)))
  (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                              "(${author editor}) ${title}")))
    (org-roam-capture- :templates
                       '(("r" "reference" plain "%?" :if-new
                          (file+head "reference/${citekey}/${citekey}.org"
                                     ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n
#+cite_export: csl ~/research/braindump/org/mrb-siggraph.csl\n")
                          :immediate-finish t
                          :unnarrowed t))
                       :info (list :citekey (car keys-entries))
                       :node (org-roam-node-create :title title)
                       :props '(:finalize find-file))))

(defun tag-new-node-as-draft () (org-roam-tag-add '("draft")))
(add-hook 'org-roam-capture-new-node-hook #'tag-new-node-as-draft)

;; julia-snail configuration
(add-to-list '+org-babel-mode-alist '(julia . julia-snail))
(add-to-list 'display-buffer-alist
             '("\\*julia" (display-buffer-reuse-window display-buffer-same-window)))

(setq org-ascii-caption-above t)

;; krita in org.
(use-package! org-krita
  :config
  (add-hook 'org-mode-hook 'org-krita-mode))

;; ink in org.
(use-package! ink)

;; elfeed.
(use-package! elfeed)
(add-hook! 'elfeed-search-mode-hook #'elfeed-update)

(setq org-preview-latex-default-process 'dvisvgm)
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode) ; this auto-enables it when you enter an org-buffer, remove if you do not want this
  :config
  ;; whatever you want
  )

;; haskell
(setq haskell-stylish-on-save t)

;; Fix some LSP ui issue.
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil))

;; ob-mermaid
(setq ob-mermaid-cli-path "/usr/local/bin/mmdc")
