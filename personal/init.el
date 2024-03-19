;; Add additional packages
(prelude-require-packages '(gdscript-mode
                            vterm
                            ledger-mode
                            arduino-cli-mode
                            rg
                            csharp-mode
                            plantuml-mode
                            dap-mode))

;; Start server
(load "server")
(unless (server-running-p) (server-start))

;; Set fill-column to 80 (except in org-mode)
(setq-default fill-column 80)
(add-hook 'org-mode-hook (lambda () (setq-local fill-column 70)))

;; Open vterm from crux-visit-term-buffer
(defun crux-vterm (buffer-name)
    ;; TODO: Set crux-shell and buffer-name
    (vterm))
(customize-set-variable 'crux-term-func #'crux-vterm)

;; Make C-a work in *term-mode
(defun c-a-fixing-term-mode-hook ()
    (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
             (newmap (make-sparse-keymap)))
        (set-keymap-parent newmap oldmap)
        (define-key newmap (kbd "C-a") nil)
        (make-local-variable 'minor-mode-overriding-map-alist)
        (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))
(add-hook 'term-mode-hook 'c-a-fixing-term-mode-hook)
(add-hook 'vterm-mode-hook 'c-a-fixing-term-mode-hook)

;; Split windows more sensibly
;; From: https://emacs.stackexchange.com/a/33756
(setq split-height-threshold 120
    split-width-threshold 160)

(defun my-split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                (with-selected-window window
                    (split-window-right)))
            (and (window-splittable-p window)
                (with-selected-window window
                    (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)

;; Additional smartparens key bindings
(define-key smartparens-mode-map
  (kbd "M-S-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map
  (kbd "M-S-<backspace>") 'sp-backward-unwrap-sexp)

;; Automatically save org-mode archive files
(setq org-archive-subtree-save-file-p t)

;; Show clocked time in hours
(setq org-duration-format 'h:mm)

;; Use c-mode and arduino-cli-mode for Arduino .ino files
(add-to-list 'auto-mode-alist '("\\.ino\\'" . (lambda ()
                                                (progn
                                                  (c-mode)
                                                  (arduino-cli-mode)))))

;; Use nxml-mode for .(a)xaml and .csproj files
(add-to-list 'auto-mode-alist '("\\.a?xaml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj$" . nxml-mode))

;; Use plantuml-mode for .puml files
(add-to-list 'auto-mode-alist '("\\.puml$" . plantuml-mode))

;; Evaluate PlantUML locally
(setq plantuml-jar-path "~/Software/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)

;; Support plantuml in org-mode
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
(setq org-plantuml-jar-path (expand-file-name plantuml-jar-path))

;; Support multiple languages for org-mode -> LaTeX -> PDF conversion
(add-to-list 'org-latex-packages-alist '("AUTO" "babel" t ("pdflatex")))

;; Set C style based on language
(setq prelude-c-mode-common-hook
      (lambda ()
        (setq c-default-style '((c++-mode . "stroustrup")
                                (csharp-mode . "csharp")
                                (java-mode . "java")
                                (other . "k&r")))))

;; Fix odd indentation after const in js(2)-mode
;; This is a pretty nasty workaround I found here:
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=40760
;; Not sure how well it's gonna age...
(setq js--declaration-keyword-re "\\<\\(let\\|var\\)\\>")

;; Disable automatic formatting on save
(setq prelude-format-on-save nil)

;; Hide compilation buffer for LaTeX
(setq TeX-show-compilation nil)

;; Disable tab highlighting in gdscript-mode
(add-hook 'whitespace-mode-hook
          (lambda ()
            (when (eq major-mode 'gdscript-mode)
              (whitespace-toggle-options '(tabs)))))

;; Use web-mode for PHP files
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; Workaround for editorconfig overwriting web-mode block indentation
;; See: https://github.com/editorconfig/editorconfig-emacs/issues/79
(add-hook 'editorconfig-custom-hooks
          (lambda (hash) (setq web-mode-block-padding 0)))

;; Disable guru-mode
(setq prelude-guru nil)

;; Use Roswell for SLIME
(load (expand-file-name "~/.roswell/helper.el"))
(add-to-list 'slime-lisp-implementations '(roswell ("ros" "-Q" "run")))
(setq slime-default-lisp 'roswell)

;; Disable eglot inlay hints
(add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))

;; Set up dap-mode
(dap-auto-configure-mode t)
(add-hook 'dap-stopped-hook (lambda (arg) (call-interactively #'dap-hydra)))
(require 'dap-netcore)
(setq dap-netcore-download-url
      "https://github.com/Samsung/netcoredbg/releases/download/3.0.0-1012/netcoredbg-linux-amd64.tar.gz")

;; Make treemacs windows keyboard navigable (double check that this works)
(customize-set-variable 'treemacs-is-never-other-window nil)

;; Activate column ruler globally
(global-display-fill-column-indicator-mode)

;; Customise whitespace-mode symbols and colours

(setq whitespace-style
      '(face
        tabs
        tab-mark
        trailing
        missing-newline-at-eof
        space-after-tab::tab
        space-after-tab::space
        space-before-tab::tab
        space-before-tab::space))

(defun my-whitespace-faces (&rest _)
  (let ((bg (face-background 'default))
        (fg (face-foreground 'shadow)))
    (custom-set-faces
     `(whitespace-big-indent ((t :background ,bg)))
     `(whitespace-empty ((t :background ,bg)))
     `(whitespace-hspace ((t :background ,bg :foreground ,fg)))
     `(whitespace-indentation ((t :background ,bg :foreground ,fg)))
     `(whitespace-line ((t :background ,bg :foreground ,fg)))
     `(whitespace-newline ((t :background ,bg :foreground ,fg)))
     `(whitespace-space ((t :background ,bg :foreground ,fg)))
     `(whitespace-space-after-tab ((t :inherit warning :background ,bg)))
     `(whitespace-space-before-tab ((t :inherit warning :background ,bg)))
     `(whitespace-tab ((t :background ,bg :foreground ,fg)))
     `(whitespace-trailing ((t :background ,bg)))
     `(fill-column-indicator ((t :foreground ,(face-background 'fringe)))))))

(add-hook 'enable-theme-functions #'my-whitespace-faces)
(add-hook 'after-init-hook #'my-whitespace-faces)
