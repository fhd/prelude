;; Make C-a work in term-mode
(defun c-a-fixing-term-mode-hook ()
    (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
             (newmap (make-sparse-keymap)))
        (set-keymap-parent newmap oldmap)
        (define-key newmap (kbd "C-a") nil)
        (make-local-variable 'minor-mode-overriding-map-alist)
        (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist)))
(add-hook 'term-mode-hook 'c-a-fixing-term-mode-hook)
