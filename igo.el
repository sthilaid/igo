


(defvar igo-mode-map
  (let ((map (make-sparse-keymap)))
	;; (define-key map (kbd "<C-M-backspace>") 'ide-grep-solution)
	;; (define-key map (kbd "<C-M-return>") 'ide-grep-project)

	;; (define-key map (kbd "C-M-'") 'ide-find-file)
	;; (define-key map (kbd "M-o") 'ide-find-other-file)
	;; (define-key map (kbd "C-M-o") 'ide-find-and-create-other-file)

	;; (define-key map (kbd "<f7>")	'ide-quick-compile)
	;; (define-key map (kbd "M-<f7>")	'ide-compile-solution)
	;; (define-key map (kbd "C-<f7>")	'ide-compile-project)

	map)
  "igo-mode keymap.")

(define-minor-mode igo-mode
  "ide mode. Provides a convenient way to search for files in large projects defined in different format (.ide or text). Also support compiling projects, to a certain extent."
  :init-value nil
  :global f
  :lighter " igo"
  :keymap 'igo-mode-map
  (let ((igo-buffer (get-buffer-create "*igo*")))
    (switch-to-buffer igo-buffer)
    (setq buffer-read-only 't)))

(provide 'igo)
