
(setq igo-examble-game (let ((ex-game-file "ff4_ex.sgf"))
                         (if (file-exists-p ex-game-file)
                             (with-temp-buffer
                               (insert-file-contents ex-game-file)
                               (buffer-string))
                           "")))

(defun igo-parse-ignore-char (c)
  (or (= c ?\n)
      (= c ?\v)
      (= c ?\s)
      (= c ?\t)
      (= c ?\r)
      (= c ?\f)))

(cl-loop for c across "(allo)"
         for i from 0 to (length "(allo)")
         collect (cons c i))

(defun igo-parse-next-token (str token)
  (if (> (length token) 0)
      (let ((token-idx 0))
        (cl-loop for c across str
                 for i from 0 to (length str)

                 if (and (not (igo-parse-ignore-char c))
                         (not (= c (elt token token-idx))))
                 return nil

                 if (and (not (igo-parse-ignore-char c))
                         (= c (elt token token-idx)))
                 do (setq token-idx (+ token-idx 1))

                 if (>= token-idx (length token))
                 return (substring str (+ i 1))))
    nil))

;;(igo-parse-next-token "   \n\r(allo)" "(")
;;(igo-parse-next-token "aaa)" "aaa")

(defun igo-parse-sgf-collection (sgf-str)
  )

(defun igo-parse-sgf-list (sgf-str parsing-function)
  (labels ((parse-list (acc str) (let ((element (funcall parsing-function str)))
                                       (if element
                                           (parse-list (cons (car element) acc) (cdr element))
                                         (cons acc str)))))
    (parse-list '() sgf-str)))

(defun igo-parse-sgf-sequence-element (sgf-str)
  ;todo
  )

(defun igo-parse-sgf-sequence (sgf-str)
  (igo-parse-sgf-list sgf-str 'igo-parse-sgf-sequence-element))

(defun igo-parse-sgf-gametree (str)
  (let ((seq-str (igo-parse-next-token str "(")))
    (if (not seq-str)
        nil
      (let ((sequence (igo-parse-sgf-sequence seq-str)))
        (if (not sequence)
            nil
          (progn
            (let ((subtrees (igo-parse-sgf-list (cdr sequence) 'igo-parse-sgf-gametree)))
              (let ((rest (igo-parse-next-token (cdr subtrees) ")")))
                (if rest
                    (cons (list 'gametree (car sequence) (car subtrees))
                          rest)
                  nil)))))))))

(pp (igo-parse-sgf-gametree "   (aaa   (  aaa (aaa)  )\n)"))
(pp (igo-parse-sgf-gametree "(aaa)"))
(pp (igo-parse-sgf-gametree "(aaa(aaa))"))

(defun igo-sgf-parsing (sgf-data)
  "Converts sgf game into internal igo-mode format."
  'todo)

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
