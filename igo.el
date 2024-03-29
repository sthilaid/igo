;;; - igo mode -
;;; by David St-Hilaire
;;;
;;; - Installation instructions -
;;;
;;; Add this file to your load path with (add-to-list 'load-path
;;; "path-to-this-file") then add (require 'igo)
;;;
;;; - Usage -
;;;
;;; You can load an SGF file with <M-x> igo-load-sgf-file
;;; or you can start a new gamw with <M-x> igo.
;;;
;;; In the *igo* buffer, you can use <space> to change modes.  The view mode lets
;;; you navigate the game loaded from an SGF file using the arrows.
;;;
;;; In "play" mode you can add stones using the arrows followed by <return>, or
;;; directly input the coordinates of the move you want to enter (like "cC",
;;; <return>).
;;;
;;; The latest version can be foudn at:
;;;     https://github.com/sthilaid/igo

(defvar igo-example-game)
(define-error 'igo-error-sgf-parsing "sgf parsing error occured: ")
(define-error 'igo-error-invalid-player "Invalid player used in game of go, should be 'b or 'w")
(define-error 'igo-error-invalid-move "Played move is not valid: ")
(define-error 'igo-error-invalid-coord "Invalid coordinate: ")
(define-error 'igo-error-invalid-move-input "Invalid move input: ")
(define-error 'igo-error-unknown-property "Unknown property: ")
(define-error 'igo-error-invalid-property-values "Invalid property values for type: ")
(define-error 'igo-error-invalid-sgf-data "Invalid sgf data: ")
(define-error 'igo-error-unsupported "Unsupported game: ")
(define-error 'igo-error-invalid-str-coord "Invalid string coordinate: ")
(define-error 'igo-error-invalid-path "Invalid path element: ")

(setq igo-debug-on-error 'debug-only-non-parsing) ;; nil, 'non-parsing, anything else: all
(defmacro igo-signal (err val)
  `(progn ,(if (and igo-debug-on-error
                    (or (not (eq igo-debug-on-error 'debug-only-non-parsing))
                        (eq err 'igo-error-sgf-parsing)))
               '(debug))
          (signal ,err ,val)))

(setq igo-buffer-name "*igo*")
(setq igo-show-labels 't)
(setq igo-current-mode nil)
(setq igo-current-gamestate nil)
(setq igo-current-gameflow nil)
(setq igo-play-last-move (cons nil nil))
(setq igo-play-current-move (cons nil nil))
(setq igo-active-overlays nil)

(defun igo-setup-example-game ()
  (setq igo-example-game (let ((ex-game-file "example2.sgf"))
                           (if (file-exists-p ex-game-file)
                               (with-temp-buffer
                                 (insert-file-contents ex-game-file)
                                 (buffer-string))
                             "")))
  ;; (with-output-to-temp-buffer (generate-new-buffer-name "igo-example-game")
  ;;   (pp (igo-sgf-parse-str igo-example-game)))

  (let ((flow (igo-new-gameflow)))
  (setq igo-current-gamestate (igo-new-gamestate (cons 19 19)))
  (igo-gameflow-set-path flow (list 1))
  (igo-gameflow-set-flow flow (car (igo-parse-sgf-gametree igo-example-game)))
  (setq igo-current-gameflow flow)
  (setq igo-current-gamestate (igo-gameflow-apply igo-current-gameflow igo-current-gamestate)))
  (igo-redraw)
  'done)

(defun igo-load-sgf-file ()
  "Load a SGF go game"
  (interactive)
  (let* ((file (ido-read-file-name "load sgf file: "))
         (sgf-data (if (file-exists-p file)
                       (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string))
                     "")))
    (igo)
    (setq igo-current-gameflow (igo-new-gameflow))
    (igo-gameflow-set-path igo-current-gameflow (list 0))
    (igo-gameflow-set-flow igo-current-gameflow (car (igo-parse-sgf-gametree sgf-data)))
    ;; (save-excursion
    ;;   (switch-to-buffer (generate-new-buffer-name "igo-example-game"))
    ;;   (insert (with-output-to-string (pp (igo-gameflow-get-flow igo-current-gameflow)))))
    
    (setq igo-current-gamestate (igo-new-gamestate (cons 19 19)))
    (setq igo-current-gamestate (igo-gameflow-apply igo-current-gameflow igo-current-gamestate))
    (igo-redraw)
    (message (concat "finshed loading " file))))


(defun igo-is-igo-buffer? ()
  (string= (buffer-name (current-buffer))
           igo-buffer-name))

;; TEMPORARY
(setq max-lisp-eval-depth 50000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SGF Parsing

  ;; -------------------------------------------------------------------
  ;; SGF Syntax
  ;; -------------------------------------------------------------------
  ;; Collection = GameTree { GameTree }
  ;; GameTree   = "(" Sequence { GameTree } ")"
  ;; Sequence   = Node { Node }
  ;; Node       = ";" { Property }
  ;; Property   = PropIdent PropValue { PropValue }
  ;; PropIdent  = UcLetter { UcLetter }
  ;; PropValue  = "[" CValueType "]"
  ;; CValueType = (ValueType | Compose)
  ;; ValueType  = (None | Number | Real | Double | Color | SimpleText |
  ;;               Text | Point  | Move | Stone)
  ;; UcLetter   = "A".."Z"
  ;; Digit      = "0".."9"
  ;; None       = ""

  ;; Number     = [("+"|"-")] Digit { Digit }
  ;; Real       = Number ["." Digit { Digit }]

  ;; Double     = ("1" | "2")
  ;; Color      = ("B" | "W")

  ;; SimpleText = { any character (handling see below) }
  ;; Text       = { any character (handling see below) }

  ;; Point      = game-specific
  ;; Move       = game-specific
  ;; Stone      = game-specific

  ;; Compose    = ValueType ":" ValueType

(defun igo-parse-ignore-char (c)
  (or (= c ?\n)
      (= c ?\v)
      (= c ?\s)
      (= c ?\t)
      (= c ?\r)
      (= c ?\f)))

;; (cl-loop for c across "(allo)"
;;          for i from 0 to (length "(allo)")
;;          collect (cons c i))

(defun igo-parse-next-token (str token)
  (if (> (length token) 0)
      (let ((token-idx 0))
        (cl-loop for c across str
                 for i from 0 to (length str)

                 if (and (not (igo-parse-ignore-char c))
                         (not (= c (elt token token-idx))))
                 do (igo-signal 'igo-error-sgf-parsing (list (concat "wrong token, expecting: " token " got: " str)))

                 if (and (not (igo-parse-ignore-char c))
                         (= c (elt token token-idx)))
                 do (setq token-idx (+ token-idx 1))

                 if (>= token-idx (length token))
                 return (substring str (+ i 1))))
    (igo-signal 'igo-error-sgf-parsing (list (concat "wrong token, expecting: " token " got: " str)))))

;;(igo-parse-next-token "   \n\r(allo)" "(")
;; (igo-parse-next-token "aaa)" "aab")

(defun igo-parse-sgf-list (sgf-str parsing-function list-type)
  (cl-labels ((parse-list (acc str)
						  (condition-case err
							  (let ((element (funcall parsing-function str)))
								(if element
									(parse-list (cons (car element) acc) (cdr element))
								  (cons acc str)))
							(igo-error-sgf-parsing (cons acc str)))))
    (let ((result (parse-list '() sgf-str)))
      (cons (cons (intern (concat (symbol-name list-type) "-list")) (reverse (car result))) (cdr result)))))

(defun igo-parse-is-letter? (char)
  (let ((downcase-char (downcase char)))
    (and (>= downcase-char ?a)
         (<= downcase-char ?z))))

;; (igo-parse-is-letter? (elt "aaa" 0))
;; (igo-parse-is-letter? ?a)

(defun igo-parse-sgf-property-id (sgf-str)
  (cl-labels ((call-error () (igo-signal 'igo-error-sgf-parsing (list (concat "invalid property for " sgf-str))))
			  (parse-proptery (acc str)
							  (if (string= str "")
								  (cons acc str)
								(let ((char (elt str 0)))
								  (if (igo-parse-is-letter? char)
									  (parse-proptery (concat acc (list char)) (substring str 1))
									(if (string= acc "")
										(if (igo-parse-ignore-char char)
											(parse-proptery acc (substring str 1))
										  (call-error))
									  (cons acc str)))))))
    (parse-proptery "" sgf-str)))

;; (igo-parse-sgf-property-id "aaa")
;; (igo-parse-sgf-property-id "   \raaa[")
;; (igo-parse-sgf-property-id "   \raBaZ  [")

(defun igo-parse-sgf-value-type (sgf-str)
  (cl-labels ((parse-double (str) (if (and (>= (length str) 2)
										   (let ((char (elt str 0))) (or (= char ?1) (= char ?2)))
										   (= (elt str 1) ?\]))
									  (cons (list 'double (string (elt str 0))) (substring str 1))
									nil))
			  (parse-color (str) (if (and (>= (length str) 2)
										  (let ((char (elt str 0))) (or (= char ?B) (= char ?W)))
										  (= (elt str 1) ?\]))
									 (cons (list 'color (string (elt str 0))) (substring str 1))
								   nil))
			  (parse-point (str) nil)      ; todo
			  (parse-move (str) nil)       ; todo
			  (parse-stone (str) nil)      ; todo
			  (parse-number (acc str has-decimal?)
							(let ((char (elt str 0)))
							  (cond ((or (and (string= acc "")
											  (or (= char ?+)
												  (= char ?-)))
										 (and (>= char ?0)
											  (<= char ?9)))
									 (parse-number (concat acc (list char)) (substring str 1) nil))

									((and (not has-decimal?) (= char ?.))
									 (parse-number (concat acc (list char)) (substring str 1) t))

									((= char ?\])
									 (cons (list 'number acc) str))

									(t nil))))
			  (parse-text (acc str)
						  (let ((char (elt str 0)))
							(if (= char ?\])
								(cons (list 'text acc) str)
							  (parse-text (concat acc (list char)) (substring str 1))))))
    (or (parse-double sgf-str)
        (parse-color sgf-str)
        (parse-point sgf-str)
        (parse-move sgf-str)
        (parse-stone sgf-str)
        (parse-number "" sgf-str nil)
        (parse-text "" sgf-str))))

;; (igo-parse-sgf-value-type "1]")
;; (igo-parse-sgf-value-type "B]")
;; (igo-parse-sgf-value-type "BB]")
;; (igo-parse-sgf-value-type "+12.33322]")
;; (igo-parse-sgf-value-type "+12.+33322]")
;; (igo-parse-sgf-value-type "+12.33.322]") ;; need to fix this
;; (igo-parse-sgf-value-type "bla blua\r]")

(defun igo-parse-sgf-property-value (sgf-str)
  (if (or (not sgf-str) (string= sgf-str "")) (igo-signal 'igo-error-sgf-parsing (list (concat "invalid value string: " sgf-str))))
  (let* ((str (igo-parse-next-token sgf-str "["))
               (value-type (igo-parse-sgf-value-type str))
               (result (igo-parse-next-token (cdr value-type) "]")))
          (cons (car value-type) result)))

;; (igo-parse-sgf-property-value nil)
;; (igo-parse-sgf-property-value "")
;; (igo-parse-sgf-property-value "[text test]")

(defun igo-parse-sgf-property (sgf-str)
  (if (or (not sgf-str) (string= sgf-str "")) (igo-signal 'igo-error-sgf-parsing (list (concat "invalid property string: " sgf-str))))
  (let* ((property-id (igo-parse-sgf-property-id sgf-str))
         (property-values (igo-parse-sgf-list (cdr property-id) 'igo-parse-sgf-property-value 'value)))
    (cons (list (car property-id) (car property-values)) (cdr property-values))))

;(igo-parse-sgf-property "C[text test][another comment]")

(defun igo-parse-sgf-node (sgf-str)
  (let ((node-str-rest (igo-parse-next-token sgf-str ";")))
    (if node-str-rest
		;(igo-parse-sgf-property node-str-rest)
		(igo-parse-sgf-list node-str-rest 'igo-parse-sgf-property 'property)
      (igo-signal 'igo-error-sgf-parsing (list (concat "while paring node: " sgf-str))))))

;; (igo-parse-sgf-node ";C[Comment]")
;; (igo-parse-sgf-node ";AB[B]")
;; (pp (igo-parse-sgf-node ";AB[dd][de][df][dg][dh][di][dj][nj][ni][nh][nf][ne][nd][ij][ii][ih][hq][gq][fq][eq][dr][ds][dq][dp][cp][bp][ap][iq][ir][is][bo][bn][an][ms][mr]"))
;; (pp (igo-parse-sgf-node ";AB[dd][de][df][dg][dh][di][dj][nj][ni][nh][nf][ne][nd][ij][ii][ih][hq]
;; [gq][fq][eq][dr][ds][dq][dp][cp][bp][ap][iq][ir][is][bo][bn][an][ms][mr]
;; AW[pd][pe][pf][pg][ph][pi][pj][fd][fe][ff][fh][fi][fj][kh][ki][kj][os][or]
;; [oq][op][pp][qp][rp][sp][ro][rn][sn][nq][mq][lq][kq][kr][ks][fs][gs][gr]
;; [er]N[Markup]C[Position set up without compressed point lists.]"))

;; need have error/exception management!

(defun igo-parse-sgf-sequence (sgf-str)
  (igo-parse-sgf-list sgf-str 'igo-parse-sgf-node 'sequence))

;; (pp (igo-parse-sgf-sequence ";B[qr]N[Time limits, captures & move numbers]
;; BL[120.0]C[Black time left: 120 sec];W[rr]
;; WL[300]C[White time left: 300 sec];B[rq]
;; BL[105.6]OB[10]C[Black time left: 105.6 sec
;; Black stones left (in this byo-yomi period): 10];W[qq]
;; WL[200]OW[2]C[White time left: 200 sec
;; White stones left: 2];B[sr]
;; BL[87.00]OB[9]C[Black time left: 87 sec
;; Black stones left: 9];W[qs]
;; WL[13.20]OW[1]C[White time left: 13.2 sec
;; White stones left: 1];B[rs]
;; C[One white stone at s2 captured];W[ps];B[pr];W[or]
;; MN[2]C[Set move number to 2];B[os]
;; C[Two white stones captured
;; (at q1 & r1)]
;; ;MN[112]W[pq]C[Set move number to 112];B[sq];W[rp];B[ps]
;; ;W[ns];B[ss];W[nr]"))

(defun igo-parse-sgf-gametree (str)
  (let ((seq-str (igo-parse-next-token str "(")))
    (if (not seq-str)
        (igo-signal 'igo-error-sgf-parsing (list (concat "invalid gametree token ( for: " str)))
      (let ((sequence (igo-parse-sgf-sequence seq-str)))
        (progn
          (let ((subtrees (igo-parse-sgf-list (cdr sequence) 'igo-parse-sgf-gametree 'gametree)))
            (let ((rest (igo-parse-next-token (cdr subtrees) ")")))
              (if (not rest)
                  (igo-signal 'igo-error-sgf-parsing (list (concat "invalid gametree token ) for: " (cdr subtrees))))
                (cons (list 'gametree (car sequence) (car subtrees))
                      rest)))))))))

;;(igo-parse-sgf-gametree igo-example-game)
;;(igo-parse-sgf-gametree "(;B[ab]C[comment];B[cd]C[comment](;Z[12]C[comment];Z[45]C[comment])(;X[12.0]C[comment];Y[12]))")
;;(pp (igo-parse-sgf-gametree "(;B[ab]C[comment];B[cd]C[comment])"))
;;(pp (igo-parse-sgf-gametree "(;B[ab]C[comment];B[cd]C[comment](;XY[12]C[comment];YZ[15])(;H[1]B[B];H[2]B[W]))"))

;;(pp (igo-parse-sgf-gametree "(;FF[4]GM[1]SZ[19]CA[UTF-8]SO[gokifu.com]BC[cn]WC[kr]EV[]PB[Ke Jie]BR[9p]PW[Lee Sedol]WR[9p]KM[7.5]DT[2018-05-21]RE[W+R]TM[150]LT[]LC[5]GK[1];B[pd];W[dp];B[qp];W[cc];B[np];W[fq];B[qf];W[fd];B[bo];W[cp];B[ck];W[ci];B[cn];W[ob];B[pb];W[ej];B[oc];W[jd];B[he];W[hd];B[cd];W[dc];B[fe];W[ge];B[gf];W[gd];B[di];W[dj];B[cj];W[ch];B[ei];W[dl];B[cl];W[dg];B[fi];W[fg];B[ff];W[gg];B[hg];W[hh];B[ed];W[de];B[ee];W[dd];B[ec];W[ih];B[ie];W[id];B[ig];W[ef];B[fb];W[nc];B[kg];W[nb];B[le];W[lc];B[hb];W[ib];B[nd];W[qk];B[qm];W[qh];B[gp];W[hr];B[iq];W[hq];B[hp];W[ip];B[fp];W[ep];B[gq];W[gr];B[fr];W[eq];B[ir];W[er];B[kq];W[io];B[ko];W[ok];B[pj];W[pk];B[ri];W[ki];B[qi];W[ni];B[mk];W[nm];B[mi];W[mh];B[ld];W[mc];B[li];W[lh];B[kh];W[oh];B[ng];W[nh];B[rh];W[lk];B[jb];W[ia];B[kc];W[kb];B[kj];W[ji];B[ml];W[md];B[ne];W[pm];B[jj];W[ij];B[oj];W[nj];B[kl];W[ik];B[ll];W[me];B[mf];W[lg];B[lf];W[kf];B[je];W[jg];B[jf];W[jh];B[kd];W[jc];B[qn];W[nk];B[gn];W[em];B[dk];W[ek];B[dm];W[en];B[el];W[fl];B[hm];W[hn];B[fk];W[dl];B[lb];W[ja];B[el];W[qc];B[pc];W[dl];B[oa];W[la];B[el];W[hf];B[if];W[dl];B[na];W[mb];B[el];W[kg];B[ke];W[dl];B[hs];W[el];B[bi];W[fs];B[bh];W[bg];B[im];W[go];B[on];W[qg];B[rg];W[pf];B[qe];W[bp];B[ap];W[gm];B[om];W[nl];B[db];W[cb];B[ca];W[ba];B[da];W[ah];B[bj];W[jn];B[jm];W[kn];B[in];W[lo];B[ho];W[kp];B[lq];W[mn];B[fo];W[mp];B[mq];W[co];B[bn];W[jl];B[gl];W[fm];B[hk];W[fj];B[il];W[jk];B[hj];W[km];B[eo];W[dn];B[do];W[gk];B[gi];W[hl];B[cf];W[df];B[gl];W[cm];B[cg];W[dh];B[bm];W[hi];B[kk];W[ii])"))q

(defun igo-parse-sgf-collection (sgf-str)
  (igo-parse-sgf-list sgf-str 'igo-parse-sgf-gametree 'collection))

;;(pp (igo-parse-sgf-collection "(;A[1])(B[2])"))

(defun igo-sgf-parse-str (sgf-str)
  "Converts sgf game into internal igo-mode format."
  (car (igo-parse-sgf-collection sgf-str)))

(defun igo-sgf-collection-get-gametrees (sgf-data)
  (if (not (eq (car sgf-data) 'collection-list))
      (igo-signal 'igo-error-invalid-sgf-data (list "expecting 'collection-list got: " (car sgf-data))))
  (cdr sgf-data))

(defun igo-sgf-gametree-get-sequence (sgf-data)
  (let ((tag 'gametree))
    (if (not (eq (car sgf-data) tag))
        (igo-signal 'igo-error-invalid-sgf-data (list "expecting: " tag " got: "(car sgf-data)))))
  (elt sgf-data 1))

(defun igo-sgf-gametree-get-gametrees (sgf-data)
  (if (or (not (listp sgf-data))
          (not (eq (car sgf-data) 'gametree))
          (not (listp (elt sgf-data 2)))
          (not (eq (car (elt sgf-data 2)) 'gametree-list)))
      (igo-signal 'igo-error-invalid-sgf-data (list "expecting: gamtree, got: " (car sgf-data))))
  (cdr (elt sgf-data 2)))

(defun igo-sgf-sequence-get-nodes (sgf-data)
  (let ((tag 'sequence-list))
    (if (not (eq (car sgf-data) tag))
        (igo-signal 'igo-error-invalid-sgf-data (list "expecting: " tag " got: "(car sgf-data)))))
  (cdr sgf-data))

(defun igo-sgf-node-get-properties (sgf-data)
  (let ((tag 'property-list))
    (if (not (eq (car sgf-data) tag))
        (igo-signal 'igo-error-invalid-sgf-data (list "expecting: " tag " got: "(car sgf-data)))))
  (cdr sgf-data))

(defun igo-sgf-property-get-ident (sgf-data)
  (if (not (stringp (car sgf-data)))
      (igo-signal 'igo-error-invalid-sgf-data (list "expecting string type for property ident, got: " (car sgf-data))))
  (car sgf-data))

(defun igo-sgf-property-get-values (sgf-data)
  (let ((tag 'value-list))
    (if (or (not (consp sgf-data))
            (not (consp (cdr sgf-data)))
            (not (listp (cadr sgf-data)))
            (not (eq (cl-caadr sgf-data) tag))
            (not (listp (cl-cdadr sgf-data))))
        (igo-signal 'igo-error-invalid-sgf-data (list "expecting: " tag " got: "(cl-caadr sgf-data)))))
  (cl-cdadr sgf-data))

;;(igo-sgf-property-get-values '("W" (value-list (text "eh"))))

(defun igo-sgf-property-get-move-value (sgf-data)
  (let ((values (igo-sgf-property-get-values sgf-data)))
    (if (or (not (= (length values) 1))
            (not (eq (caar values) 'text))
            (not (stringp (cl-cadar values))))
        (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'move)))
    (cl-cadar values)))

;;(igo-sgf-property-get-move-value '("W" (value-list (text "eh"))))

(defun igo-sgf-property-get-move-list-value (sgf-data)
  (let ((values (igo-sgf-property-get-values sgf-data)))
    (if (= (length values) 0)
        (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'move-list)))
    (mapcar (lambda (value) (igo-sgf-property-get-move-value (list (car sgf-data) (list 'value-list value))))
            values)))

;; (igo-sgf-property-get-move-list-value '("AB" (value-list (text "dd")
;;                                                          (text "pd")
;;                                                          (text "dp")
;;                                                          (text "pp"))))

(defun igo-sgf-property-get-number-value (sgf-data)
  (let ((values (igo-sgf-property-get-values sgf-data)))
    (if (or (not (= (length values) 1))
            (and (not (eq (caar values) 'number))
                 (not (eq (caar values) 'double)))
            (not (stringp (cl-cadar values))))
        (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'number)))
    (string-to-number (cl-cadar values))))

;;(igo-sgf-property-get-number-value '("HA" (value-list (number "4"))))

(defun igo-sgf-property-get-double-value (sgf-data)
  (let ((values (igo-sgf-property-get-values sgf-data)))
    (if (or (not (= (length values) 1))
            (not (eq (caar values) 'double))
            (not (stringp (cl-cadar values))))
        (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'double)))
    (let ((double-value (string-to-number (cl-cadar values))))
      (if (not (or (= double-value 1)
                   (= double-value 2)))
          (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'double))
        double-value))))

;;(igo-sgf-property-get-double-value '("WG" (value-list (double "2"))))

(defun igo-sgf-property-get-color-value (sgf-data)
  (let ((values (igo-sgf-property-get-values sgf-data)))
    (if (or (not (= (length values) 1))
            (not (eq (caar values) 'color))
            (not (stringp (cl-cadar values)))
            (not (= (length (cl-cadar values)) 1)))
        (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'color)))
    (let ((color-char (elt (cl-cadar values) 0)))
      (if (not (or (eq color-char ?W)
                   (eq color-char ?B)))
          (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'color))
        (intern (downcase (cl-cadar values)))))))

;;(igo-sgf-property-get-color-value '("PL" (value-list (color "W"))))

(defun igo-sgf-property-get-text (sgf-property)
  (let ((values (igo-sgf-property-get-values sgf-property)))
    (if (or (not (= (length values) 1))
            (not (eq (caar values) 'text))
            (not (stringp (cl-cadar values))))
        (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'text)))
    (cl-cadar values)))

(defun igo-sgf-apply-property (sgf-property gamestate)
  (let ((identifier (igo-sgf-property-get-ident sgf-property))
        (values     (igo-sgf-property-get-values sgf-property))
        (info       (igo-state-get-game-info gamestate)))
    (cond
     ;; move
     ((string= identifier "B")      (let* ((move     (igo-sgf-property-get-move-value sgf-property))
                                           (coord    (igo-convert-string-coord-to-coord move)))
                                      (if (or (string= move "tt")
                                              (string= move ""))
                                          (igo-state-set-move-comment "black passes" gamestate)
                                        (igo-play-move 'b coord gamestate))))
     
     ((string= identifier "W")      (let* ((move     (igo-sgf-property-get-move-value sgf-property))
                                           (coord    (igo-convert-string-coord-to-coord move)))
                                      (if (or (string= move "tt")
                                              (string= move ""))
                                          (igo-state-set-move-comment "white passes" gamestate)
                                        (igo-play-move 'w coord gamestate))))
     
     ((string= identifier "KO")     '(optional-force-move   move))
     ;;((string= identifier "MN")     '(set-move-number       number))

     ;; setup
     ((string= identifier "AB")     (let* ((moves     (igo-sgf-property-get-move-list-value sgf-property)))
                                      (cl-loop for move in moves
                                               do (let ((coord (igo-convert-string-coord-to-coord move)))
                                                    (igo-add-stone 'b coord gamestate)))))
     
     ((string= identifier "AW")     (let* ((moves     (igo-sgf-property-get-move-list-value sgf-property)))
                                      (cl-loop for move in moves
                                               do (let ((coord (igo-convert-string-coord-to-coord move)))
                                                    (igo-add-stone 'w coord gamestate)))))
     
     ((string= identifier "AE")     (let* ((moves     (igo-sgf-property-get-move-list-value sgf-property)))
                                      (cl-loop for move in moves
                                               do (let ((coord (igo-convert-string-coord-to-coord move)))
                                                    (igo-add-stone nil coord gamestate)))))
     
     ((string= identifier "PL")     (let* ((color (igo-sgf-property-get-color-value sgf-property)))
                                      (igo-state-set-current-player color gamestate)))

     ;; node annotations (double 1: good 2: very good)
     ((string= identifier "N")      (let ((text (igo-sgf-property-get-text sgf-property)))
                                      (igo-state-set-move-comment text gamestate)))
     ((string= identifier "C")      (let ((text (igo-sgf-property-get-text sgf-property)))
                                      (igo-state-set-move-comment text gamestate)))
     ((string= identifier "DM")     (let* ((double (igo-sgf-property-get-double-value sgf-property))
                                           (str-double (number-to-string double)))
                                      (igo-state-set-last-move-annotation (concat "even-position: " str-double) gamestate)))
     ((string= identifier "GB")     (let* ((double (igo-sgf-property-get-double-value sgf-property))
                                           (str-double (number-to-string double)))
                                      (igo-state-set-last-move-annotation (concat "good-for-black: " str-double) gamestate)))
     ((string= identifier "GW")     (let* ((double (igo-sgf-property-get-double-value sgf-property))
                                           (str-double (number-to-string double)))
                                      (igo-state-set-last-move-annotation (concat "good-for-white: " str-double) gamestate)))
     ((string= identifier "UC")     (let* ((double (igo-sgf-property-get-double-value sgf-property))
                                           (str-double (number-to-string double)))
                                      (igo-state-set-last-move-annotation (concat "unclear-position: " str-double) gamestate)))
     ((string= identifier "HO")     (let* ((double (igo-sgf-property-get-double-value sgf-property))
                                           (str-double (number-to-string double)))
                                      (igo-state-set-last-move-annotation (concat "hotspot: " str-double) gamestate)))
     ((string= identifier "V")      '(node-value            real)) ; TODO - positive good for white, neg good for black

     ;; move annotations
     ((string= identifier "BM")     (let* ((double (igo-sgf-property-get-double-value sgf-property))
                                           (str-double (number-to-string double)))
                                      (igo-state-set-last-move-annotation (concat "bad-move: " str-double) gamestate)))
     ((string= identifier "DO")     (igo-state-set-last-move-annotation "doubtful" gamestate))
     ((string= identifier "IT")     (igo-state-set-last-move-annotation "ineresting" gamestate))
     ((string= identifier "TE")     (let* ((double (igo-sgf-property-get-double-value sgf-property))
                                           (str-double (number-to-string double)))
                                      (igo-state-set-last-move-annotation (concat "tesuji: " str-double) gamestate)))

     ;; markup
     ((string= identifier "AR")     '(arror                 move : move))
     ((string= identifier "LN")     '(line                  move : move))
     ((string= identifier "LB")     '(board-text            move : text))
     ((string= identifier "CR")     '(circle                move-list))
     ((string= identifier "DD")     '(greyed                move-list))
     ((string= identifier "MA")     '(mark                  move-list))
     ((string= identifier "SQ")     '(square                move-list))
     ((string= identifier "TR")     '(triangle              move-list))

     ;; root
     ((string= identifier "AP")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-sgf-app-and-version info value)))
     ((string= identifier "CA")     'charset-text)
     ((string= identifier "FF")     'number)
     ((string= identifier "GM")     (let ((value (igo-sgf-property-get-number-value sgf-property)))
                                      (if (not (= value 1))
                                          (igo-signal 'igo-error-unsupported (list 'unsupported 'game value))
                                        'game-number)))
     ((string= identifier "ST")     (let ((value (igo-sgf-property-get-number-value sgf-property)))
                                      (igo-info-set-variation-style info value)))
     ((string= identifier "SZ")     '(board-size            size)) ;; !get-size! number or number:number

     ;; game info
     ((string= identifier "AN")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-annotator-name info value)))
     ((string= identifier "BR")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-black-rank info value)))
     ((string= identifier "BT")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-black-team info value)))
     ((string= identifier "CP")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-copyright info value)))
     ((string= identifier "DT")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-date info value)))
     ((string= identifier "EV")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-event info value)))
     ((string= identifier "GN")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-game-name info value)))
     ((string= identifier "GC")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-game-comment info value)))
     ((string= identifier "ON")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-opening-info info value)))
     ((string= identifier "OT")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-overtime-type info value)))
     ((string= identifier "PB")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-black-player-name info value)))
     ((string= identifier "PC")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-game-place info value)))
     ((string= identifier "PW")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-white-player-name info value)))
     ((string= identifier "RE")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-result info value)))
     ((string= identifier "RO")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-game-round-info info value)))
     ((string= identifier "RU")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-game-rules info value)))
     ((string= identifier "SO")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-game-source info value)))
     ((string= identifier "TM")     (let ((value (igo-sgf-property-get-number-value sgf-property)))
                                      (igo-info-set-time-limit info value)))
     ((string= identifier "US")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-game-scribe-user info value)))
     ((string= identifier "WR")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-white-rank info value)))
     ((string= identifier "WT")     (let ((value (igo-sgf-property-get-text sgf-property)))
                                      (igo-info-set-white-team info value)))
     
     ;; timing
     ((string= identifier "BL")     '(black-time-left       number))
     ((string= identifier "WL")     '(white-time-left       number))
     ((string= identifier "OB")     '(black-moves-left      number))
     ((string= identifier "OW")     '(white-moves-left      number))

     ;; miscelaneous
     ((string= identifier "VW")     '(view-only-part-of-the-board      point-list))

     ;; go specific
     ((string= identifier "HA")     (let ((value (igo-sgf-property-get-number-value sgf-property)))
                                      (igo-info-set-game-handicap info value)))
     ((string= identifier "KM")     (let ((value (igo-sgf-property-get-number-value sgf-property)))
                                      (igo-info-set-game-komi info value)))
     ((string= identifier "TB")     (let ((value (igo-sgf-property-get-move-list-value sgf-property)))
                                      (igo-info-set-black-territory info value)))
     ((string= identifier "TW")     (let ((value (igo-sgf-property-get-move-list-value sgf-property)))
                                      (igo-info-set-white-territory info value)))
     
     (t (igo-signal 'igo-error-unknown-property (list sgf-property))))))

(defun igo-sgf-apply-node (sgf-data gamestate)
  (if (or (not (listp sgf-data))
          (not (eq (car sgf-data) 'property-list)))
      (igo-signal 'igo-error-invalid-property-values `(invalid node: ,sgf-data)))
  (igo-state-set-move-comment nil gamestate)
  (igo-state-set-last-move-annotation nil gamestate)
  (cl-loop for prop in (cdr sgf-data)
           do (igo-sgf-apply-property prop gamestate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go Game Info

(defun igo-new-game-info ()
    (vector
     (vector 'sgf-app-and-version: nil)
     (vector 'variation-style: nil)
     (vector 'board-size: nil)
     (vector 'annotator-name: nil)
     (vector 'black-rank: nil)
     (vector 'black-team: nil)
     (vector 'copyright: nil)
     (vector 'date: nil)
     (vector 'event: nil)
     (vector 'game-name: nil)
     (vector 'game-comment: nil)
     (vector 'opening-info: nil)
     (vector 'overtime-type: nil)
     (vector 'black-player-name: nil)
     (vector 'game-place: nil)
     (vector 'white-player-name: nil)
     (vector 'result: nil)
     (vector 'game-round-info: nil)
     (vector 'game-rules: nil)
     (vector 'game-source: nil)
     (vector 'time-limit: nil)
     (vector 'game-scribe-user: nil)
     (vector 'white-rank: nil)
     (vector 'white-team: nil)
     (vector 'game-handicap: nil)
     (vector 'game-komi: nil)
     (vector 'black-territory       nil)
     (vector 'white-territory       nil)
     ))

(defun igo-info-get-sgf-app-and-version (info)
  (elt (elt info 0) 1))
(defun igo-info-set-sgf-app-and-version (info value)
  (aset (elt info 0) 1 value))

(defun igo-info-get-variation-style (info)
  (elt (elt info 1) 1))
(defun igo-info-set-variation-style (info value)
  (aset (elt info 1) 1 value))

(defun igo-info-get-board-size (info)
  (elt (elt info 2) 1))
(defun igo-info-set-board-size (info value)
  (aset (elt info 2) 1 value))

(defun igo-info-get-annotator-name (info)
  (elt (elt info 3) 1))
(defun igo-info-set-annotator-name (info value)
  (aset (elt info 3) 1 value))

(defun igo-info-get-black-rank (info)
  (elt (elt info 4) 1))
(defun igo-info-set-black-rank (info value)
  (aset (elt info 4) 1 value))

(defun igo-info-get-black-team (info)
  (elt (elt info 5) 1))
(defun igo-info-set-black-team (info value)
  (aset (elt info 5) 1 value))

(defun igo-info-get-copyright (info)
  (elt (elt info 6) 1))
(defun igo-info-set-copyright (info value)
  (aset (elt info 6) 1 value))

(defun igo-info-get-date (info)
  (elt (elt info 7) 1))
(defun igo-info-set-date (info value)
  (aset (elt info 7) 1 value))

(defun igo-info-get-event (info)
  (elt (elt info 8) 1))
(defun igo-info-set-event (info value)
  (aset (elt info 8) 1 value))

(defun igo-info-get-game-name (info)
  (elt (elt info 9) 1))
(defun igo-info-set-game-name (info value)
  (aset (elt info 9) 1 value))

(defun igo-info-get-game-comment (info)
  (elt (elt info 10) 1))
(defun igo-info-set-game-comment (info value)
  (aset (elt info 10) 1 value))

(defun igo-info-get-opening-info (info)
  (elt (elt info 11) 1))
(defun igo-info-set-opening-info (info value)
  (aset (elt info 11) 1 value))

(defun igo-info-get-overtime-type (info)
  (elt (elt info 12) 1))
(defun igo-info-set-overtime-type (info value)
  (aset (elt info 12) 1 value))

(defun igo-info-get-black-player-name (info)
  (elt (elt info 13) 1))
(defun igo-info-set-black-player-name (info value)
  (aset (elt info 13) 1 value))

(defun igo-info-get-game-place (info)
  (elt (elt info 14) 1))
(defun igo-info-set-game-place (info value)
  (aset (elt info 14) 1 value))

(defun igo-info-get-white-player-name (info)
  (elt (elt info 15) 1))
(defun igo-info-set-white-player-name (info value)
  (aset (elt info 15) 1 value))

(defun igo-info-get-result (info)
  (elt (elt info 16) 1))
(defun igo-info-set-result (info value)
  (aset (elt info 16) 1 value))

(defun igo-info-get-game-round-info (info)
  (elt (elt info 17) 1))
(defun igo-info-set-game-round-info (info value)
  (aset (elt info 17) 1 value))

(defun igo-info-get-game-rules (info)
  (elt (elt info 18) 1))
(defun igo-info-set-game-rules (info value)
  (aset (elt info 18) 1 value))

(defun igo-info-get-game-source (info)
  (elt (elt info 19) 1))
(defun igo-info-set-game-source (info value)
  (aset (elt info 19) 1 value))

(defun igo-info-get-time-limit (info)
  (elt (elt info 20) 1))
(defun igo-info-set-time-limit (info value)
  (aset (elt info 20) 1 value))

(defun igo-info-get-game-scribe-user (info)
  (elt (elt info 21) 1))
(defun igo-info-set-game-scribe-user (info value)
  (aset (elt info 21) 1 value))

(defun igo-info-get-white-rank (info)
  (elt (elt info 22) 1))
(defun igo-info-set-white-rank (info value)
  (aset (elt info 22) 1 value))

(defun igo-info-get-white-team (info)
  (elt (elt info 23) 1))
(defun igo-info-set-white-team (info value)
  (aset (elt info 23) 1 value))

(defun igo-info-get-game-handicap (info)
  (elt (elt info 24) 1))
(defun igo-info-set-game-handicap (info value)
  (aset (elt info 24) 1 value))

(defun igo-info-get-game-komi (info)
  (elt (elt info 25) 1))
(defun igo-info-set-game-komi (info value)
  (aset (elt info 25) 1 value))

(defun igo-info-get-black-territory      (info)
  (elt (elt info 26) 1))
(defun igo-info-set-black-territory      (info value)
  (aset (elt info 26) 1 value))

(defun igo-info-get-white-territory      (info)
  (elt (elt info 27) 1))
(defun igo-info-set-white-territory      (info value)
  (aset (elt info 27) 1 value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go Game State

(defun igo-new-gamestate (size)
  (let* ((w (car size))
		 (h (cdr size))
		 (state (make-vector h nil)))
	(cl-loop for j from 0 to (- h 1)
			 do (aset state j (make-vector w nil)))
	(vector state
            (vector 'black-capture: 0)
            (vector 'white-capture: 0)
            (vector 'ko: nil)
            (vector 'current-player: 'b)
            (vector 'last-move-annotations: nil)
            (vector 'game-info: (igo-new-game-info))
            (vector 'move-comment: nil)
            )))

(defun igo-state-size (gamestate)
  (let ((w (length (elt (elt gamestate 0) 0)))
		(h (length (elt gamestate 0))))
	(cons w h)))

(defun igo-state-get (coord gamestate)
  (if (or (not (car coord))
          (not (cdr coord)))
      'invalid
    (let* ((i-index (- (car coord) 1))
           (j-index (- (cdr coord) 1))
           (size (igo-state-size gamestate)))
      (if (or (< i-index 0)
              (< j-index 0)
              (>= i-index (car size))
              (>= j-index (cdr size)))
          'oob
        (elt (elt (elt gamestate 0) j-index) i-index)))))

(defun igo-state-set (coord gamestate new-value)
  (let* ((i-index (- (car coord) 1))
		 (j-index (- (cdr coord) 1)))
	(aset (elt (elt  gamestate 0) j-index) i-index new-value)))

(defun igo-state-get-capture (player gamestate)
  (elt (elt gamestate (if (eq player 'b) 1 2)) 1))

(defun igo-state-change-capture (player capture-count gamestate)
  (aset (elt gamestate (if (eq player 'b) 1 2)) 1 capture-count))

(defun igo-state-get-ko (gamestate)
  (elt (elt gamestate 3) 1))

(defun igo-state-set-ko (ko-coord gamestate)
  (aset (elt gamestate 3) 1 ko-coord))

(defun igo-state-get-current-player (gamestate)
  (elt (elt gamestate 4) 1))

(defun igo-state-set-current-player (player gamestate)
  (aset (elt gamestate 4) 1 player))

(defun igo-state-get-last-move-annotation (gamestate)
  (elt (elt gamestate 5) 1))

(defun igo-state-set-last-move-annotation (annotation gamestate)
  (aset (elt gamestate 5) 1 annotation))

(defun igo-state-get-game-info (gamestate)
  (elt (elt gamestate 6) 1))

(defun igo-state-set-game-info (game-info gamestate)
  (aset (elt gamestate 6) 1 game-info))

(defun igo-state-get-move-comment (gamestate)
  (elt (elt gamestate 7) 1))

(defun igo-state-set-move-comment (comment gamestate)
  (aset (elt gamestate 7) 1 comment))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go Game Flow

(defun igo-new-gameflow ()
  (vector nil nil))

(defun igo-gameflow-get-path (gameflow)
  (elt gameflow 0))

(defun igo-gameflow-get-flow (gameflow)
  (elt gameflow 1))

(defun igo-gameflow-set-path (gameflow path)
  (aset gameflow 0 path))

(defun igo-gameflow-set-flow (gameflow flow)
  (aset gameflow 1 flow))

(defun igo-validate-gametree (sgf-gametree)
  (if (or (not (listp sgf-gametree))
          (not (>= (length sgf-gametree) 3))
          (not (eq (elt sgf-gametree 0)  'gametree))
          (not (listp (elt sgf-gametree 1)))
          (not (eq (car (elt sgf-gametree 1)) 'sequence-list))
          (not (listp (elt sgf-gametree 2)))
          (not (eq (car (elt sgf-gametree 2)) 'gametree-list)))
      (igo-signal 'igo-error-invalid-property-values (list 'values: values 'type: 'gametree))))

(defun igo-gameflow-path-branch-instruction? (path-el)
  (if (and (listp path-el)
           (eq (elt path-el 0) 'branch)
           (numberp (elt path-el 1)))
      (elt path-el 1)
    nil))

(defun igo-gameflow-get-current-gametree (gameflow)
  (let ((path (igo-gameflow-get-path gameflow))
        (gametree (igo-gameflow-get-flow gameflow)))
    (cl-loop for path-element in path
             do (let ((branch-instruction (igo-gameflow-path-branch-instruction? path-element)))
                  (if branch-instruction
                      (let ((gametrees (igo-sgf-gametree-get-gametrees gametree)))
                        (if (>= branch-instruction (length gametrees))
                            (igo-signal 'igo-error-invalid-path (list "branch num: " branch-instruction
                                                                      " too big, max is: " (- (length gametrees) 1))))
                        (setq gametree (elt gametrees branch-instruction))))))
    gametree))

(defun igo-gameflow-apply (gameflow gamestate)
  (let ((path (igo-gameflow-get-path gameflow))
        (gametree (igo-gameflow-get-flow gameflow))
        (new-gamestate (igo-new-gamestate (igo-state-size gamestate))))
    (cl-loop for path-element in path
             for idx = 0 then (+ idx 1)
             with path-is-ok = t
             while path-is-ok
             do (let ((branch-instruction (igo-gameflow-path-branch-instruction? path-element)))
                  (if branch-instruction
                      (let ((gametrees (igo-sgf-gametree-get-gametrees gametree)))
                        (if (= (length gametrees) 0)
                            (progn (if (> idx 0) ; fix the path if it's broken
                                       (setcdr (nthcdr (- idx 1) path) nil))
                                   (setq path-is-ok nil))
                          (if (>= branch-instruction (length gametrees))
                              (igo-signal 'igo-error-invalid-path (list "branch num: " branch-instruction
                                                                        " too big, max is: " (- (length gametrees) 1)))))
                        (setq gametree (elt gametrees branch-instruction)))
                    (let* ((sequence (igo-sgf-gametree-get-sequence gametree))
                           (sequence-nodes (igo-sgf-sequence-get-nodes sequence)))
                      (if (not (numberp path-element))
                          (igo-signal 'igo-error-invalid-path (list "expected node number, got: " path-element)))
                      (if (>= path-element (length sequence-nodes))
                          (igo-signal 'igo-error-invalid-path (list "path node number too big: " path-element
                                                                    " max is: " (- (length sequence-nodes) 1))))
                      (cl-loop for node-idx from 0 to path-element
                               do (igo-sgf-apply-node (elt sequence-nodes node-idx) new-gamestate))))))
    new-gamestate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go Game Internals


(defun igo-get-neighbours (coord)
  (let* ((i (car coord))
		 (j (cdr coord)))
	(list (cons (- i 1) j)
		  (cons (+ i 1) j)
		  (cons i (- j 1))
		  (cons i (+ j 1)))))

(defun igo-get-group (start-coord gamestate)
  (let* ((group-type (igo-state-get start-coord gamestate)))
	(cl-labels ((get-group-rec (acc coord)
							   ;;(debug `(coord: ,coord acc: ,acc))
							   (let ((current (igo-state-get coord gamestate)))
								 (cond ((not (eq current group-type))			acc)
									   ((member coord acc)						acc)
									   (t (let* ((neighbours (igo-get-neighbours coord)))
											(seq-reduce (lambda (a c) (get-group-rec a c)) neighbours (cons coord acc))))))))
	  (get-group-rec '() start-coord))))

(defun igo-get-liberties (group gamestate)
  (cl-loop for group-coord in group
		   sum (cl-loop for neighbour-coord in (igo-get-neighbours group-coord)
						sum (let ((neighbour-value (igo-state-get neighbour-coord gamestate)))
							  (if (not neighbour-value) 1 0)))))

(defun igo-capture-group (group gamestate)
  (let ((capturing-player (igo-other-player (igo-state-get (car group) gamestate))))
   (dolist (group-coord group)
	 (igo-state-set group-coord gamestate nil))
   (igo-state-change-capture capturing-player (+ (igo-state-get-capture capturing-player gamestate) (length group)) gamestate)))

(defun igo-other-player (player)
  (if (eq player 'b) 'w 'b))

(defun igo-add-stone (player coord gamestate)
  (igo-state-set coord gamestate player))

(defun igo-play-move (player coord gamestate)
  (if (and (not (eq player 'b))
		   (not (eq player 'w)))
	  (igo-signal 'igo-error-invalid-player (list player)))
  (let* ((current-value (igo-state-get coord gamestate)))
	(if current-value (igo-signal 'igo-error-invalid-move (list (concat "already " (symbol-name current-value)
																	" stone at (" (number-to-string (car coord)) " . " (number-to-string (cdr coord)) ")"))))
	(if (equal coord (igo-state-get-ko gamestate))
		(igo-signal 'igo-error-invalid-move (list (concat "Ko rule: cannot play where a single stone was just capture..."))))

	(igo-state-set-ko nil gamestate) ; reset ko
	(igo-state-set coord gamestate player)

    (let ((other-player (igo-other-player player))
		  (neighbours (igo-get-neighbours coord)))
	  (dolist (neighbour-coord neighbours)
		(let ((neighbour-value (igo-state-get neighbour-coord gamestate)))
		  (if (eq neighbour-value other-player)
			  (let ((neighbour-group (igo-get-group neighbour-coord gamestate)))
				(if (= (igo-get-liberties neighbour-group gamestate) 0)
					(progn (igo-capture-group neighbour-group gamestate)
						   (if (= (length neighbour-group) 1)
							   (igo-state-set-ko neighbour-coord gamestate)))))))))
    
	(let ((group-liberties (igo-get-liberties (igo-get-group coord gamestate) gamestate)))
	  (if (= group-liberties 0)
		  (progn (igo-state-set coord gamestate nil)
				 (igo-signal 'igo-error-invalid-move (list (concat "Suicide move are not allowed..."))))))
	))

;; (let ((state (igo-new-gamestate (cons 9 9))))
;;   (igo-play-move 'w '(1 . 1) state)
;;   (igo-play-move 'w '(1 . 2) state)
;;   (igo-play-move 'w '(1 . 3) state)
;;   (igo-play-move 'b '(2 . 1) state)
;;   (igo-play-move 'b '(2 . 2) state)
;;   (igo-play-move 'b '(2 . 3) state)
;;   (igo-play-move 'b '(1 . 4) state)
;;   ;;(igo-get-liberties (igo-get-group '(1 . 1) state) state)
;;   (newline)
;;   (igo-draw-goban state)
;;   (insert (with-output-to-string (pp `(black: ,(igo-state-get-capture 'b state) white: ,(igo-state-get-capture 'w state)))))
;;   )

;; (let ((state (igo-new-gamestate (cons 9 9))))
;;   (igo-play-move 'b '(2 . 2) state)
;;   (igo-play-move 'w '(1 . 2) state)
;;   (igo-play-move 'w '(3 . 2) state)
;;   (igo-play-move 'w '(2 . 1) state)
;;   ;;(igo-play-move 'w '(2 . 3) state)
;;   (newline)
;;   (igo-draw-goban state)
;;   (insert (with-output-to-string (pp `(black: ,(igo-state-get-capture 'b state) white: ,(igo-state-get-capture 'w state) ko: ,(igo-state-get-ko state)))))
;;   )


(defun igo-is-star-coord? (i j w h)
  (let* ((w-star-dist (if (< w 13) 3 4))
		 (h-star-dist (if (< h 13) 3 4))
		 (w-center (+ (/ w 2) 1))
		 (h-center (+ (/ h 2) 1)))
	(and (or (= i w-star-dist)
			 (= i w-center)
			 (= i (- (+ w 1) w-star-dist)))
		 (or (= j h-star-dist)
			 (= j h-center)
			 (= j (- (+ h 1) h-star-dist))))))

(defun igo-player-stone (player)
  (cond ((eq player 'b) ?#)
        ((eq player 'w) ?O)
        (t nil)))

(defun igo-draw-position (i j w h gamestate)
  (let ((position-value (igo-state-get (cons i j) gamestate)))
	(cond ((igo-player-stone position-value)    (insert (string (igo-player-stone position-value) ?\s)))
		  ((igo-is-star-coord? i j w h)         (insert "+ "))
		  (t                                    (insert ". ")))))

(defun igo-draw-goban (gamestate)
  (let* ((inhibit-read-only t)
		 (size (igo-state-size gamestate))
		 (w (car size))
		 (h (cdr size)))

	(if igo-show-labels
		(progn (insert "  ")
			   (cl-loop for i from 0 to (- w 1)
						do (insert (string (+ ?a i) ?\s)))))
	
	(newline)
	(cl-loop for j from 1 to h
			 do (progn (if igo-show-labels (insert (string (+ ?A (- j 1)) ?\s)))
					   (cl-loop for i from 1 to w
								do (igo-draw-position i j w h gamestate))
					   (newline)))))

(defun igo-draw-game-info-el (label value)
  (if (and value
           (not (string= value "")))
      (insert (concat label value "\n"))))

(defun igo-draw-game-info (gamestate)
  (let ((game-info (igo-state-get-game-info gamestate)))
    (insert "    ---- Game Information (<i> to close) ----") (newline)
    (igo-draw-game-info-el "board-size: "(igo-info-get-board-size game-info))
    (igo-draw-game-info-el "annotator-name: "(igo-info-get-annotator-name game-info))
    (igo-draw-game-info-el "copyright: "(igo-info-get-copyright game-info))
    (newline)
    (igo-draw-game-info-el "date: "(igo-info-get-date game-info))
    (igo-draw-game-info-el "event: "(igo-info-get-event game-info))
    (igo-draw-game-info-el "game-name: "(igo-info-get-game-name game-info))
    (igo-draw-game-info-el "game-place: "(igo-info-get-game-place game-info))
    (igo-draw-game-info-el "game-round-info: "(igo-info-get-game-round-info game-info))
    (igo-draw-game-info-el "game-rules: "(igo-info-get-game-rules game-info))
    (igo-draw-game-info-el "game-source: "(igo-info-get-game-source game-info))
    (igo-draw-game-info-el "game-scribe-user: "(igo-info-get-game-scribe-user game-info))
    (newline)
    (igo-draw-game-info-el "opening-info: "(igo-info-get-opening-info game-info))
    (igo-draw-game-info-el "overtime-type: "(igo-info-get-overtime-type game-info))
    (igo-draw-game-info-el "time-limit: " (number-to-string (igo-info-get-time-limit game-info)))
    (newline)
    (igo-draw-game-info-el "black-player-name: "(igo-info-get-black-player-name game-info))
    (igo-draw-game-info-el "black-rank: "(igo-info-get-black-rank game-info))
    (igo-draw-game-info-el "black-team: "(igo-info-get-black-team game-info))
    (newline)
    (igo-draw-game-info-el "white-player-name: "(igo-info-get-white-player-name game-info))
    (igo-draw-game-info-el "white-rank: "(igo-info-get-white-rank game-info))
    (igo-draw-game-info-el "white-team: "(igo-info-get-white-team game-info))
    (newline)
    (igo-draw-game-info-el "game-handicap: " (number-to-string (igo-info-get-game-handicap game-info)))
    (igo-draw-game-info-el "game-komi: " (number-to-string (igo-info-get-game-komi game-info)))
    (igo-draw-game-info-el "result: "(igo-info-get-result game-info))
    (igo-draw-game-info-el "game-comment: "(igo-info-get-game-comment game-info))
    ))

(defun igo-draw-gamestate-info (gamestate)
  (let* ((inhibit-read-only t)
		 (width (car (igo-state-size gamestate)))
         (black-captures (igo-state-get-capture 'b gamestate))
         (white-captures (igo-state-get-capture 'w gamestate))
         (captures-str (concat "- Captures - black: " (number-to-string black-captures)
                               " white: " (number-to-string white-captures)))
         (black-territory (igo-info-get-black-territory (igo-state-get-game-info gamestate)))
         (white-territory (igo-info-get-white-territory (igo-state-get-game-info gamestate)))
         (territory-str (if (or black-territory white-territory)
                            (concat "- Territory - black: "
                                    (number-to-string (if black-territory (length black-territory) 0))
                                    " white: "
                                    (number-to-string (if white-territory (length white-territory) 0)))
                          nil))
         (black-player (igo-info-get-black-player-name (igo-state-get-game-info gamestate)))
         (black-rank (igo-info-get-black-rank (igo-state-get-game-info gamestate)))
         (white-player (igo-info-get-white-player-name (igo-state-get-game-info gamestate)))
         (white-rank (igo-info-get-white-rank (igo-state-get-game-info gamestate)))
         (players-str (if (and black-player black-rank white-player white-rank)
                          (concat "- Players - black: " black-player "(" black-rank
                                  ") white: " white-player "(" white-rank ")")
                        ""))
         (annotation    (igo-state-get-last-move-annotation gamestate))
         (comment       (igo-state-get-move-comment gamestate)))
    (insert players-str)
    (newline)
    (insert captures-str)
    (newline)
    (if territory-str
        (progn (insert territory-str) (newline)))
    (if annotation
        (insert (concat "- Annotation - " annotation)))
    (if comment
        (progn (insert (concat "- Comment - " comment))
               (newline)))))

;; (let ((state (igo-new-gamestate (cons 9 9))))
;;   (newline)
;;   (igo-draw-goban state))

(defun igo-draw-gametree (gametree start-node-num)
  (igo-validate-gametree gametree)

  (let ((seq-nodes (igo-sgf-sequence-get-nodes (igo-sgf-gametree-get-sequence gametree)))
        (rest-trees (igo-sgf-gametree-get-gametrees gametree))
        (node-num start-node-num)
        (seq-string ""))
    (cl-loop for node in seq-nodes
             do (progn (setq seq-string (concat seq-string (number-to-string node-num) "-"))
                       (setq node-num (+ node-num 1))))
    (insert seq-string)
    (cl-loop for tree in rest-trees
             for i from 0 to (- (length rest-trees) 1)
             do (progn (if (not (= i 0)) (progn (newline) (insert (make-string (length seq-string) ?\s))))
                       (igo-draw-gametree tree node-num)))
    nil))

(defun igo-draw-gameflow (gameflow)
  (let ((path (igo-gameflow-get-path gameflow))
        (flow (igo-gameflow-get-flow gameflow)))
    ;(igo-draw-gametree flow 0)

    ;; draw path
    (insert (concat "- Path - Move: " (number-to-string
                                       (cl-reduce (lambda (sum v) (if (consp v) (+ sum 1) (+ sum v))) path :initial-value 0))
                    (with-output-to-string (print (igo-gameflow-get-path gameflow)))))
    (newline)))

(defun igo-redraw ()
  (if (igo-is-igo-buffer?)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;;(insert (concat "igo mode: " (symbol-name igo-current-mode)) ) (newline) (newline)
        (igo-draw-goban igo-current-gamestate)
        (igo-draw-gamestate-info igo-current-gamestate)
        (igo-draw-gameflow igo-current-gameflow)
        )))

(defun igo-read-coord (gamestate)
  (let* ((play-str (read-string "coord: " nil nil))
		 (col (+ (- (elt play-str 0) ?a) 1))
		 (row (+ (- (elt play-str 1) ?A) 1))
		 (size (igo-state-size gamestate)))
	(if (or (< col 0)
			(>= row (car size))
			(< row 0)
			(>= row (cdr size)))
		(igo-signal 'igo-error-invalid-coord (list (cons col row))))
	(cons col row)))

(defun igo-get-buffer-position (col row)
  (save-excursion
    (goto-char (point-min))
    (forward-line row)
    (move-to-column (* col 2)) ; x2 for spaces
    (point)))

(defun igo-get-line-end (buff-pos)
  (save-excursion
    (goto-char buff-pos)
    (end-of-line)
    (point)))

(defun igo-convert-string-coord-to-coord (str-coord)
  (if (or (not (stringp str-coord))
          (not (= (length str-coord) 2)))
      (igo-signal 'igo-error-invalid-str-coord (list str-coord)))
  (igo-convert-char-coord-to-num-coord (cons (downcase (elt str-coord 0)) (upcase (elt str-coord 1)))))

(defun igo-convert-char-coord-to-num-coord (char-coord)
  (cl-labels ((convert-char (char base) (if (not char)
                                            nil
                                          (- char base -1))))
    (cons (convert-char (car char-coord) ?a)
          (convert-char (cdr char-coord) ?A))))

(defun igo-convert-num-coord-to-char-coord (num-coord)
  (cons (+ ?a (car num-coord) -1) (+ ?A (cdr num-coord) -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; igo placement mode (need to rename play to placement)

(defun igo-remove-display-current-move ()
  (while igo-active-overlays
    (let ((overlay (pop igo-active-overlays)))
      (delete-overlay overlay))))

(defun igo-display-current-move ()
  (let* ((coord (igo-convert-char-coord-to-num-coord igo-play-current-move))
         (board-size (igo-state-size igo-current-gamestate))
         (board-width (car board-size))
         (board-height (cdr board-size))
         (col-num (car coord))
         (row-num (cdr coord))
         (value (igo-state-get (cons col-num row-num) igo-current-gamestate)))
    (if (not (eq value 'oob))
        (progn (if (not value)
                   (let ((move-position (igo-get-buffer-position col-num row-num))
                         (inhibit-read-only t))
                     (save-excursion
                       (goto-char move-position)
                       (delete-char 1)
                       (insert (igo-player-stone (igo-state-get-current-player igo-current-gamestate))))))

               ;; delete existing overlays
               (igo-remove-display-current-move)

               ;; add row highlight overlay
               (if row-num
                   (cl-loop for c from 0 to board-height
                            do (if (or (not col-num) (not (= c col-num)))
                                   (let* ((row-position  (igo-get-buffer-position c row-num))
                                          (row-overlay (make-overlay row-position (min (+ row-position 2) (save-excursion (goto-char row-position) (line-end-position))))))
                                     (push row-overlay igo-active-overlays)
                                     (overlay-put row-overlay 'face '(:background "grey80"))))))
               ;; add col highlight overlay
               (if col-num
                   (cl-loop for r from 0 to board-width
                            do (if (or (not row-num) (not (= r row-num)))
                                   (let* ((col-position  (igo-get-buffer-position col-num r))
                                          (col-overlay (make-overlay col-position (+ col-position 1))))
                                     (push col-overlay igo-active-overlays)
                                     (overlay-put col-overlay 'face '(:background "grey80"))))))

               ;; add current move position highlight overlay
               (if (or (eq value 'b) (eq value 'w) (not value))
                   (let* ((move-pos  (igo-get-buffer-position col-num row-num))
                          (move-overlay (make-overlay move-pos (+ move-pos 1)))
                          (move-overlay-border (make-overlay (+ move-pos 1) (+ move-pos 2)))
                          (color (if value "red10" "green10")))
                     (push move-overlay igo-active-overlays)
                     (push move-overlay-border igo-active-overlays)
                     (overlay-put move-overlay 'face `(:background ,color))
                     (overlay-put move-overlay-border 'face `(:background "grey80"))))))

    ;; display message 
    (let* ((col-char (if (car igo-play-current-move) (car igo-play-current-move) ??))
           (row-char (if (cdr igo-play-current-move) (cdr igo-play-current-move) ??))
           (current-player (igo-state-get-current-player igo-current-gamestate))
           (player-str (cond ((eq current-player 'b) "black") ((eq current-player 'w) "white") (t "unknown"))))
      (message (concat "Next move for " player-str ":" (string col-char row-char) " press [enter] to submit or ctl-g to abort")))))

(defun igo-play-set-col (char)
  (setcar igo-play-current-move char)
  (igo-redraw)
  (igo-display-current-move))

(defun igo-play-set-row (char)
  (setcdr igo-play-current-move char)
  (igo-redraw)
  (igo-display-current-move))

(defun igo-play-commit-move ()
  (interactive)
  (if (or (not (car igo-play-current-move)) (not (cdr igo-play-current-move)))
      (igo-signal 'igo-error-invalid-move-input (list (with-output-to-string (pp igo-play-current-move)))))
  (let* ((coord (igo-convert-char-coord-to-num-coord igo-play-current-move))
         (value (igo-state-get coord igo-current-gamestate))
         (current-player (igo-state-get-current-player igo-current-gamestate)))
    (if (eq value 'oob)
        (igo-signal 'igo-error-invalid-move-input (list (with-output-to-string (pp igo-play-current-move)))))
    (igo-play-move current-player coord igo-current-gamestate)
    (setq igo-play-last-move igo-play-current-move)
    (setq igo-play-current-move (cons nil nil))
    (igo-state-set-current-player (igo-other-player current-player) igo-current-gamestate)
    (igo-redraw)
    (igo-display-current-move)))

(defun igo-exit-play-mode ()
  (interactive)
  (igo-play-mode nil))

(defun igo-clamp (v min max)
  (if (< v min)
      min
    (if (> v max)
        max
      v)))

(defun igo-play-arrow-input (deltaCol deltaRow)
  (let* ((last-coord    (let ((last (igo-convert-char-coord-to-num-coord igo-play-last-move)))
                          (if (and last (car last) (cdr last)) last (cons 0 0))))
         (coord         (igo-convert-char-coord-to-num-coord igo-play-current-move))
         (coord-valid?  (and (car coord) (cdr coord)))
         (col           (if coord-valid? (car coord) (car last-coord)))
         (row           (if coord-valid? (cdr coord) (cdr last-coord)))
         (board-size    (igo-state-size igo-current-gamestate))
         (new-col       (igo-clamp (+ col deltaCol) 1 (car board-size)))
         (new-row       (igo-clamp (+ row deltaRow) 1 (cdr board-size))))
    (setq igo-play-current-move (igo-convert-num-coord-to-char-coord (cons new-col new-row))))
  (igo-redraw)
  (igo-display-current-move))

(defmacro igo-gen-play-key-fun (fun start-char end-char)
  (append '(progn) (cl-loop for i from start-char to end-char
                            collect `(define-key map ,(string i) (lambda () (interactive) (,fun ,i))))))

(defun igo-play-mode-map ()
  (let ((size (igo-state-size igo-current-gamestate))
		(map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (igo-gen-play-key-fun igo-play-set-col ?a ?z)
    (igo-gen-play-key-fun igo-play-set-row ?A ?Z)
    (define-key map (kbd "<return>")    'igo-play-commit-move)
    (define-key map (kbd "<end>")       'igo-exit-play-mode)
    (define-key map (kbd "<right>")     (lambda () (interactive) (igo-play-arrow-input 1 0)))
    (define-key map (kbd "<left>")      (lambda () (interactive) (igo-play-arrow-input -1 0)))
    (define-key map (kbd "<up>")        (lambda () (interactive) (igo-play-arrow-input 0 -1)))
    (define-key map (kbd "<down>")      (lambda () (interactive) (igo-play-arrow-input 0 1)))
    (define-key map (kbd "<SPC>")     'igo-cycle-mode)
	map))

(defun igo-play-mode (&optional optionalNewMode)
  (interactive)
  
  (setq igo-current-mode 'play)
  (setq igo-mode-map (igo-play-mode-map))

  (if (igo-is-igo-buffer?)
      (use-local-map igo-mode-map))
  (igo-redraw)
  (igo-display-current-move)
  (message (concat "igo current mode: "(symbol-name igo-current-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; igo view mode

(defun igo-get-prev-path-el (path idx &optional keep-branch?)
  (if (>= idx (length path))
      nil
    (if (<= idx 0)
        nil
      (let ((prev-index (- idx 1)))
        (if (>= prev-index 0)
            (let ((prev-el (nthcdr prev-index path)))
              (if (igo-gameflow-path-branch-instruction? (car prev-el))
                  (if keep-branch?
                      prev-el
                    (igo-get-prev-path-el path prev-index))
                prev-el))
          nil)))))

(defun igo-get-parent-path (path)
  (let* ((path-copy (copy-sequence path))
         (current-path-idx (- (length path-copy) 1))
         (prev-path-el (igo-get-prev-path-el path-copy current-path-idx)))
    (if (not prev-path-el)
        nil
      (progn (setcdr prev-path-el nil)
             path-copy))))

(defun igo-get-parent-gametree (path)
  (let ((parent-path (igo-get-parent-path path))
        (flow-copy (copy-sequence igo-current-gameflow)))
    (igo-gameflow-set-path flow-copy parent-path)
    (igo-gameflow-get-current-gametree flow-copy)))

(defun igo-view-apply-delta-row (delta-row)
  (if (not (= delta-row 0))
      (let* ((path (igo-gameflow-get-path igo-current-gameflow))
             (current-path-idx  (- (length path) 1))
             (prev-path-branch  (car (igo-get-prev-path-el path current-path-idx 'keep-branch))))
        (if (igo-gameflow-path-branch-instruction? prev-path-branch)
            (let* ((parent-gametree   (igo-get-parent-gametree path))
                   (max-branch        (- (length (igo-sgf-gametree-get-gametrees parent-gametree)) 1))
                   (branch-id-cell   (cdr prev-path-branch)))
              (setcar branch-id-cell (min max-branch (max 0 (+ (car branch-id-cell) delta-row))))

              ;; must get after changing the tree in the path to get the right sequence
              (let* ((current-path-el   (nthcdr (- (length path) 1) path))
                     (gametree          (igo-gameflow-get-current-gametree igo-current-gameflow))
                     (current-seq-nodes (igo-sgf-sequence-get-nodes (igo-sgf-gametree-get-sequence gametree))))
                (setcar current-path-el (max 0 (min (car current-path-el) (- (length current-seq-nodes) 1))))))))))

(defun igo-view-apply-delta-col (delta-col)
  (if (not (= delta-col 0))
   (let* ((path (igo-gameflow-get-path igo-current-gameflow))
          (current-path-idx (- (length path) 1))
          (current-path-el (if (>= current-path-idx 0)
                               (nthcdr current-path-idx path)
                             nil)))
     (if (igo-gameflow-path-branch-instruction? (car current-path-el))
         (igo-signal 'igo-error-invalid-path (list "unexpected branch path")))
    
     (if (not current-path-el)
         (igo-gameflow-set-path igo-current-gameflow (list (max 0 delta-col)))
       (let* ((new-current-path-value (+ (car current-path-el) delta-col))
              (current-gametree (igo-gameflow-get-current-gametree igo-current-gameflow))
              (current-gametree-seq (igo-sgf-gametree-get-sequence current-gametree))
              (current-tree-nodes (igo-sgf-sequence-get-nodes current-gametree-seq))
              (node-count (length current-tree-nodes)))
         (cond ((< new-current-path-value 0)
                (let ((prev-path-el (igo-get-prev-path-el path current-path-idx)))
                  (if prev-path-el
                      (progn (setcdr prev-path-el nil)
                             (igo-view-apply-delta-col (- new-current-path-value (- 1))))
                    (igo-gameflow-set-path igo-current-gameflow (list 0)))))
               ((>= new-current-path-value node-count)
                (setcdr current-path-el (cons (list 'branch 0) (cons (- new-current-path-value node-count) nil))))
               (t
                (setcar current-path-el new-current-path-value))))))))

(defun igo-view-arrow-input (delta-col delta-row)
  (igo-view-apply-delta-row delta-row)
  (igo-view-apply-delta-col delta-col) delta-row
  (setq igo-current-gamestate (igo-gameflow-apply igo-current-gameflow igo-current-gamestate))
  (igo-redraw))

(setq igo-showing-game-info nil)
(defun igo-view-show-game-info ()
  (interactive)
  (setq igo-showing-game-info (not igo-showing-game-info))
  (if igo-showing-game-info
      (if (igo-is-igo-buffer?)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (igo-draw-game-info igo-current-gamestate)))
    (igo-redraw)))

(defun igo-view-mode-map ()
  (let ((size (igo-state-size igo-current-gamestate))
		(map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "<right>")     (lambda () (interactive) (igo-view-arrow-input 1 0)))
    (define-key map (kbd "<left>")      (lambda () (interactive) (igo-view-arrow-input -1 0)))
    (define-key map (kbd "<up>")        (lambda () (interactive) (igo-view-arrow-input 0 -1)))
    (define-key map (kbd "<down>")      (lambda () (interactive) (igo-view-arrow-input 0 1)))
    (define-key map (kbd "<S-right>")   (lambda () (interactive) (igo-view-arrow-input 10 0)))
    (define-key map (kbd "<S-left>")    (lambda () (interactive) (igo-view-arrow-input -10 0)))
    (define-key map (kbd "l")           'igo-load-sgf-file)
    (define-key map (kbd "i")           'igo-view-show-game-info)
    (define-key map (kbd "<SPC>")       'igo-cycle-mode)
	map))

(defun igo-view-mode ()
  "Sets igo into view mode."
  (interactive)
  (setq igo-current-mode 'view)
  (setq igo-mode-map (igo-view-mode-map))
  (if (igo-is-igo-buffer?)
      (use-local-map igo-mode-map))
  (igo-redraw)
  (igo-remove-display-current-move)
  (message (concat "igo current mode: "(symbol-name igo-current-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; igo-mode definition

(defun igo-cycle-mode ()
  "Cycles through igo modes"
  (interactive)
  (cond ((eq igo-current-mode 'play) (igo-view-mode))
        ((eq igo-current-mode 'view) (igo-play-mode))
        (t (igo-view-mode))))

(defun igo ()
  "Play go"
  (interactive)
  (let ((igo-buffer (get-buffer-create igo-buffer-name)))
    (switch-to-buffer igo-buffer)
    (igo-mode)
	(if (not igo-current-gamestate)
        (setq igo-current-gamestate (igo-new-gamestate (cons 19 19))))
	(igo-redraw)
    (igo-view-mode)))

(defun igo-test () (interactive) (message "test"))

(defun igo-default-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
	(define-key map "p" 'igo-play-next-move)
    (define-key map (kbd "<SPC>")     'igo-cycle-mode)
	;; (define-key map (kbd "<C-M-backspace>") 'ide-grep-solution)
	map))

(setq igo-mode-map (igo-default-map))

(define-derived-mode igo-mode special-mode "Igo" "Major Mode for playing Go"
  ;; :syntax-table
  ;; :abbrev-table
  ;; :group
  )

(provide 'igo)

