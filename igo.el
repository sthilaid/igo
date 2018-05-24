
(define-error 'igo-error-sgf-parsing "sgf parsing error occured...")

(setq igo-buffer-name "*igo*")

(setq igo-examble-game (let ((ex-game-file "ff4_ex.sgf"))
                         (if (file-exists-p ex-game-file)
                             (with-temp-buffer
                               (insert-file-contents ex-game-file)
                               (buffer-string))
                           "")))

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
                 do (signal 'igo-error-sgf-parsing (concat "wrong token, expecting: " token " got: " str))

                 if (and (not (igo-parse-ignore-char c))
                         (= c (elt token token-idx)))
                 do (setq token-idx (+ token-idx 1))

                 if (>= token-idx (length token))
                 return (substring str (+ i 1))))
    (signal 'igo-error-sgf-parsing (concat "wrong token, expecting: " token " got: " str))))

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
      (cons (cons (make-symbol (concat (symbol-name list-type) "-list")) (reverse (car result))) (cdr result)))))

(defun igo-parse-is-letter? (char)
  (let ((downcase-char (downcase char)))
    (and (>= downcase-char ?a)
         (<= downcase-char ?z))))

;; (igo-parse-is-letter? (elt "aaa" 0))
;; (igo-parse-is-letter? ?a)

(defun igo-parse-sgf-property-id (sgf-str)
  (cl-labels ((call-error () (signal 'igo-error-sgf-parsing (concat "invalid property for " sgf-str)))
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
									  (cons (list 'double (elt str 0)) (substring str 1))
									nil))
			  (parse-color (str) (if (and (>= (length str) 2)
										  (let ((char (elt str 0))) (or (= char ?B) (= char ?W)))
										  (= (elt str 1) ?\]))
									 (cons (list 'color (elt str 0)) (substring str 1))
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
  (if (or (not sgf-str) (string= sgf-str "")) (signal 'igo-error-sgf-parsing (concat "invalid value string: " sgf-str)))
  (let* ((str (igo-parse-next-token sgf-str "["))
               (value-type (igo-parse-sgf-value-type str))
               (result (igo-parse-next-token (cdr value-type) "]")))
          (cons (car value-type) result)))

;; (igo-parse-sgf-property-value nil)
;; (igo-parse-sgf-property-value "")
;; (igo-parse-sgf-property-value "[text test]")

(defun igo-parse-sgf-property (sgf-str)
  (if (or (not sgf-str) (string= sgf-str "")) (signal 'igo-error-sgf-parsing (concat "invalid property string: " sgf-str)))
  (let* ((property-id (igo-parse-sgf-property-id sgf-str))
         (property-values (igo-parse-sgf-list (cdr property-id) 'igo-parse-sgf-property-value 'value)))
    (cons (list (car property-id) (car property-values)) (cdr property-values))))

;(igo-parse-sgf-property "C[text test][another comment]")

(defun igo-parse-sgf-node (sgf-str)
  (let ((node-str-rest (igo-parse-next-token sgf-str ";")))
    (if node-str-rest
		;(igo-parse-sgf-property node-str-rest)
		(igo-parse-sgf-list node-str-rest 'igo-parse-sgf-property 'property)
      (signal 'igo-error-sgf-parsing (concat "while paring node: " sgf-str)))))

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
        (signal 'igo-error-sgf-parsing (concat "invalid gametree token ( for: " str))
      (let ((sequence (igo-parse-sgf-sequence seq-str)))
        (progn
          (let ((subtrees (igo-parse-sgf-list (cdr sequence) 'igo-parse-sgf-gametree 'gametree)))
            (let ((rest (igo-parse-next-token (cdr subtrees) ")")))
              (if (not rest)
                  (signal 'igo-error-sgf-parsing (concat "invalid gametree token ) for: " (cdr subtrees)))
                (cons (list 'gametree (car sequence) (car subtrees))
                      rest)))))))))

;;(igo-parse-sgf-gametree igo-examble-game)
;;(igo-parse-sgf-gametree "(;B[ab]C[comment];B[cd]C[comment](;Z[12]C[comment];Z[45]C[comment])(;X[12.0]C[comment];Y[12]))")
;;(pp (igo-parse-sgf-gametree "(;B[ab]C[comment];B[cd]C[comment])"))
;;(pp (igo-parse-sgf-gametree "(;B[ab]C[comment];B[cd]C[comment](;XY[12]C[comment];YZ[15])(;H[1]B[B];H[2]B[W]))"))

;;(pp (igo-parse-sgf-gametree "(;FF[4]GM[1]SZ[19]CA[UTF-8]SO[gokifu.com]BC[cn]WC[kr]EV[]PB[Ke Jie]BR[9p]PW[Lee Sedol]WR[9p]KM[7.5]DT[2018-05-21]RE[W+R]TM[150]LT[]LC[5]GK[1];B[pd];W[dp];B[qp];W[cc];B[np];W[fq];B[qf];W[fd];B[bo];W[cp];B[ck];W[ci];B[cn];W[ob];B[pb];W[ej];B[oc];W[jd];B[he];W[hd];B[cd];W[dc];B[fe];W[ge];B[gf];W[gd];B[di];W[dj];B[cj];W[ch];B[ei];W[dl];B[cl];W[dg];B[fi];W[fg];B[ff];W[gg];B[hg];W[hh];B[ed];W[de];B[ee];W[dd];B[ec];W[ih];B[ie];W[id];B[ig];W[ef];B[fb];W[nc];B[kg];W[nb];B[le];W[lc];B[hb];W[ib];B[nd];W[qk];B[qm];W[qh];B[gp];W[hr];B[iq];W[hq];B[hp];W[ip];B[fp];W[ep];B[gq];W[gr];B[fr];W[eq];B[ir];W[er];B[kq];W[io];B[ko];W[ok];B[pj];W[pk];B[ri];W[ki];B[qi];W[ni];B[mk];W[nm];B[mi];W[mh];B[ld];W[mc];B[li];W[lh];B[kh];W[oh];B[ng];W[nh];B[rh];W[lk];B[jb];W[ia];B[kc];W[kb];B[kj];W[ji];B[ml];W[md];B[ne];W[pm];B[jj];W[ij];B[oj];W[nj];B[kl];W[ik];B[ll];W[me];B[mf];W[lg];B[lf];W[kf];B[je];W[jg];B[jf];W[jh];B[kd];W[jc];B[qn];W[nk];B[gn];W[em];B[dk];W[ek];B[dm];W[en];B[el];W[fl];B[hm];W[hn];B[fk];W[dl];B[lb];W[ja];B[el];W[qc];B[pc];W[dl];B[oa];W[la];B[el];W[hf];B[if];W[dl];B[na];W[mb];B[el];W[kg];B[ke];W[dl];B[hs];W[el];B[bi];W[fs];B[bh];W[bg];B[im];W[go];B[on];W[qg];B[rg];W[pf];B[qe];W[bp];B[ap];W[gm];B[om];W[nl];B[db];W[cb];B[ca];W[ba];B[da];W[ah];B[bj];W[jn];B[jm];W[kn];B[in];W[lo];B[ho];W[kp];B[lq];W[mn];B[fo];W[mp];B[mq];W[co];B[bn];W[jl];B[gl];W[fm];B[hk];W[fj];B[il];W[jk];B[hj];W[km];B[eo];W[dn];B[do];W[gk];B[gi];W[hl];B[cf];W[df];B[gl];W[cm];B[cg];W[dh];B[bm];W[hi];B[kk];W[ii])"))q

(defun igo-parse-sgf-collection (sgf-str)
  (igo-parse-sgf-list sgf-str 'igo-parse-sgf-gametree 'collection))

;;(pp (igo-parse-sgf-collection "(;A[1])(B[2])"))

(defun igo-sgf-parsing (sgf-data)
  "Converts sgf game into internal igo-mode format."
  'todo)

(defun igo ()
  "Play go"
  (interactive)
  (let ((igo-buffer (get-buffer-create igo-buffer-name)))
    (switch-to-buffer igo-buffer)
    ))

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

(define-derived-mode igo-mode special-mode "Igo" "Major Mode for playing Go"
  ;; :syntax-table
  ;; :abbrev-table
  ;; :group
  )

(provide 'igo)
