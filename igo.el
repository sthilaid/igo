(define-error 'igo-error-sgf-parsing "sgf parsing error occured: ")
(define-error 'igo-error-invalid-player "Invalid player used in game of go, should be 'b or 'w")
(define-error 'igo-error-invalid-move "Played move is not valid: ")
(define-error 'igo-error-invalid-coord "Invalid coordinate: ")
(define-error 'igo-error-invalid-move-input "Invalid move input: ")

(setq igo-buffer-name "*igo*")
(setq igo-show-labels 't)
(setq igo-current-mode nil)
(setq igo-current-gamestate nil)
(setq igo-current-gameflow nil)
(setq igo-play-last-move (cons nil nil))
(setq igo-play-current-move (cons nil nil))
(setq igo-play-current-player 'b)
(setq igo-active-overlays nil)
;; (setq igo-examble-game (let ((ex-game-file "ff4_ex.sgf"))
;;                          (if (file-exists-p ex-game-file)
;;                              (with-temp-buffer
;;                                (insert-file-contents ex-game-file)
;;                                (buffer-string))
;;                            "")))

;; (setq igo-examble-game (let ((ex-game-file "c:/Users/dsthillaire/Downloads/9989-Lebertran-hkkmomo-zaphod.sgf"))
;;                          (if (file-exists-p ex-game-file)
;;                              (with-temp-buffer
;;                                (insert-file-contents ex-game-file)
;;                                (buffer-string))
;;                            "")))
;; (pp (igo-parse-sgf-collection igo-examble-game))
;;()

(defun igo-is-igo-buffer? ()
  (string= (buffer-name (current-buffer))
           igo-buffer-name))

;; TEMPORARY
(setq max-specpdl-size 50000)
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
                 do (signal 'igo-error-sgf-parsing (list (concat "wrong token, expecting: " token " got: " str)))

                 if (and (not (igo-parse-ignore-char c))
                         (= c (elt token token-idx)))
                 do (setq token-idx (+ token-idx 1))

                 if (>= token-idx (length token))
                 return (substring str (+ i 1))))
    (signal 'igo-error-sgf-parsing (list (concat "wrong token, expecting: " token " got: " str)))))

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
  (cl-labels ((call-error () (signal 'igo-error-sgf-parsing (list (concat "invalid property for " sgf-str))))
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
  (if (or (not sgf-str) (string= sgf-str "")) (signal 'igo-error-sgf-parsing (list (concat "invalid value string: " sgf-str))))
  (let* ((str (igo-parse-next-token sgf-str "["))
               (value-type (igo-parse-sgf-value-type str))
               (result (igo-parse-next-token (cdr value-type) "]")))
          (cons (car value-type) result)))

;; (igo-parse-sgf-property-value nil)
;; (igo-parse-sgf-property-value "")
;; (igo-parse-sgf-property-value "[text test]")

(defun igo-parse-sgf-property (sgf-str)
  (if (or (not sgf-str) (string= sgf-str "")) (signal 'igo-error-sgf-parsing (list (concat "invalid property string: " sgf-str))))
  (let* ((property-id (igo-parse-sgf-property-id sgf-str))
         (property-values (igo-parse-sgf-list (cdr property-id) 'igo-parse-sgf-property-value 'value)))
    (cons (list (car property-id) (car property-values)) (cdr property-values))))

;(igo-parse-sgf-property "C[text test][another comment]")

(defun igo-parse-sgf-node (sgf-str)
  (let ((node-str-rest (igo-parse-next-token sgf-str ";")))
    (if node-str-rest
		;(igo-parse-sgf-property node-str-rest)
		(igo-parse-sgf-list node-str-rest 'igo-parse-sgf-property 'property)
      (signal 'igo-error-sgf-parsing (list (concat "while paring node: " sgf-str))))))

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
        (signal 'igo-error-sgf-parsing (list (concat "invalid gametree token ( for: " str)))
      (let ((sequence (igo-parse-sgf-sequence seq-str)))
        (progn
          (let ((subtrees (igo-parse-sgf-list (cdr sequence) 'igo-parse-sgf-gametree 'gametree)))
            (let ((rest (igo-parse-next-token (cdr subtrees) ")")))
              (if (not rest)
                  (signal 'igo-error-sgf-parsing (list (concat "invalid gametree token ) for: " (cdr subtrees))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go Game State

(defun igo-new-gamestate (size)
  (let* ((w (car size))
		 (h (cdr size))
		 (state (make-vector h nil)))
	(cl-loop for j from 0 to (- h 1)
			 do (aset state j (make-vector w nil)))
	(vector state (vector 'black-capture: 0) (vector 'white-capture: 0) (vector 'ko: nil))))

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

;; (defun igo-gameflow-apply (gameflow gamestate)
;;   (let ((new-gamestate (igo-new-gamestate (igo-state-size gamestate)))
;;         (path (igo-gameflow-get-path gameflow)))
;;     (cl-loop for path-element in path
;;              do (let ((branch   (car path-element))
;;                       (count    (cdr path-element)))
;;                   ;; find right branch in flow
;;                   ;; apply count moves from that branch
;;                   (cl-loop n from 1 to count
;;                            do)))))

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

(defun igo-play-move (player coord gamestate)
  (if (and (not (eq player 'b))
		   (not (eq player 'w)))
	  (signal 'igo-error-invalid-player (list player)))
  (let* ((current-value (igo-state-get coord gamestate)))
	(if current-value (signal 'igo-error-invalid-move (list (concat "already " (symbol-name current-value)
																	" stone at (" (number-to-string (car coord)) " . " (number-to-string (cdr coord)) ")"))))
	(if (equal coord (igo-state-get-ko gamestate))
		(signal 'igo-error-invalid-move (list (concat "Ko rule: cannot play where a single stone was just capture..."))))

	(igo-state-set-ko nil gamestate) ; reset ko
	(igo-state-set coord gamestate player)

	(let ((group-liberties (igo-get-liberties (igo-get-group coord gamestate) gamestate)))
	  (if (= group-liberties 0)
		  (progn (igo-state-set coord gamestate nil)
				 (signal 'igo-error-invalid-move (list (concat "Suicide move are not allowed..."))))))

	(let ((other-player (igo-other-player player))
		  (neighbours (igo-get-neighbours coord)))
	  (dolist (neighbour-coord neighbours)
		(let ((neighbour-value (igo-state-get neighbour-coord gamestate)))
		  (if (eq neighbour-value other-player)
			  (let ((neighbour-group (igo-get-group neighbour-coord gamestate)))
				(if (= (igo-get-liberties neighbour-group gamestate) 0)
					(progn (igo-capture-group neighbour-group gamestate)
						   (if (= (length neighbour-group) 1)
							   (igo-state-set-ko neighbour-coord gamestate)))))))))))

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

(defun igo-draw-gameinfo (gamestate)
  (let* ((inhibit-read-only t)
		 (width (car (igo-state-size gamestate)))
         (black-captures (igo-state-get-capture 'b gamestate))
         (white-captures (igo-state-get-capture 'w gamestate))
         (black-str (concat "black: " (number-to-string black-captures)))
         (white-str (concat "white: " (number-to-string white-captures)))
         (header "- Captures -")
         (padding-count (max 0 (- (* 2 width) (+ (length black-str) (length white-str) (length header)))))
         (pad (max 1 (/ padding-count 4)))
         (pad-str (make-string pad ?\s)))
    (insert pad-str header pad-str black-str pad-str white-str pad-str)
    (newline)))

;; (let ((state (igo-new-gamestate (cons 9 9))))
;;   (newline)
;;   (igo-draw-goban state))

(defun igo-redraw ()
  (if (igo-is-igo-buffer?)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (igo-draw-goban igo-current-gamestate)
        (igo-draw-gameinfo igo-current-gamestate))))

(defun igo-read-coord (gamestate)
  (let* ((play-str (read-string "coord: " nil nil))
		 (col (+ (- (elt play-str 0) ?a) 1))
		 (row (+ (- (elt play-str 1) ?A) 1))
		 (size (igo-state-size gamestate)))
	(if (or (< col 0)
			(>= row (car size))
			(< row 0)
			(>= row (cdr size)))
		(signal 'igo-error-invalid-coord (list (cons col row))))
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

(defun igo-convert-char-coord-to-num-coord (char-coord)
  (cl-labels ((convert-char (char base) (if (not char)
                                            nil
                                          (- char base -1))))
    (cons (convert-char (car char-coord) ?a)
          (convert-char (cdr char-coord) ?A))))

(defun igo-convert-num-coord-to-char-coord (num-coord)
  (cons (+ ?a (car num-coord) -1) (+ ?A (cdr num-coord) -1)))

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
                       (insert (igo-player-stone igo-play-current-player)))))

               ;; delete existing overlays
               (while igo-active-overlays
                 (let ((overlay (pop igo-active-overlays)))
                   (delete-overlay overlay)))

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
    (let ((col-char (if (car igo-play-current-move) (car igo-play-current-move) ??))
          (row-char (if (cdr igo-play-current-move) (cdr igo-play-current-move) ??))
          (player-str (cond ((eq igo-play-current-player 'b) "black") ((eq igo-play-current-player 'w) "white") (t "unknown"))))
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
      (signal 'igo-error-invalid-move-input (list (with-output-to-string (pp igo-play-current-move)))))
  (let* ((coord (igo-convert-char-coord-to-num-coord igo-play-current-move))
         (value (igo-state-get coord igo-current-gamestate)))
    (if (eq value 'oob)
        (signal 'igo-error-invalid-move-input (list (with-output-to-string (pp igo-play-current-move)))))
    (igo-play-move igo-play-current-player coord igo-current-gamestate)
    (setq igo-play-last-move igo-play-current-move)
    (setq igo-play-current-move (cons nil nil))
    (setq igo-play-current-player (igo-other-player igo-play-current-player))
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
    (define-key map (kbd "<SPC>")     'igo-play-mode)
	map))

(defun igo-play-mode (&optional optionalNewMode)
  (interactive)
  (let ((next-mode (if optionalNewMode
                       optionalNewMode
                     (if (eq igo-current-mode 'play) nil 'play))))
   (setq igo-current-mode next-mode))

  (if (eq igo-current-mode 'play)
	  (setq igo-mode-map (igo-play-mode-map))
	(setq igo-mode-map (igo-default-map)))

  (if (igo-is-igo-buffer?)
      (use-local-map igo-mode-map))
  
  (if (eq igo-current-mode 'play)
      (progn
        (igo-redraw)
        (igo-display-current-move))
    (message (concat "igo current mode: "(symbol-name igo-current-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; igo-mode definition

(defun igo ()
  "Play go"
  (interactive)
  (let ((igo-buffer (get-buffer-create igo-buffer-name)))
    (switch-to-buffer igo-buffer)
    (igo-mode)
	(setq igo-current-gamestate (igo-new-gamestate (cons 19 19)))
	(igo-redraw)))

(defun igo-test () (interactive) (message "test"))

(defun igo-default-map ()
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
	(define-key map "p" 'igo-play-next-move)
    (define-key map (kbd "<SPC>")     'igo-play-mode)
	;; (define-key map (kbd "<C-M-backspace>") 'ide-grep-solution)
	map))

(setq igo-mode-map (igo-default-map))

(define-derived-mode igo-mode special-mode "Igo" "Major Mode for playing Go"
  ;; :syntax-table
  ;; :abbrev-table
  ;; :group
  )

(provide 'igo)
