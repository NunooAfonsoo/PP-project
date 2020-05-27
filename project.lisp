(in-package :user)

;
;
;A função get-
;internal-run-time e a constante INTERNAL-TIME-UNITS-PER-SECOND
;podem ser utilizadas para ajudar a controlar o tempo de execução.
;
;

(load "procura")
(setq time (get-internal-run-time))


(defstruct state
            currentGameTable
            previousState
            score
            piecesEliminated
            piecesEliminatedSoFar
            playMadeSoFar
            >)


(setq bestState (make-state :currentGameTable nil
                            :previousState nil
                            :score 0
                            :piecesEliminated 0
                            :piecesEliminatedSoFar 0
                            :playMadeSoFar nil
                            ))

(defun giveRow (pos)
    (car pos)
)

(defun giveColumn (pos)
    (car (cdr pos))
)

(defun checkEqualRight (pos list)
    (eq (nth (car (cdr pos)) list) (nth (1+ (car (cdr pos))) list))
)

(defun checkEqualLeft (pos list)
    (eq (nth (car (cdr pos)) list) (nth (1- (car (cdr pos))) list))
)

(defun checkEqualAbove (pos list1 list2)
    (eq (nth (car (cdr pos)) list1) (nth (car (cdr pos)) (nth (1- (car pos)) list2)))
)

(defun checkEqualBelow (pos list1 list2)
    (eq (nth (car (cdr pos)) list1) (nth (car (cdr pos)) (nth (1+ (car pos)) list2)))
)

(defun checkFirstColumn (pos list)
    (= (car (cdr pos)) 0)
)

(defun checkLastColumn (pos list)
    (= (car (cdr pos)) (1- (list-length list)))
)

(defun checkFirstRow (pos list)
    (= (car pos) 0)
)

(defun checkLastRow (pos list)
    (= (car pos) (1- (list-length list)))
)


(defun objectiveState (state)
    (if (>= (/ (- (get-internal-run-time) time) INTERNAL-TIME-UNITS-PER-SECOND) 5) 
        t
        nil 
    )
)

(defun generateSucces (state)
    (if (> (state-score state) (state-score bestState))
        (setq bestState state)
    )


    (setq generated '())
    (setq expanded '())
    (setq gameTable (state-currentGameTable state))

    (loop for row from 0 to (1- (list-length gameTable))
    do
        (loop for column from 0 to (1- (list-length (nth 0 gameTable)))
        do
            (progn
                (if (and (null (elementInListOfLists (list row column) expanded)) (giveElement (list row column) gameTable) )
                (progn
                    (setq generated (performExpansion '() (list (list row column)) gameTable) )
                    (if (/= 1 (list-length generated))
                        (setq expanded (appendLists expanded (list generated)))
                    )
                    (setq generated '())
                )
                )
            )
        )
    )

    (setq newBoards '())
    (dolist (pieces expanded)
        (setq newGameTable (copy-tree gameTable))
        (setq board (clearBoard pieces newGameTable))
        (setq newBoards (append newBoards (list (make-state :currentGameTable  (propagateChange board)
                                                      :previousState state
                                                      :score (+ (state-score state) (* (- (list-length pieces) 2 ) (- (list-length pieces) 2 )))
                                                      :piecesEliminated (list-length pieces)
                                                      :piecesEliminatedSoFar (+ (state-piecesEliminatedSoFar state) (list-length pieces))
                                                      :playMadeSoFar (append (list (car pieces)) (state-playMadeSoFar state))
                                                      ))))
    )
    newBoards
)

(defun makePlay (expanded gameTable)
    (setq newGameTable (copy-tree gameTable))
    (cond
        ( (null  expanded) nil)
        (t (append (list (clearBoard (car expanded) newGameTable)) (makePlay (cdr expanded) gameTable))   )
    )
)


(defun clearBoard (positions gameTable)
    (cond 
        ( (null positions) gameTable)
        (t (progn 
                (setf (nth (giveColumn (car positions)) (nth (giveRow (car positions)) gameTable)) nil)
                (clearBoard (cdr positions) gameTable)
           )
        )
    )
)


(defun propagateChange (gameTable)
    (loop for row from 0 to (1- (list-length gameTable))
    do
        (loop for column from 0 to (1- (list-length (nth 0 gameTable)))
        do
            (if (giveElement (list row column) gameTable)
                (progn
                
                    (if (and (/= row (1- (list-length gameTable))) (and (giveElement (list row column) gameTable) (null (giveElement (list (1+ row) column) gameTable)) ))
                        (progn
                            (propagateChangeVertical (list row column) gameTable)
                        )
                    )
                )
            )
            (if (and (= row (1- (list-length gameTable))) )
                (if (and (null (giveElement (list row column) gameTable)) (giveElement (list row (1+ column)) gameTable))
                    (goleft (list row column) gameTable)
                )
            )
        )
    )
    gameTable
)


(defun goLeft (pos gameTable)
    (propagateChangeHorizontal pos gameTable)
    (setq posUp (list (1- (car pos)) (car (cdr pos))))
    (setq posLeft (list (1- (list-length gameTable)) (1- (car (cdr pos)))))

    (cond
        ( (and (= (car pos) 0) (= (car (cdr pos)) 0)) nil )
        ( (and (= (car pos) 0) (null (giveElement posLeft gameTable))) (goleft posLeft gameTable))
        ( (= (car pos) 0) nil)
        (t (goleft posUp gameTable))
    )
)


(defun propagateChangeVertical (pos gameTable)
    (setq posDown (list (1+ (car pos)) (car (cdr pos))))
    (if (/= (car pos) 0)
        (progn
            (setq posUp (list (1- (car pos)) (car (cdr pos))))
            (switchPosition pos posDown gameTable)
            (propagateChangeVertical posUp gameTable)
        )
    (switchPosition pos posDown gameTable)
    )
)


(defun propagateChangeHorizontal (pos gameTable)
    (setq posRight (list (car pos) (1+ (car (cdr pos)))))
    (setq posLeft (list (car pos) (1- (car (cdr pos)))))

    (if (/= (car (cdr pos)) (1- (list-length (nth 0 gameTable))))
            (progn
                (setq posLeft (list (car pos) (1- (car (cdr pos)))))
                (switchPosition pos posRight gameTable)
                (propagateChangeHorizontal posRight gameTable)
            )
            (setf (nth (giveColumn pos) (nth (giveRow pos) gameTable)) nil)    
    )
)


(defun switchPosition (pos1 pos2 gameTable)
    (setf element1 (giveElement pos1 gameTable))
    (setf element2 (giveElement pos2 gameTable))
    (setf (nth (giveColumn pos2) (nth (giveRow pos2) gameTable)) element1)
    (setf (nth (giveColumn pos1) (nth (giveRow pos1) gameTable)) element2)
)


(defun checkEqualPos (pos1 pos2 gameTable)
    (= (giveElement pos1 gameTable) (giveElement))
)


(defun giveElement (pos gameTable)
    (nth (car (cdr pos)) (nth (car pos) gameTable))
)


(defun expandRight (pos gameTable)
    (setq res (copy-tree pos))
    (setq row (nth (car pos) gameTable))
    (if (and (eq nil (checkLastColumn pos row)) (checkEqualRight pos row)) 
        (progn 
            (setf (nth 1 res) (1+ (car (cdr res))))
            (setf res (appendLists (list res) (list pos)))
        )
    )
    (cond 
        ( (listp (car res)) res)
        (t (list res))
    )
)


(defun expandBottom (pos gameTable)
    (setq res (copy-tree pos))
    (setq row (nth (car pos) gameTable))

    (if (and (eq nil (checkLastRow pos gameTable)) (checkEqualBelow pos row gameTable))
        (progn 
            (setf (nth 0 res) (1+ (car res)))  
            (setf res (appendLists (list res) (list pos)))
        )
    )
    (cond 
        ( (listp (car res)) res)
        (t (list res))
    )   
)


(defun expandLeft (pos gameTable)
    (setq res (copy-tree pos))
    (setq row (nth (car pos) gameTable))
    (if (and (eq nil (checkFirstColumn pos row)) (checkEqualLeft pos row)) 
        (progn 
            (setf (nth 1 res) (1- (car (cdr res))))
            (setf res (appendLists (list res) (list pos)))
        )
    )
    (cond 
        ( (listp (car res)) res)
        (t (list res))
    )
)


(defun expandTop (pos gameTable)
    (setq res (copy-tree pos))
    (setq row (nth (car pos) gameTable))

    (if (and (eq nil (checkFirstRow pos gameTable)) (checkEqualAbove pos row gameTable))
        (progn 
            (setf (nth 0 res) (1- (car res)))  
            (setf res (appendLists (list res) (list pos)))
        )
    )
    (cond 
        ( (listp (car res)) res)
        (t (list res))
    )
)


(defun performExpansion (visited expanded gameTable)
    (setq list1 (appendLists (expandRight (car expanded) gameTable) (expandBottom (car expanded) gameTable)))
    (setq list2 (appendLists (expandLeft (car expanded) gameTable) (expandTop (car expanded) gameTable)))
    (setq newExpanded (appendLists list1 list2))
    (setq newVisited nil)
    (dolist (element newExpanded)
        (if (and (null (elementInList element visited)))
            (progn
                (setq visited (append visited (list element)))
                (if (and (null (elementInList element expanded)) expanded)
                    (setq expanded (append expanded (list element)))
                )
            )
        )
    )
    (cond
        ( (null (cdr expanded)) visited)
        (t (performExpansion visited (cdr expanded) gameTable) )
    )
)


(defun appendLists (list1 list2)
    (setq list (copy-tree list2))
    (loop for element1 in list1 
    do
        (if (null (elementInList element1 list2)) 
        (setq list (append list (list element1)))
        )
    )
    list
)

(defun elementInList (element list)
    (cond
        ( (eq list nil) nil)
        ( (equal element (car list)) t )
        ( (eq nil (equal element (car list))) (elementInList element (cdr list)) )
    )
)

(defun elementInListOfLists (element list)
    (cond
    ( (eq list nil) nil)
    ( (elementInList element (car list)) t ) 
    ( (eq nil (equal (elementInList element (car list)) t)) (elementInListOfLists element (cdr list)) )
    )

)


(defun gameTableSize (gameTable)
    (* (list-length gameTable) (list-length (nth 0 gameTable)))
)

;;;;;;;;;;;;;;;;;;;;;;;Heuristics;;;;;;;;;;;;;;;;;;;;;;;;;

(defun largestGroupSize (state)   
    (state-piecesEliminated state)
)


(defun leastSinglesNDoubles (state)
    (setq nSinglesNDoubles 0)
    (setq nPiecesGroups 0)

    (setq generated '())
    (setq expanded '())
    (setq gameTable (state-currentGameTable state))

    (loop for row from 0 to (1- (list-length gameTable))
    do
        (loop for column from 0 to (1- (list-length (nth 0 gameTable)))
        do
            (progn
                (if (and (null (elementInListOfLists (list row column) expanded)) (giveElement (list row column) gameTable) )
                (progn
                    (setq generated (performExpansion '() (list (list row column)) gameTable) )
                    (if (/= 1 (list-length generated))
                        (setq expanded (appendLists expanded (list generated)))
                    )
                    (setq generated '())
                )
                )
            )
        )
    )

    (dolist (group expanded)
        (setq nPiecesGroups (+ nPiecesGroups (list-length group)))
        (if (= (list-length group) 2) 
            (setq nSinglesNDoubles (1+ nSinglesNDoubles))
        )
    )

    (setq nSinglesNDoubles (+ nSinglesNDoubles (- (gameTableSize (state-currentGameTable state)) (state-piecesEliminatedSoFar state))))
    (setq nSinglesNDoubles (- nSinglesNDoubles nPiecesGroups))

    (if (/= nSinglesNDoubles 0)
    (/ 1 nSinglesNDoubles)
    0
    )
)


(defun resolve-same-game (gameTable approach)
    (setq initialState (make-state :currentGameTable gameTable
                                   :previousState nil
                                   :score 0
                                   :piecesEliminated 0
                                   :piecesEliminatedSoFar 0
                                   :playMadeSoFar nil
                                   ))

    (cond 
        ((string-equal approach "melhor.abordagem")
        (setq problema (cria-problema initialState '(generatesucces) :objectivo? #'objectiveState :heuristica 'largestGroupSize)) 
        (procura problema 'largura)

        (makeOutput bestState))

        ((string-equal approach "a*.melhor.heuristica")
        (setq problema (cria-problema initialState '(generatesucces) :objectivo? #'objectiveState :heuristica 'largestGroupSize)) (procura problema 'a*)
        (makeOutput bestState))

        ((string-equal approach "a*.melhor.heuristica.alternativa")
        (setq problema (cria-problema initialState '(generatesucces) :objectivo? #'objectiveState :heuristica 'leastSinglesNDoubles)) (procura problema 'a*)
        (makeOutput bestState))

        ((string-equal approach "sondagem.iterativa")
        (setq problema (cria-problema initialState '(generatesucces) :objectivo? #'objectiveState :heuristica 'largestGroupSize)) (sondagemIterativa problema))

        ((string-equal approach "abordagem.alternativa")
        (setq problema (cria-problema initialState '(generatesucces) :objectivo? #'objectiveState :heuristica 'largestGroupSize)) (abordagemIterativa problema))

    )
)


(defun makeOutput (state)
    (reverse (state-playMadeSoFar state))
)




(defun example1 () '((1 2 3 4 5) (6 7 8 9 10) (1 2 3 4 5)) )
(defun example2 () '((1 2 2 1 3) (1 2 3 2 3) (1 1 3 3 3)) )
(defun example3 () '((nil 2 2 1 3) (nil 2 3 2 3) (nil nil 3 3 3)))
(defun example4 () '((2 1 3 2 3 3 2 3 3 3) (1 3 2 2 1 3 3 2 2 2) (1 3 1 3 2 2 2 1 2 1) (1 3 3 3 1 3 1 1 1 3)))

(setq a (example3))


(defun imprime-puzzle (game)
    (write 'puzzle)
    (terpri)
    (dolist (l game)
        (dolist (e l)
            (if (null e)
                (write e)
                (progn (princ #\space) (write e) (princ #\space))
            )
            (princ #\space)
        )
        (terpri)
    )
    (terpri))


(defun estado-inicial()
    '((1 2 2 3 3)
      (2 2 2 1 3)
      (1 2 2 2 2)
      (1 1 1 1 1)))

(setq b (make-state :currentGameTable (estado-inicial)
            :previousState nil
            :score 0
            :piecesEliminated 0
            :piecesEliminatedSoFar 0
            :playMadeSoFar nil
            ))


(setq problema (cria-problema b '(generatesucces) :objectivo? #'objectivestate :heuristica 'largestGroupSize))
(setq c (procura problema 'a*))


(dolist (game (nth 0 c)) 
    (imprime-puzzle (state-currentGameTable game))
)