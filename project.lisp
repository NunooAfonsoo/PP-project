(in-package :user)

;
;
;A função get-
;internal-run-time e a constante INTERNAL-TIME-UNITS-PER-SECOND
;podem ser utilizadas para ajudar a controlar o tempo de execução.
;
;


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


(defun objectiveState (gameTable)
    (null (generateSucces gameTable))
)

(defun generateSucces (gameTable)
    (setq generated '())
    (setq expanded '())

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
    (dolist (board (makePlay expanded gameTable))
        (setq newBoards (append newBoards (list (propagateChange board)))) 
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

                    (if (= column (1- (list-length (nth 0 gameTable))))
                        (if (null (giveElement (list row column) gameTable))
                            (goleft (list row column) gameTable)
                        )
                    )
                )
            )
        )
    )
    gameTable
)


(defun goLeft (pos gameTable)
    (propagateChangeHorizontal pos gameTable)
    (setq posUp (list (1- (car pos)) (car (cdr pos))))
    (cond
        ( (= (car pos) 0) t)
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
   ; (if (/= (car (cdr pos2)) (list-length (nth 0 gameTable)))
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



; (append res (list pos))(write res)

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
