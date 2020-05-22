(in-package :user)

;
;
;A função get-
;internal-run-time e a constante INTERNAL-TIME-UNITS-PER-SECOND
;podem ser utilizadas para ajudar a controlar o tempo de execução.
;
;


(defun checkEqualRight (pos list)
    (= (nth (car (cdr pos)) list) (nth (1+ (car (cdr pos))) list))
)

(defun checkEqualLeft (pos list)
    (= (nth (car (cdr pos)) list) (nth (1- (car (cdr pos))) list))
)

(defun checkEqualAbove (pos list1 list2)
    (= (nth (car (cdr pos)) list1) (nth (car (cdr pos)) (nth (1- (car pos)) list2)))
)

(defun checkEqualBelow (pos list1 list2)
    (= (nth (car (cdr pos)) list1) (nth (car (cdr pos)) (nth (1+ (car pos)) list2)))
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
    (loop for row from 0 to (1- (list-length gameTable))
    do    
    (setq copiedList (copy-tree (nth row gameTable)))
        (loop for column from 0 to (1- (list-length (nth 0 gameTable)))
        do
            (cond 
                ( (and (checkLastColumn (list row column) copiedList) (eq nil (checkLastRow (list row column) gameTable))) (cond ((checkEqualBelow (list row column) copiedList gameTable)  (return-from objectiveState nil))))
                ( (and (checkLastRow (list row column) gameTable) (eq nil (checkLastColumn (list row column) copiedList))) (cond ((checkEqualRight (list row column) copiedList) (return-from objectiveState nil))))
                ( (and (eq nil (checkLastColumn (list row column) copiedList)) (= (nth column copiedlist) (nth (1+ column) copiedlist)))  (return-from objectiveState  nil))
                ( (and (eq nil (checkLastRow (list row column) gameTable)) (= (nth column copiedList) (nth column (nth (1+ row) gameTable)))) (return-from objectiveState nil) )
            )
        )
    )
    
    (return-from objectiveState t)
    
)


(defun generate-succes (gameTable)
    (setq generated '())
    (setq expanded '())

    (loop for row from 0 to (1- (list-length gameTable))
    do
        (loop for column from 0 to (1- (list-length (nth 0 gameTable)))
        do
            (progn
                (if (null (elementInListOfLists (list row column) expanded))
                (progn
                    (setq generated (performExpansion '() (list (list row column)) gameTable) )
                    ;(write generated)(terpri)
                    (if (/= 1 (list-length generated))
                        (progn
                    ;(write "here")(terpri)
                        (setq expanded (appendLists expanded (list generated)))
                        ;(write expanded)(terpri)(terpri)(terpri)(terpri)       
                        )
                    )
                    (setq generated '())
                )
                )
            )
        )
    )
    expanded
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun recFunc (generated gameTable)
    (setq res (performExpansion (car generated) gameTable))
    (cond
    ( (eq (list-length generated) (list-length res)) generated)
    (t (progn (setq generated (appendLists generated res)) (write generated)) (appendLists res (recFunc (cdr generated) gameTable) )   )
    )
);(appendLists (expandRight (car generated) gameTable) (expandBottom (car generated) gameTable))



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
    (setq newExpanded1 '())

    (dolist (element newExpanded)
        (if (and (null (elementInList element visited)))
            (progn
                (setq visited (append visited (list element)))
                (setq newVisited t)
                
                (if (and (null (elementInList element expanded)))
                    (setq expanded (append expanded (list element)))
                )
            )
        )
    )
    (cond
        ( (null (cdr expanded)) visited)
        ( (null newVisited) visited)
        (newVisited (performExpansion visited (cdr expanded) gameTable) )
    )
)


(defun objectiveState1 (gameTable)
    (null (generate-succes gameTable))
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
(defun example2 () '((1 2 2 3 3) (1 2 3 2 3) (1 1 3 3 3)) )

(write (objectiveState (example1)))
(terpri)
(write (objectiveState (example2)))
(terpri)
(write (example2))
(terpri)

(write (expandRight '(0 2) (example2)))
(terpri)
(write (expandBottom '(0 4) (example2)))
(terpri)

(setq generated (list '(0 0)))

(write (appendLists (expandRight '(0 2) (example2)) (expandBottom '(0 4) (example2))))

'((1 2 3 3 3) 
  (1 2 2 1 3) 
  (1 2 2 3 3))

  '((1 2 2 3 3) (2 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1))
