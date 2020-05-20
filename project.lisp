(in-package :user)

;
;
;A função get-
;internal-run-time e a constante INTERNAL-TIME-UNITS-PER-SECOND
;podem ser utilizadas para ajudar a controlar o tempo de execução.
;
;


(defun example1 () '((1 2 3 4 5) (6 7 8 9 10) (1 2 3 4 5)) )
(defun example2 () '((1 2 3 3 3) (1 2 2 1 3) (1 2 2 3 2)) )



(defun checkEqualRight (pos list)
    (= (nth (car (cdr pos)) list) (nth (1+ (car (cdr pos))) list))
)

(defun checkEqualBelow (pos list1 list2)
    (= (nth (car (cdr pos)) list1) (nth (car (cdr pos)) (nth (1+ (car pos)) list2)))
)

(defun checkLastRow (pos list)
    (= (car (cdr pos)) (1- (list-length (nth 0 list))))
)

(defun checkLastLine (pos list)
    (= (car pos) (1- (list-length list)))
)


(defun objectiveState (gameTable) 
    (loop for line from 0 to (1- (list-length gameTable))
    do    
    (setq copiedList (copy-list (nth line gameTable)))
        (loop for row from 0 to (1- (list-length (nth 0 gameTable)))
        do
        (cond 
        ( (and (checkLastRow (list line row) gameTable) (eq nil (checkLastLine (list line row) gameTable))) (cond ((checkEqualBelow (list line row) copiedList gameTable)  (return-from objectiveState nil))))
        ( (and (checkLastLine (list line row) gameTable) (eq nil (checkLastRow (list line row) gameTable))) (cond ((checkEqualRight (list line row) copiedList) (return-from objectiveState nil))))
        ( (and (eq nil (checkLastRow (list line row) gameTable)) (= (nth row copiedlist) (nth (1+ row) copiedlist)))  (return-from objectiveState  nil))
        ( (and (eq nil (checkLastLine (list line row) gameTable)) (= (nth row copiedList) (nth row (nth (1+ line) gameTable)))) (return-from objectiveState nil) )
        )
        )
    )
    
    (return-from objectiveState t)
    
)


(defun generate-succes (gameTable)
    ()

)


(defun gera-nos-sucessores () )


(write (objectiveState (example1)))
(terpri)
(write (objectiveState (example2)))
(terpri)

'((1 2 3 3 3) 
  (1 2 2 1 3) 
  (1 2 2 3 2))

  '((1 2 2 3 3) (2 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1))