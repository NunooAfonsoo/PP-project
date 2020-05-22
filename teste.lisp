(print "Hello")

;;; Define variable
(defvar *age* (read)) ; reads from input
(defvar *name* "Artur")
(defvar *idade* 18)

;;; change variable value
(setf *name* "Ernestu")

;;;Define functions
(defun hello(*age*)
    (format t "Hello ~a ~%" *name*) ; ~a mostra o valor, ~s mostra o valor com aspas 
    (format t "PI to 5 chars ~,5f ~%" 3.1415926)
    (format t "10 percent ~,,2f ~%" .10)
    (format t "~d ~%" (+ 5 4))
    (format t "Isto ignora cases, comparar ints com doubles, etc ~d ~%" (equalp "Artur" "artur"))
)

;;; if statement
(if (and (< 3 5) (> 5 3)) 
    (format t "Fantastic ~%")
    (format t "wtf? ~%") ; this is else
)

(if (and (< 3 5) (> 5 3)) 
    (progn
        (format t "Fantastic ~%")
        (format t "This is so autistic, I need progn to make more than 1 instruction ~%")
    )
    (format t "wtf?") ; this is else
)

;;; switch case
(case *idade*
    (18 (print "Fucking amazing"))
    (19 (print "WTF??"))
    (otherwise (print "WTF2"))
)

;;; cond
(cond 
    ( (>= *idade* 18)
        (print "A idade é menor ou igual que 18")
    )
    (t ; else
        (print "WTF 3??")
    )
)

;;; when 

(when (>= *idade* 18)
    (print "Já tens idade para formar família")
)

;;; unless
(unless (not(>= *idade* 18))
    (print "Já tens idade para formar família")
) 

;;; for loops
(loop for x from 1 to 10
    do    
        (print x)
)

(setq x 1)
(loop
    (print x)
    (setq x (+ x 1))
    (when (= x 10)
        (return x)
    )
)

(loop for x in '(Couve Alface Banana)
    do    
        (format t "~s ~%" x)
)

;;; do loops
(dotimes (y 12)
    (print y)
)

;;; Call function
(hello *age*)

;;; lists
(format t "First = ~a ~%" (car '(Primeiro Segundo Terceiro)))
(format t "Rest = ~a ~%" (cdr '(Primeiro Segundo Terceiro)))

(defparameter *nums* '(2 4 6))
(push 1 *nums*)

;;; Prevent writing all in caps
(setq *print-case* :capitalize)

;;; adds \n to end of print 
(terpri)