(defun set-dif (L1 L2)
    (cond ((null L1) nil)
          ((null L2) L1)
          ((member (car L1) L2) (set-dif (cdr L1) L2))
          (t (cons (car L1) (set-dif (cdr L1) L2)))
    )
)

(print '(set-dif '(1 2 3 4) '(3 4 5 6)))
(print (set-dif '(1 2 3 4) '(3 4 5 6)))

(print '(set-dif '(a 2 c 4) '(c d e 2)))
(print (set-dif '(a 2 c 4) '(c d e 2)))