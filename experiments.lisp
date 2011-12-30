(let ((bill '("test"))
      (hardy '("another"))
      )
  (print bill)
  (print hardy))

(defun ingredients (order)
  (mapcar (lambda (burger)
            (case burger
              (single '(patty))
              (double '(patty patty))
              (double-cheese '(patty patty cheese))))
          order))

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro split (val yes no)
  (let1 g (gensym)
        `(let1 ,g ,val
               (if ,g
                   (let ((head (car ,g))
                         (tail (cdr ,g)))
                     ,yes)
                 ,no))))

(defun pairs (lst)
  (labels ((f (lst acc)
              (split lst
                     (if tail
                         (f (cdr tail) (cons (cons head (car tail)) acc))
                       (reverse acc))
                     (reverse acc))))
    (f lst nil)))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
        `(labels ((self ,(mapcar #'car p)
                        ,@body))
           (self ,@(mapcar #'cdr p)))))

(recurse (n 9)
         (fresh-line)
         (if (zerop n)
             (princ "lift-off!")
           (progn (princ n)
                  (self (1- n)))))


(LABELS ((SELF (N)
               (FRESH-LINE)
               (IF (ZEROP N)
                   (PRINC "lift-off!")
                   (PROGN (PRINC N)
                          (SELF (1- N))))))
        (SELF 9)) ;
