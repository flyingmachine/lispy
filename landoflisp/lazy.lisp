(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

;; cons when forced
(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

;; returns a lazy function? have to lazy-car to actually get a value?
(defun lazy-cdr (x)
  (cdr (force x)))

(defun lazy-nil ()
  (lazy nil))

;; test whether it's the end of the list
(defun lazy-null (x)
  (not (force x)))

;; When you use cdr, you get the remainder of the list
;; when you use lazy-cdr, you get the function that will
;; get you the remainder of the list
(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

;; returns a lazy list
(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
              (if (lazy-null lst-cur)
                  (force (lazy-mapcan fun (lazy-cdr lst)))
                (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
        (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst) 
  (if (zerop n)
      (lazy-car lst)
    (lazy-nth (1- n) (lazy-cdr lst))))
