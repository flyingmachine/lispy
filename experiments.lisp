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
