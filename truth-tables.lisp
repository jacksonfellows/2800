(defun _vars (expr)
  (when expr
    (apply #'append (mapcar (lambda (x) (if (listp x) (_vars x) (list x))) (rest expr)))))

(defun vars (expr)
  (delete-duplicates (_vars expr) :from-end t))

(defun subexprs (expr)
  (append
   (apply #'append (mapcar (lambda (x) (when (listp x) (subexprs x))) (rest expr)))
   (list expr)))

(defun eval-func (func vars)
  (ecase func
    (and (reduce (lambda (a b) (and a b)) vars))
    (or (reduce (lambda (a b) (or a b)) vars))
    (implies (not (and (first vars) (not (second vars)))))
    (not (not (first vars)))))

(defun _vals (n)
    (if (> n 0)
        (let ((x (_vals (1- n))))
          (append
           (mapcar (lambda (v) (cons t v)) x)
           (mapcar (lambda (v) (cons nil v)) x)))
        '(())))

(defun vals (vars)
  (_vals (length vars)))

(defun truth-table (expr)
  (let* ((vars (vars expr))
         (subexprs (subexprs expr))
         (headers (append vars subexprs)))
    (cons headers
          (mapcar (lambda (vals)
                    (let ((row vals))
                      (dolist (sub subexprs)
                        (setf row (append row (list (eval-func (car sub) (mapcar (lambda (x) (elt row (position x headers))) (rest sub)))))))
                      row))
                  (vals vars)))))

(defun print-wrapped (expr)
  (if (and (listp expr) (< 2 (length expr)))
      (progn
        (format t "(")
        (print-expr expr)
        (format t ")"))
      (print-expr expr)))

(defun print-expr (expr)
  (cond
    ((atom expr) (format t "~a" (case expr
                                  ((nil) 'f)
                                  (and "\\land")
                                  (or "\\lor")
                                  (implies "\\implies")
                                  (not "\\neg")
                                  (t expr))))
    ((listp expr) (if (= 1 (length (rest expr)))
                      (progn
                        (print-expr (first expr))
                        (format t " ")
                        (print-wrapped (second expr)))
                      (loop for x in (rest expr) and i from 0
                            do (print-wrapped x)
                            unless (= i (1- (length (rest expr))))
                              do (progn
                                   (format t " ")
                                   (print-expr (car expr))
                                   (format t " ")))))))

(defun print-row (row)
  (loop for expr in row and i from 0
    do (print-expr expr)
    unless (= i (1- (length row)))
      do (format t " & "))
  (format t "\\\\~%"))

(defun print-table (table)
  (format t "\\begin{displaymath}~%")
  (format t "\\begin{array}{|")
  (loop for i from 0 to (length (car table))
        do (format t "c|"))
  (format t  "}~%")  
  (format t "\\hline~%")
  (print-row (car table))
  (format t "\\hline~%")
  (dolist (row (cdr table))
    (print-row row))
  (format t "\\hline~%")
  (format t "\\end{array}~%")
  (format t "\\end{displaymath}~%"))
