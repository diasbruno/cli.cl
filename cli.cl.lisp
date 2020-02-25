;;;; cli.cl.lisp

(in-package #:cli.cl)

(defun starts-with (str1 str2)
  (let ((l (length str1)))
    (string-equal str1 str2 :end1 l :end2 l)))

(defun intercalate (ch ls)
  (reduce (lambda (a b)
            (concatenate 'string a ch b))
          (cdr ls)
          :initial-value (car ls)))

(defun cli-args (cliargs)
  (loop :for x :in cliargs
        :with stop := nil
        :with cmds := nil
        :with args := nil
        :when (lpm-cli:starts-with "-" x) :do (setf stop t)
          :if (not stop)
            :do (setf cmds (push x cmds))
        :else :do (setf args (push x args))
        :finally (return-from cli-args (cons (nreverse cmds) (nreverse args)))))

(defun cli-opts (opts args)
  "needs to run after we know which command to execute."
  (just-getopt-parser:getopt args opts))

(defun cli-command (tb cmd)
  (let* ((name (cadr cmd))
         (callname (substitute #\- #\/ (symbol-name name))))
    (progn
      (setf (gethash callname tb) (cons name (nth 2 cmd)))
      `(defun ,name ,(nth 3 cmd)
         ,@(cddddr cmd)))))

(defmacro cli (name &rest schema)
  (let* ((opts (car schema))
         (cmds (cddr schema))
         (tb (make-hash-table :test 'equal))
         (opts-name (gensym)))
    (flet ((make-command (cmd)
             (cli-command tb cmd))
           (make-runner ()
             (let ((runner-args (gensym))
                   (cmd (gensym))
                   (found (gensym))
                   (parsed-args (gensym)))
               `(defun ,name (,runner-args)
                  (let* ((,parsed-args (cli-args ,runner-args))
                         (,cmd (intercalate "-" (car ,parsed-args)))
                         (,found (gethash (string-upcase ,cmd)
                                          *commands* nil)))
                    (if ,found
                        (funcall (car ,found)
                                 (cli-opts (cdr ,found)
                                           (cdr ,parsed-args)))
                        (funcall #',opts-name
                                 (cli-opts ',(cadr opts)
                                           (cdr ,parsed-args)))))))))
      (let ((opts-args (gensym))
            (built (mapcar #'make-command cmds)))
        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (defvar *commands* ,tb)
           ,@built
           (defun ,opts-name (,opts-args)
             (format t "baseopts: ~a~&" ,opts-args))
           ,(make-runner))))))
