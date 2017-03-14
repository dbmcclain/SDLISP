;; -------------------------------------------
(in-package #:sdlisp-tramp)
;; -------------------------------------------

;; ------------------------------------------

(defstruct lispm
  env ans ktl)

(defvar *lispm* (make-lispm))

(defmacro with-lispm (lispm &body body)
  `(with-accessors ((%env lispm-env)
                    (%ans lispm-ans)
                    (%ktl lispm-ktl)) ,lispm
     (declare (ignorable %env %ans %ktl))
     ,@body))

(editor:setup-indent "with-lispm" 1)

(defmacro answer (arg)
  `(setf %ans ,arg))

(defmacro set-env (arg)
  `(setf %env ,arg))

(defmacro inject (&rest args)
  `(setf %ktl (list* ,@args (the list %ktl))))

(defmacro does (&body body)
  `(lambda ()
     (with-lispm *lispm*
       ,@body)))


(defmacro next ()
  `(funcall (pop (the cons %ktl))))

#|
(defmacro xnext ()
  `(next))
|#

(defun trampoline (env ans k)
  #f
  (let* ((lispm (make-lispm
                 :env env
                 :ans ans
                 :ktl (list k))) ;;  (lm-EXIT))))
         (*lispm* lispm))
    (declare (dynamic-extent lispm)
             (lispm lispm))
    (with-lispm lispm
      (do ()
          ((null (the list %ktl)) %ans)
        (next)) )))

;; ------------------------------------------
#|
(defun lm-EXIT ()
  #f
  (does))
|#

