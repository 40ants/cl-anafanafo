(mgl-pax-minimal:define-package :anafanafo
  (:use #:cl)
  (:nicknames :anafanafo/core)
  (:import-from #:mgl-pax-minimal
                #:section
                #:defsection)
  (:import-from #:function-cache
                #:defcached)
  (:import-from #:jonathan)
  (:export
   #:string-width
   #:char-width
   #:load-data))
(in-package anafanafo)

(defvar *default-font-family* "Helvetica")
(defvar *default-font-weight* "bold")
(defvar *default-font-size* 16)


(defsection @index (:title "Anafanafo, Common Lisp implementation!")
  "This library is implementation of JavaScript version of [anafanafo](https://github.com/metabolize/anafanafo).

   It is useful, when you want to know how long in pixels will be the string when the browser will
   render it in a given font.

   Library uses prebuilt data about character widths from the [original repository](https://github.com/metabolize/anafanafo).
   It supports these fonts:

   - Verdana Normal 10px
   - Verdana Bold 10px
   - Verdana Normal 11px
   - Helvetica Bold 11px

   To use it, you need to load data using the LOAD-DATA function:"
  
  (load-data function)

  "Then you can calculate the width of the string:"

  (string-width function)

  "Or width of a single character:"

  (char-width function))


(defclass data ()
  ((family :initarg :family
           :initform *default-font-family*
           :reader family)
   (weight :initarg :weight
           :initform *default-font-weight*
           :reader weight)
   (size :initarg :size
         :initform *default-font-size*
         :reader size)
   (data-font-size :initarg :data-font-size
         :reader data-font-size)
   (filename :initarg :filename
             :reader filename)
   (data :initarg :data
         :reader data)))


(defmethod print-object ((obj data) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "\"~A\" \"~A\" ~Apx :file \"~A\""
            (family obj)
            (weight obj)
            (size obj)
            (filename obj))))


;; (defvar *font-to-filename* '(("Verdana Normal 10px" . "data/verdana-10px-normal.json")
;;                              ("Verdana Bold 10px" . "data/verdana-10px-bold.json")
;;                              ("Verdana Normal 11px" . "data/verdana-11px-normal.json")
;;                              ("Helvetica Bold 11px" . "data/helvetica-11px-bold.json")))


(defcached
    load-data (&key
               (family *default-font-family*)
               (weight *default-font-weight*)
               (size *default-font-size*))
  "Loads data for specified font name.

   Returns an object which can be used to retrieve text width:

   ```lisp
   CL-USER> (load-data \"Verdana Normal 10px\")

   #<DATA \"Verdana Normal 10px\" :file \"data/verdana-10px-normal.json\">
   ```
"
  (check-type family string)
  (check-type weight string)

  (let* ((data-font-size 11)
         (filename (format nil
                           "data/~A-~Apx-~A.json"
                           (string-downcase family)
                           data-font-size
                           (string-downcase weight)))
         (filename (uiop:parse-unix-namestring filename))
         (path (asdf:system-relative-pathname
                "anafanafo"
                filename)))
    (unless (probe-file path)
      (error "Unable to find file for font ~A ~A. File ~A does not exist."
             family
             weight
             path))
    (make-instance 'data
                   :family family
                   :weight weight
                   :size size
                   :data-font-size data-font-size
                   :filename filename
                   :data (jonathan:parse (uiop:read-file-string path)))))


(defun scale-ratio (data)
  (/ (size data)
     (data-font-size data)))


(defun char-width (data char &key (guess t))
  "Returns a float width of given char. Width is measured in pixels.

   CL-USER> (let ((data (anafanafo:load-data \"Verdana Normal 10px\")))
              (values (cons #\щ
                            (anafanafo:char-width data #\щ))
                      (cons #\i
                            (anafanafo:char-width data #\i))))
   (#\CYRILLIC_SMALL_LETTER_SHCHA . 8.88)
   (#\i . 2.74)
"
  (check-type data data)
  (check-type char character)
  
  (loop with code = (char-code char)
        for (lower upper width) in (data data)
        when (<= lower code upper)
          do (return (* width
                        (scale-ratio data)))
        finally (if guess
                    (return (char-width data #\m))
                    (error "Unable to find width for ~S character"
                           char))))


(defun string-width (data text)
  "Returns width of the text in pixels.

   Result just a sum of all text characters:

   CL-USER> (let ((data (anafanafo:load-data \"Verdana Normal 10px\")))
              (anafanafo:string-width data
                                      \"борщ\"))
   27.32
   CL-USER> (let ((data (anafanafo:load-data \"Verdana Normal 10px\")))
              (+ (anafanafo:char-width data
                                       #\б)
                 (anafanafo:char-width data
                                       #\о)
                 (anafanafo:char-width data
                                       #\р)
                 (anafanafo:char-width data
                                       #\щ)))
   27.32"
  (check-type data data)
  (check-type text string)
  
  (loop for char across text
        when (char= char #\Newline)
          do (error "Function STRING-WIDTH does not support strings with new lines yet!")
        summing (char-width data char)))
