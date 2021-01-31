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

   - Verdana Normal
   - Verdana Bold
   - Helvetica Bold

   This implementation differ from the JavaScript version.
   It returns text width for any font size, automatically applying
   needed math transformation. All you need is to specify font family
   and weight.

   Default font is \"Helvetica Bold 16px\".

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


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun relative-path (full-path prefix-path)
    (make-pathname :directory (cons :relative
                                    (nthcdr (length (pathname-directory prefix-path))
                                            (pathname-directory full-path)))
                   :name (pathname-name full-path)
                   :type (pathname-type full-path)))
  
  (defun preload-data ()
    (loop with data-dir = (asdf:system-relative-pathname "anafanafo"
                                                         #P"data/")
          with data-files = (directory
                             (uiop:merge-pathnames* #P"*.json"
                                                    data-dir))
          for filename in data-files
          for relative-filename = (relative-path filename
                                                 data-dir)
          for content = (jonathan:parse (uiop:read-file-string filename))
          collect (cons (princ-to-string
                         relative-filename)
                        content)))

  (defvar *preloaded-data*
    (preload-data)))


(defcached get-best-font-data (family weight)
  (loop for size downfrom 20 to 10
        for filename = (format nil
                               "~A-~Apx-~A.json"
                               (string-downcase family)
                               size
                               (string-downcase weight))
        for data = (assoc filename
                          *preloaded-data*
                          :test #'string=)
        when data
          do (return (values filename
                             (cdr data)
                             size))
        finally
           (error "Unable to find suitable file for font ~A ~A in ~A dir."
                  family
                  weight
                  (asdf:system-relative-pathname "anafanafo"
                                                 #P"data/"))))


(defcached
    load-data (&key
               (family *default-font-family*)
               (weight *default-font-weight*)
               (size *default-font-size*))
  "Loads data for specified font name.

   Returns an object which can be used to retrieve text width:

   ```lisp

   CL-USER> (anafanafo:load-data :family \"Verdana\"
                                 :weight \"bold\")
   #<ANAFANAFO::DATA \"Verdana\" \"bold\" 16px :file \"data/verdana-10px-bold.json\">

   ```
"
  (check-type family string)
  (check-type weight string)

  (multiple-value-bind (path font-data data-font-size)
      (get-best-font-data family weight)
    (make-instance 'data
                   :family family
                   :weight weight
                   :size size
                   :data-font-size data-font-size
                   :filename path
                   :data font-data)))


(defun scale-ratio (data)
  (/ (size data)
     (data-font-size data)))


(defun char-width (data char &key (guess t))
  "Returns a float width of given char. Width is measured in pixels.

   ```lisp
   CL-USER> (let ((data (anafanafo:load-data :family \"Verdana\"
                                             :weight \"normal\")))
              (values (cons #\щ
                            (anafanafo:char-width data #\щ))
                      (cons #\i
                            (anafanafo:char-width data #\i))))
   (#\CYRILLIC_SMALL_LETTER_SHCHA . 14.196364)
   (#\i . 4.3927274)
   ```
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

   ```lisp
   CL-USER> (let ((data (anafanafo:load-data :family \"Verdana\"
                                             :weight \"normal\")))
              (anafanafo:string-width data
                                      \"борщ\"))
   43.70909
   CL-USER> (let ((data (anafanafo:load-data :family \"Verdana\"
                                             :weight \"normal\")))
              (+ (anafanafo:char-width data
                                       #\б)
                 (anafanafo:char-width data
                                       #\о)
                 (anafanafo:char-width data
                                       #\р)
                 (anafanafo:char-width data
                                       #\щ)))
   43.70909
   ```"
  (check-type data data)
  (check-type text string)
  
  (loop for char across text
        when (char= char #\Newline)
          do (error "Function STRING-WIDTH does not support strings with new lines yet!")
        summing (char-width data char)))
