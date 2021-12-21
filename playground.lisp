(uiop:define-package #:esrap-playground
  (:use #:cl
        #:esrap
        #:rove)
  (:nicknames #:esrap-playground/playground))
(in-package esrap-playground)

(defrule eof (! character)
  (:constant ""))

(defrule newline (or #\linefeed (and #\return #\linefeed))
  (:text t))

(defrule raw-line (or (and (* (and (! #\newline) (! #\return) character))
                           newline)
                      (and (+ character) eof))
  (:text t))

(defvar *current-indent2* 0)
(defvar *first-line-without-indent* nil)

(defun current-indent2-p (indent)
  (prog1 (or *first-line-without-indent*
             (= indent *current-indent2*))
    ;; After ignoring indent of the first line,
    ;; we have to reset this flag to require
    ;; all following lines have an indentation.
    (setf *first-line-without-indent* nil)))


(defrule indent2 (* #\space)
  (:function length))

(defrule current-indent2
    (or (current-indent2-p indent2)
        (and blank-line
             (current-indent2-p indent2))))

(defrule line (and current-indent2
                   (or bullet-list
                       raw-line))
  (:function second))

(defrule blank-line (and (* #\space)
                         newline)
  (:constant "
"))

(defun parse-list-item (text position end)
  (multiple-value-bind (list-item-indent new-position)
      (parse 'bullet text :start position :end end
                          :junk-allowed t)
    (if list-item-indent
        (let ((*current-indent2* (+ *current-indent2*
                                    list-item-indent))
              (*first-line-without-indent* t))
          (multiple-value-bind (result new-position)
              (parse 'document
                     text
                     :start (+ position
                               list-item-indent)
                     :end end
                     :junk-allowed t)
            (values (list :list-item
                          result)
                    new-position)))
        (values nil new-position "Expected indent"))))

(defrule list-item #'parse-list-item)

(defrule bullet (and #\* #\space)
  (:function length))

(defrule bullet-list (and (& bullet)
                          (+ (and list-item
                                  (* blank-line)
                                  ;; (! bullet)
                                  )))
  (:destructure (bullet items)
    (declare (ignore bullet))
    (list* :bullet-list
           (mapcar #'car items))))

(defrule document-part (and (& current-indent2)
                            (or bullet-list
                                line))
  (:destructure (indent content)
    (declare (ignore indent))
    content))

(defrule document (+ document-part))

(deftest simple-case
  (let ((result (parse 'document
                       "* Foo
* Bar"))
        (expected '((:BULLET-LIST
                     (:LIST-ITEM
                      ("Foo
"))
                     (:LIST-ITEM ("Bar"))))))
    (ok (equalp result
                expected))))

(deftest case-with-empty-line
  (let ((result (parse 'document
                       "* Foo

* Bar"))
        (expected '((:BULLET-LIST
                     (:LIST-ITEM
                      ("Foo
"))
                     (:LIST-ITEM ("Bar"))))))
    (ok (equalp result
                expected))))


(deftest case-with-multiline-items
  (let ((result (parse 'document
                       "* Foo

  Second line
  splitted.

* Bar

  Fourth line
  splitted.
"))
        (expected '((:BULLET-LIST
                     (:LIST-ITEM
                      ("Foo
"
                       "Second line
"
                      "splitted.
"))
                     (:LIST-ITEM ("Bar
"
                                  "Fourth line
"
                      "splitted.
"))))))
    (ok (equalp result
                expected))))


(defun test (text &optional (rule 'bullet-list))
  (parse rule
         text
         ;; :junk-allowed t
         ))
