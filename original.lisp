;;;; Esrap example: some grammars with function-based terminals.
(ql:quickload :esrap)


(cl:defpackage #:esrap-example.function-terminals
  (:use #:cl #:esrap)
  (:export #:indented-block #:common-lisp))

(cl:in-package #:esrap-example.function-terminals)

;;; Ex. 1. Using a custom terminal for context sensitive parsing.
;;;
;;; Among many other things, this can be used to implement
;;; indentation-based grammars such as Python's.

(defrule whitespace (+ #\space)
  (:constant nil))

;; *CURRENT-INDENT* tracks the current indentation and CURRENT-INDENT
;; *succeeds when it can consume exactly CURRENT-INDENT* units of
;; *indentation.
(defvar *current-indent* 0)

(defun current-indent-p (indent)
  (= indent *current-indent*))

(defrule indent (* #\space)
  (:function length))

(defrule newline (or #\linefeed (and #\return #\linefeed))
  (:text t))

(defrule blank-line (and (* #\space)
                         newline)
  (:constant "
"))

(defrule current-indent
    (or (current-indent-p indent)
        (and blank-line
             (current-indent-p indent))))

;; Just a dummy rule for the statement-like elements of the
;; grammar. This is not the focus of this example. For simplicity,
;; each statement is on one line.
(defrule statement (+ (character-ranges (#\a #\z)))
  (:text t))

(defrule line (and statement #\newline)
  (:function first))

(defrule block-content
    (or if line))

(defrule indented-block-content
    (and current-indent block-content)
  (:function second))

;; PARSE-INDENTED-BLOCK is the real meat. It determines the new
;; indentation depth via a nested (PARSE INDENT ...) call which does
;; not consume input. The block's content can then be parsed with a
;; suitably increased current indent.
;;
;; The result of the second PARSE call is returned "raw" in case of
;; success. This allows the associated result tree to be attached to
;; the global result tree and permits lazy computation of rule
;; productions within the sub-tree (beneficial if e.g. the result of
;; the parse, despite successful, is not used in the global result).
(defun parse-indented-block (text position end)
  (multiple-value-bind (new-indent new-position)
      (parse 'indent text :start position :end end
                          :junk-allowed t)
    (if (> new-indent *current-indent*)
        (let ((*current-indent* new-indent))
          (parse '(+ indented-block-content) text
                 :start position :end end :raw t))
        (values nil new-position "Expected indent"))))

(defrule indented-lines #'parse-indented-block)

(defrule newline (or #\linefeed
                     (and #\return #\linefeed))
  (:text t))

(defrule blank-line (and (* #\space) newline)
  (:constant "
"))

(defrule indented-block (and indented-lines
                             (+ (and blank-line
                                     indented-lines))
                             ))

(defrule if
    (and (and "if" whitespace) statement (and #\: #\Newline)
         indented-block
         (? (and (and current-indent "else" #\: #\Newline)
                 indented-block)))
  (:destructure (if-keyword condition colon then
                 (&optional else-keyword else))
    (declare (ignore if-keyword colon else-keyword))
    (list* 'if condition then (when else (list else)))))

(defun test-indentation (&optional (text "   foo
   bar
   quux
   if foo:
    bla
    if baz:
     bli
     blo
    else:
     whoop
   blu
"))
  (parse 'indented-block text))

;;; Ex. 2. Using CL:READ to parse lisp.

(defun parse-using-read (text position end)
  (with-input-from-string (stream text :start position :end end)
    (handler-case
        ;; When successful, READ returns the read object and the
        ;; position up to which TEXT has been consumed.
        (read stream t nil)
      ;; When READ-FROM-STRING fails, indicate the parse failure,
      ;; including CONDITION as explanation.
      (error (condition)
        ;; We try to determine the position of the parse failure using
        ;; the stream position.
        (values nil (file-position stream) condition)))))

(defrule common-lisp #'parse-using-read)

;; When parsing anything by using CL:READ, it is probably a good idea
;; to disable *READ-EVAL*. The package in which symbols will be
;; interned has to be kept in mind as well.
(defun test-read ()
  (with-standard-io-syntax
    (let (; (*package* (find-package :my-package-for-symbols))
          (*read-eval* nil))
      ;; This contains deliberate syntax errors to highlight the error
      ;; position and error message reporting implemented in
      ;; PARSE-USING-READ.
      (parse 'common-lisp "(list 'i ::love 'lisp)"))))


;;;;; Bullet list

(defrule eof (! character)
  (:constant ""))

(defrule newline (or #\linefeed (and #\return #\linefeed))
  (:text t))

(defrule raw-line (or (and (* (and (! #\newline) (! #\return) character))
                           newline)
                      (and (+ character) eof))
  (:text t))


;; Indentation handling

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
             (current-indent2-p indent))))

;; END of indentation handling



(defrule line (and current-indent2
                   (or bullet-list
                       raw-line))
  (:function second))

(defrule first-list-item-line (and bullet raw-line))

(defrule list-item (and first-list-item-line
                        (* rest-list-item-lines))
  (:destructure (first rest)
    (list :bullet-item
          first
          rest)))


(defrule bullet (and #\* #\space)
  (:function length))


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

(defrule bullet-list (and (& bullet)
                          (+ list-item)
                          (* blank-line)
                          (! bullet))
  (:destructure (bullet other-items blank-line not-a-bullet)
    (declare (ignore bullet blank-line not-a-bullet))
    (list :bullet-list
          other-items)))

(defrule document (+ (or bullet-list
                         line)))


(defun test-bullet (text)
  (parse 'document
         text
         ;; :junk-allowed t
         ))


;;; TODO

;; * Я застрял на том, что не получается объединить последовательные list-item в один bullet-list
;;   и при этом вложенные списки чтобы были по-отдельности. 
