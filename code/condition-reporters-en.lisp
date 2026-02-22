(cl:in-package #:invistra)

(defun range-contains-p (regions position)
  (some (lambda (region)
          (and (consp region)
               (<= (car region) position)
               (< position (cdr region))))
        regions))

(defun point-equals-p (regions position)
  (and (position position regions)
       t))

(defun overlapping-p (regions start end)
  (and (< start end)
       (some (lambda (region)
               (or (and (consp region)
                        (not (or (<= end (car region))
                                 (<= (cdr region) start))))
                   (and (numberp region)
                        (<= start region)
                        (< region end))))
             regions)))

(defmethod acclimation:report-condition :after
    ((condition control-string-error) stream (language acclimation:english))
  (with-accessors ((client client)
                   (control-string control-string)
                   (regions regions))
      condition
    (let ((line-start 0))
      (flet ((print-markers (position)
               (when (overlapping-p regions line-start position)
                 (when (zerop line-start)
                   (write-string "   " stream))
                 (loop for i from line-start below position
                       for ch = (let ((ch (char control-string i)))
                                  (cond ((point-equals-p regions i)
                                         #\^)
                                        ((range-contains-p regions i)
                                         #\=)
                                        ((whitespace-char-p client ch)
                                         ch)
                                        (t
                                         #\space)))
                       do (write-char ch stream)
                       when (position (char control-string i) "\\\"")
                         do (write-char ch stream))
                 (terpri stream))
               (setf line-start (1+ position))))
        (loop for ch across control-string
              for position from 0
                initially (write-string "  \"" stream)
              finally (write-char #\" stream)
                      (terpri stream)
                      (print-markers (length control-string))
              when (position ch "\\\"")
                do (write-char #\\ stream)
              do (write-char ch stream)
              when (char= #\newline ch)
                do (print-markers position))))))

;;; Parsing conditions

(defmethod acclimation:report-condition
    ((condition end-of-control-string) stream (language acclimation:english))
  (write-line "Unexpected end of the control string." stream))

(defmethod acclimation:report-condition
    ((condition expected-integer-error) stream (language acclimation:english))
  (write-line "Expected an integer parameter in control string." stream))

(defmethod acclimation:report-condition
    ((condition duplicate-modifiers) stream (language acclimation:english))
  (write-line "Duplicate modifiers were found in control string." stream))

;;; Syntax conditions

(defmethod acclimation:report-condition
    ((condition unknown-directive-character) stream (language acclimation:english))
  (cl:format stream
             "Unknown directive ~:c character in control string.~%"
             (directive-character condition)))

(defmethod acclimation:report-condition
    ((condition illegal-modifiers) stream (language acclimation:english))
  (cl:format stream "~:[Illegal~;Conflicting~] modifier~p ~{~#[~;~:c~;~:c and ~
                        ~:c~:;~@{~:c~#[~;, and ~:;, ~]~}~]~} found in directive.~%"
             (conflictingp condition)
             (length (modifier-characters condition))
             (modifier-characters condition)))

(defmethod acclimation:report-condition
    ((condition illegal-outer-modifier) stream (language acclimation:english))
  (write-line "Outer escape upward modifier can only be used inside of a sublist iteration directive." stream))

(defmethod acclimation:report-condition
    ((condition illegal-conditional-modifier) stream (language acclimation:english))
  (write-line "Modifiers can only be used on a conditional directive that does not have a parameter." stream))

(defmethod acclimation:report-condition
    ((condition illegal-default-modifier) stream (language acclimation:english))
  (write-line "The default clause modifier (:) can only appear on the last clause separator directive in a conditional without modifiers." stream))

(defmethod acclimation:report-condition
    ((condition illegal-parameter) stream (language acclimation:english))
  (cl:format stream
             "Illegal parameter found. Directive can have no more than ~a parameter~:p.~%"
             (maximum-count condition)))

(defmethod acclimation:report-condition
    ((condition parameter-type-error) stream (language acclimation:english))
  (cl:format stream
             "A type of ~s was expected as parameter, but ~a was found.~%"
             (type-error-expected-type condition)
             (type-error-datum condition)))

(defmethod acclimation:report-condition
    ((condition no-such-package) stream (language acclimation:english))
  (cl:format stream "A package named ~a does not exist.~%"
             (no-such-package-package-name condition)))

(defmethod acclimation:report-condition
    ((condition no-such-symbol) stream (language acclimation:english))
  (cl:format stream "A symbol with a name of ~a does not exist.~%"
             (no-such-symbol-symbol-name condition)))

(defmethod acclimation:report-condition
    ((condition symbol-not-external) stream (language acclimation:english))
  (cl:format stream "The symbol ~s is not external in the package.~%"
             (symbol-not-external-symbol condition)))

(defmethod acclimation:report-condition
    ((condition illegal-directive) stream (language acclimation:english))
  (write-line  "Illegal directive." stream))

(defmethod acclimation:report-condition
    ((condition illegal-clause-separator) stream (language acclimation:english))
  (write-line "Clause separator directive must appear inside of a structured directive that permits multiple clauses."
              stream))

(defmethod acclimation:report-condition
    ((condition illegal-fix-directive) stream (language acclimation:english))
  (write-line "Directives are not permitted in the prefix or suffix of a logical block directive."
              stream))

(defmethod acclimation:report-condition
    ((condition excessive-clause-separators) stream (language acclimation:english))
  (write-line "Too many clauses in directive." stream))

(defmethod acclimation:report-condition
    ((condition global-layout-conflict) stream (language acclimation:english))
  (write-line "Dynamic justification and logical block directives may not be used in the same control string."
              stream))

(defmethod acclimation:report-condition
    ((condition local-layout-conflict) stream (language acclimation:english))
  (write-line "Logical block directives may not be used inside of a justification directive."
              stream))

(defmethod acclimation:report-condition
    ((condition missing-directive) stream (language acclimation:english))
  (cl:format stream "Missing ~~~c directive." (directive-character condition)))

(defmethod acclimation:report-condition
    ((condition missing-end-logical-block-or-end-justification) stream (language acclimation:english))
  (write-line "Missing end of justification (~>) or end of logical block (~:>) directive."  stream))

(defmethod acclimation:report-condition
    ((condition missing-end-conditional) stream (language acclimation:english))
  (write-line "Missing end of conditional (~]) directive." stream))

(defmethod acclimation:report-condition
    ((condition missing-end-case-conversion) stream (language acclimation:english))
  (write-line "Missing end of case conversion (~)) directive." stream))

(defmethod acclimation:report-condition
    ((condition missing-end-iteration) stream (language acclimation:english))
  (write-line "Missing end of iteration (~}) directive." stream))

(defmethod acclimation:report-condition
    ((condition missing-clause-separator) stream (language acclimation:english))
  (write-line "Not enough clauses in directive, i.e. missing clause separator (~;) directive." stream))

;;; Runtime conditions

(defmethod acclimation:report-condition
    ((condition no-more-arguments) stream (language acclimation:english))
  (cl:format stream "An attempt was made to access more arguments than available."))

(defmethod acclimation:report-condition
    ((condition argument-type-error) stream (language acclimation:english))
  (cl:format stream
             "A type of ~s was required as argument, but ~a was found."
             (type-error-expected-type condition)
             (type-error-datum condition)))

(defmethod acclimation:report-condition
    ((condition go-to-out-of-bounds) stream (language acclimation:english))
  (cl:format stream "An attempt was made to go to argument number ~d ~
                     instead of one between 0 and ~d."
          (argument-position condition)
          (argument-count condition)))

(defmethod acclimation:report-condition
    ((condition invalid-destination) stream (language acclimation:english))
  (cl:format stream
          "The object ~s is not a valid destination for a format operation."
          (destination condition)))
