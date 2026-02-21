(cl:in-package #:invistra)

(defmethod acclimation:report-condition :after
    ((condition control-string-error) stream (language acclimation:english))
  (with-accessors ((client client)
                   (control-string control-string)
                   (start start)
                   (end end)
                   (positions positions))
      condition
    (let ((line-start 0))
      (flet ((print-markers (position)
               (when (and (< line-start position)
                          (or (and (<= line-start start)
                                   (< start position))
                              (and (< line-start end)
                                   (<= end position))))
                 (when (zerop line-start)
                   (write-string "   " stream))
                 (loop for i from line-start below position
                       for ch = (let ((ch (char control-string i)))
                                  (cond ((member i positions)
                                         #\^)
                                        ((and (<= start i)
                                              (< i end))
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
             (eq (format-error-reason condition) :conflicting)
             (length (modifier-characters condition))
             (modifier-characters condition)))

(defmethod acclimation:report-condition
    ((condition too-many-parameters) stream (language acclimation:english))
  (cl:format stream
             "Directive takes at most ~a parameters, but ~a found.~%"
             (at-most-how-many condition)
             (how-many-found condition)))

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
    ((condition modifier-and-parameter) stream (language acclimation:english))
  (write-line "A parameter can be used only of there are no modifiers." stream))

(defmethod acclimation:report-condition
    ((condition illegal-directive) stream (language acclimation:english))
  (write-line (case (format-error-reason condition)
                  (:clause-separator
                   "Clause separator directive must appear inside of a structured directive that permits multiple clauses.")
                  (:outer-escape-upward
                   "Outer escape upward directive must occur inside of a sublist iteration directive.")
                  (:logical-block-fix
                   "Directives are not permitted in the prefix or suffix of a logical block directive.")
                  (otherwise
                   "Illegal directive."))
                stream))

(defmethod acclimation:report-condition
    ((condition parameter-omitted) stream (language acclimation:english))
  (cl:format stream
          "Parameter number ~d was given, but parameter ~d ~
           was omitted, which is not allowed."
          (parameter1 condition)
          (parameter2 condition)))

(defmethod acclimation:report-condition
    ((condition unmatched-directive) stream (language acclimation:english))
  (write-line "Begin directive is missing corresponding end directive." stream))

(defmethod acclimation:report-condition
    ((condition invalid-clause-count) stream (language acclimation:english))
  (cl:format stream
             "Directive contains ~a clause~:p but ~:[only between ~a and ~a clauses are ~
              permitted~;exactly ~a clause~:p is required~].~%"
             (actual-count condition)
             (= (minimum-count condition) (maximum-count condition))
             (minimum-count condition) (maximum-count condition)))

(defmethod acclimation:report-condition
    ((condition incompatible-layout-requirements) stream (language acclimation:english))
  (cl:format stream
             "The layout requirement ~a is not compatible with the requirement of ~
              ~a~:[~; as an ancestor~].~%"
             (requirement2 condition)
             (requirement1 condition)
             (ancestor condition)))

#+(or)(defmethod acclimation:report-condition
    ((condition illegal-clause-separator) stream (language acclimation:english))
  (cl:format stream "At most the last clause separator can have ~
                  a `:' modifier"))

#+(or)(defmethod acclimation:report-condition
    ((condition clause-separator-with-colon-modifier-not-allowed) stream
     (language acclimation:english))
  (cl:format stream "A default clause is incompatible with modifiers."))

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
          (what-argument condition)
          (max-arguments condition)))

(defmethod acclimation:report-condition
    ((condition invalid-destination) stream (language acclimation:english))
  (cl:format stream
          "The object ~s is not a valid destination for a format operation."
          (destination condition)))
