(cl:in-package #:invistra)

(defmethod acclimation:report-condition :after
    ((condition directive-parse-error) stream (language acclimation:english))
  (with-accessors ((directive directive)
                   (index index))
      condition
    (with-accessors ((control-string control-string)
                     (start start)
                     (end end))
        directive
      (cond ((= start index end)
             (cl:format stream "  ~a~%~vt^" control-string (+ index 2)))
            ((= start index)
             (cl:format stream "  ~a~%  ~vt^~vt]" control-string (+ index 2) (+ end 2)))
            ((= index end)
             (cl:format stream "  ~a~%  ~vt[~vt^" control-string (+ start 2) (+ index 2)))
            (t
             (cl:format stream "  ~a~%  ~vt[~vt^~vt]"
                        control-string (+ start 2) (+ index 2) (+ end 2)))))))

(defmethod acclimation:report-condition :after
    ((condition directive-syntax-error) stream (language acclimation:english))
  (with-accessors ((directive directive))
      condition
    (with-accessors ((control-string control-string)
                     (start start)
                     (end end))
        directive
      (cl:format stream "  ~a~%~vt[~vt]"
                 control-string (+ start 2) (+ end 2)))))

(defmethod acclimation:report-condition
    ((condition end-of-control-string) stream (language acclimation:english))
  (write-line "Unexpected end of the control string." stream))

(defmethod acclimation:report-condition
    ((condition expected-integer-error) stream (language acclimation:english))
  (write-line "Expected an integer parameter in control string." stream))

#+(or)(defmethod acclimation:report-condition
    ((condition expected-parameter-start) stream (language acclimation:english))
  (cl:format stream
          "expected one of ', +, -, or a decimal digit at index ~a,~%~
              but found the character `~a' instead."
          (index condition)
          (char (control-string condition) (index condition))))

(defmethod acclimation:report-condition
    ((condition duplicate-modifiers) stream (language acclimation:english))
  (write-line "Duplicate modifiers were found in control string." stream))

#+(or)(defmethod acclimation:report-condition
    ((condition unknown-format-directive) stream (language acclimation:english))
  (cl:format stream
          "unknown format directive `~a' at index ~a."
          (char (control-string condition) (index condition))
          (index condition)))

#+(or)(defmethod acclimation:report-condition :before
    ((condition directive-syntax-error) stream (language acclimation:english))
  (with-accessors ((control-string control-string)
                   (start start)
                   (end end))
      (directive condition)
    (cl:format stream
            "In the control-string \"~a\", the directive \"~a\""
            control-string
            (subseq control-string start end))))

(defmethod acclimation:report-condition
    ((condition unknown-directive-character) stream (language acclimation:english))
  (cl:format stream
             "Unknown directive ~:c character in control string.~%"
             (directive-character (directive condition))))

(defmethod acclimation:report-condition
    ((condition illegal-modifiers) stream (language acclimation:english))
  (cl:format stream "~:[Illegal~;Conflicting~] modifier~p ~{~#[~;~:c~;~:c and ~:c~:;~@{~:c~#[~;, and ~:;, ~]~}~]~} found in directive.~%"
             (conflictingp condition)
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
             "~a was required as parameter, but ~a was found~%"
             (type-name (type-error-expected-type condition))
             (type-error-datum condition)))

(defmethod acclimation:report-condition
    ((condition no-more-arguments) stream (language acclimation:english))
  (cl:format stream "An attempt was made to access more arguments than available."))

(defmethod acclimation:report-condition
    ((condition argument-type-error) stream (language acclimation:english))
  (cl:format stream
             "~a was required as argument, but ~a was found"
             (type-name (type-error-expected-type condition))
             (type-error-datum condition)))

(defmethod acclimation:report-condition
    ((condition too-many-package-markers) stream (language acclimation:english))
  (cl:format stream "the function name contains too many package markers."))

(defmethod acclimation:report-condition
    ((condition no-such-package) stream (language acclimation:english))
  (cl:format stream "A package named ~a does not exist."
             (no-such-package-package-name condition)))

(defmethod acclimation:report-condition
    ((condition no-such-symbol) stream (language acclimation:english))
  (cl:format stream "A symbol with a name of ~a does not exist."
             (no-such-symbol-symbol-name condition)))

(defmethod acclimation:report-condition
    ((condition symbol-not-external) stream (language acclimation:english))
  (cl:format stream "The symbol ~s is not external in the package."
             (symbol-not-external-symbol condition)))

(defmethod acclimation:report-condition
    ((condition go-to-out-of-bounds) stream (language acclimation:english))
  (cl:format stream "An attempt was made to go to argument number ~d ~
                  instead of one between 0 and ~d."
          (what-argument condition)
          (max-arguments condition)))

(defmethod acclimation:report-condition
    ((condition modifier-and-parameter) stream (language acclimation:english))
  (cl:format stream "A parameter can be used only of there are no modifiers."))

(defmethod acclimation:report-condition
    ((condition illegal-clause-separators) stream (language acclimation:english))
  (cl:format stream "At most the last clause separator can have ~
                  a `:' modifier"))

(defmethod acclimation:report-condition
    ((condition clause-separator-with-colon-modifier-not-allowed) stream
     (language acclimation:english))
  (cl:format stream "A default clause is incompatible with modifiers."))

(defmethod acclimation:report-condition
    ((condition parameter-omitted) stream (language acclimation:english))
  (cl:format stream
          "Parameter number ~d was given, but parameter ~d ~
           was omitted, which is not allowed."
          (parameter1 condition)
          (parameter2 condition)))

(defmethod acclimation:report-condition
    ((condition unmatched-directive) stream (language acclimation:english))
  (cl:format stream
             "there is no matching directive"))

(defmethod acclimation:report-condition
    ((condition nesting-violation) stream (language acclimation:english))
  (with-accessors ((parent-directive parent-directive))
      condition
    (if parent-directive
        (with-accessors ((control-string control-string)
                         (start start)
                         (end end))
            parent-directive
          (cl:format stream " cannot be nested inside of a \"~a\" directive."
                     (subseq control-string start end)))
        (write-string " cannot be used as a top level directive." stream))))

(defmethod acclimation:report-condition
    ((condition invalid-destination) stream (language acclimation:english))
  (cl:format stream
          "The object ~s is not a valid destination for a format operation."
          (destination condition)))

(defmethod acclimation:report-condition
    ((condition invalid-clause-count) stream (language acclimation:english))
  (cl:format stream
             " contains ~a clause~:p but ~:[only between ~a and ~a clauses are permitted~;exactly ~a clause~:p is required~]."
             (length (clauses (directive condition)))
             (= (minimum-count condition) (maximum-count condition))
             (minimum-count condition) (maximum-count condition)))

(defmethod acclimation:report-condition
    ((condition incompatible-layout-requirements) stream (language acclimation:english))
  (cl:format stream
             "The layout requirement ~a is not compatible with the requirement of ~%
              ~a~:[~; as an ancestor~]."
             (requirement2 condition)
             (requirement1 condition)
             (ancestor condition)))
