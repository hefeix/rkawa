
(define-alias REXP org.rosuda.REngine.REXP)
(define-alias REXPDouble org.rosuda.REngine.REXPDouble)
(define-alias REXPEnvironment org.rosuda.REngine.REXPEnvironment)
(define-alias REXPExpressionVector org.rosuda.REngine.REXPExpressionVector)
(define-alias REXPFactor org.rosuda.REngine.REXPFactor)
(define-alias REXPGenericVector org.rosuda.REngine.REXPGenericVector)
(define-alias REXPInteger org.rosuda.REngine.REXPInteger)
(define-alias REXPLanguage org.rosuda.REngine.REXPLanguage)
(define-alias REXPList org.rosuda.REngine.REXPList)
(define-alias REXPLogical org.rosuda.REngine.REXPLogical)
(define-alias REXPNull org.rosuda.REngine.REXPNull)
(define-alias REXPString org.rosuda.REngine.REXPString)
(define-alias REXPSymbol org.rosuda.REngine.REXPSymbol)
(define-alias REXPVector org.rosuda.REngine.REXPVector)
(define-alias REXPUnknown org.rosuda.REngine.REXPUnknown)
(define-alias RList org.rosuda.REngine.RList)
(define-alias RFactor org.rosuda.REngine.RFactor)

;; Return a seq of attributes, or nil if the REXP has no attributes
(define (r-attr-names rexp ::REXP) ::java.util.Vector
  ((rexp:asList):names))

;; Returns the attribute with the passed in name, or nil.
(define (r-attr rexp ::REXP attr ::String) ::String
  ((rexp:getAttribute attr):asString))

;;  Returns true if the attribute with the passed in name exists, or false.
(define (r-has-attr? rexp ::REXP attr ::String)
  (rexp:hasAttribute attr))

(define (dataframe? rexp ::REXP)
  ((r-attr rexp "class"):contains "data.frame"))

(define (from-dataframe rexp ::REXP)
  (rexp:asStrings))

(define (from-r rexp ::REXP)
  (cond
    ;; int[]
    ((rexp:isFactor) ((rexp:asFactor):asIntegers))
    ;; int[]
    ((rexp:isInteger) (rexp:asIntegers))
    ;; String[]
    ((rexp:isList) (rexp:asStrings))
    ;; int[]
    ((rexp:isLogical) (rexp:asIntegers))
    ;; int[]
    ((rexp:isNA) (rexp:asIntegers))
    ;; int[]
    ((rexp:isNull) (rexp:asIntegers))
    ;; double[]
    ((rexp:isNumeric) (rexp:asDoubles))
    ;;
    ((dataframe? rexp) (from-dataframe rexp))
    ;;
    (else (format #t "from-r: rexp = ~A~%" (rexp:toDebugString))
          (rexp:asStrings))))

(define (to-r obj)
  (cond
    ((number? obj) (REXPDouble (as double obj)))
    (else (format #t "to-r: obj = ~A~%" obj)
          (REXPNull))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-alias Exception java.lang.Exception)
(define-alias System java.lang.System)
(define-alias IllegalStateException java.lang.IllegalStateException)

(define-alias JRIEngine org.rosuda.REngine.JRI.JRIEngine)
(define-alias REngineEvalException org.rosuda.REngine.REngineEvalException)
(define-alias REngineException org.rosuda.REngine.REngineException)
(define-alias REXPMismatchException org.rosuda.REngine.REXPMismatchException)

;; (define-simple-class JriCallbacks (org.rosuda.JRI.RMainLoopCallbacks)
;;   ((rBusy e which)
;;    (format #t "r is busy"))
;;   ((rChooseFile e new-file)
;;    (format #t "r choose file is called") #!null)
;;   ((rFlushConsole e)
;;    #!null)
;;   ((rLoadHistory e file-name)
;;    (format #t "r load history from ~A" file-name))
;;   ((rSaveHistory e file-name)
;;    (format #t "r save history to ~A" file-name))
;;   ((rReadConsole e prompt add-to-history)
;;    #!null)
;;   ((rShowMessage e message)
;;    (format #t "r show message ~A" message))
;;   ((rWriteConsole e text type)
;;    (format #t "r write console ~A" text)))

(define *jri-engine* #f)

;; Initialize the jri engine. This can only be done once in the
;; lifetime of the jvm-- not our rules- that's how the embedded R engine
;; seems to want things.
(define (init-jri-engine #!optional (handler #f))
  (when *jri-engine*
    (throw (IllegalStateException
             "JRIEngine can only be initialized once per lifetime of the JVM")))
  ;; Tell Rengine to not die if it can't load the JRI native lib
  ;; This allows us to catch and deal with this situation ourselves
  (System:setProperty "jri.ignore.ule" "yes")
  (set! *jri-engine* (JRIEngine (String[] "--vanilla" "--quiet"))))

;; Get the jri-engine. If it doesn't exist, create it using the
;; default init-jri-engine function and return it.
(define (get-jri-engine) ::JRIEngine
  (if *jri-engine*
      *jri-engine*
      (begin (init-jri-engine)
             (if *jri-engine*
                 *jri-engine*
                 (throw (IllegalStateException
                          "Unable to initialize JRIEngine"))))))

;; Eval expression in the R engine. Will not catch any exceptions that
;; happen during evaluation
(define (r-eval-no-catch expr ::String)
  (let ((r (get-jri-engine)))
    (r:parseAndEval expr)))

;; Eval expression in the R engine. Just return the raw JRI/R wrapper,
;; don't convert to Scheme object
(define (r-eval-raw expr ::String)
  (let ((r (get-jri-engine)))
    (try-catch
      (r:parseAndEval expr)
      (exn REngineException
        (format #t "Caught exception evaluating expression: ~A~%: ~A" expr exn))
      (exn REXPMismatchException
        (format #t "Caught exception evaluating expression: ~A~%: ~A" expr exn)))))

;; Eval expression in the R engine. Convert the return value from
;; JRI/R to Scheme
(define (r-eval expr)
  (from-r (r-eval-raw expr)))

;; Eval expression in the R engine, wrapped (on the R side) in
;; try/catch. Will catch errors on the R side and convert to Exception
;; and throw
(define (r-try-parse-eval expr ::String)
  (let ((r (get-jri-engine)))
    (try-catch
      (begin
        (r:assign ".tmp." expr)
        (let ((rexp ::REXP (r:parseAndEval "try(eval(parse(text=.tmp.)),slient=TRUE)")))
          (if (rexp:inherits "try-error")
              (throw (Exception
                       (format #f "Error in R evaluating: ~%~A~% ~A~%" expr (rexp:asString))))
              rexp)))
      (exn REngineException
        (format #t "Caught exception evaluating expression: ~A~%: ~A" expr exn))
      (exn REXPMismatchException
        (format #t "Caught exception evaluating expression: ~A~%: ~A" expr exn)))))

;; Evaluate forms that are string using r-eval-no-catch, otherwise, just eval
;; clojure code normally
(defmacro with-r-eval-no-catch (#!rest forms)
  `(begin
     ,@(map (lambda (form)
              (if (string? form)
                  `(r-eval-no-catch ,form)
                  form))
            forms)))

;; Evaluate forms that are string using r-eval-raw, otherwise, just eval
;; Scheme code normally
(defmacro with-r-eval-raw (#!rest forms)
  `(begin
     ,@(map (lambda (form)
              (if (string? form)
                  `(r-eval-raw ,form)
                  form))
            forms)))

;; Evaluate forms that are string using r-try-parse-eval, otherwise
;; just eval Scheme code normally
(defmacro with-r-try-parse-eval (#!rest forms)
  `(begin
     ,@(map (lambda (form)
              (if (string? form)
                  `(r-try-parse-eval ,form)
                  form))
            forms)))

(defmacro with-r-eval (#!rest forms)
  `(begin
     ,@(map (lambda (form)
              (if (string? form)
                  `(r-eval ,form)
                  form))
            forms)))

;; Assign r-name to val within the R engine
(define (r-set! name ::String value ::REXP)
  (let ((r (get-jri-engine)))
    (try-catch
      (r:assign name value)
      (exn REngineException
        (format #t "Caught exception assigning R value: ~A~%~A~%" name exn)))))

;; Retrieve the value with this name in the R engine. Do not convert
;; from JRI to Scheme type.
(define (r-get-raw name ::String)
  (r-eval-raw name))

;; Retrieve the value with this name in the R engine
(define (r-get name)
  (r-eval name))

(define (r-begin-gd)
  (r-eval-raw "library(\"JavaGD\"); JavaGD()"))

(define (r-end-gd)
  (r-eval-raw "graphics.off()"))
