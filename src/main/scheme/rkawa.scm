;;----------------------------------------------------------------------

(define-alias Exception java.lang.Exception)
(define-alias IllegalArgumentException java.lang.IllegalArgumentException)
(define-alias IllegalStateException java.lang.IllegalStateException)
(define-alias Object java.lang.Object)

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
(define-alias REXPUnknown org.rosuda.REngine.REXPUnknown)
(define-alias REXPVector org.rosuda.REngine.REXPVector)
(define-alias REXPWrapper org.rosuda.REngine.REXPWrapper)
(define-alias RFactor org.rosuda.REngine.RFactor)
(define-alias RList org.rosuda.REngine.RList)

;;----------------------------------------------------------------------

(define (jarray->vector array)
  (let* ((n array:length)
         (v (make-vector n)))
    (do ((i ::int 0 (+ i 1)))
        ((>= i n) v)
      (vector-set! v i (array i)))))

(define (r-na->scheme rexp)
  0)

(define (r-null->scheme rexp)
  #!void)

(define (r-factor->scheme rexp ::REXP)
  (let* ((value (->REXPFactor rexp)))
    (jarray->vector (value:asStrings))))

(define (r-logical->scheme rexp ::REXP)
  (let* ((value (->REXPLogical rexp))
         (bools (value:isTRUE))
         (length (value:length)))
    (if (= length 1)
        (bools 0)
        (jarray->vector bools))))

(define (r-integer->scheme rexp ::REXP)
  (let* ((value (->REXPInteger rexp))
         (length (value:length)))
    (if (= length 1)
        (value:asInteger)
        (jarray->vector (value:asIntegers)))))

(define (r-numeric->scheme rexp ::REXP)
  (let* ((value (->REXPDouble rexp))
         (length (value:length)))
    (if (= length 1)
        (value:asDouble)
        (jarray->vector (value:asDoubles)))))

(define (r-string->scheme rexp ::REXP)
  (let* ((value (->REXPString rexp))
         (length (value:length)))
    (if (= length 1)
        (value:asString)
        (jarray->vector (value:asStrings)))))

(define (r-symbol->scheme rexp ::REXP)
  (let* ((value (->REXPSymbol rexp))
         (length (value:length)))
    (if (= length 1)
        (value:asString)
        (jarray->vector (value:asStrings)))))

(define (r-complex->scheme rexp ::REXP)
  (throw (IllegalArgumentException "r-comple->scheme no conversion rules")))

(define (r-vector->scheme rexp ::REXP)
  (let* ((n (rexp:length))
         (rlist (rexp:asList))
         (result (make-vector n)))
    (do ((i ::int 0 (+ i 1)))
        ((>= i n) result)
      (vector-set! result i (r->scheme (rlist:at i))))))

(define (r-list->scheme rexp ::REXP)
  (let* ((n (rexp:length))
         (rlist (rexp:asList))
         (result (make-vector n)))
    (do ((i ::int 0 (+ i 1)))
        ((>= i n) (vector->list result))
      (vector-set! result i (r->scheme (rlist:at i))))))

(define (r->scheme rexp ::REXP)
  (cond
    ((rexp:isNull) (r-null->scheme rexp))

    ((rexp:isFactor) (r-factor->scheme rexp))
    ((rexp:isLogical) (r-logical->scheme rexp))
    ((rexp:isInteger) (r-integer->scheme rexp))
    ((rexp:isNumeric) (r-numeric->scheme rexp))
    ((rexp:isString) (r-string->scheme rexp))
    ((rexp:isSymbol) (r-symbol->scheme rexp))
    ((rexp:isComplex) (r-complex->scheme rexp))

    ((rexp:isVector) (r-vector->scheme rexp))
    ((rexp:isList) (r-list->scheme rexp))

    (else (format #t "r->scheme: rexp = ~A~%" (rexp:toDebugString))
          (rexp:toDebugString))))

;;----------------------------------------------------------------------

(define (jstring->r value ::String)
  (REXPString value))

(define (jobject->r object ::Object)
  (REXPWrapper:wrap object))

(define (number->r value)
  (REXPDouble (->double value)))

(define (quantity->r value)
  (number->r (quantity->number value)))

(define (complex->r value)
  (throw (IllegalArgumentException "complex->r no conversion rules")))

(define (real->r value)
  (number->r value))

(define (rational->r value)
  (number->r value))

(define (integer->r value)
  (REXPInteger (->integer value)))

(define (symbol->r value)
  (REXPSymbol (symbol->string value)))

(define (keyword->r value)
  (REXPSymbol (keyword->string value)))

(define (vector->rexp-array value) ::REXP[]
  (let* ((n (vector-length value))
         (result (REXP[] length: n)))
    (do ((i ::int 0 (+ i 1)))
        ((>= i n) result)
      (set! (result i) (scheme->r (vector-ref value i))))))

(define (list->r value)
  (REXPList (RList (vector->rexp-array (list->vector value)))))

(define (vector->r value)
  (REXPGenericVector (RList (vector->rexp-array value))))

(define (string->r value ::string)
  (REXPString value))

(define (character->r value)
  (integer->r (char->integer value)))

(define (procedure->r value)
  (throw (IllegalArgumentException "procedure->r no conversion rules")))

(define (input-port->r value)
  (throw (IllegalArgumentException "input-port->r no conversion rules")))

(define (output-port->r value)
  (throw (IllegalArgumentException "output-port->r no conversion rules")))

(define (scheme->r value) ::REXP
  (cond
    ((number? value) (number->r value))
    ((quantity? value) (quantity->r value))
    ((complex? value) (complex->r value))
    ((real? value) (real->r value))
    ((rational? value) (rational->r value))
    ((integer? value) (integer->r value))
    ((symbol? value) (symbol->r value))
    ((keyword? value) (keyword->r value))
    ((list? value) (list->r value))
    ((string? value) (string->r value))
    ((character? value) (character->r value))
    ((vector? value) (vector->r value))
    ((procedure? value) (procedure->r value))
    ((input-port? value) (input-port->r value))
    ((output-port? value) (output-port->r value))
    ((String? value) (jstring->r value))
    ((Object? value) (jobject->r value))
    (else (let ((e (format #f "scheme->r unknown conversion: value = ~A" value)))
            (format #t "~A~%" e)
            (throw (IllegalArgumentException e))))))

;;----------------------------------------------------------------------

(define-alias System java.lang.System)

(define-alias JRIEngine org.rosuda.REngine.JRI.JRIEngine)
(define-alias REXPMismatchException org.rosuda.REngine.REXPMismatchException)
(define-alias REngineEvalException org.rosuda.REngine.REngineEvalException)
(define-alias REngineException org.rosuda.REngine.REngineException)

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
(define (init-jri-engine #!optional (handler #!null))
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

;;----------------------------------------------------------------------

;; Eval expression in the R engine. Just return the raw JRI/R wrapper,
;; don't convert to Scheme object
(define (r% expr ::String)
  (let ((r (get-jri-engine)))
    (try-catch
      (r:parseAndEval expr)
      (exn REngineException
        (format #t "Caught exception evaluating expression: ~A~%: ~A" expr exn))
      (exn REXPMismatchException
        (format #t "Caught exception evaluating expression: ~A~%: ~A" expr exn)))))

;; Eval expression in the R engine. Convert the return value from
;; JRI/R to Scheme
(define (r expr)
  (r->scheme (r% expr)))

;; Eval expresion in the R engine. Wrap the return value in print
(define (r* expr)
  (r &{capture.output(&[expr])}))

;; Eval expression in the R engine. Will not catch any exceptions that
;; happen during evaluation
(define (r-eval-no-catch expr ::String)
  (let ((r (get-jri-engine)))
    (r:parseAndEval expr)))

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

;; Evaluate forms that are string using r-eval-raw, otherwise, just eval
;; Scheme code normally
(defmacro with-r% (#!rest forms)
  `(begin
     ,@(map (lambda (form)
              (if (string? form)
                  `(r% ,form)
                  form))
            forms)))

(defmacro with-r (#!rest forms)
  `(begin
     ,@(map (lambda (form)
              (if (string? form)
                  `(r ,form)
                  form))
            forms)))

;; Evaluate forms that are string using r-eval-no-catch, otherwise, just eval
;; scheme code normally
(defmacro with-r-eval-no-catch (#!rest forms)
  `(begin
     ,@(map (lambda (form)
              (if (string? form)
                  `(r-eval-no-catch ,form)
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

;;----------------------------------------------------------------------
;; Assign r-name to val within the R engine
(define (r/set! name ::String value)
  (let ((r (get-jri-engine))
        (rexp (scheme->r value)))
    (try-catch
      (r:assign name rexp)
      (exn REngineException
        (format #t "Caught exception assigning R value: ~A~%~A~%" name exn)))))

;; Retrieve the value with this name in the R engine. Do not convert
;; from JRI to Scheme type.
(define (r/get% name ::String)
  (r% name))

;; Retrieve the value with this name in the R engine
(define (r/get name)
  (r name))

;;----------------------------------------------------------------------

(define (r/attrs rexp ::REXP)
  #!null)

;;----------------------------------------------------------------------

(define (r/dev.on)
  (r &{library("JavaGD")
       JavaGD()}))

(define (r/dev.off)
  (r &{graphics.off()}))

(define (r/library library)
  (r &{library("&[library]")}))

;;----------------------------------------------------------------------
