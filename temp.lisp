;( (a string (check-fn check-form)) )
;;; => (&key (a string :check check-fn :default [default]))


(defparameter *ll* 
  '(&key (a string :check check-fn :default [default])))

(defun extra-sub-lambda-lists-positions (ll &optional (start 0))
  (let ((found 
          (position-if 
                       (lambda (x) (member x '(&key &optional &rest)))
                       ll :start start)))
    (when found (cons found (extra-sub-lambda-lists-positions ll (1+ found))))))

(defun extra-sub-lambda-list (ll)
  (let* ((xtra-pos (extra-sub-lambda-lists-positions ll))
         (xtra-pos 
           (if (<= (length xtra-pos) 1) 
               (first xtra-pos)
               (error "more than one occurrence of &KEY|&OPTIONAL|&REST in lambda list.")))
         (xtra (when xtra-pos (nthcdr xtra-pos ll))))
    xtra))

(defun split-lambda-list (ll)
  (let* ((xtra (extra-sub-lambda-list ll))
         (required (if xtra
                       (subseq ll 0 (- (length ll)  (length xtra)))
                       ll)))
    (list required xtra)))


(defun make-parameter-descriptor (p)
  ; SYM | (SYM [TYPE] [CHECK-FN | (CHECK-FN CHECK-FORM)] [DEFAULT])
  (assert (or (atom p) (and (listp p) (>= #1=(length p) 1) (<= #1# 4))))
  (let* ((p-structured (not (atom p)))
         (p-len (if p-structured (length p) 1))
         (p-typed-p (>= p-len 2))
         (p-validated-p (>= p-len 3))
         (p-validated-structured-p (and p-validated-p (consp (third p))))
         (p-name (if p-structured (first p) p))
         (p-type (or (and p-typed-p (second p)) 'T))
         (p-validate-fn-sym
           (and p-validated-p 
                (if p-validated-structured-p
                    (first (third p)) 		    
                    (third p))))
         (p-validate-fn 
           (if (fboundp p-validate-fn-sym)
               (symbol-function p-validate-fn-sym)
               (and p-validate-fn-sym
                    (error "Unknown validate function: ~a" p-validate-fn-sym))))
         (p-validate-parameter 
           (and p-validated-p p-validated-structured-p (second (third p))))
         (p-default (and p-structured (fourth p))))
    (lambda (request &optional parameter-value)
      (case request
        (:name p-name)
        (:type p-type)
        (:default p-default)
        (:check-type (subtypep (type-of parameter-value) p-type ))
        (:validate (cond
                     ((not p-validated-p) nil)
                     (p-validated-structured-p 
                       (funcall p-validate-fn parameter-value p-validate-parameter))
                     (T (funcall p-validate-fn parameter-value))))
        (otherwise 
          (error "Unknown request to parameter descriptor: ~a" request))))))

(defun parse-sub-lambda-list (sub-ll)
  (mapcar #'make-parameter-descriptor sub-ll))

(defun parse-extra-sub-lambda-list  (xtra)
  (let ((xtra-type (first xtra)))
    (list xtra-type (parse-sub-lambda-list (rest xtra)))))

(defun parse-lambda-list (ll)
  (let* ((split-ll (split-lambda-list ll))
         (required (first split-ll))
         (parsed-required (parse-sub-lambda-list required))
         (xtra (second split-ll))
         (parsed-xtra (parse-extra-sub-lambda-list xtra)))
    (list parsed-required  parsed-xtra)))

(defun parameter-descriptor-list-type (parameter-descriptors)
  (let ((maybe-xtra (first parameter-descriptors)))
    (if (typep maybe-xtra 'function)
        'required
        maybe-xtra)))

(defun parameter-names-from-descriptor-list (parameter-descriptors)
  (let* ((pd-list-type (parameter-descriptor-list-type parameter-descriptors))
         (pd-list 
           (if (eq pd-list-type 'required) 
               parameter-descriptors 
               (cadr parameter-descriptors))))
    (mapcar
      (lambda (p) (funcall p :name))
      pd-list)))

(defun valid-return-type-specification (exp)
  (and
    (consp exp)
    (= (length exp) 2)
    (eq (first exp) :function-return-type)))

(defun valid-restarts-specification (exp parameter-symbols)
  (and
    (consp exp)
    (>= (length exp) 2)
    (eq (first exp) :function-restarts)
    (iter 
      (for r in (rest exp)) 
      (when 
          (not (ignore-errors (subtypep (first r) 'condition))) 
        (error "invalid condition type in restart: ~a" (first r)))
      (when (not (= (length (second r)) (1+ (length parameter-symbols))))  
        (error "restart for condition ~a must have exactly this lambda list: ~a"
               (first r)
               (destructuring-bind 
                   ((&rest args) condition) 
                   (list parameter-symbols 'condition-instance) 
                 `(,@args ,condition))))
      (finally (return T)))))


(defmacro make-function-descriptor (name lambda-list &body body)
  `(let* ((name ',name)
          (original-lambda-list ',lambda-list)
          (parameter-sets (parse-lambda-list original-lambda-list))
          (required-parameters 
            (parameter-names-from-descriptor-list  (first parameter-sets)))
          ;; next block generates lists for parameter symbols, like
          ;; the previous required-parameters.
          ;; bindings: optional-parameters, keyword-parameters, rest-parameter
          ,@(iter 
              (for (sym . pd-type) in
                   '((optional-parameters . &optional)
                     (keyword-parameters . &key)
                     (rest-parameter . &rest)))
              (collect
                `(,sym
                   (when (eq (parameter-descriptor-list-type (second parameter-sets)) 
                             ',pd-type)
                     (parameter-names-from-descriptor-list 
                       (second parameter-sets))))))
          (xtra-parameters 
            (first (delete-if #'null 
                              (list optional-parameters keyword-parameters rest-parameter))))
          (xtra-type 
            (when xtra-parameters 
              (cdr 
                (first 
                  (delete-if 
                             (lambda (x) (null (car x)))
                             (list (cons keyword-parameters  '&key)
                                   (cons optional-parameters '&optional)
                                   (cons rest-parameter '&rest)))))))
          (flat-parameters-list (append required-parameters xtra-parameters))
          (ansi-lambda-list 
            (let ((result (list)))
              (iter (for p in (first parameter-sets))
                    (push (funcall p :name) result))
              (and xtra-type
                   (push xtra-type result)
                   (iter (for p in (cadr (second parameter-sets)))
                         (if (eq xtra-type '&optional)
                             (push #1=(funcall p :name) result)
                             (if #2=(funcall p :default)
                                 (push (list #1# #2#) result)
                                 (push #1# result)))))
              (reverse result)))
          (original-body '(,@body))
          (documentation-strings 
            (or
              (iter  
                    (while (<= i (length original-body))
                      )
                    (generate i upfrom 0)
                    (next i)
                    (when (stringp (first (nthcdr i original-body)))
                      (collect (first (nthcdr i original-body))))
                    )
              (list "REMINDER: this function needs documentation")))
          (return-type 
            (let 
                ((first-form 
                  (first (nthcdr (length documentation-strings) original-body)))) 
              (when (valid-return-type-specification first-form)
                (second first-form))))
          (restarts (let
                      ((first-form (first (nthcdr (+ (length documentation-strings) (if return-type 1 0)) original-body))))
                      (when 
                          (valid-restarts-specification first-form flat-parameters-list)
                        (rest first-form)
                        )
                      ))
          )
     (lambda (request)
       (case request
         (:query
           (reverse (pairlis
                      (list :name
                            :original-lambda-list
                            :required
                            :optional
                            :keyword
                            :rest
                            :has-xtra
                            :xtra-type
                            :parameter-symbols
                            :ansi-lambda-list
                            :documentation
                            :return-type
                            :restarts)
                      (list name
                            original-lambda-list
                            required-parameters
                            optional-parameters
                            keyword-parameters
                            rest-parameter
                            (not (null xtra-parameters))
                            xtra-type
                            flat-parameters-list
                            ansi-lambda-list
                            documentation-strings
                            return-type
                            restarts))))
         (otherwise (error "Unknown request for function descriptor: ~a" request)))
)))

