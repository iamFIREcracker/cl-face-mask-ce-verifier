(uiop:define-package #:fmcv
  (:import-from #:binding-arrows
   :-<>)
  (:export
   :notified-body
   :notified-body-id
   :notified-body-name
   :notified-body-details-url
   :notified-body-country
   :notified-body-ppe-legislation-url
   :notified-body-conformity-assessment-modules
   :nb-authorized-p
   :nb-find
   :nb-search))
(in-package #:fmcv)


(defstruct notified-body
  id
  name
  details-url
  country
  ppe-legislation-url
  conformity-assessment-modules)

(defparameter *nando-base-url* "https://ec.europa.eu/growth/tools-databases/nando")

(defun nb-authorized-p (nb-or-id)
  (let ((nb (if (notified-body-p nb-or-id) nb-or-id (nb-find nb-or-id))))
    (and (find 'b (notified-body-conformity-assessment-modules nb))
         (or (find 'c2 (notified-body-conformity-assessment-modules nb))
             (find 'd (notified-body-conformity-assessment-modules nb)))
         nb)))


(defun nb-find (number-or-string)
  (first (nb-search number-or-string)))

(defun nb-search (number-or-string)
  (let* ((response (dex:post (make-nando-url "index.cfm?fuseaction=search.notifiedbody")
                             :content `(("critere_body_nb" . ,number-or-string))))
         (nbs (extract-notified-bodies response)))
    (loop
      :for nb :across nbs :do
      (let* ((response (dex:get (notified-body-details-url nb)))
             (ppe-legislation-url (extract-ppe-legislation-url response)))
        (when ppe-legislation-url
          (setf (notified-body-ppe-legislation-url nb) ppe-legislation-url)
          (let ((response (dex:get ppe-legislation-url)))
            (setf (notified-body-conformity-assessment-modules nb)
                  (extract-conformity-assessment-modules response))))))
    (coerce nbs 'list)))

(defun extract-notified-bodies (response)
  (lquery:$ (initialize response)
    "#main_content table table tr"
    ;; The above matches all the rows from two different tables: the first
    ;; one, displays all the search criteria (2 rows, one for the label, one
    ;; for the actual values); the second one, displays the notification
    ;; bodies matching search criterias (1 row for the header, one per result)
    ;;
    ;; Here, we are trying to pull all the matching notification bodies!
    (slice 3)
    (combine (function parse-nb-id)
             (function parse-nb-name)
             (function parse-nb-details-page)
             (function parse-nb-country))
    (map-apply #'(lambda (id name details-page country)
                  (make-notified-body :id id
                                      :name name
                                      :details-url (make-nando-url details-page)
                                      :country country)))))

(defun parse-nb-id (row)
  (-<> (lquery:$ row "td" (first) (text))
       (elt 0)
       trim
       digits-only))

(defun parse-nb-name (row)
  (-<> (lquery:$ row "a" (text))
       (elt 0)
       trim))

(defun parse-nb-details-page (row)
  (-<> (lquery:$ row "a" (attr :href))
       (elt 0)))

(defun parse-nb-country (row)
  (-<> (lquery:$ row "td" (last) (text))
       (elt 0)
       trim))

(defun extract-ppe-legislation-url (response)
  (let ((results (lquery:$ (initialize response)
                   "#main_content td"
                   (filter (text-contains "Regulation (EU) 2016/425 Personal protective equipment"))
                   (next)
                   (find "a")
                   (attr :href))))
    (when (> (length results) 0)
      (make-nando-url (elt results 0)))))

(defun extract-conformity-assessment-modules (response)
  (let ((results (lquery:$ (initialize response)
                   "td"
                   (filter (text-contains "Equipment providing respiratory system protection"))
                   (next)
                   (combine (filter (text-contains "EU type-examination"))
                            (filter (text-contains "Supervised product checks at random intervals"))
                            (filter (text-contains "Quality assurance of the production process")))
                   (map-apply #'(lambda (module-b module-c2 module-d)
                                 (concatenate 'list
                                              (list (and module-b  'b))
                                              (list (and module-c2 'c2))
                                              (list (and module-d  'd))))))))
    (when (> (length results) 0)
      (elt results 0))))

(defun make-nando-url (page)
  (format nil "~a/~a" *nando-base-url* page))

(defun text-contains (string)
  (lambda (node)
    (when (search string (plump:text node))
      node)))

(defun contains (string)
  (lambda (node)
    (search string (plump:text node))))

(defun digits-only (string)
  (cl-ppcre:register-groups-bind (digits)
      ("([0-9]+)" string)
    digits))

(defun trim-all-lines (string)
  (format nil "~{~A~&~}"
          (mapcar #'trim (split-sequence:split-sequence #\Return string))))

(defun trim (string)
  (string-trim
    '(#\Space #\Newline #\Backspace #\Tab
      #\Linefeed #\Page #\Return #\Rubout)
    string))

#; Scratch
(cffi:load-foreign-library "libssl.dylib")

;; Happy path
(nb-find 2233)
(nb-find "2233")

(nb-authorized-p 2233)
(nb-authorized-p "2233")

(nb-authorized-p (nb-find "2233"))

;; No PPE regulation authorization
(nb-find 1282)
(nb-authorized-p (nb-find 1282))
