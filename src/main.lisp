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
    (filter (complement (text-contains "No data selected")))
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
    (unless (array-empty-p results)
      (make-nando-url (elt results 0)))))

(defun extract-conformity-assessment-modules (response)
  (let ((results (lquery:$ (initialize response)
                           "td"
                           (filter (text-contains "Equipment providing respiratory system protection"))
                           (next)
                           (combine (filter (text-contains "EU type-examination"))
                                    (filter (text-contains "Supervised product checks at random intervals"))
                                    (filter (text-contains "Quality assurance of the production process")))
                           (map #'(lambda (modules)
                                    (loop :for m :in modules
                                          :for n :in '(b c2 d)
                                          :unless (array-empty-p m)
                                          :collect n))))))
    (unless (array-empty-p results)
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

(defun array-empty-p (array)
  (zerop (length array)))

#; Scratch
(cffi:load-foreign-library "libssl.dylib")

;; Happy path
(nb-find 2233)
(nb-find "2233")

(nb-authorized-p "2233")

(nb-authorized-p (nb-find "2233"))

;; Another few happy ones
(nb-authorized-p "0370")
(nb-authorized-p "2841") ;; this one only has B, and C2 modules, but that's fine

;; No PPE regulation authorization
(nb-find "1282")
(nb-authorized-p (nb-find "1282"))

;; 0099, Although the agency has a small category of masks authorized by PPE
;; Regulation (EU) 2016/425, the agency can only conduct Module D audits and
;; has no right to issue Module B type inspection certificates. Therefore, the
;; CE certificate for protective masks issued by it does not comply with EU
;; law!
;; Actually, this is all good as well -- since 28/04/2020 at least
(nb-authorized-p "0099")

(remove-if-not #'nb-authorized-p (nb-search "370"))
