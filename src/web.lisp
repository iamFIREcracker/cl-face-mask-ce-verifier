(uiop:define-package #:fmcv-web
  (:import-from #:parenscript
   :ps)
  (:import-from #:spinneret
   :with-html
   :with-html-string)
  (:export
   #:start
   #:stop))
(in-package #:fmcv-web)

(defvar *my-acceptor* nil)

(defun start (&key (web-interface "localhost"))
  (serve web-interface))

(defun serve (address)
  (setf *my-acceptor* (make-instance 'hunchentoot:easy-acceptor
                                     :address address
                                     :port 4444))
  (hunchentoot:start *my-acceptor*))

(defun stop()
  (when *my-acceptor*
    (prog1 (hunchentoot:stop *my-acceptor*)
      (setf *my-acceptor* nil))))


(defmacro with-page ((&key (title "Title placeholder")) &body body)
  `(progn
    (setf (hunchentoot:content-type*) "text/html")
    (let ((spinneret:*suppress-inserted-spaces* t)
          (spinneret:*fill-column* 9999999))
      (with-html-string
        (:doctype)
        (:html
         (:head
          (:meta :name :viewport :content "width=device-width, initial-scale=1")
          (:title ,title)
          (page-css)
          (page-js))
         (:body ,@body))))))

(defmacro with-lass (&body blocks)
  (let ((quoted (mapcar #'(lambda (b) `(quote ,b)) blocks)))
    `(lass:compile-and-write
      ,@quoted)))

(defparameter *page-css-raw*
  (with-lass
    (.description
      :font-weight 200)
    ((:or .search .results)
     :margin-top 1rem)
    (.nb
      :display flex
      (.__not-authorized
        :opacity 0.5))
    (.nb-certification
      :text-align center
      :width 1rem)
    (.nb-id
      :text-align center
      :width 5rem)
    (.nb-name
      :display inline-block
      :width 400px
      :white-space nowrap
      :overflow hidden
      :text-overflow ellipsis)
    ((:or .nb-id .nb-name .nb-country)
     :margin-left 0.5rem)
    (.__authorized
      :color green)
    (.__not-authorized
      :color (hex aaaaaa))
    (:media "(max-width: 576px)"
     (.id-prefix
       :display none))))

(defun page-css ()
  (with-html
    (:style :type "text/css"
     (:raw *page-css-raw*))))

(defun page-js ()
  (with-html
    (:script :type "module" :src "https://cdn.skypack.dev/pin/@hotwired/turbo@v7.0.1-PTwzYYo5FVYxpdtkOcWe/mode=imports/optimized/@hotwired/turbo.js")))

(hunchentoot:define-easy-handler (index :uri "/") (q)
  (let ((bodies (and q (fmcv:nb-search q))))
    (with-page (:title "Face Mask CE Verifier")
      (:header
       (:h1 "Face Mask CE Verifier")
       (:aside.description
        (:p
         "Quickly verify if the CE marking used on face masks was issued by an
        accredited "
         (:a :href "https://en.wikipedia.org/wiki/Notified_body" "notified body")
         ".")
        (:p
         "Here are the steps to verify a CE marking:"

         (:ol
          (:li
           "Find the ID of the notified body that issued CE marking (it's the
           numeric part of the marking, i.e. the " (:code "2233") " inside "
           (:code "CE-2233") ")")
          (:li
           "Type that into the search bar below and press the Search button")
          (:li
           "When the list of matching notified bodies is loaded, check if the
           one you searched for had a " (authorized-badge) " alongside its ID
           (that would be a good sign!), or a " (not-authorized-badge)
           " instead (in which case...)")))
        (:details
         (:summary "How does this work?")
         (:p
          "You can read more about this "
          (:a :href "https://matteolandi.net/plan.html#day-2021-10-20" "here")
          ", but very high level:"

          (:ul
           (:li
            "In the context of face masks, notified bodies need to be competent
            with the '2016/425 Personal Protective equipment' legislation")
           (:li
            "Notified bodies also need to be entitled to issue specific
            conformity assessment modules ("
            (:a :href "https://support.ce-check.eu/hc/en-us/articles/360019298431-Conformity-Assessment-Module-B" "B")
            ", and either "
            (:a :href "https://support.ce-check.eu/hc/en-us/articles/360019507611-Conformity-Assessment-Module-C2-" "C2")
            " or "
            (:a :href "https://support.ce-check.eu/hc/en-us/articles/360019191712-Conformity-Assessment-Modules-D-D1" "D")
            ") for 'Equipment providing respiratory system protection' products
            (e.g. face masks)")
           (:li
            "An online database exists, "
           (:a :href
             "https://ec.europa.eu/growth/tools-databases/nando/index.cfm"
             "NANDO") ", storing all this information about all the registered
             notified bodies; even though it does not have an API, it's not too
             difficult to scrape this information out of it and implement the
             above checks"))))))
      (:section
       (:div.search
        (:form
         (:label :for "q" "CE-")
         (:input#q
          :type :search
          :placeholder "Numeric CE mark"
          :name "q" :value (or q ""))
         (:input :type :submit :value "Search")))
       (when (and q (null bodies))
             (:p "No notified body was found."))
       (:div.results
        (loop :for nb :in bodies
              :do (render-notified-body nb)))))))

(defun render-notified-body (nb)
  (with-html
    (:div :class (if (fmcv:nb-authorized-p nb) "nb" "nb __not-authorized")
     (:span.nb-certification (if (fmcv:nb-authorized-p nb)
                                 (authorized-badge)
                                 (not-authorized-badge)))
     " "
     (:span.nb-id
      (:span.id-prefix "NB-")
      (:span.id-number (:strong (fmcv:notified-body-id nb))))
     " "
     (:span.nb-name
      (:a :href (fmcv:notified-body-details-url nb) (fmcv:notified-body-name nb)))
     " "
     (:span.nb-country
      (format nil "(~a)" (fmcv:notified-body-country nb))))))

(defun authorized-badge ()
  (with-html
    (:span.__authorized "✓")))

(defun not-authorized-badge ()
  (with-html
    (:span.__not-authorized "✗")))
#+nil (setf hunchentoot:*catch-errors-p* nil)
