# face-bask-ce-verifier
Common Lisp system for verifying if the CE making used on face masks is
legit, i.e. if it's been issued by an authorized [notified
body](https://en.wikipedia.org/wiki/Notified_body).

## Overview
You can read more about this
[here](https://matteolandi.net/plan.html#day-2021-10-20), but very high level:

- To sell face masks in the UE, you need the CE marking
- The CE marking usually comes with the ID of the notified body that issues the
  marking itself (it's the `2233` part of `CE-2233`)
- For this matter, notified bodies need to be competent with the "2016/425
  Personal protective equipment" legislation
- They also need to be entitled to issue specific conformity assessment
  modules
  ([B](https://support.ce-check.eu/hc/en-us/articles/360019298431-Conformity-Assessment-Module-B)
  and either
  [C2](https://support.ce-check.eu/hc/en-us/articles/360019507611-Conformity-Assessment-Module-C2-)
  or
  [D](https://support.ce-check.eu/hc/en-us/articles/360019191712-Conformity-Assessment-Modules-D-D1)),
  for the "Equipment providing respiratory system protection" product
- An online database exists,
  [NANDO](https://ec.europa.eu/growth/tools-databases/nando/index.cfm), storing
  all this information about all the notified bodies; it does not have an API,
  but information can be _easily_ scraped from it

## Instructions
Clone the repository:

    $ cd ~/Workspace
    $ git clone https://github.com/iamFIREcracker/cl-face-mask-ce-verifier.git

Setup ADFS so it can load any system defined in this repository:

    > (load "build/setup.lisp")

Quickload the main system:

    > (ql:quickload "face-mask-ce-verifier")
    To load "face-mask-ce-verifier":
      Load 1 ASDF system:
        face-mask-ce-verifier
    ; Loading "face-mask-ce-verifier"
    .......
    ("face-mask-ce-verifier")

And that's it, you are ready to go!

### Find a notified body by ID

    > (fmcv:nb-find "2233")
    #S(NOTIFIED-BODY
       :ID "2233"
       :NAME "GÉPTESZT Termelőeszközöket Felülvizsgáló és Karbantartó Kft."
       :DETAILS-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=search.nb&refe_cd=NANDO%5FINPUT%5F166341"
       :COUNTRY "Hungary"
       :PPE-LEGISLATION-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=notification.html&ntf_id=309049&version_no=1"
       :CONFORMITY-ASSESSMENT-MODULES (B C2 D))

Note: this function simply finds notified bodies.  You can can check if the
returned body is authorized or not, using the NB-AUTHORIZED-P function.

    > (fmcv:nb-authorized-p *)
    #S(NOTIFIED-BODY
       :ID "2233"
       :NAME "GÉPTESZT Termelőeszközöket Felülvizsgáló és Karbantartó Kft."
       :DETAILS-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=search.nb&refe_cd=NANDO%5FINPUT%5F166341"
       :COUNTRY "Hungary"
       :PPE-LEGISLATION-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=notification.html&ntf_id=309049&version_no=1"
       :CONFORMITY-ASSESSMENT-MODULES (B C2 D))

### Search all the notified bodies by partial ID match

    > (fmcv:nb-search "370")
    (#S(NOTIFIED-BODY
        :ID "0370"
        :NAME "LGAI TECHNOLOGICAL CENTER, S. A./Applus"
        :DETAILS-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=search.nb&refe_cd=EPOS%5F43692"
        :COUNTRY "Spain"
        :PPE-LEGISLATION-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=notification.html&ntf_id=314052&version_no=16"
        :CONFORMITY-ASSESSMENT-MODULES (B C2 D))
     #S(NOTIFIED-BODY
        :ID "1370"
        :NAME "BUREAU VERITAS ITALIA S.P.A."
        :DETAILS-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=search.nb&refe_cd=EPOS%5F50348"
        :COUNTRY "Italy"
        :PPE-LEGISLATION-URL NIL
        :CONFORMITY-ASSESSMENT-MODULES NIL)
     #S(NOTIFIED-BODY
        :ID "2370"
        :NAME "Handwerkskammer Dresden Zertifizierungsstelle der Handwerkskammer Dresden"
        :DETAILS-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=search.nb&refe_cd=NANDO%5FINPUT%5F202341"
        :COUNTRY "Germany"
        :PPE-LEGISLATION-URL NIL
        :CONFORMITY-ASSESSMENT-MODULES NIL))

Note: use NB-AUTHORIZED-P to filter out all the unauthorized bodies.

    > (remove-if-not #'nb-authorized-p *)
    (#S(NOTIFIED-BODY
        :ID "0370"
        :NAME "LGAI TECHNOLOGICAL CENTER, S. A./Applus"
        :DETAILS-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=search.nb&refe_cd=EPOS%5F43692"
        :COUNTRY "Spain"
        :PPE-LEGISLATION-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=notification.html&ntf_id=314052&version_no=16"
        :CONFORMITY-ASSESSMENT-MODULES (B C2 D)))

### Check if a notified body is authorized or not to certify face masks

    > (fmcv:nb-authorized-p "0370")
    #S(NOTIFIED-BODY
       :ID "0370"
       :NAME "LGAI TECHNOLOGICAL CENTER, S. A./Applus"
       :DETAILS-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=search.nb&refe_cd=EPOS%5F43692"
       :COUNTRY "Spain"
       :PPE-LEGISLATION-URL "https://ec.europa.eu/growth/tools-databases/nando/index.cfm?fuseaction=notification.html&ntf_id=314052&version_no=16"
       :CONFORMITY-ASSESSMENT-MODULES (B C2 D))
    > (fmcv:nb-authorized-p "1370")
    NIL

Note: you can feed to NB-AUTHORIZED-P the ID of the notified body, or an
instance of NOTIFIED-BODY, should you already have that handy.
