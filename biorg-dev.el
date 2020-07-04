;;; biorg-dev.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Anton Davydov
;;
;; Author: Anton Davydov <http://github/fetsorn>
;; Maintainer: Anton Davydov <fetsorn@gmail.com>
;; Created: July 03, 2020
;; Modified: July 03, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/fetsorn/biorg-dev.el
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defun om-fetsorn-headline-get-node-property (key headline)
  "Return value of property with KEY in HEADLINE or nil if not found.
If multiple properties with KEY are present, only return the first."
  (let ((node (->> (om-headline-get-node-properties headline)
                   (--first (equal key (om-get-property :key it))))))
    (if (not (eq node nil))
        (om-get-property :value node))))

(defun biorg-org-ravdi-to-sexp (ORGFILE)
  "Read org ravdia file ORGFILE, return associated list."
  (with-current-buffer (find-file-noselect ORGFILE)
    (let ((node (om-parse-subtree-at 0)))
      `(("p-comment"    . ,(om-to-string (om-fetsorn-headline-get-node-property "COMMENT"    node)))
        ("p-guest-date" . ,(om-to-string (om-fetsorn-headline-get-node-property "GUEST_DATE" node)))
        ("p-guest"      . ,(om-to-string (om-fetsorn-headline-get-node-property "GUEST"      node)))
        ("p-host-date"  . ,(om-to-string (om-fetsorn-headline-get-node-property "HOST_DATE"  node)))
        ("p-host"       . ,(om-to-string (om-fetsorn-headline-get-node-property "HOST"       node)))
        ("p-label"      . ,(om-to-string (om-fetsorn-headline-get-node-property "LABEL"      node)))
        ("p-module"     . ,(om-to-string (om-fetsorn-headline-get-node-property "MODULE"     node)))
        ("p-type"       . ,(om-to-string (om-fetsorn-headline-get-node-property "TYPE"       node)))
        ("uuid"         . ,(car (split-string (shell-command-to-string "uuidgen"))))
        ("entry"        . ,(concat (let ((section (->> (om-headline-get-section node)
                                                       (--remove (om-is-type 'property-drawer it)))))
                                     (if (not (eq nil section))
                                         (apply #'concat (-map #'om-to-string section))))
                                   (let ((subtrees (->> (om-headline-get-subheadlines node))))
                                     (if (not (eq nil subtrees))
                                         (apply #'concat (-map #'om-to-string subtrees))))))
        ))))

(defun biorg-sexp-to-storg (STORGASSOC)
  "Read associated list STORGASSOC, return encoded storg json."
  (json-encode `(("metadatum"
                  ,`("COMMENT"    . ,(cdr (assoc "p-comment"    STORGASSOC)))
                  ,`("GUEST_DATE" . ,(cdr (assoc "p-guest-date" STORGASSOC)))
                  ,`("GUEST"      . ,(cdr (assoc "p-guest"      STORGASSOC)))
                  ,`("HOST_DATE"  . ,(cdr (assoc "p-host-date"  STORGASSOC)))
                  ,`("HOST"       . ,(cdr (assoc "p-host"       STORGASSOC)))
                  ,`("LABEL"      . ,(cdr (assoc "p-label"      STORGASSOC)))
                  ,`("MODULE"     . ,(cdr (assoc "p-module"     STORGASSOC)))
                  ,`("TYPE"       . ,(cdr (assoc "p-type"       STORGASSOC))))
                 ("datum"
                  ,`("uuid"       . ,(cdr (assoc "uuid"         STORGASSOC)))
                  ,`("entry"      . ,(cdr (assoc "entry"        STORGASSOC)))
                  )
                 )))

(defun biorg-org-ravdia-to-storg (ORGDIR STORGDIR)
  "Read org ravdia files in ORGDIR, write storg json to STORGDIR."
  (-map (lambda (ORGFILE)
          (write-region
           (biorg-sexp-to-storg (biorg-org-ravdi-to-sexp ORGFILE))
           nil
           (concat STORGDIR "/" (cdr (assoc "uuid" (biorg-org-ravdi-to-sexp ORGFILE))) ".json") ))
        (directory-files ORGDIR t "^[^.]")))

(defun biorg-storg-to-sexp (JSONNAME)
  "Read storg file JSONNAME, output assoc list."
  (progn
    (require 'jeison)
    (jeison-defclass storg-node-class nil
                     ((p-comment    :initarg :p-comment    :path (metadatum COMMENT))
                      (p-guest-date :initarg :p-guest-date :path (metadatum GUEST_DATE))
                      (p-guest      :initarg :p-guest      :path (metadatum GUEST))
                      (p-host-date  :initarg :p-host-date  :path (metadatum HOST_DATE))
                      (p-host       :initarg :p-host       :path (metadatum HOST))
                      (p-label      :initarg :p-label      :path (metadatum LABEL))
                      (p-module     :initarg :p-module     :path (metadatum MODULE))
                      (p-type       :initarg :p-type       :path (metadatum TYPE))
                      (entry        :initarg :entry        :path (datum     entry))
                      (uuid         :initarg :uuid         :path (datum     uuid))))
    (let ((storg-node (jeison-read storg-node-class (json-read-file JSONNAME))))
      `(("p-comment"    . ,(oref storg-node p-comment))
        ("p-guest-date" . ,(oref storg-node p-guest-date))
        ("p-guest"      . ,(oref storg-node p-guest))
        ("p-host-date"  . ,(oref storg-node p-host-date))
        ("p-host"       . ,(oref storg-node p-host))
        ("p-label"      . ,(oref storg-node p-label))
        ("p-module"     . ,(oref storg-node p-module))
        ("p-type"       . ,(oref storg-node p-type))
        ("entry"        . ,(oref storg-node entry))
        ("uuid"         . ,(oref storg-node uuid))))))

(defun biorg-sexp-to-org (STORGASSOC)
  "Read STORGASSOC, output org string and uuid."
  (om-to-string
   (om-build-headline :title '(".")
                      (om-build-section
                       (om-build-property-drawer
                        (om-build-node-property "COMMENT"    (cdr (assoc "p-comment"    STORGASSOC)))
                        (om-build-node-property "GUEST_DATE" (cdr (assoc "p-guest-date" STORGASSOC)))
                        (om-build-node-property "GUEST"      (cdr (assoc "p-guest"      STORGASSOC)))
                        (om-build-node-property "HOST_DATE"  (cdr (assoc "p-host-date"  STORGASSOC)))
                        (om-build-node-property "HOST"       (cdr (assoc "p-host"       STORGASSOC)))
                        (om-build-node-property "LABEL"      (cdr (assoc "p-label"      STORGASSOC)))
                        (om-build-node-property "MODULE"     (cdr (assoc "p-module"     STORGASSOC)))
                        (om-build-node-property "TYPE"       (cdr (assoc "p-type"       STORGASSOC)))
                        (om-build-node-property "UUID"       (cdr (assoc "uuid"         STORGASSOC))))
                       (om-build-paragraph (cdr (assoc "entry" STORGASSOC)))))))

(defun biorg-storg-to-org (JSONNAME)
  "Read storg file JSONNAME, output assoc list of org string and uuid."
  (let ((storgassoc (biorg-storg-to-sexp JSONNAME)))
    `(("uuid" . ,(cdr (assoc "uuid" storgassoc)))
      ("node" . ,(biorg-sexp-to-org storgassoc)))))

(defun biorg-storg-to-org-ravdia (JSONDIR ORGDIR)
  "Read storg files in JSONDIR, output org files to ORGDIR."
  (-map
   (lambda (JSONNAME)
     (let ((node (cdr (assoc "node" (biorg-storg-to-org JSONNAME))))
           (uuid (cdr (assoc "uuid" (biorg-storg-to-org JSONNAME)))))
       (write-region node nil (concat ORGDIR "/" uuid ".org"))))
   (directory-files JSONDIR t "^[^.]"))) ; exclude "." and "..", gotta limit to .json files later

(defun biorg-storg-to-org-desmi (JSONDIR ORGNAME)
  "Read storg files in JSONDIR, output org file ORGNAME."
  (-map
   (lambda (JSONNAME)
     (let ((node (cdr (assoc "node" (biorg-storg-to-org JSONNAME)))))
       (write-region node nil ORGNAME t)))
   (directory-files JSONDIR t "^[^.]"))) ; exclude "." and "..", gotta limit to .json files later

(defun ensure-db (DBNAME)
  "Ensure DBNAME exists with proper table."
  (progn
   (require 'emacsql)
   (require 'emacsql-sqlite3)
   (defvar db (emacsql-sqlite3 DBNAME))
   (emacsql db [:create-table nodes
              ([(entry text) (date text) (uuid text)])])
  ))

; biorg-storg-to-sexp is defined in storg-to-org part above

(defun biorg-sexp-to-sql (STORGASSOC DBNAME)
  "Read associated list STORGASSOC, write to sqlite DBNAME."
  (progn
    (require 'emacsql)
    (require 'emacsql-sqlite3)
    (defvar db (emacsql-sqlite3 DBNAME))
    (let ((date       (cdr (assoc "p-guest-date" STORGASSOC)))
          (uuid       (cdr (assoc "uuid"        STORGASSOC)))
          (entry (->> (cdr (assoc "entry"       STORGASSOC))
                      (format "%S") ; format to escape quotes
                      (s-chop-prefix "\"")
                      (s-chop-suffix "\"")
                      (s-truncate 50)
                      (s-replace "\n" "\\n")
                      (s-replace "" " "))) )
      (emacsql db [:insert :into nodes
                           :values ([$s1 $s2 $s3])]
               entry date uuid))))


(defun biorg-storg-to-sql (STORGDIR DBNAME)
  "Read json storg files in STORGDIR, write to sqlite DBNAME."
  (-map (lambda (STORGNAME)
          (biorg-sexp-to-sql (biorg-storg-to-sexp STORGNAME) DBNAME))
        (directory-files STORGDIR t "^[^.]")))

(defun biorg-sql-to-sexp (DBNAME)
  "Read database DBNAME into a list of associated lists."
  (progn
    (require 'emacsql)
    (require 'emacsql-sqlite3)
    (defvar db (emacsql-sqlite3 DBNAME))
    (setq nodelist (emacsql db [:select :distinct [date entry uuid]
                                :from nodes
                                :order-by [(asc date), (asc entry)]]))
                                ;:where (like date '"%2015%")
                                ;:limit 3160
    (-map
     (lambda (node) `(("date"  . ,(nth 0 node))
                      ("entry" . ,(nth 1 node))
                      ("uuid"  . ,(nth 2 node))))
     nodelist)))

(defun biorg-sexp-to-dot--ranks (NODELIST)
  "Read NODELIST, return dot notation of ranks."
  (concat "/* date <-> versions ranks */\n"
          "/* {rank=same; \"1995-05-01\" \"Postgres95-0.01\"} */\n"
          (mapconcat
           (lambda (node) (concat "{rank=same; "
                                  "\"" (cdr (assoc "date" node)) "\""
                                  "\"" (number-to-string (-elem-index node NODELIST)) "\""
                                  "}"))
           NODELIST "\n")
          "\n"))

(defun biorg-sexp-to-dot--dates (NODELIST)
  "Read NODELIST, return dot notation of dates."
  (concat "/* Dates relations */\n"
          "/*\"<1995-01-01>\" -> */\n"
          (mapconcat
           ; add dot relations and newlines between dates
           (lambda (date-chunk)
             (--reduce (concat acc it)
                       (-interpose " -> \n"
                                   (-map (lambda (date) (concat "\"" date "\""))
                                         date-chunk))))
           (let ((nodelist-dates-split (-partition-all 2400 ; split list of dates every 2400 items 'cause gv breaks at 2500
                                                       ; filter and deduplicate dates from NODELIST
                                                       (delq nil (delete-dups (-map (lambda (node) (cdr (assoc "date" node))) NODELIST))))))
             (-map-indexed (lambda (index item) ; prepend last date of each 2400 chunk to the next chunk for dot relations
                             (if (> index 0)
                                 (cons
                                  (nth 0 (-take-last 1 (nth (1- index) nodelist-dates-split)))
                                  item)
                              item))
                           nodelist-dates-split))
           "\n")
          "\n"))

(defun biorg-sexp-to-dot--entries (NODELIST)
  "Read NODELIST, returns dot notation of entries."
  (concat "/* Versions relations */\n"
          "/*\"1\" [label=\"Postgres95-0.03\"] ;*/\n"
          (mapconcat
           (lambda (node) (concat "\n"
                               "\""(number-to-string (-elem-index node NODELIST))"\""
                               " ["
                               "label="   "\"*\""                                              ","
                               "tooltip=" "\"" (cdr (assoc "entry" node)) "\""                 ","
                               "href="    "\"" "ravdia/" (cdr (assoc "uuid" node)) ".org" "\"" ","
                               "target="  "\"_blank\""
                               "];"))
           NODELIST
           "")
          "\n"))

(defun biorg-sexp-to-dot (NODELIST)
  "Read NODELIST, return dot notation."
  (concat "digraph storg_timeline {\n"
          "imap=yes;\n"
          "node [shape=\"plain\"];"
          "rankdir=TB;\n"
          (biorg-sexp-to-dot--ranks   NODELIST)
          (biorg-sexp-to-dot--dates   NODELIST)
          (biorg-sexp-to-dot--entries NODELIST)
          "}\n"))

(defun biorg-sql-to-gv (DBNAME GVNAME)
  "Read DBNAME, graph dot notation to GVNAME."
  (write-region (biorg-sexp-to-dot (biorg-sql-to-sexp DBNAME)) nil GVNAME))

(provide 'biorg-dev)
;;; biorg-dev.el ends here
