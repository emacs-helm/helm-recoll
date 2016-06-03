;;; helm-recoll.el --- helm interface for the recoll desktop search tool. -*- lexical-binding: t -*-

;; Copyright (C) 2012 Thierry Volpiatto
;; Copyright (C) 2013-2016 Joe Bloggs, Michael Heerdegen and Cayetano Santos

;; Author:      Thierry Volpiatto <thierry.volpiatto at gmail.com>
;; Maintainers: Joe Bloggs <vapniks at yahoo.com>,
;;              Michael Heerdegen <michael_heerdegen at web.de>
;;              Cayetano Santos <cayetanosantos at gmail.com>

;; Filename: helm-recoll.el
;; Description: helm interface for the recoll desktop search tool.
;; URL: https://github.com/emacs-helm/helm-recoll
;; Keywords: convenience
;; Compatibility: GNU Emacs >= 24.3
;; Version: 0.4.1
;; Package-Requires: ((helm "1.9.5"))
;;
;; Features that might be required by this library:
;;
;; helm
;;

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Helm interface for the recoll desktop search tool.

;;; Use:
;;
;; You need to create some helm-recoll sources before you can use them.
;; You can create sources by customizing `helm-recoll-directories'.
;;
;; Then just call M-x helm-recoll; 

;;; Installation:
;;
;; After customizing `helm-recoll-directories' (see above)
;; just require helm-recoll or even better autoload the helm-recoll
;; function.
;; If you use use-package you can use e.g
;;
;;     (use-package helm-recoll
;;         :commands helm-recoll
;;         :init (setq helm-recoll-directories
;;                     '(("confdir" . "~/.emacs.d")
;;                       ("lisp sources" . "~/elisp")
;;                       ("work" . "~/work"))))

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'helm)
(require 'helm-files)

(defvar helm-recoll-options '("recoll" "-t" "-b")
  "A list where the `car' is the name of the recoll program followed by options.
You do not need to include the -c option since this is already included, and the
config directory can be passed as a argument to `helm-recoll-create-source'")

(defvar helm-recoll-sources-buffer "*helm recoll source select*")

(defvar helm-recoll-history nil
  "History of helm-recoll queries.")

;;; Help

(defvar helm-recoll-help-message
  "* Helm Recoll Help

** Helm Recoll Query Options

Enter one of the following options before your query to specify the query type:

-l      advanced search query (default, see next section)
-f      (exact) file name search query
-a      all words search query (matches if document contains all the words)
-o      any words search query (matches if document contains any of the words)

** Recoll Advanced Queries

Queries are sequences of terms with implicit AND and explicit OR and NOT (-)
logical operators. NOT gets priority over OR which gets priority over AND
(i.e. disjunctive normal form).

Terms can be either a word or double quoted phrase to search for in the
document, or a <FIELD>:<VALUE> pair as listed below. Wildcard (*/?/[]) and
anchor characters (^/$) can be used in words, phrases and field values, and
modifiers can be used for phrases (see below).

Example:

  ext:pdf -date:/2010 dir:local OR dir:share this OR that

is equivalent to

  ext:pdf AND (NOT date:/2010) AND (dir:local OR dir:share) AND (this OR that)

meaning search for

  pdf files BEFORE 2010 IN pathnames containing \"local\" or \"share\" with
  WORDS this or that IN

*** <FIELD>:<VALUE> pairs

title:      searching text in the document title or subject

author:     searching the documents originators

recipient:  searching the documents recipients

keyword:    searching document-specified keywords (few documents actually
							have any)
filename:   the document's file name

ext:        specifies the file name extension
            e.g.: ext:html

dir:        filtering on file location
            accepts negation, wildcards and tilde expansion; case sensitive

size:       filtering on file size
            use <,> or = and letters k/K, m/M, g/G, t/T as multipliers
            e.g (files between 100k and 1MB): size>100k size<1m

date:       for filtering on dates (but not times). General syntax is 2
            elements separated by a /. If either element is missing it is
            interpreted as the first/last date in the index. Each element can
            be a date in the form YYYY-MM-DD (the month or day may be missing)
            or a period in the form pNyNmNd where the N numbers are the
            respective numbers of years, months or days, any of which may be
            missing.
            e.g.:
            2001-03-01/p1y2m   the period covering 1 year and 2 months after
                                the 1st of March 2003
            2001/              from the beginning of 2001 up to now
            /2001              all dates up to 2001
            p2d/               from 2 days ago up to now

mime:       mime type
            values will be OR'ed by default except for negated terms which
            are AND'ed; you can use wildcards in the value (mime:text/*)
            e.g.: mime:application/* -mime:application/pdf

type:       category as defined in /usr/share/recoll/mimeconf
            (e.g. text/media/presentation/etc.).
            categories are OR'ed like mime types above, but can't be negated
            with -

*** Wildcards & Anchors

*           matches 0 or more characters
?           matches a single character
[]          match any character within the square brackets
^           match the beginning of the document text or field value
$           match the end of the document text or field value

Note: wildcards at the beginning of a word/phrase can slow recoll down a lot

*** Phrase Modifiers

Any of the following phrase modifiers may be placed at the end of a double
quoted phrase:

oN          allow upto N arbitrary words between words in phrase,
            e.g. ^\"test\"o5
            if N is not specified it defaults to 10
l           turn off stemming (mostly makes sense with p because stemming is
				      off by default for phrases)
p           turn default phrase search into a proximity one (unordered),
            e.g. \"order any in\"p
C           turn on case sensitivity
D           turn on diacritics sensitivity (if the index supports it)

** References

For more details see:

    http://www.lesbonscomptes.com/recoll/usermanual/RCL.SEARCH.LANG.html ")

;;; Keymap

(defvar helm-recoll-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-find-files-map)
    ;; Useless for help message, but maybe will be used later
    ;; (define-key map (kbd "C-c ?")    'helm-recoll-help)
    map)
  "Keymap used in recoll sources.")

;;; Actions

(defun helm-recoll-action-make-links (_candidate)
  "Make symlinks to the selected candidates."
  (let ((dir (ido-read-directory-name "Dir in which to place symlinks: ")))
    (dolist (item (helm-marked-candidates))
      (condition-case err
          (make-symbolic-link item (concat dir (file-name-nondirectory item)) 1)
        (error (message "%s" (error-message-string err)))))))

(defun helm-recoll-action-require-helm (_candidate)
  "Invoke helm with selected candidates."
  (helm :sources (helm-build-sync-source "Select"
		   :candidates (helm-marked-candidates)
		   :help-message helm-ff-help-message
		   :keymap helm-find-files-map
		   ;; :no-matchplugin t
		   :fuzzy-match t
		   :action helm-find-files-actions)))

;;; Main

(defun helm-recoll--candidates-process (&optional confdir)
  "Function used as candidates-process by `helm-recoll-source'."
  (setq confdir (or confdir (helm-attr 'confdir)))
  (let ((process-connection-type nil))
    (prog1 (apply #'start-process "recoll-process" helm-buffer
                  (append helm-recoll-options (list "-c" confdir) (split-string  helm-pattern " ")))
      (set-process-sentinel
       (get-process "recoll-process")
       (lambda (_process event)
         (if (string= event "finished\n")
             (with-helm-window
               (setq mode-line-format
                     '(" " mode-line-buffer-identification " "
                       (line-number-mode "%l") " "
                       (:eval (propertize
                               (format "[Recoll Process Finish- (%s results)]"
                                       (max (1- (count-lines
                                                 (point-min) (point-max)))
                                            0))
                               'face 'helm-grep-finish))))
               (force-mode-line-update))
           (helm-log "Error: Recoll %s"
                     (replace-regexp-in-string "\n" "" event))))))))

(defun helm-recoll-filter-one-by-one (file)
  "Function used as filter-one-by-one by `helm-recoll-source'."
  (replace-regexp-in-string "\\`file://" "" (if (consp file) (cdr file) file)))

(defun helm-recoll-source-action-transformer (&rest _)
  "Default action-transformer of the `helm-recoll-source' class."
  `(("Run helm with selected candidates" . helm-recoll-action-require-helm)
    ,@helm-type-file-actions
    ("Make link to file(s)" . helm-recoll-action-make-links)))

(defclass helm-recoll-source (helm-source-async)
  ((confdir :initarg :confdir
            :initform nil
            :custom 'file)
   (filter-one-by-one :initform #'helm-recoll-filter-one-by-one)
   (candidates-process :initform #'helm-recoll--candidates-process)
   (help-message :initform helm-recoll-help-message)
   (keymap :initform helm-recoll-map)
   (requires-pattern :initform 3)
   (history :initform helm-recoll-history)
   (candidate-number-limit :initform 9999)
   (nohighlight :initform t)
   (action-transformer :initform #'helm-recoll-source-action-transformer)))

(defun helm-recoll-build-sources (var value)
  (set var value)
  (cl-loop for (n . d) in value
           do (helm-recoll-create-source n d)))

(defun helm-recoll-create-source (name confdir)
  "Create helm source and associated functions for recoll search results.
The first argument NAME is a string.  Define a source variable
named `helm-source-recoll-NAME' and a command named
`helm-recoll-NAME'.  CONFDIR is the path to the config directory
which recoll should use."
  (let ((source  (intern (concat "helm-source-recoll-" name))))
    (defalias (intern (concat "helm-recoll-" name))
        (lambda ()
          (interactive)
          (require 'helm-recoll)
          (helm :sources source
                :keymap helm-recoll-map
                :history 'helm-recoll-history
                :buffer helm-recoll-sources-buffer)))
    (set source
         (helm-make-source (concat "Recoll " name)
             'helm-recoll-source :confdir (expand-file-name confdir)))
    (put source 'variable-documentation (format "\
Source for retrieving files matching the current input pattern
using recoll with the configuration in \"%s\"." confdir))))

(defcustom helm-recoll-directories nil
  "Alist of (name . directory) where to build a recoll source.
A command to call directly this source will be created also.
The source will be called helm-source-recoll-<name> and the command
helm-recoll-<name>."
  :group 'helm-recoll
  :type '(alist :key-type string :value-type string)
  :set 'helm-recoll-build-sources)

(defclass helm-recoll-sources (helm-source-sync)
  ((candidate-number-limit :initform 9999)
   (candidates :initform
               (lambda ()
                 (cl-loop for vname in (all-completions "helm-source-recoll-" obarray)
                          for var = (intern vname)
                          for name = (ignore-errors (assoc-default 'name (symbol-value var)))
                          if name collect (cons (format "%s (%s)" name vname) var))))
   (action :initform `(("Invoke helm with selected sources" .
                        ,(lambda (_candidate)
                           (helm :sources (helm-marked-candidates)
                                 :buffer helm-recoll-sources-buffer
                                 :keymap helm-recoll-map)))
                       ("Describe variable" . describe-variable)))
   (persistent-action :initform #'describe-variable)))

(defvar helm-recoll-sources-source
  (helm-make-source "helm-recoll sources" 'helm-recoll-sources)
  "Source for browsing all defined recoll sources.")

;;;###autoload
(defun helm-recoll nil
  "Select recoll sources for helm."
  (interactive)
  (helm :sources 'helm-recoll-sources-source
	:buffer helm-recoll-sources-buffer))

(provide 'helm-recoll)

;; Test:
;; (helm-recoll-create-source "main" "~/.recoll")
;; (helm :sources 'helm-source-recoll-main)

;;; helm-recoll.el ends here
