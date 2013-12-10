;;; helm-recoll.el --- helm interface for the recoll desktop search tool.

;; Filename: helm-recoll.el
;; Description: helm interface for the recoll desktop search tool.
;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com> and Michael Heerdegen
;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2013 Joe Bloggs <vapniks@yahoo.com> and Michael Heerdegen
;; Version: 20130905.2330
;; X-Original-Version: 1.1
;; Last-Updated: 2013-09-05 23:30:00
;;           By: Joe Bloggs
;; URL: https://github.com/emacs-helm/helm-recoll
;; Keywords: convenience
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires: ((helm "1.5.4"))
;;
;; Features that might be required by this library:
;;
;; helm
;;

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
;;  ===========
;;
;; You need to create some helm-recoll sources before you can use them.
;; You can create sources using the `helm-recoll-create-source' function,
;; e.g. like this:
;;  (helm-recoll-create-source "docs" "~/.recoll/docs")
;;  (helm-recoll-create-source "progs" "~/.recoll/progs")
;;
;; Press C-c ? in the helm buffer to see information about how to query recoll

;; Then you can use the sources in helm like this: (helm :sources '(helm-source-recoll-docs helm-source-recoll-progs))

;;; Installation:
;;  =============
;;
;; Add code to your init (~/.emacs) file to create some sources (see above),
;; and then add a require statement for the library: (require 'helm-recoll)

;;; Code:

(require 'helm)

(defvar helm-recoll-options
  '("recoll" "-t" "-b")
  "A list where the `car' is the name of the recoll program followed by options.
You do not need to include the -c option since this is already included, and the config directory
can be passed as a argument to `helm-recoll-create-source'")

(defvar helm-recoll-sources-buffer "*helm recoll source select*")

(defvar helm-recoll-history nil
  "History of helm-recoll queries")

(defvar helm-recoll-sources-source
  `((name . "helm-recoll sources")
    (candidate-number-limit . 9999)
    (candidates
     . (lambda nil
         (loop for vname in (all-completions "helm-source-recoll-" obarray)
               for var = (intern vname)
               for name = (ignore-errors (assoc-default 'name (symbol-value var)))
               if name collect (cons (format "%s (%s)" name vname) var))))
    (action . (("Invoke helm with selected sources" .
                (lambda (candidate)
                  (helm :sources (helm-marked-candidates)
                        :buffer helm-recoll-sources-buffer
                        :keymap helm-recoll-map)))
               ("Describe variable" . describe-variable)))
    (persistent-action . describe-variable)))

;; http://www.lesbonscomptes.com/recoll/usermanual/RCL.SEARCH.LANG.html
(defvar helm-recoll-help-message
  "=== Helm Recoll Query Options ===

Enter one of the following options before your query to specify the query type:

-l           advanced search query (default, see next section)
-f           file name search query
-a           all words search query (matches if document contains all the words)
-o           any words search query (matches if document contains any of the words)


=== Helm Recoll Advanced Queries ===

Queries are sequences of terms with implicit AND and explicit OR and NOT (-) logical operators.
NOT gets priority over OR which gets priority over AND (i.e. disjunctive normal form).
Terms can be either a word or double quoted phrase to search for in the document, or a <FIELD>:<VALUE> pair
as listed below. Wildcard (*/?/[]) and anchor characters (^/$) can be used in words, phrases and field
values, and modifiers can be used for phrases (see below).

For example:        ext:pdf -date:/2010 dir:local OR dir:share this OR that
is equivalent to:   ext:pdf AND (NOT date:/2010) AND (dir:local OR dir:share) AND (this OR that)
 (search for words this or that in pdf files before 2010 with pathnames containing \"local\" or \"share\")

== <FIELD>:<VALUE> pairs ==

title:       for searching text in the document title or subject.
author:      for searching the documents originators.
recipient:   for searching the documents recipients.
keyword:     for searching document-specified keywords (few documents actually have any).
filename:    for the document's file name.
ext:         specifies the file name extension (Ex: ext:html)
dir:         for filtering on file location. Accepts negation, wildcards and tilde expansion. Case sensitive.
size:        for filtering on file size. Use <,> or = as operators and letters k/K, m/M, g/G, t/T as multipliers.
             e.g: size>100k size<1m (files between 100k and 1MB)
date:        for filtering on dates (but not times). General syntax is 2 elements separated by a /. If either element
             is missing it is interpreted as the first/last date in the index. Each element can be a date in the form
             YYYY-MM-DD (the month or day may be missing) or a period in the form pNyNmNd where the N numbers are the
             respective numbers of years, months or days, any of which may be missing.
             Examples: 2001-03-01/p1y2m   the period covering 1 year and 2 months after the 1st of March 2003
                       2001/              from the beginning of 2001 up to now
                       /2001              all dates up to 2001
                       p2d/               from 2 days ago up to now
mime:        for specifying the mime type. Values will be OR'ed by default except for negated terms which are AND'ed.
             You can use wildcards in the value (mime:text/*). Example: mime:application/* -mime:application/pdf 
type:        for specifying the category as defined in /usr/share/recoll/mimeconf (e.g. text/media/presentation/etc.).
             Categories are OR'ed like mime types above, but can't be negated with -

== Wildcards & Anchors ==

 *           matches 0 or more characters
 ?           matches a single character
 []          match any character within the square brackets
 ^           match the beginning of the document text or field value
 $           match the end of the document text or field value 

Note: using wildcards at the beginning of a word/phrase can slow recoll down a lot.

== Phrase Modifiers ==

Any of the following phrase modifiers may be placed at the end of a double quoted phrase:

 oN          allow upto N arbitrary words between words in phrase, e.g. ^\"test\"o5
             if N is not specified it defaults to 10
 l           turn off stemming (mostly makes sense with p because stemming is off by default for phrases)
 p           turn default phrase search into a proximity one (unordered), e.g. \"order any in\"p
 C           turn on case sensitivity
 D           turn on diacritics sensitivity (if the index supports it)


For more details see: http://www.lesbonscomptes.com/recoll/usermanual/RCL.SEARCH.LANG.html
")

(defvar helm-recoll-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c ?")    'helm-recoll-help)
    (delq nil map))
  "Keymap used in recoll sources.")

(defun helm-recoll-help ()
  (interactive)
  (let ((helm-help-message helm-recoll-help-message))
    (helm-help)))

;;;###autoload
(defun helm-recoll-create-source (name confdir)
  "Function to create helm source and associated functions for recoll search results.
A source variable named `helm-source-recoll-NAME' and a command named `helm-recoll-NAME'
where NAME is the first arg to the function will be created.
Also an init function named `helm-recoll-init-NAME' will be created.
The CONFDIR arg should be a string indicating the path to the config directory which recoll should use."
  (require 'helm-mode)
  (let ((initfunc (intern (concat "helm-recoll-init-" name)))
        (source (intern (concat "helm-source-recoll-" name)))
        (command (intern (concat "helm-recoll-" name))))
    (eval
     `(defun ,initfunc nil
        (let ((process-connection-type nil))
          (prog1
              (start-process-shell-command
               "recoll-process" helm-buffer
               (mapconcat #'identity (append helm-recoll-options
                                             (list "-c" ,confdir (shell-quote-argument helm-pattern)))
                          " "))
            (set-process-sentinel
             (get-process "recoll-process")
             (lambda (process event)
               (if (string= event "finished\n")
                   (with-helm-window
                     (setq mode-line-format
                           '(" " mode-line-buffer-identification " "
                             (line-number-mode "%l") " "
                             (:eval (propertize
                                     (format "[Recoll Process Finish- (%s results)]"
                                             (max (1- (count-lines
                                                       (point-min) (point-max))) 0))
                                     'face 'helm-grep-finish))))
                     (force-mode-line-update))
                 (helm-log "Error: Recoll %s"
                           (replace-regexp-in-string "\n" "" event)))))))))
    (eval
     `(setq ,source
        '((name . ,(concat "Recoll " name " (press C-c ? for query help)"))
          (candidates-process . ,initfunc)
          (candidate-transformer
           . (lambda (cs)
               (mapcar (function (lambda (c)
                                   (replace-regexp-in-string "file://" "" c)))
                       cs)))
          ;(type . file)
	  (action
	   ("Find File"   . find-file)
	   ("Print path" . (lambda (candidate) (message "%s" candidate))))
          (keymap . ,helm-recoll-map)
          (no-matchplugin)
          (requires-pattern . 3)
          (delayed)
          (history . ,'helm-recoll-history)
          (candidate-number-limit . 9999)
          (nohighlight))))
    (eval `(helm-add-action-to-source "Make link to file(s)" 'helm-recoll-make-links ,source))
    (eval `(defun ,command nil
             ,(concat "Search " name " recoll database")
             (interactive)
             (helm :sources ',source
                   :keymap helm-recoll-map
                   :history 'helm-recoll-history
                   :buffer helm-recoll-sources-buffer)))))

(defun helm-recoll-make-links (candidate)
  "Make symlinks to the selected candidates."
  (let ((dir (ido-read-directory-name "Dir in which to place symlinks: ")))
    (dolist (item (helm-marked-candidates))
      (condition-case err
          (make-symbolic-link item (concat dir (file-name-nondirectory item)) 1)
        (error (message "%s" (error-message-string err)))))))

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


