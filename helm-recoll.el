;;; helm-recoll.el --- helm interface for the recoll desktop search tool.

;; Filename: helm-recoll.el
;; Description: helm interface for the recoll desktop search tool.
;; Author: Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com> and Michael Heerdegen
;; Copyright (C) 2012 Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2013 Joe Bloggs <vapniks@yahoo.com> and Michael Heerdegen
;; Version: 20130830.2030
;; X-Original-Version: 1.1
;; Last-Updated: 2013-08-30 14:42:00
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/helm/blob/master/helm-recoll.el
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
                  (helm :sources (helm-marked-candidates) :buffer helm-recoll-sources-buffer)))
               ("Describe variable" . describe-variable)))
    (persistent-action . describe-variable)))

;;;###autoload
(defun helm-recoll-create-source (name confdir)
  "Function to create helm source for recoll search results.
The source variable will be named `helm-source-recoll-NAME' where NAME is the first arg to the function
 (and should be a valid symbol name - i.e. no spaces).
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
                                             (list "-c" ,confdir helm-pattern))
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
     `(defvar ,source
        '((name . ,(concat "Recoll " name))
          (candidates-process . ,initfunc)
          (candidate-transformer
           . (lambda (cs)
               (mapcar (function (lambda (c)
                                   (replace-regexp-in-string "file://" "" c)))
                       cs)))
          (type . file)
          (no-matchplugin)
          (requires-pattern . 3)
          (delayed)
          (candidate-number-limit . 9999)
          (nohighlight))
        ,(concat "Source for retrieving files matching the current input pattern, using recoll with the configuration in "
                 confdir)))
    (eval `(helm-add-action-to-source "Make link to file(s)" 'helm-recoll-make-links ,source))
    (eval `(defun ,command nil (interactive) (helm ',source)))))

(defun helm-recoll-make-links (candidate)
  "Make symlinks to the selected candidates."
  (let ((dir (ido-read-directory-name "Dir in which to place symlinks: ")))
    (dolist (item (helm-marked-candidates))
      (condition-case err
          (make-symbolic-link item (concat dir (file-name-nondirectory item)) 1)
        (error (message "%s" (error-message-string err)))))))

;;;###autoload
(defun helm-recoll nil
  "Call helm  source."
  (interactive)
  (helm 'helm-recoll-sources-source nil nil nil nil helm-recoll-sources-buffer))

(provide 'helm-recoll)

;; Test:
;; (helm-recoll-create-source "main" "~/.recoll")
;; (helm :sources 'helm-source-recoll-main)


;;; helm-recoll.el ends here

