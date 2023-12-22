;;; helm-ement.el --- Helm interace for Ement   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Jakub Kadlčík

;; Author: Jakub Kadlčík <frostyx@email.cz>
;; URL: https://github.com/FrostyX/helm-ement
;; Version: 1.0
;; Package-Requires: ((emacs "26.3") (ement "0.14"))
;; Keywords: helm, ement

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Helm interface for the Ement matrix client


;;; Code:

;;;; Commands

;;;###autoload
(defun helm-ement-directs ()
  "Show all Ement direct chat rooms"
  (interactive)
  (helm :sources
        (helm-make-source "Ement directs" 'helm-source-sync
          :candidates (helm-ement--directs)
          :action
          (lambda (direct)
            (ement-view-room direct (helm-ement--session))))))

;;;###autoload
(defun helm-ement-spaces ()
  "Show all Ement spaces"
  (interactive)
  (helm :sources
        (helm-make-source "Ement spaces" 'helm-source-sync
          :candidates (helm-ement--spaces)
          :action
          (lambda (space)
            (ement-view-space space (helm-ement--session))))))

;;;###autoload
(defun helm-ement-rooms ()
  "Show all Ement rooms, excluding direct chat rooms"
  (interactive)
  (helm :sources
        (helm-make-source "Ement rooms" 'helm-source-sync
          :candidates (helm-ement--rooms)
          :action
          (lambda (room)
            (ement-view-room room (helm-ement--session))))))

;;;###autoload
(defun helm-ement-buffers ()
  "Show all Ement buffers"
  (interactive)
  (helm :sources
        (helm-make-source "Ement buffers" 'helm-source-buffers
          :buffer-list
          #'helm-ement--buffers)))

;;;; Functions

;;;;; Private

(defun helm-ement--session ()
  (if (length> ement-sessions 0)
      (cdr (first ement-sessions))
    (error "Ement not running. Start it with `M-x ement-connect'")))

(defun helm-ement--everything ()
  (slot-value (helm-ement--session) 'rooms))

(defun helm-ement--directs ()
  (cl-loop for item in (helm-ement--everything)
           when (ement--room-direct-p item (helm-ement--session))
           collect (cons (slot-value item 'display-name) item)))

(defun helm-ement--spaces ()
  (cl-loop for item in (helm-ement--everything)
           when (ement--space-p item)
           collect (cons (slot-value item 'display-name) item)))

(defun helm-ement--rooms ()
  (cl-loop for item in (helm-ement--everything)
           when (not (ement--room-direct-p item (helm-ement--session)))
           when (not (ement--space-p item))
           collect (cons (slot-value item 'display-name) item)))

(defun helm-ement--buffers ()
  (mapcar #'buffer-name
          (cl-remove-if-not
           (lambda (buf)
             (with-current-buffer buf
               (derived-mode-p 'ement-room-list-mode
                               'ement-room-mode
                               'ement-directory-mode)))
           (buffer-list))))

;;;; Footer

(provide 'helm-ement)

;;; helm-ement.el ends here
