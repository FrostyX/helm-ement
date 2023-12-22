(defun helm-ement-directs ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "Ement directs"
          :candidates (helm-ement--directs)
          :action
          (lambda (direct)
            (ement-view-room direct (helm-ement--session))))))

(defun helm-ement-spaces ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "Ement spaces"
          :candidates (helm-ement--spaces)
          :action
          (lambda (space)
            (ement-view-space space (helm-ement--session))))))

(defun helm-ement-rooms ()
  (interactive)
  (helm :sources
        (helm-build-sync-source "Ement rooms"
          :candidates (helm-ement--rooms)
          :action
          (lambda (room)
            (ement-view-room room (helm-ement--session))))))

(defun helm-ement-buffers ()
  (interactive)
  (helm :sources
        (helm-make-source "Ement buffers" 'helm-source-buffers
          :buffer-list
          #'helm-ement--buffers)))

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
               (member major-mode '(ement-room-list-mode
                                    ement-room-mode
                                    ement-directory-mode))))
           (buffer-list))))
