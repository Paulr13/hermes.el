;;; hermes-subagents.el --- Live subagent status buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Paul Richter
;; Author: Paul Richter <paul.riley.richter@gmail.com>
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Small dedicated buffer that auto-opens when a subagent spawns and
;; updates live on `subagent.*' events.  Data source is the canonical
;; session state (hermes-state.el reduces `subagent.spawn_requested',
;; `.start', `.thinking', `.tool', `.progress', `.complete' into a
;; vector of `hermes-subagent' on `hermes-state-stream-subagents').
;;
;; `hermes-subagents--format' is a pure subagent-vector -> string
;; renderer; everything else (buffer, window, hook) is thin plumbing.
;; The buffer auto-opens on spawn/start transitions
;; (`hermes-subagents-auto-open') and re-renders only when the
;; serialized subagent list actually changed (fingerprint check).

;;; Code:

(require 'cl-lib)
(require 'hermes-state)

(defgroup hermes-subagents nil
  "Live subagent status buffer for the Hermes Emacs client."
  :group 'hermes)

(defcustom hermes-subagents-auto-open t
  "When non-nil, pop the subagent buffer when a subagent spawns or starts."
  :type 'boolean
  :group 'hermes-subagents)

(defcustom hermes-subagents-window-height 14
  "Height of the auto-opened subagent status window, in lines."
  :type 'natnum
  :group 'hermes-subagents)

(defcustom hermes-subagents-max-tools 6
  "Show at most this many of a subagent's most recent tool calls."
  :type 'natnum
  :group 'hermes-subagents)

(defcustom hermes-subagents-max-notes 3
  "Show at most this many of a subagent's most recent progress notes."
  :type 'natnum
  :group 'hermes-subagents)

(defcustom hermes-subagents-thinking-tail 160
  "Show at most this many trailing characters of a subagent's thinking."
  :type 'natnum
  :group 'hermes-subagents)

(defconst hermes-subagents-buffer-name "*hermes-subagents*"
  "Buffer showing live subagent status.")

(defvar hermes-subagents--last-sid nil
  "Session id of the most recent subagent activity.
Used to render the buffer when opened manually.")

(defvar hermes-subagents--fingerprint nil
  "Serialized (sid plist-list) of the last rendered subagent state.
Guards against re-rendering when nothing changed.")

;;;; Pure formatting core (batch-testable — no buffers, no windows)

(defun hermes-subagents--status-icon (status)
  "One-character icon for subagent STATUS keyword."
  (pcase status
    ('queued   "◇")
    ('running  "⏺")
    ('complete "✔")
    ('error    "✖")
    (_         "·")))

(defun hermes-subagents--fmt-duration (sec)
  "Format duration in seconds SEC as \"42s\", \"2m03s\" or \"1h05m\".
SEC may be nil (returns empty string) or non-numeric (ignored)."
  (cond
   ((not (numberp sec)) "")
   ((< sec 60)  (format "%ds" (truncate sec)))
   ((< sec 3600) (format "%dm%02ds" (truncate (/ sec 60))
                         (% (truncate sec) 60)))
   (t (format "%dh%02dm" (truncate (/ sec 3600))
              (% (truncate (/ sec 60)) 60)))))

(defun hermes-subagents--oneline (s &optional maxlen)
  "Collapse S to one line, truncated to MAXLEN (default 72) with ellipsis."
  (setq maxlen (or maxlen 72))
  (let ((text (and s (replace-regexp-in-string "[\r\n]+" " " (format "%s" s)))))
    (when (and text (> (length text) maxlen))
      (setq text (concat (substring text 0 (max 0 (- maxlen 1))) "…")))
    text))

(defun hermes-subagents--args-abbrev (args &optional maxlen)
  "One-line abbreviation of tool-call ARGS (hash-table or string)."
  (setq maxlen (or maxlen 48))
  (cond
   ((null args) "")
   ((hash-table-p args)
    (let ((pick (or (gethash "command" args) (gethash "path" args)
                    (gethash "url" args) (gethash "query" args)
                    (gethash "pattern" args))))
      (if pick
          (hermes-subagents--oneline pick 48)
        ;; No recognizable key: show the first pair.
        (let (kv)
          (maphash (lambda (k v) (unless kv (setq kv (format "%s=%s" k v)))) args)
          (hermes-subagents--oneline (or kv "...") 48)))))
   (t (hermes-subagents--oneline args 48))))

(defun hermes-subagents--format-subagent (sa)
  "Format one `hermes-subagent' SA into a small block of lines."
  (let* ((status  (hermes-subagent-status sa))
         (icon    (hermes-subagents--status-icon status))
         (label   (upcase (format "%s" status)))
         (dur     (hermes-subagents--fmt-duration (hermes-subagent-duration sa)))
         (id      (or (hermes-subagent-id sa) "?"))
         (goal    (hermes-subagents--oneline (hermes-subagent-goal sa) 76))
         (tools   (append (hermes-subagent-tools sa) nil))
         (notes   (append (hermes-subagent-notes sa) nil))
         (thinking (hermes-subagent-thinking sa))
         (summary  (hermes-subagent-summary sa))
         (lines ()))
    (push (propertize
           (format "%s %s · %s%s" icon label id
                   (if (equal dur "") "" (concat " · " dur)))
           'hermes-subagent-id id
           'mouse-face 'highlight
           'help-echo "RET: open detail buffer")
          lines)
    (when goal (push (format "  Goal: %s" goal) lines))
    (let ((shown (last tools (min (length tools) hermes-subagents-max-tools))))
      (dolist (tool shown)
        (push (format "  ├ %s(%s)"
                      (or (plist-get tool :name) "?")
                      (hermes-subagents--args-abbrev (plist-get tool :args)))
              lines)))
    (let ((shown (last notes (min (length notes) hermes-subagents-max-notes))))
      (dolist (note shown)
        (push (format "  ├ %s" (hermes-subagents--oneline note 76)) lines)))
    (when (and thinking (not (string-empty-p thinking))
               (not (eq status 'complete)) (not (eq status 'error)))
      (push (format "  ⋯ %s"
                    (hermes-subagents--oneline
                     (let* ((len (length thinking))
                            (start (max 0 (- len hermes-subagents-thinking-tail))))
                       (substring thinking start)))
                    76)
            lines))
    (when summary
      (push (format "  → %s" (hermes-subagents--oneline summary 110)) lines))
    (nreverse lines)))

(defun hermes-subagents--format (subagents)
  "Render SUBAGENTS (vector of `hermes-subagent') to a string.
Empty/missing vector renders as a single placeholder line."
  (if (or (null subagents) (zerop (length subagents)))
      "No subagents this session.\n"
    (let ((blocks (mapcar #'hermes-subagents--format-subagent
                          (append subagents nil))))
      (concat (mapconcat
               (lambda (block) (mapconcat #'identity block "\n"))
               blocks "\n")
              "\n"))))

;;;; Detail buffer (one per subagent)

(defcustom hermes-subagents-detail-thinking-tail 4000
  "Show at most this many trailing characters of thinking in the detail buffer."
  :type 'natnum
  :group 'hermes-subagents)

(defconst hermes-subagents-detail-buffer-format "*hermes-subagent:%s*"
  "Format for a per-subagent detail buffer name (single %s = subagent id).")

(defvar-local hermes-subagents--sid nil
  "Session id whose subagents this buffer renders.")

(defun hermes-subagents--detail-buffer-name (id)
  (format hermes-subagents-detail-buffer-format id))

(defun hermes-subagents--format-detail (sa)
  "Format one `hermes-subagent' SA into full detail lines.
Unlike the overview block, this shows the whole goal, all tools,
all notes, and a large thinking tail."
  (let* ((status (hermes-subagent-status sa))
         (lines (list
                 (format "%s %s%s"
                         (hermes-subagents--status-icon status)
                         (upcase (format "%s" status))
                         (let ((d (hermes-subagents--fmt-duration
                                   (hermes-subagent-duration sa))))
                           (if (equal d "") "" (concat " · " d)))))))
    (push (concat "ID: " (or (hermes-subagent-id sa) "?")) lines)
    (push (concat "Goal: " (or (hermes-subagent-goal sa) "")) lines)
    (push "" lines)
    (let ((tools (append (hermes-subagent-tools sa) nil)))
      (if tools
          (progn
            (push (format "Tools (%d):" (length tools)) lines)
            (dolist (tool tools)
              (push (format "  %s(%s)"
                            (or (plist-get tool :name) "?")
                            (hermes-subagents--args-abbrev
                             (plist-get tool :args) 120))
                    lines)))
        (push "Tools: none yet" lines)))
    (push "" lines)
    (let ((notes (append (hermes-subagent-notes sa) nil)))
      (if notes
          (progn
            (push (format "Progress (%d):" (length notes)) lines)
            (dolist (note notes)
              (push (concat "  " (hermes-subagents--oneline note 100)) lines)))
        (push "Progress: no notes yet" lines)))
    (push "" lines)
    (when (and (hermes-subagent-thinking sa)
               (not (string-empty-p (hermes-subagent-thinking sa))))
      (let ((th (hermes-subagent-thinking sa)))
        (push (format "Thinking (last %d chars):" (length th)) lines)
        (let* ((start (max 0 (- (length th) hermes-subagents-detail-thinking-tail))))
          (dolist (l (split-string (substring th start) "\n" t))
            (push (concat "  " l) lines)))))
    (when (hermes-subagent-summary sa)
      (push (concat "Summary: " (hermes-subagent-summary sa)) lines))
    (nreverse lines)))

(defun hermes-subagents--visible-subagents (state)
  "Subagents to display for STATE.
Live stream first; when the stream is gone (post-commit), fall back to
the most recent committed turn's copy, where subagents keep living."
  (let ((stream (hermes-state-stream state)))
    (or (and stream (hermes-stream-subagents stream))
        (let ((turns (hermes-state-turns state))
              (found nil))
          (cl-loop
           for i downfrom (1- (length turns)) to 0
           while (null found)
           do (let ((sas (hermes-message-subagents (aref turns i))))
                (when (and sas (> (length sas) 0))
                  (setq found sas))))
          found))))

(defun hermes-subagents--collect-turn-sas (state)
  "All subagents in STATE's committed turns, newest turn first."
  (let ((turns (hermes-state-turns state)))
    (when (> (length turns) 0)
      (apply #'append
             (mapcar
              (lambda (m)
                (append (or (hermes-message-subagents m) []) nil))
              (nreverse (append turns nil)))))))

(defun hermes-subagents--find (id)
  "Return (SA . SID) for subagent ID, searching all known sessions.
Live streams first, then committed turns (subagents keep living there
after the turn commits)."
  (let (found)
    (maphash
     (lambda (sid state)
       (let* ((stream (hermes-state-stream state))
              (stream-sas (and stream
                               (append
                                (or (hermes-stream-subagents stream) [])
                                nil)))
              (hit (and (not found)
                        (cl-find-if
                         (lambda (sa)
                           (equal id (hermes-subagent-id sa)))
                         stream-sas))))
         (when hit
           (setq found (cons hit sid)))))
     hermes--sessions)
    (unless found
      (maphash
       (lambda (sid state)
         (let* ((turn-sas (hermes-subagents--collect-turn-sas state))
                (hit (and (not found)
                          (cl-find-if
                           (lambda (sa)
                             (equal id (hermes-subagent-id sa)))
                           turn-sas))))
           (when hit
             (setq found (cons hit sid)))))
       hermes--sessions))
    found))

(defun hermes-subagents--render-detail (id sa)
  "Re-render the detail buffer for subagent ID with SA, if it exists."
  (let ((buf (get-buffer (hermes-subagents--detail-buffer-name id))))
    (when buf
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (mapconcat #'identity
                             (hermes-subagents--format-detail sa) "\n"))
          (goto-char (point-min)))))))

(defun hermes-subagents-detail-open (id)
  "Open or show the detail buffer for subagent ID, live-updating."
  (interactive
   (list (completing-read "Subagent id: "
                          (let (ids)
                            (maphash
                             (lambda (_ state)
                               (dolist (sa (append
                                            (or (hermes-subagents--visible-subagents
                                                 state) []) nil))
                                 (cl-pushnew (hermes-subagent-id sa) ids
                                             :test #'equal)))
                             hermes--sessions)
                            (nreverse ids)))))
  (pcase-let ((`(,sa . ,sid) (hermes-subagents--find id)))
    (unless sa (user-error "No subagent %s known" id))
    (let ((buf (get-buffer-create (hermes-subagents--detail-buffer-name id))))
      (with-current-buffer buf
        (setq hermes-subagents--sid sid)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (mapconcat #'identity
                             (hermes-subagents--format-detail sa) "\n"))
          (goto-char (point-min)))
        (setq buffer-read-only t))
      (display-buffer buf `(display-buffer-below-selected
                            (window-height . ,hermes-subagents-window-height)
                            (inhibit-same-window . t)))
      (hermes-subagents--render-detail id sa)
      buf)))

(defun hermes-subagents--render-details (subagents)
  "Re-render every open detail buffer matching SUBAGENTS."
  (dolist (sa (append (or subagents []) nil))
    (when (get-buffer (hermes-subagents--detail-buffer-name
                       (hermes-subagent-id sa)))
      (hermes-subagents--render-detail (hermes-subagent-id sa) sa))))

;;;; Change detection + buffer plumbing

(defun hermes-subagents--serialize (subagents)
  "Return the plists of SUBAGENTS for fingerprinting."
  (mapcar #'hermes--subagent-to-plist (append subagents nil)))

(defun hermes-subagents--spawned-p (old-sas new-sas)
  "Non-nil when NEW-SAS shows a fresh spawn or queued→running start."
  (let ((old-list (append (or old-sas []) nil))
        (new-list (append (or new-sas []) nil)))
    (or (/= (length old-list) (length new-list))
        (cl-some (lambda (o n)
                   (and (eq (hermes-subagent-status o) 'queued)
                        (eq (hermes-subagent-status n) 'running)))
                 old-list new-list))))

(defun hermes-subagents--buffer ()
  (get-buffer-create hermes-subagents-buffer-name))

(defvar hermes-subagents--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'hermes-subagents-open-at-point)
    (define-key map (kbd "q") #'hermes-subagents-toggle)
    map)
  "Keymap for the subagent overview buffer.")

(defun hermes-subagents-open-at-point ()
  "Open the detail buffer for the subagent on the current line."
  (interactive)
  (let ((id (get-char-property (point) 'hermes-subagent-id)))
    (unless id
      (save-excursion
        (beginning-of-line)
        (setq id (get-char-property (point) 'hermes-subagent-id))))
    (unless id (user-error "No subagent on this line"))
    (hermes-subagents-detail-open id)))

(defun hermes-subagents--render (sid)
  "Render session SID's subagents into the buffer, if any."
  (let* ((state (and sid (gethash sid hermes--sessions)))
         (subagents (and state (hermes-subagents--visible-subagents state))))
    (when (and subagents (> (length subagents) 0))
      (with-current-buffer (hermes-subagents--buffer)
        (setq hermes-subagents--sid sid)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (hermes-subagents--format subagents))
          (goto-char (point-min)))
        (use-local-map hermes-subagents--keymap)
        (setq buffer-read-only t))
      (hermes-subagents--render-details subagents))))

(defun hermes-subagents--on-state-change (old new)
  "`hermes-state-change-hook' handler: refresh on subagent changes."
  (let ((sid hermes--current-session-id))
    (when sid
      (let* ((old-sas (hermes-subagents--visible-subagents old))
             (new-sas (hermes-subagents--visible-subagents new)))
        (when (and new-sas (> (length new-sas) 0))
          (setq hermes-subagents--last-sid sid)
          (let ((fp (list sid (hermes-subagents--serialize new-sas))))
            (unless (equal fp hermes-subagents--fingerprint)
              (setq hermes-subagents--fingerprint fp)
              (when (and hermes-subagents-auto-open
                         (hermes-subagents--spawned-p old-sas new-sas))
                (hermes-subagents-open sid))
              (when (get-buffer hermes-subagents-buffer-name)
                (hermes-subagents--render sid)))))))))

(defun hermes-subagents-open (&optional sid)
  "Show the subagent status buffer below the selected window.
SID defaults to the session with the most recent subagent activity."
  (interactive)
  (let ((target (or sid hermes-subagents--last-sid
                    ;; Manual open: find any session with subagents.
                    (let (found)
                      (maphash (lambda (s state)
                                 (unless found
                                   (let ((sas (hermes-subagents--visible-subagents
                                               state)))
                                     (when (and sas (> (length sas) 0))
                                       (setq found s)))))
                               hermes--sessions)
                      found))))
    (when (or target (get-buffer hermes-subagents-buffer-name))
      (when target (hermes-subagents--render target))
      (display-buffer (hermes-subagents--buffer)
                      `(display-buffer-below-selected
                        (window-height . ,hermes-subagents-window-height)
                        (inhibit-same-window . t)))
      (let ((win (get-buffer-window hermes-subagents-buffer-name)))
        (when (window-live-p win)
          (set-window-parameter win 'dedicated t)
          (set-window-parameter win 'never-select t)))
      hermes-subagents-buffer-name)))

(defun hermes-subagents-toggle ()
  "Show or hide the subagent status window."
  (interactive)
  (let ((win (get-buffer-window hermes-subagents-buffer-name)))
    (if (window-live-p win)
        (quit-window nil win)
      (hermes-subagents-open))))

(add-hook 'hermes-state-change-hook #'hermes-subagents--on-state-change)

(provide 'hermes-subagents)
;;; hermes-subagents.el ends here
