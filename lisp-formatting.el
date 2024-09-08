(defun format-lisp-buffer()
  (interactive)
  (let* ((buffer-text (scratch-text))
         (lines (split-string buffer-text "\n"))
         (new-lines (cons (car lines)
                          (format-lisp-process-line-pairs (cl-mapcar 'list (cdr lines) lines)))))
    (replace-temp-output (string-join new-lines "\n")))
)

(defun scratch-text()
    (with-current-buffer "*scratch*"
        (buffer-string)))

(defun replace-temp-output (text-to-write)
  (with-current-buffer "*temp-output*"
    (erase-buffer)
    (insert text-to-write)))
    
    
(defun is-blank-line-p (line)
  (equal "" (s-trim line)))

(defun starts-with-colon (line)         
  (s-match "^\s*:" line))

(defun index-of-colon (line)
  (s-index-of ":" line))

(defun index-of-last-token (line)
  (let* ((trimmed (s-trim-right line))
         (reversed (reverse trimmed)))
  (awhen (s-match "[^[:space:]]+[[:space:]]" reversed)
    (1+ (- (length trimmed) (length (car it)))))))

(defun index-of-first-non-blank-character (line)
  (string-match "[^[:space:]]" line))

(index-of-last-token "  d  ")
(index-of-first-non-blank-character "   ")

;; Ths position of the opening bracket matching the
;; last ')'
(defun index-of-last-matching-open-bracket (line)
  (let* ((trimmed (s-trim-right line))
         (r-line (reverse trimmed)))
    (when (s-starts-with? ")" r-line)
        (let* ((i-pos 1)
               (n-rights 1)
               (len (length trimmed))
               (n-lefts 0))
          (while (and (< n-lefts n-rights)
                      (< i-pos len))
            (let ((char (substring r-line i-pos (1+ i-pos))))
              (cond ((string= char ")") (cl-incf n-rights))
                    ((string= char "(") (cl-incf n-lefts))))
            (cl-incf i-pos))
          (when (< i-pos len)
            (- len i-pos))))))

(index-of-last-matching-open-bracket "    (foo ((blah) (fooble))   ")
          
          

    
        
  
(defun format-lisp-process-pair (pair)
  (cl-destructuring-bind (line previous) pair
    (cond ((is-blank-line-p line) line)
          ((starts-with-colon line) 
           (aif (index-of-colon previous)
               (s-concat (s-pad-left it " " "")
                         (s-chop-left (index-of-colon line) line))
             line))
          (t
           (aif (or (index-of-last-matching-open-bracket previous)
                    (index-of-last-token previous))
               (s-concat (s-pad-left it " " "")
                         (s-chop-left (index-of-first-non-blank-character line) line))
             line)))))
               

(defun format-lisp-process-line-pairs (pairs)
  (mapcar 'format-lisp-process-pair pairs)
  
)
