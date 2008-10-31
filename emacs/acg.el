;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        
;;                 ACG development toolkit                                
;;                                                                        
;;                  Copyright 2008 INRIA                                  
;;                                                                        
;;  More information on "http://acg.gforge.loria.fr/"                     
;;  License: CeCILL, see the LICENSE file or "http://www.cecill.info"     
;;  Authors: see the AUTHORS file                                         
;;                                                                        
;;                                                                        
;;                                                                        
;;                                                                        
;;  $Rev::                              $:  Revision of last commit       
;;  $Author::                           $:  Author of last commit         
;;  $Date::                             $:  Date of last commit           
;;                                                                        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main missing features: indentation

(require 'generic-x)

(define-generic-mode
  'acg-mode
  (cons "(*" '("*)"))
  '("prefix"
    "infix"
    "binder"
    "end"
    "type")
  '(("\\(signature\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \t\n]*\\(=\\)" 1 'font-lock-keyword-face)
   ("\\(signature\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 2 'font-lock-constant-face)
    ("\\(signature\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 3 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 1 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 2 'font-lock-constant-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 3 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 4 'font-lock-constant-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 5 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 6 'font-lock-keyword-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 7 'font-lock-constant-face)
    ("\\(lexicon\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\((\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\()\\)[ \n\t]*\\(:\\)[ \n\t]*\\([a-zA-Z0-9_']*\\)[ \n\t]*\\(=\\)" 8 'font-lock-keyword-face)
        )
  '(".*\\.acg")
  nil
)

(defgroup acg-faces nil
  "Special faces for the Acg mode."
  :group 'acg)

(defconst acg-faces-inherit-p
  (if (boundp 'face-attribute-name-alist)
      (assq :inherit face-attribute-name-alist)))

(defface acg-font-lock-error-face
  (if acg-faces-inherit-p
      '((t :inherit font-lock-warning-face))
    '((t (:foreground "yellow" :background "red")))
    )
  "Face description for all errors reported to the source."
  :group 'acg-faces)
(defvar acg-font-lock-error-face
  'acg-font-lock-error-face)


;; Define the key map for the acg-mode
(setq acg-mode-map (make-sparse-keymap))
(define-key acg-mode-map "\C-c\C-c" 'compile)

(defun acg-set-mode-map ()
  "Set the ACG mode map."
  (interactive)
  (use-local-map acg-mode-map)
)
(add-hook 'acg-mode-hook 'acg-set-mode-map)

;; Define the compilation command
(defun acg-set-compile-command ()
  "Hook to set compile-command locally." 
  (interactive)
  (let* ((filename (file-name-nondirectory buffer-file-name))
	 (basename (file-name-sans-extension filename))
	 (command nil))
    (if (executable-find "acgc.opt")
	(setq command "acgc.opt")
      (if (executable-find "acgc")
	  (setq command "acgc")))
    (progn
      (make-local-variable 'compile-command)
      (setq compile-command (concat command " " filename))))
  )

(add-hook 'acg-mode-hook 'acg-set-compile-command)

;; find the line of the error
(defconst acg-error-regexp
;;  "^[^\0-@]+ \"\\([^\"\n]+\\)\", [^\0-@]+ \\([0-9]+\\)[-,:]"
  "^.*File \"\\([^\"]+\\)\",[^0-9]+\\([0-9]+\\),"
  "Regular expression matching the error messages produced by acgc.")

(if (boundp 'compilation-error-regexp-alist)
    (or (assoc acg-error-regexp
               compilation-error-regexp-alist)
        (setq compilation-error-regexp-alist
              (cons (list acg-error-regexp 1 2)
               compilation-error-regexp-alist))))

;; A regexp to extract the range info.
;; Needs to be augmented with the possible optional range info
;; (for instance in case of non linear application on linear variable
(defconst acg-error-chars-single-line-regexp
;;  ".*, .*, [^\0-@]+ \\([0-9]+\\)-\\([0-9]+\\)"
  ".*line [0-9]+, characters \\([0-9]+\\)-\\([0-9]+\\)"
  "Regexp matching the char numbers in an error message produced by acgc.")


(defconst acg-error-chars-multi-line-regexp
  ".*line [0-9]+, character \\([0-9]+\\) to line \\([0-9]+\\), character \\([0-9]+\\)"
  "Regexp matching the char numbers in an error message produced by acgc.")


(defalias 'acg-match-string
  (if (fboundp 'match-string-no-properties)
      'match-string-no-properties
    'match-string))

(defadvice next-error (after acg-next-error activate)
 "Read the extra positional information provided by the Acg compiler.

Puts the point and the mark exactly around the erroneous program
fragment. The erroneous fragment is also temporarily highlighted if
possible."
 (if (eq major-mode 'acg-mode)
     (let ((beg nil) (end nil) (line-end nil) (char-end nil))
       (save-excursion
	 (set-buffer compilation-last-buffer)
	 (save-excursion
	   (goto-char (window-point (get-buffer-window (current-buffer) t)))
	   (if (looking-at acg-error-chars-single-line-regexp)
	       (setq beg (string-to-int (acg-match-string 1))
		     end (string-to-int (acg-match-string 2)))
	     (if (looking-at acg-error-chars-multi-line-regexp)
	       (setq beg (string-to-int (acg-match-string 1))
		     line-end (string-to-int (acg-match-string 2))
		     char-end (string-to-int (acg-match-string 3))
		     )))))
       (beginning-of-line)
       (if beg
	   (progn
	     (if end
		 (progn
		   (setq beg (+ (point) beg) end (+ (point) end))
		   (goto-char beg)
		   (push-mark (+ end 1) t t))
	       (progn
		 (setq beg (+ (point) beg))
		 (goto-char beg)
		 (setq current-position nil)
		 (point-to-register current-position)
		 (goto-line line-end)
		 (beginning-of-line)
		 (setq end (+ (point) char-end))
		 (push-mark (+ end 1) t t)
		 (jump-to-register current-position))
	       )
	     )
	 )
       )
   )
 )

(ad-activate 'next-error)
