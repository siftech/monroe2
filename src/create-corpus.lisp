#!clisp
;; command line version: only works with clisp - don't try to load
;; directly into lisp - use create-corpus-code.lisp instead

;; args: (outputfilename, corpus_size, chunk_size)


;; loads
(load "create-corpus-code.lisp")

;; shortcuts
(defun cc ()
  (load "create-corpus"))

;; get command line args
(if (not (or (= (length *args*) 4)
	     (= (length *args*) 3)))
    (format t "~%~%usage: create-corpus.lisp planlib_filename corpus_filename corpus-size~% [is_flat_output? (1 for yes, blank for no)]")
  ;; if ok, assign them
  (let ((plib-filename (pop *args*))
	(corpus-filename (pop *args*))
	(corpus-size (pop *args*))
	(flat-output (pop *args*))
	)
    
    ;; call main code with correct data types (not strings)
    (cc-main 
     plib-filename
     corpus-filename
     (parse-integer corpus-size)
     flat-output
     )))
