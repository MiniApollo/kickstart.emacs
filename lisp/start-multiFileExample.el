;;; start-multiFileExample.el --- Example how to write multi file config -*- lexical-binding: t -*-

;;; Code:

(defun start/hello ()
  "Say hello from another file"
  (message "Hello from another file"))

(provide 'start-multiFileExample)
;;; start-multiFileExample.el ends here
