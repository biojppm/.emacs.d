
(defvar imgsave--default-dir "img"
    "suffix directory where images are saved to")

(defvar imgsave--py-script
    (format "%simgsave.py"
       (file-truename
        (if load-file-name
           (file-name-directory load-file-name)
         default-directory
         )))
    "the full path to the imgsave python script")


(defun imgsave-save-and-paste()
  (interactive)
  (let* (
         ;;(_ (message "aqui 0 %s" imgsave--py-script))
         ;; get the current buffer's directory
         (fname (buffer-file-name (current-buffer)))
         (fname (if fname fname default-directory))
         (fndir (file-name-directory fname))
         ;; the directory to save to
         (imgdir (format "%s/%s" fndir imgsave--default-dir))
         ;; make sure it gets created
         (_ (unless (file-exists-p imgdir) (mkdir imgdir)))
         ;; prompt for the image filename
         (img-name (file-truename (ido-read-file-name "img save name: " imgdir "")))
         ;; get the filename relative to the buffer's dir
         (img-name-rel (format "./%s" (file-relative-name img-name fndir)))
         ;; save the image using python
         (ret (call-process "python" nil nil nil imgsave--py-script img-name))
         )
    (if (= ret 0)
        (progn
          (message "success: saved image to %s -- relative=%s" img-name img-name-rel)
          (insert img-name-rel)
          )
      (progn
        (warn "failed: check clipboard is image data. return: %d" ret)
        (insert (format "...FAILED... %s" img-name-rel))
        )
      )
    )
  )
