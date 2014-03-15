(use posix)

(define %src-root% "./site")

(define %dev-root% "/srv/http/oweb.omegasuite.org")
(define %test-root% "/srv/http/oweb.omegasuite.org/test")
(define %prod-root% "/srv/http/oweb.omegasuite.org/prod")

(define %dev-group% "http")
(define %prod-group% "www")

(define %dir-mode%
  (bitwise-ior perm/irwxu perm/irwxg perm/iroth perm/ixoth))
(define %file-mode%
  (bitwise-ior perm/irusr perm/iwusr perm/irgrp perm/iwgrp perm/iroth))

(define (get-gid env)
  (let ((groupname
          (case env
            ((dev) %dev-group%)
            ((testing) %prod-group%)
            ((prod) %prod-group%))))
    (caddr (group-information groupname))))

(define (get-dest-root env)
  (case env
    ((dev) %dev-root%)
    ((testing) %test-root%)
    ((prod) %prod-root%)))

(define (create-dest-dir env subpath)
  (create-directory (make-pathname (get-dest-root env) subpath) #t))

(define (copy-file env subpath)
  (let ((dest-path (make-pathname (get-dest-root env) subpath)))
    (file-copy subpath dest-path)))

(define (copy-recursive env #!optional (path #f))
  (let* ((top (not path))
         (prev-path (current-directory)))
    (when top
      (create-directory (get-dest-root env))
      (change-directory %src-root%))
    (let ((contents
            (if top
              (directory)
              (directory path))))
      (for-each
        (lambda (fil)
          (let ((path* (make-pathname path fil)))
            (if (directory? path*)
              (begin
                (create-dest-dir env path*)
                (copy-recursive env path*))
              (copy-file env path*))))
        contents))
    (when top
      (change-directory prev-path))))

(define (set-file-permissions env file)
  (change-file-owner file (current-user-id) (get-gid env))
  (change-file-mode file %file-mode%))

(define (set-dir-permissions env dir)
  (change-file-owner dir (current-user-id) (get-gid env))
  (change-file-mode dir %dir-mode%))

(define (set-dest-permissions env #!optional (dir #f))
  (let ((init (not dir))
        (prev (current-directory))
        (dir (or dir (get-dest-root env))))
    (when init
      (set-dir-permissions env dir))
    (change-directory dir)
    (for-each
      (lambda (fil)
        (if (directory? fil)
          (begin
            (set-dir-permissions env fil)
            (set-dest-permissions env fil))
          (set-file-permissions env fil)))
      (directory))
    (change-directory prev)))

(define (run #!optional (env #f))
  (let ((env (or env (string->symbol (cadr (argv))))))
    (copy-recursive env)
    (set-dest-permissions env)))
