(provide directory-list
         directory-entry-path
         directory-entry-name
         directory-entry-directory?)

(struct directory-entry (path name directory?))

(define (directory-list path #:show-hidden? [show-hidden? #t] #:ignore [ignore '()])
  (define children (with-handler (lambda (_) '()) (read-dir path)))
  (define (entry-name<? a b)
    (string<? (directory-entry-name a) (directory-entry-name b)))
  (let loop ([paths children] [dirs '()] [files '()])
    (cond
      [(null? paths) (append (sort dirs entry-name<?) (sort files entry-name<?))]
      [else
       (define child (car paths))
       (define name (file-name child))
       (if (or (member name ignore) (and (not show-hidden?) (starts-with? name ".")))
           (loop (cdr paths) dirs files)
           (let* ([dir? (is-dir? child)]
                  [entry (directory-entry child name dir?)])
             (if dir?
                 (loop (cdr paths) (cons entry dirs) files)
                 (loop (cdr paths) dirs (cons entry files)))))])))
