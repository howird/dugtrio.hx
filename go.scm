(provide GoGroup go-labels go-resolve
         GoPending? GoPending-prefix GoMatched-target)

;; Supply only visible, eligible groups in display order: left groups from
;; farthest to nearest, at most one active group, and at most one right group.
;; Rows are in display order. Targets are opaque to Go; the active group's
;; focus target is unused. All used focus and row targets must be distinct
;; across groups so the returned bindings can also be indexed by target.
(struct GoGroup (role focus rows))
(struct GoPending (prefix))
(struct GoMatched (target))

(define go-letters
  (map (lambda (ch) (string ch)) (string->list "abcdefghijklmnopqrstuvwxyz")))
(define go-unreserved (filter (lambda (key) (not (member key '("h" "j" "k" "l")))) go-letters))

(define (go-bind-rows labels prefix letters rows)
  (let loop ([labels labels] [letters letters] [rows rows])
    (if (or (null? letters) (null? rows))
        labels
        (loop (hash-insert labels (string-append prefix (car letters)) (car rows))
              (cdr letters) (cdr rows)))))

(define (go-bind-group labels prefix group)
  ;; Only the doubled prefix is unavailable as a suffix.
  (go-bind-rows (hash-insert labels (string-append prefix prefix) (GoGroup-focus group))
                prefix (filter (lambda (key) (not (equal? key prefix))) go-letters)
                (GoGroup-rows group)))

;; Return a hash from label to target, for display and resolution. The caller
;; may supply distinct direct, next, and previous targets; each takes its special
;; label (l, j, or k) only if it matches a visible active row.
(define (go-labels groups direct #:next [next #f] #:previous [previous #f])
  (define (with-role role)
    (filter (lambda (group) (equal? role (GoGroup-role group))) groups))
  (define active (with-role 'active))
  (define rows (if (null? active) '() (GoGroup-rows (car active))))
  (define special
    (filter (lambda (binding) (and (cdr binding) (member (cdr binding) rows)))
            (list (cons "l" direct) (cons "j" next) (cons "k" previous))))
  (define special-rows (map cdr special))
  (define ordinary-rows (filter (lambda (row) (not (member row special-rows))) rows))
  (define special-labels (go-bind-rows (hash) "" (map car special) special-rows))
  (define active-labels (go-bind-rows special-labels "" go-unreserved ordinary-rows))
  (define right (with-role 'right))
  (define labels (if (and (not (hash-contains? special-labels "l")) (pair? right))
                     (go-bind-group active-labels "l" (car right))
                     active-labels))
  (let loop ([groups (reverse (with-role 'left))]
             [prefixes (cons "h" (drop go-unreserved (min (length ordinary-rows) (length go-unreserved))))]
             [labels labels])
    (if (or (null? groups) (null? prefixes))
        labels
        (loop (cdr groups) (cdr prefixes) (go-bind-group labels (car prefixes) (car groups))))))

;; Start with an empty prefix. A GoPending supplies the prefix for the next
;; key, a GoMatched supplies the caller's target, and #f cancels the sequence.
(define (go-resolve labels prefix key)
  (and labels (string? key) (= (string-length key) 1)
       (let ([label (string-append prefix key)])
         (cond
           [(hash-contains? labels label) (GoMatched (hash-try-get labels label))]
           [(and (equal? prefix "") (hash-contains? labels (string-append key key))) (GoPending key)]
           [else #f]))))
