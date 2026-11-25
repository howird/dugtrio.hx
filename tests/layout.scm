;; Run from the repository root with: steel tests/layout.scm
(require "../layout.scm")

(define (check name expected actual)
  (unless (equal? expected actual)
    (error! (list name 'expected expected 'actual actual))))

(define (listing kind path entries height highlight active? anchor)
  (DugtrioPane kind entries 4 height path highlight active? anchor path))

(define (plan panes viewport-width frame-right frame-bottom side jump?
              #:direct [direct #f] #:targetable? [targetable? (lambda (entry) #t)])
  (dugtrio-plan-view panes viewport-width frame-right frame-bottom side 1 1 jump?
                     (lambda (entry) entry) direct targetable?))

(define (rect-values rect)
  (list (DugtrioRect-x rect) (DugtrioRect-y rect) (DugtrioRect-width rect) (DugtrioRect-height rect)))

(define (geometry view)
  (map (lambda (placed)
         (list (DugtrioPane-title (DugtrioPlacedPane-pane placed))
               (rect-values (DugtrioPlacedPane-outer placed))
               (rect-values (DugtrioPlacedPane-inner placed))))
       (DugtrioView-panes view)))

(define entries '(0 1 2 3 4 5 6 7 8))
(define oldest (listing 'col "oldest" entries 7 4 #f 4))
(define ancestor (listing 'col "ancestor" entries 7 #f #f 4))
(define active (listing 'col "active" entries 7 4 #t 4))
(define preview (listing 'preview-dir "preview" '() 7 #f #f 0))

(for-each
 (lambda (case)
   (define view (plan (list oldest ancestor active preview) (list-ref case 1) (list-ref case 2)
                      (list-ref case 3) (list-ref case 4) #f))
   (check (car case) (list-ref case 5) (geometry view))
   (check (car case) (list-ref case 6) (DugtrioView-active-height view)))
 '((left 23 23 8 left
         (("ancestor" (1 1 6 7) (2 2 4 5))
          ("active" (8 1 6 7) (9 2 4 5))
          ("preview" (15 1 6 7) (16 2 4 5))) 5)
   (right 25 25 8 right
          (("ancestor" (4 1 6 7) (5 2 4 5))
           ("active" (11 1 6 7) (12 2 4 5))
           ("preview" (18 1 6 7) (19 2 4 5))) 5)
   (clipped-preview 11 12 8 right
                     (("active" (0 1 6 7) (1 2 4 5))
                      ("preview" (7 1 5 7) (8 2 3 5))) 5)
   (omitted-preview 8 8 4 left (("active" (1 1 6 3) (2 2 4 1))) 1)
   (no-width 3 3 8 left () 0)
   (no-height 23 23 3 left () 0)))

(define (rows-of placed)
  (map (lambda (row)
         (list (DugtrioRow-kind row) (DugtrioRow-offset row) (DugtrioRow-content row)
               (DugtrioRow-selected? row) (DugtrioRow-label row)))
       (DugtrioPlacedPane-rows placed)))

(for-each
 (lambda (case)
   (define anchor (list-ref case 1))
   (define pane (listing 'col "active" entries 7 anchor #t anchor))
   (define view (plan (list pane) 20 20 (+ 3 (list-ref case 2)) 'left #f))
   (check (car case) (list-ref case 3) (rows-of (car (DugtrioView-panes view)))))
 '((single-row 4 1 ((entry 0 4 #t #f)))
   (short-pane 4 2 ((overflow 0 #f #f #f) (entry 1 4 #t #f)))
   (clamped-top 0 3 ((entry 0 0 #t #f) (entry 1 1 #f #f) (overflow 2 #f #f #f)))
   (clamped-bottom 8 3 ((overflow 0 #f #f #f) (entry 1 7 #f #f) (entry 2 8 #t #f)))))

(define jump-view (plan (list oldest ancestor active preview) 30 30 8 'left #t))
(define jump-panes (DugtrioView-panes jump-view))
(check 'focus-priority '("bb" "hh" #f "ll") (map DugtrioPlacedPane-focus-label jump-panes))
(check 'missing-ancestor-highlight
       '((overflow 0 #f #f #f) (entry 1 3 #f "ha") (entry 2 4 #f "hb") (entry 3 5 #f "hc") (overflow 4 #f #f #f))
       (rows-of (cadr jump-panes)))
(check 'active-labels
       '((overflow 0 #f #f #f) (entry 1 3 #f "k") (entry 2 4 #t "a") (entry 3 5 #f "j") (overflow 4 #f #f #f))
       (rows-of (list-ref jump-panes 2)))
(check 'empty-preview '((empty 0 #f #f #f)) (rows-of (list-ref jump-panes 3)))

;; The rows and title are the drawing inputs: every label has exactly its
;; intended target, and the jump map has no extra targets for undrawn labels.
(define (label-targets view)
  (let ([targets (DugtrioView-targets view)])
    (define count 0)
    (for-each
     (lambda (placed)
       (define path (DugtrioPane-title (DugtrioPlacedPane-pane placed)))
       (define (target label entry)
         (when label
           (set! count (+ 1 count))
           (check label (cons path entry) (hash-try-get targets label))))
       (target (DugtrioPlacedPane-focus-label placed) #f)
       (for-each (lambda (row) (target (DugtrioRow-label row) (DugtrioRow-content row)))
                 (DugtrioPlacedPane-rows placed)))
     (DugtrioView-panes view))
    (check 'target-count count (length (hash-keys->list targets)))))

(label-targets jump-view)

;; Count and round-trip all labels with rows in both left and right panes.
;; The listings reuse entry IDs; the pane path makes each target distinct.
(define populated-preview (listing 'preview-dir "preview" entries 7 #f #f 4))
(define multi-pane-view (plan (list oldest ancestor active populated-preview) 30 30 8 'left #t))
(check 'preview-row-target '("preview" . 3) (hash-try-get (DugtrioView-targets multi-pane-view) "la"))
(label-targets multi-pane-view)

(define (check-neighbors name view next previous)
  (define targets (DugtrioView-targets view))
  (check name (list next previous) (map (lambda (key) (hash-try-get targets key)) '("j" "k")))
  (label-targets view))

(for-each
 (lambda (case)
   (define pane (listing 'col "active" entries 7 (cadr case) #t (cadr case)))
   (define view (plan (list pane) 20 20 (+ 3 (list-ref case 2)) 'left #t))
   (check-neighbors (car case) view (list-ref case 3) (list-ref case 4)))
 '((clipped-to-selection 4 1 #f #f)
   (both-neighbors-replaced-by-overflow 4 3 #f #f)
   (next-replaced-by-overflow 4 4 #f ("active" . 3))
   (first-entry 0 5 ("active" . 1) #f)
   (last-entry 8 5 #f ("active" . 7))))

(check-neighbors 'empty-active (plan (list (listing 'col "active" '() 3 #f #t 0)) 20 20 8 'left #t) #f #f)
(check-neighbors 'no-labels-outside-go (plan (list active preview) 20 20 8 'left #f) #f #f)
(define blocked-view
  (plan (list active) 20 20 10 'left #t #:targetable? (lambda (entry) (not (= entry 5)))))
(check-neighbors 'no-skipping-untargetable-next blocked-view #f (cons "active" 3))

(define tight-view (plan (list active (listing 'preview-dir "preview" '(1) 7 0 #f 0)) 12 11 8 'left #t))
(check 'no-focus-label-in-one-cell '(#f #f) (map DugtrioPlacedPane-focus-label (DugtrioView-panes tight-view)))
(check 'no-row-label-in-one-cell '((entry 0 1 #t #f)) (rows-of (cadr (DugtrioView-panes tight-view))))
(label-targets tight-view)
(check 'nearest-visible-ancestor '("hh" #f "ll")
       (map DugtrioPlacedPane-focus-label (DugtrioView-panes (plan (list oldest ancestor active preview) 23 23 8 'left #t))))

(define file-preview (DugtrioPane 'preview-file '("line 1" "line 2") 4 7 "file" #f #f #f #f))
(define file-view (plan (list ancestor active file-preview) 23 23 8 'left #t #:direct (cons "active" 4)))
(check 'selected-file-label
       '((overflow 0 #f #f #f) (entry 1 3 #f "k") (entry 2 4 #t "l") (entry 3 5 #f "j") (overflow 4 #f #f #f))
       (rows-of (cadr (DugtrioView-panes file-view))))
(check 'no-prefix-on-file-preview '("hh" #f #f) (map DugtrioPlacedPane-focus-label (DugtrioView-panes file-view)))
(label-targets file-view)

;; Direct open still fits when the active listing is one cell wide and the preview is omitted.
(define one-cell-view (plan (list active file-preview) 4 4 4 'left #t #:direct (cons "active" 4)))
(check 'direct-in-one-cell '((entry 0 4 #t "l")) (rows-of (car (DugtrioView-panes one-cell-view))))
(check-neighbors 'no-neighbors-in-one-cell one-cell-view #f #f)
(define pending-preview (DugtrioPane 'preview-pending #f 4 7 "pending" #f #f #f #f))
(define preview-view (plan (list file-preview pending-preview) 20 20 4 'left #t))
(check 'file-and-pending-previews '(((line 0 "line 1" #f #f)) ()) (map rows-of (DugtrioView-panes preview-view)))
(label-targets preview-view)

(displayln "Layout tests passed")
