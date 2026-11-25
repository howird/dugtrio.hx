(require "go.scm")

(provide dugtrio-plan-view
         DugtrioPane DugtrioPane-kind DugtrioPane-width DugtrioPane-height
         DugtrioPane-title DugtrioPane-active?
         DugtrioView-panes DugtrioView-active-height DugtrioView-targets
         DugtrioPlacedPane-pane DugtrioPlacedPane-outer DugtrioPlacedPane-inner
         DugtrioPlacedPane-focus-label DugtrioPlacedPane-rows
         DugtrioRect-x DugtrioRect-y DugtrioRect-width DugtrioRect-height
         DugtrioRow-kind DugtrioRow-offset DugtrioRow-content DugtrioRow-selected? DugtrioRow-label)

;; Width and height are preferred interior dimensions in terminal cells.
;; Content holds the original entries or preview lines. A listing's path is its jump destination.
;; For nonempty listings, anchor is the index used to center the window, independently of highlight-index.
(struct DugtrioPane (kind content width height title highlight-index active? anchor path))
(struct DugtrioView (panes active-height targets))
(struct DugtrioPlacedPane (pane outer inner focus-label rows))
(struct DugtrioRect (x y width height))
;; Offsets are relative to the inner rectangle. Only entries and lines carry content.
(struct DugtrioRow (kind offset content selected? label))

(define (dugtrio-listing-pane? pane)
  (or (equal? (DugtrioPane-kind pane) 'col) (equal? (DugtrioPane-kind pane) 'preview-dir)))

(define (dugtrio-total-width panes gap)
  (+ (apply + (map (lambda (pane) (+ (DugtrioPane-width pane) 2)) panes))
     (* gap (max 0 (- (length panes) 1)))))

(define (dugtrio-overflow-row? row start count height anchor)
  ;; Keep the anchor visible even if a very short pane has no room for a marker.
  (and (not (= (+ start row) anchor))
       (or (and (= row 0) (> start 0))
           (and (= row (- height 1)) (< (+ start height) count)))))

(define (dugtrio-pane-rows pane height)
  (define kind (DugtrioPane-kind pane))
  (define content (DugtrioPane-content pane))
  (define (empty-rows) (list (DugtrioRow 'empty 0 #f #f #f)))
  (cond
    [(dugtrio-listing-pane? pane)
     (if (null? content)
         (empty-rows)
         (let* ([anchor (DugtrioPane-anchor pane)]
                [highlight (DugtrioPane-highlight-index pane)]
                [count (length content)]
                ;; Center on the anchor, then clamp the window to the listing.
                [start (max 0 (min (max 0 (- count height)) (- anchor (quotient height 2))))])
           (let loop ([items (drop content start)] [row 0] [rows '()])
             (if (or (null? items) (>= row height))
                 (reverse rows)
                 (loop (cdr items) (+ row 1)
                       (cons (if (dugtrio-overflow-row? row start count height anchor)
                                 (DugtrioRow 'overflow row #f #f #f)
                                 (DugtrioRow 'entry row (car items) (and highlight (= (+ start row) highlight)) #f))
                             rows))))))]
    [(equal? kind 'preview-file)
     (let loop ([items content] [row 0] [rows '()])
       (if (or (null? items) (>= row height))
           (reverse rows)
           (loop (cdr items) (+ row 1) (cons (DugtrioRow 'line row (car items) #f #f) rows))))]
    [(equal? kind 'preview-pending) '()]
    [else (empty-rows)]))

(define (dugtrio-place-panes panes x0 y frame-right frame-bottom gap)
  (let loop ([remaining panes] [x x0] [placed '()])
    (cond
      [(null? remaining) (reverse placed)]
      [else
       (define pane (car remaining))
       (define width (min (+ (DugtrioPane-width pane) 2) (- frame-right x)))
       (define height (min (+ (DugtrioPane-height pane) 2) (- frame-bottom y)))
       ;; Once a pane cannot fit its border and a cell, stop placing panes.
       (if (and (>= width 3) (>= height 3))
           (let ([outer (DugtrioRect x y width height)]
                 [inner (DugtrioRect (+ x 1) (+ y 1) (- width 2) (- height 2))])
             (loop (cdr remaining) (+ x width gap)
                   (cons (DugtrioPlacedPane pane outer inner #f (dugtrio-pane-rows pane (- height 2))) placed)))
           (reverse placed))])))

(define (dugtrio-entry-row? row)
  (equal? (DugtrioRow-kind row) 'entry))

(define (dugtrio-row-target pane row entry-path)
  (cons (DugtrioPane-path pane) (entry-path (DugtrioRow-content row))))

(define (dugtrio-neighbor-targets placed entry-path targetable?)
  (define pane (DugtrioPlacedPane-pane placed))
  (define (target row)
    (and row (dugtrio-entry-row? row)
         (let ([candidate (dugtrio-row-target pane row entry-path)])
           (and (targetable? (cdr candidate)) candidate))))
  ;; Inspect the rows before filtering out overflow markers; a marker or a
  ;; clipped row must not turn a farther entry into a neighbor.
  (let loop ([rows (DugtrioPlacedPane-rows placed)] [previous #f])
    (cond
      [(null? rows) (cons #f #f)]
      [(DugtrioRow-selected? (car rows))
       (cons (target (and (pair? (cdr rows)) (cadr rows))) (target previous))]
      [else (loop (cdr rows) (car rows))])))

(define (dugtrio-go-group placed entry-path)
  (define pane (DugtrioPlacedPane-pane placed))
  (define role
    (cond
      [(not (dugtrio-listing-pane? pane)) #f]
      [(DugtrioPane-active? pane) 'active]
      ;; A focus label and its prefixed row labels both need two cells.
      [(< (DugtrioRect-width (DugtrioPlacedPane-inner placed)) 2) #f]
      [(equal? (DugtrioPane-kind pane) 'col) 'left]
      [else 'right]))
  (and role
       (GoGroup role (cons (DugtrioPane-path pane) #f)
                (map (lambda (row) (dugtrio-row-target pane row entry-path))
                     (filter dugtrio-entry-row? (DugtrioPlacedPane-rows placed))))))

(define (dugtrio-label-pane placed labels entry-path)
  (define pane (DugtrioPlacedPane-pane placed))
  (define rows
    (map (lambda (row)
           (if (dugtrio-entry-row? row)
               (DugtrioRow 'entry (DugtrioRow-offset row) (DugtrioRow-content row) (DugtrioRow-selected? row)
                           (hash-try-get labels (dugtrio-row-target pane row entry-path)))
               row))
         (DugtrioPlacedPane-rows placed)))
  (DugtrioPlacedPane pane (DugtrioPlacedPane-outer placed) (DugtrioPlacedPane-inner placed)
                     (hash-try-get labels (cons (DugtrioPane-path pane) #f)) rows))

;; Panes are ordered oldest ancestor to active column, then preview. Frame
;; bounds are exclusive. direct is the selected file's (column . entry) target, or #f.
;; neighbor-targetable? receives the path of a visible neighboring entry.
(define (dugtrio-plan-view panes viewport-width frame-right frame-bottom side margin gap jump? entry-path direct neighbor-targetable?)
  ;; Drop the oldest ancestors first, leaving the last two panes to frame clipping.
  (define visible
    (let fit ([items panes])
      (if (or (<= (dugtrio-total-width items gap) (- viewport-width 2)) (<= (length items) 2))
          items
          (fit (cdr items)))))
  ;; Anchor relative to the viewport width, not the frame's origin.
  (define x0 (if (equal? side 'right)
                 (max 0 (- viewport-width (dugtrio-total-width visible gap) margin))
                 margin))
  (define placements (dugtrio-place-panes visible x0 margin frame-right frame-bottom gap))
  (define active
    (let loop ([items placements])
      (cond
        [(null? items) #f]
        [(DugtrioPane-active? (DugtrioPlacedPane-pane (car items))) (car items)]
        [else (loop (cdr items))])))
  (define active-height (if active (DugtrioRect-height (DugtrioPlacedPane-inner active)) 0))
  (if jump?
      (let* ([groups (filter (lambda (group) group)
                             (map (lambda (placed) (dugtrio-go-group placed entry-path)) placements))]
             [neighbors (and active (dugtrio-neighbor-targets active entry-path neighbor-targetable?))]
             [targets (go-labels groups direct
                                 #:next (and neighbors (car neighbors))
                                 #:previous (and neighbors (cdr neighbors)))]
             ;; The same bindings drive drawing and dispatch. Invert them for drawing.
             [labels (let loop ([keys (hash-keys->list targets)] [labels (hash)])
                       (if (null? keys)
                           labels
                           (loop (cdr keys) (hash-insert labels (hash-try-get targets (car keys)) (car keys)))))])
        (DugtrioView (map (lambda (placed) (dugtrio-label-pane placed labels entry-path)) placements)
                     active-height targets))
      (DugtrioView placements active-height (hash))))
