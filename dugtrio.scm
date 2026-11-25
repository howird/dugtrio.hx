(require "helix/components.scm")
(require "helix/misc.scm")
(require "helix/editor.scm")
(require (prefix-in helix. "helix/commands.scm"))
(require "glyph/glyph.scm")
(require "directory.scm")

(provide dugtrio-open)
(provide dugtrio-close)
(provide dugtrio-configure!)
(provide dugtrio-set-keybinds!)

(define *dugtrio-side* 'left)
(define *dugtrio-ignore* '())
(define *dugtrio-show-hidden* #t)
(define *dugtrio-blink-selection* #t)
(define *dugtrio-blink-border* #f)

(define *dugtrio-active* #f)
(define *dugtrio-session* 0)

(define *dugtrio-keybinds*
  (hash 'down "j"
        'up "k"
        'enter "l"
        'back "h"
        'refresh "R"
        'toggle-hidden "."
        'jump "g"
        'quit "q"))

;; looks up which action (if any) a keypress is bound to
(define (dugtrio-action-for-char ch)
  (define s (string ch))
  (let loop ([ks (hash-keys->list *dugtrio-keybinds*)])
    (cond
      [(null? ks) #f]
      [(equal? (hash-try-get *dugtrio-keybinds* (car ks)) s) (car ks)]
      [else (loop (cdr ks))])))

(define (dugtrio-set-keybinds! overrides)
  (set! *dugtrio-keybinds*
        (let loop ([ks (hash-keys->list overrides)] [acc *dugtrio-keybinds*])
          (cond
            [(null? ks) acc]
            [(not (hash-contains? *dugtrio-keybinds* (car ks)))
             (error! (string-append "dugtrio: unknown keybind action " (to-string (car ks))))]
            [else
             (define action (car ks))
             (loop (cdr ks) (hash-insert acc action (hash-try-get overrides action)))]))))

;;@doc
;; Set the anchor side, visibility policy, and selection and border blink.
(define (dugtrio-configure! side #:ignore [ignore '()]
                         #:show-hidden? [show-hidden? #t]
                         #:blink-selection? [blink-selection? #t]
                         #:blink-border? [blink-border? #f])
  (set! *dugtrio-side* side)
  (set! *dugtrio-ignore* ignore)
  (set! *dugtrio-show-hidden* show-hidden?)
  (set! *dugtrio-blink-selection* blink-selection?)
  (set! *dugtrio-blink-border* blink-border?)
  (when *dugtrio-active* (dugtrio-refresh-all!)))

;; Component::Text is a bare builtin that no helix cog exports, so look it up
;; lazily: a build without it falls back to counting chars instead of failing to load.
(define *dugtrio-text-component* 'unresolved)

(define (dugtrio-cell-width s)
  (define len (string-length s))
  (cond
    ;; ASCII is one cell per char; this covers nearly every name without a native call
    [(= len (bytes-length (string->bytes s))) len]
    [else
     (when (equal? *dugtrio-text-component* 'unresolved)
       (set! *dugtrio-text-component* (with-handler (lambda (_) #f) (eval 'Component::Text))))
     (if *dugtrio-text-component*
         (with-handler (lambda (_) len)
           (car (native-component-required-size (*dugtrio-text-component* s) (list 10000 1))))
         len)]))

(define (dugtrio-truncate s max-w)
  (cond
    [(<= (dugtrio-cell-width s) max-w) s]
    [(<= max-w 0) ""]
    [else
     ;; Width grows with the prefix length, so binary-search the longest
     ;; prefix that leaves one cell for the ellipsis.
     (define room (- max-w 1))
     (define (fits? n) (<= (dugtrio-cell-width (substring s 0 n)) room))
     (define top (min (string-length s) room))
     (define n
       (if (fits? top)
           top
           (let loop ([lo 0] [hi (- top 1)])
             (if (>= lo hi)
                 lo
                 (let ([mid (quotient (+ lo hi 1) 2)])
                   (if (fits? mid) (loop mid hi) (loop lo (- mid 1))))))))
     (string-append (substring s 0 n) "…")]))

;; File names and file contents are untrusted display text: a newline, ESC or
;; tab would break the row, so every control char is drawn as a space.
(define (dugtrio-sanitize-label s)
  (define (control? c)
    (define n (char->integer c))
    (or (< n 32) (and (<= 127 n) (<= n 159))))
  (define chars (string->list s))
  (if (let any ([cs chars]) (and (pair? cs) (or (control? (car cs)) (any (cdr cs)))))
      (list->string (map (lambda (c) (if (control? c) #\space c)) chars))
      s))

(define (dugtrio-toggle-hidden!)
  (set! *dugtrio-show-hidden* (not *dugtrio-show-hidden*))
  (set-status! (if *dugtrio-show-hidden* "dugtrio: showing dotfiles" "dugtrio: hiding dotfiles"))
  (dugtrio-refresh-all!))

(define *dugtrio-min-w* 14)
(define *dugtrio-max-w* 40)
(define *dugtrio-min-h* 3)
(define *dugtrio-max-h* 24)
(define *dugtrio-gap* 0)
(define *dugtrio-margin* 1)

(define *dugtrio-stack* '()) ; list of columns, oldest first and active last
(define *dugtrio-remembered-entries* (hash)) ; directory path -> selected entry path

;; name-width is the widest entry name in cells, measured once per listing
(struct DugtrioColumn (path entries cursor name-width))

(define (dugtrio-make-column path entries cursor-index)
  (DugtrioColumn path entries (box cursor-index) (dugtrio-longest-name entries)))

(define (dugtrio-list-dir path)
  (directory-list path
                  #:show-hidden? *dugtrio-show-hidden*
                  #:ignore *dugtrio-ignore*))

(define (dugtrio-cursor col) (unbox (DugtrioColumn-cursor col)))
(define (dugtrio-set-cursor! col v)
  (unless (= (dugtrio-cursor col) v) (dugtrio-preview-defer!))
  (set-box! (DugtrioColumn-cursor col) v))

;; Steel's `last` can fail on a prefix returned by `take`. Build a fresh
;; list when truncating the stack so subsequent list operations work normally.
(define (dugtrio-stack-prefix n)
  (let loop ([cols *dugtrio-stack*] [remaining n] [kept '()])
    (if (= remaining 0)
        (reverse kept)
        (loop (cdr cols) (- remaining 1) (cons (car cols) kept)))))

(define (dugtrio-active-column)
  (last *dugtrio-stack*))

(define (dugtrio-column-entry col)
  (define entries (DugtrioColumn-entries col))
  (and (pair? entries) (list-ref entries (dugtrio-cursor col))))

(define (dugtrio-current-entry)
  (dugtrio-column-entry (dugtrio-active-column)))

(define (dugtrio-remember-selection!)
  (define col (dugtrio-active-column))
  (define entry (dugtrio-current-entry))
  (set! *dugtrio-remembered-entries*
        (hash-insert *dugtrio-remembered-entries* (DugtrioColumn-path col)
                     (and entry (directory-entry-path entry)))))

(define (dugtrio-entry-index entries key target)
  (let loop ([items entries] [i 0])
    (cond
      [(null? items) #f]
      [(equal? (key (car items)) target) i]
      [else (loop (cdr items) (+ i 1))])))

(define (dugtrio-landing-index path entries)
  (define remembered (hash-try-get *dugtrio-remembered-entries* path))
  (and (pair? entries) (or (and remembered (dugtrio-entry-index entries directory-entry-path remembered)) 0)))

(define (dugtrio-move! delta)
  (define col (dugtrio-active-column))
  (define n (length (DugtrioColumn-entries col)))
  (when (> n 0)
    (dugtrio-set-cursor! col (max 0 (min (- n 1) (+ (dugtrio-cursor col) delta))))))

(define (dugtrio-close!)
  (set! *dugtrio-active* #f)
  (dugtrio-preview-invalidate!)
  (set! *dugtrio-preview-last-size* #f)
  (set! *dugtrio-remembered-entries* (hash))
  (pop-last-component-by-name! "dugtrio"))

(define (dugtrio-descend! path)
  ;; The preview has normally just listed and measured this directory.
  (define cache *dugtrio-preview-cache*)
  (define cached? (and cache (equal? (car cache) path) (equal? (cadr cache) 'dir)))
  (define entries (if cached? (list-ref cache 2) (dugtrio-list-dir path)))
  (define name-width (if cached? (list-ref cache 3) (dugtrio-longest-name entries)))
  (dugtrio-preview-invalidate!)
  (dugtrio-remember-selection!)
  (set! *dugtrio-stack*
        (append *dugtrio-stack*
                (list (DugtrioColumn path entries
                                     (box (or (dugtrio-landing-index path entries) 0))
                                     name-width))))
  event-result/consume)

(define (dugtrio-entry-kind path open-file?)
  (cond
    [(not path) #f]
    [(is-dir? path) 'dir]
    [(and open-file? (is-file? path)) 'file]
    [else #f]))

(define (dugtrio-enter-path! path kind)
  (cond
    [(equal? kind 'dir) (dugtrio-descend! path)]
    [(equal? kind 'file)
     (dugtrio-close!)
     (enqueue-thread-local-callback (lambda () (helix.open path)))
     event-result/consume]
    [else event-result/consume]))

(define (dugtrio-enter! open-file?)
  (define entry (dugtrio-current-entry))
  (define path (and entry (directory-entry-path entry)))
  (dugtrio-enter-path! path (dugtrio-entry-kind path open-file?)))

;; steps back to the parent column
(define (dugtrio-back!)
  (define depth (length *dugtrio-stack*))
  (when (> depth 1)
    (dugtrio-preview-invalidate!)
    (dugtrio-remember-selection!)
    (set! *dugtrio-stack* (dugtrio-stack-prefix (- depth 1)))))

;; Re-lists every column from the root down. A column whose directory is no
;; longer listed in its parent (hidden, ignored or deleted) is dropped along
;; with everything below it; the root column always stays.
(define (dugtrio-refresh-all!)
  (dugtrio-preview-invalidate!)
  (set! *dugtrio-stack*
        (let loop ([cols *dugtrio-stack*] [acc '()])
          (define col (car cols))
          (define selected (dugtrio-column-entry col))
          (define path (DugtrioColumn-path col))
          (define new-entries (dugtrio-list-dir path))
          (define new-cursor
            (or (and selected (dugtrio-entry-index new-entries directory-entry-path (directory-entry-path selected)))
                (max 0 (min (dugtrio-cursor col) (- (length new-entries) 1)))))
          (define kept (cons (dugtrio-make-column path new-entries new-cursor) acc))
          (define child-idx
            (and (pair? (cdr cols))
                 (dugtrio-entry-index new-entries directory-entry-path (DugtrioColumn-path (cadr cols)))))
          (if (and child-idx (directory-entry-directory? (list-ref new-entries child-idx)))
              (loop (cdr cols) kept)
              (reverse kept)))))

;; path segments below root
(define (dugtrio-relative-components root path)
  (define sep (path-separator))
  ;; a root of "/" or one given with a trailing separator must not double it
  (define base (if (ends-with? root sep)
                   (substring root 0 (- (string-length root) (string-length sep)))
                   root))
  (define prefix (string-append base sep))
  (if (and (>= (string-length path) (string-length prefix))
           (equal? (substring path 0 (string-length prefix)) prefix))
      (split-many (substring path (string-length prefix) (string-length path)) (path-separator))
      '()))

;; cascades a column per ancestor from root down to path
(define (dugtrio-build-stack-for root path)
  (let loop ([dir root] [comps (dugtrio-relative-components root path)] [acc '()])
    (define entries (dugtrio-list-dir dir))
    (cond
      [(null? comps) (reverse (cons (dugtrio-make-column dir entries 0) acc))]
      [else
       (define idx (dugtrio-entry-index entries directory-entry-name (car comps)))
       (define col (dugtrio-make-column dir entries (or idx 0)))
       (if (and idx (pair? (cdr comps)))
           (loop (directory-entry-path (list-ref entries idx)) (cdr comps) (cons col acc))
           (reverse (cons col acc)))])))

;; expands ancestors down to whatever file the editor has focused
(define (dugtrio-reveal-current-file! root)
  (define path (editor-document->path (editor->doc-id (editor-focus))))
  (if (string? path)
      (dugtrio-build-stack-for root path)
      (list (dugtrio-make-column root (dugtrio-list-dir root) 0))))

;; panels grow and shrink with their own content with safe bound clamping
(define (dugtrio-longest-name entries)
  (let loop ([lst entries] [best 0])
    (if (null? lst) best (loop (cdr lst) (max best (dugtrio-cell-width (directory-entry-name (car lst))))))))

(define (dugtrio-col-width name-width)
  (min *dugtrio-max-w* (max *dugtrio-min-w* (+ name-width 4))))

(define (dugtrio-col-height count max-h)
  (min max-h (max *dugtrio-min-h* count)))

(define (dugtrio-clamp-window start count height)
  (max 0 (min (max 0 (- count height)) start)))

;; centers the cursor within a column's visible window
(define (dugtrio-window-start cursor count height)
  (dugtrio-clamp-window (- cursor (quotient height 2)) count height))

(define *dugtrio-preview-max-lines* 200)
(define *dugtrio-preview-max-bytes* (* 64 1024))
(define *dugtrio-preview-min-w* 15)
(define *dugtrio-preview-max-w* 70)

(define (dugtrio-preview-content data)
  (cond
    [(eof-object? data) ""]
    [else
     (define len (bytes-length data))
     ;; The byte limit can cut a UTF-8 character in half. Try dropping up to
     ;; three trailing bytes at the limit; otherwise leave decoding errors alone.
     (define min-end (if (= len *dugtrio-preview-max-bytes*) (- len 3) len))
     (let loop ([end len])
       (if (= end min-end)
           (bytes->string/utf8 data 0 end)
           (or (with-handler (lambda (_) #f) (bytes->string/utf8 data 0 end))
               (loop (- end 1)))))]))

(define (dugtrio-preview-lines path)
  (with-handler
    (lambda (_) (list "(unable to preview)"))
    (let* ([p (open-input-file path)]
           [data (dynamic-wind (lambda () void)
                               (lambda () (read-bytes *dugtrio-preview-max-bytes* p))
                               (lambda () (close-input-port p)))]
           [content (dugtrio-preview-content data)])
      ;; Clip and clean once here: no pane shows more than the preview's maximum
      ;; width, and measuring a very long line on every frame is slow.
      (define keep (+ *dugtrio-preview-max-w* 1))
      (map (lambda (line)
             (dugtrio-sanitize-label
              (if (> (string-length line) keep) (substring line 0 keep) line)))
           (take (split-many content "\n") *dugtrio-preview-max-lines*)))))

(define (dugtrio-longest-line lines cap)
  (let loop ([lst lines] [best 0])
    (if (null? lst) best (loop (cdr lst) (max best (min cap (dugtrio-cell-width (car lst))))))))

;; One prepared preview per session. The content width is measured once here;
;; the remembered landing row is derived on every render rather than cached.
(define *dugtrio-preview-cache* #f) ; (path kind data content-width)


;; Cursor moves hold the preview back until the cursor has rested, so holding
;; a movement key never lists a directory or reads a file per row.
(define *dugtrio-preview-delay-ms* 80)
(define *dugtrio-preview-gen* 0)
(define *dugtrio-preview-hold* #f) ; generation the preview is waiting on, or #f
(define *dugtrio-preview-last-size* #f) ; (width . height) of the last prepared preview pane

;; The next render can use or prepare the preview without waiting.
(define (dugtrio-preview-release-hold!)
  (set! *dugtrio-preview-hold* #f))

(define (dugtrio-preview-invalidate!)
  (set! *dugtrio-preview-cache* #f)
  (dugtrio-preview-release-hold!))

;; the preview is prepared once the cursor has stayed put for the delay
(define (dugtrio-preview-defer!)
  (define gen (+ *dugtrio-preview-gen* 1))
  (set! *dugtrio-preview-gen* gen)
  (set! *dugtrio-preview-cache* #f)
  (set! *dugtrio-preview-hold* gen)
  ;; Local callbacks trigger a redraw, which then prepares the preview.
  (enqueue-thread-local-callback-with-delay
   *dugtrio-preview-delay-ms*
   (lambda ()
     (when (equal? *dugtrio-preview-hold* gen)
       (dugtrio-preview-release-hold!)))))

(define (dugtrio-preview entry)
  (cond
    [(not entry) (dugtrio-preview-invalidate!) (list 'empty #f 0)]
    [*dugtrio-preview-hold* (list 'pending #f 0)]
    [else
      (let ([path (directory-entry-path entry)])
        (unless (and *dugtrio-preview-cache* (equal? path (car *dugtrio-preview-cache*)))
          (set! *dugtrio-preview-cache*
                (cond
                  [(is-dir? path)
                   (define entries (dugtrio-list-dir path))
                   (list path 'dir entries (dugtrio-longest-name entries))]
                  [(is-file? path)
                   (define lines (dugtrio-preview-lines path))
                   (list path 'file lines (dugtrio-longest-line lines *dugtrio-preview-max-w*))]
                  [else (list path 'empty #f 0)])))
        (define kind (cadr *dugtrio-preview-cache*))
        (define data (list-ref *dugtrio-preview-cache* 2))
        (define width (list-ref *dugtrio-preview-cache* 3))
        (if (equal? kind 'dir)
            (list kind data width (dugtrio-landing-index path data))
            (list kind data width)))]))

;; The jump targets and height describe the active column's last rendered frame.
(struct DugtrioState (session blink-bright? blink-pending? active-height jump-prefix jump-targets))

(define (dugtrio-schedule-blink! state)
  (define pending? (DugtrioState-blink-pending? state))
  (define bright? (DugtrioState-blink-bright? state))
  (unless (unbox pending?)
    (set-box! pending? #t)
    ;; Local callbacks trigger a redraw. Only a subsequent render schedules
    ;; another tick, so a component Helix drops cannot leave a timer running.
    (enqueue-thread-local-callback-with-delay
     500
     (lambda ()
       (set-box! pending? #f)
       (when (and *dugtrio-active* (= (DugtrioState-session state) *dugtrio-session*))
         ;; A key press leaves 'fresh, which this tick only settles to #t, so a
         ;; timer already running cannot dim the selection right after a move.
         (set-box! bright? (or (not (or *dugtrio-blink-selection* *dugtrio-blink-border*))
                              (equal? (unbox bright?) 'fresh)
                              (not (unbox bright?)))))))))
;; highlight-index is the row drawn selected: the cursor in the active column,
;; the child directory in an ancestor, the landing row in a directory preview.
(struct DugtrioPane (kind content width height title highlight-index active?))
(struct DugtrioPlacement (pane x width height))

(define (dugtrio-place-panes panes x0 y frame-right frame-bottom)
  (let loop ([remaining panes] [x x0] [placed '()])
    (cond
      [(null? remaining) (reverse placed)]
      [else
       (define pane (car remaining))
       (define pw (min (+ (DugtrioPane-width pane) 2) (- frame-right x)))
       (define ph (min (+ (DugtrioPane-height pane) 2) (- frame-bottom y)))
       ;; Once a pane cannot fit its border and a cell, no later pane can fit.
       (if (and (>= pw 3) (>= ph 3))
           (loop (cdr remaining) (+ x pw *dugtrio-gap*)
                 (cons (DugtrioPlacement pane x pw ph) placed))
           (reverse placed))])))

(define (dugtrio-overflow-row? row ws count h anchor)
  ;; Keep the anchor visible even if a very short pane has no room for a marker.
  (and (not (= (+ ws row) anchor))
       (or (and (= row 0) (> ws 0))
           (and (= row (- h 1)) (< (+ ws h) count)))))

;; An overflow marker has a row and index but no entry. An empty listing
;; has no rows; its placeholder is painted separately and never gets a label.
(define (dugtrio-for-each-entry-row entries anchor h visit!)
  (define count (length entries))
  (define ws (dugtrio-window-start anchor count h))
  (let loop ([items (drop entries ws)] [row 0])
    (unless (or (null? items) (>= row h))
      (visit! row (+ ws row)
              (and (not (dugtrio-overflow-row? row ws count h anchor)) (car items)))
      (loop (cdr items) (+ row 1)))))

(define (dugtrio-active-label-count placements)
  (let loop ([items placements])
    (cond
      [(null? items) 0]
      [else
       (define placement (car items))
       (define pane (DugtrioPlacement-pane placement))
       (if (DugtrioPane-active? pane)
           (let ([count 0])
             (dugtrio-for-each-entry-row
              (DugtrioColumn-entries (DugtrioPane-content pane))
              (DugtrioPane-highlight-index pane) (- (DugtrioPlacement-height placement) 2)
              (lambda (row idx entry) (when entry (set! count (+ count 1)))))
             count)
           (loop (cdr items)))])))

(define (dugtrio-column-prefixes placements active-label-count)
  (define (eligible? placement kind)
    (define pane (DugtrioPlacement-pane placement))
    (and (>= (DugtrioPlacement-width placement) 4)
         (not (DugtrioPane-active? pane)) (equal? (DugtrioPane-kind pane) kind)))
  ;; Nearest ancestor first, then the directory preview if a letter remains.
  (define candidates
    (append (reverse (filter (lambda (p) (eligible? p 'col)) placements))
            (filter (lambda (p) (eligible? p 'preview-dir)) placements)))
  (let loop ([items candidates] [i active-label-count] [prefixes (hash)])
    (if (or (null? items) (>= i 26))
        prefixes
        (loop (cdr items) (+ i 1)
              (hash-insert prefixes (DugtrioPlacement-x (car items)) (dugtrio-letter i))))))

(define (dugtrio-render-title frame x y w title style)
  (when (and title (> w 0))
    (frame-set-string! frame x y (dugtrio-truncate (dugtrio-sanitize-label title) w) style)))

(define (dugtrio-render-entry frame x y w entry hl? label label-style text-style hl-style dir-style)
  (define dir? (directory-entry-directory? entry))
  (define basename (directory-entry-name entry))
  (define icon (or label (if dir? (glyph-dir-icon basename) (glyph-icon basename))))
  (define icon-color (and (not label) (if dir? (glyph-dir-color basename) (glyph-color basename))))
  (define row-style (cond [hl? hl-style] [dir? dir-style] [else text-style]))
  (define name (dugtrio-sanitize-label (string-append basename (if dir? (path-separator) ""))))
  (define icon-w (string-length icon))
  (define avail (max 0 (- w icon-w 1)))
  (when hl? (frame-set-string! frame x y (make-string w #\space) row-style))
  ;; Paint the row's base style before applying the theme's jump-label overlay.
  (when (and label (not hl?)) (frame-set-string! frame x y (make-string icon-w #\space) row-style))
  (frame-set-string! frame x y (if (> w (+ icon-w 1)) icon (dugtrio-truncate icon w))
                     (if label label-style (glyph-style icon-color #:base row-style)))
  (when (> avail 0)
    (frame-set-string! frame (+ x icon-w 1) y (dugtrio-truncate name avail) row-style)))

(define (dugtrio-render-entries frame x y0 w h entries anchor highlight-index
                                text-style hl-style dir-style label-style prefix register-label!)
  (define label-index 0)
  (if (null? entries)
      (frame-set-string! frame x y0 (dugtrio-truncate "(empty)" w) (style-with-dim text-style))
      (dugtrio-for-each-entry-row
       entries anchor h
       (lambda (row idx entry)
         (define y (+ y0 row))
         (if entry
             (let* ([next-letter (and prefix (< label-index 26) (dugtrio-letter label-index))]
                    ;; The doubled column letter is reserved for focusing it.
                    [i (if (and next-letter (equal? next-letter prefix)) (+ label-index 1) label-index)]
                    [label (and prefix (< i 26) (string-append prefix (dugtrio-letter i)))])
               (when label (register-label! label (directory-entry-path entry)))
               (dugtrio-render-entry frame x y w entry
                                     (and highlight-index (= idx highlight-index)) label label-style
                                     text-style hl-style dir-style)
               (set! label-index (+ i 1)))
             (let ([dots (make-string (min w 3) #\.)])
               (frame-set-string! frame (+ x (quotient (- w (string-length dots)) 2)) y
                                  dots (style-with-dim text-style))))))))

(define (dugtrio-letter index)
  (string (integer->char (+ (char->integer #\a) index))))

;; file-preview panel in plain text
(define (dugtrio-render-lines frame x y0 w h lines style)
  (let iloop ([items (take lines h)] [row 0])
    (unless (or (null? items) (>= row h))
      (frame-set-string! frame x (+ y0 row) (dugtrio-truncate (car items) w) style)
      (iloop (cdr items) (+ row 1)))))

(define (dugtrio-render state rect frame)
  (define sw (area-width rect))
  (define sh (area-height rect))
  (define max-h (min *dugtrio-max-h* (max *dugtrio-min-h* (- sh 4))))

  (define bg-style (theme-scope-ref "ui.background"))
  (define text-style (theme-scope-ref "ui.text"))
  (define hl-style (theme-scope-ref "ui.menu.selected"))
  (define dir-style (theme-scope-ref "ui.text.info"))
  (define dim-text-style (style-with-dim text-style))
  (define dim-dir-style (style-with-dim dir-style))
  (define dim-phase? (not (unbox (DugtrioState-blink-bright? state))))
  (define active-hl-style (if (and dim-phase? *dugtrio-blink-selection*) (style-with-dim hl-style) hl-style))
  (define active-border-style (if (and dim-phase? *dugtrio-blink-border*) dim-text-style text-style))
  (define divider-color (style->fg (theme-scope-ref "ui.window")))
  (define inactive-border-style
    (style-with-dim (if divider-color (style-fg bg-style divider-color) text-style)))

  (define column-panes
    (let build ([cols *dugtrio-stack*])
      (if (null? cols)
          '()
          (let* ([col (car cols)]
                 [entries (DugtrioColumn-entries col)]
                 [active? (null? (cdr cols))])
            (cons (DugtrioPane 'col col
                                  (dugtrio-col-width (DugtrioColumn-name-width col))
                                  (dugtrio-col-height (length entries) max-h)
                                  (string-append (file-name (DugtrioColumn-path col)) (path-separator))
                                  ;; the last column is the active one; every other has its child next
                                  (if active?
                                      (dugtrio-cursor col)
                                      (dugtrio-entry-index entries directory-entry-path (DugtrioColumn-path (cadr cols))))
                                  active?)
                  (build (cdr cols)))))))

  (define entry (dugtrio-current-entry))
  (define preview (dugtrio-preview entry))
  (define preview-kind (car preview))
  (define preview-data (cadr preview))
  (define preview-width (list-ref preview 2))
  (define preview-pane
    (cond
      [(equal? preview-kind 'dir)
       (DugtrioPane 'preview-dir preview-data
                       (dugtrio-col-width preview-width)
                       (dugtrio-col-height (length preview-data) max-h)
                       (string-append (directory-entry-name entry) (path-separator))
                       (list-ref preview 3)
                       #f)]
      [(equal? preview-kind 'file)
       (DugtrioPane 'preview-file preview-data
                       (min *dugtrio-preview-max-w*
                            (max *dugtrio-preview-min-w*
                                 (+ preview-width 4)))
                       (dugtrio-col-height (length preview-data) max-h)
                       (directory-entry-name entry)
                       #f
                       #f)]
      ;; Held back while the cursor is moving: a titled pane with nothing in it,
      ;; kept at the last preview's size so the columns beside it do not shift.
      [(equal? preview-kind 'pending)
       (DugtrioPane 'preview-pending #f
                       (if *dugtrio-preview-last-size* (car *dugtrio-preview-last-size*) *dugtrio-min-w*)
                       (if *dugtrio-preview-last-size* (cdr *dugtrio-preview-last-size*) *dugtrio-min-h*)
                       (directory-entry-name entry) #f #f)]
      ;; no entry at all, or one that is neither file nor directory (a broken symlink)
      [else (DugtrioPane 'preview-empty #f *dugtrio-min-w* *dugtrio-min-h*
                            (and entry (directory-entry-name entry)) #f #f)]))

  (unless (equal? preview-kind 'pending)
    (set! *dugtrio-preview-last-size*
          (cons (DugtrioPane-width preview-pane) (DugtrioPane-height preview-pane))))

  (define all-panes (append column-panes (list preview-pane)))

  (define (total-width panes)
    (+ (apply + (map (lambda (pane) (+ (DugtrioPane-width pane) 2)) panes))
       (* *dugtrio-gap* (max 0 (- (length panes) 1)))))

  ;; drop the oldest ancestor columns first if the stack is wider than the screen
  (define (fit panes)
    (if (or (<= (total-width panes) (- sw 2)) (<= (length panes) 2))
        panes
        (fit (cdr panes))))
  (define visible (fit all-panes))

  ;; anchor at a top corner
  (define x0 (if (equal? *dugtrio-side* 'right)
                 (max 0 (- sw (total-width visible) *dugtrio-margin*))
                 *dugtrio-margin*))
  (define y0 *dugtrio-margin*)
  (define frame-area (buffer-area frame))
  (define frame-right (+ (area-x frame-area) (area-width frame-area)))
  (define frame-bottom (+ (area-y frame-area) (area-height frame-area)))

  (define jump-style (and (unbox (DugtrioState-jump-prefix state))
                          (theme-scope-ref "ui.virtual.jump-label")))
  (define targets (DugtrioState-jump-targets state))
  (define placements (dugtrio-place-panes visible x0 y0 frame-right frame-bottom))
  (define prefixes
    (and jump-style
         (dugtrio-column-prefixes placements (dugtrio-active-label-count placements))))
  (define active-height 0)
  (when jump-style (set-box! targets (hash)))
  (for-each
   (lambda (placement)
        (define pane (DugtrioPlacement-pane placement))
        (define x (DugtrioPlacement-x placement))
        (define pw (DugtrioPlacement-width placement))
        (define ph (DugtrioPlacement-height placement))
        (define kind (DugtrioPane-kind pane))
        (define content (DugtrioPane-content pane))
        (define column? (equal? kind 'col))
        (define active? (DugtrioPane-active? pane))
        (define w (- pw 2))
        (define h (- ph 2))
        (define panel-area (area x y0 pw ph))
        (define cx (+ x 1))
        (define cy (+ y0 1))
        (when active? (set! active-height h))

        (define border-style (if active? active-border-style inactive-border-style))
        (define title-style (if active? (style-with-bold active-border-style) dim-text-style))
        (buffer/clear-with frame panel-area bg-style)
        (block/render frame panel-area (make-block bg-style border-style "all" "rounded"))
        (define column-prefix (and prefixes (hash-try-get prefixes x)))
        (if column-prefix
            (begin
              (frame-set-string! frame cx y0 (string-append column-prefix column-prefix) jump-style)
              (when (>= w 3) (frame-set-string! frame (+ cx 2) y0 " " title-style))
              (dugtrio-render-title frame (+ cx 3) y0 (- w 3) (DugtrioPane-title pane) title-style))
            (dugtrio-render-title frame cx y0 w (DugtrioPane-title pane) title-style))

        (cond
          [(or column? (equal? kind 'preview-dir))
           (define entries (if column? (DugtrioColumn-entries content) content))
           (define highlight-index (DugtrioPane-highlight-index pane))
           ;; An ancestor whose child is gone from its listing scrolls to its own cursor.
           (define anchor (or highlight-index (if column? (dugtrio-cursor content) 0)))
           (define prefix (and jump-style (if active? "" column-prefix)))
           (define path (if column? (DugtrioColumn-path content) (directory-entry-path entry)))
           (define (register-label! label entry-path)
             (set-box! targets (hash-insert (unbox targets) label (cons path entry-path))))
           (when column-prefix (register-label! (string-append column-prefix column-prefix) #f))
           (dugtrio-render-entries frame cx cy w h entries anchor highlight-index
                                   (if column? text-style dim-text-style)
                                   (if active? active-hl-style hl-style)
                                   (if column? dir-style dim-dir-style)
                                   jump-style prefix register-label!)]
          [(equal? kind 'preview-file)
           (dugtrio-render-lines frame cx cy w h content dim-text-style)]
          [(equal? kind 'preview-pending) void]
          [else
           (frame-set-string! frame cx cy (dugtrio-truncate "(empty)" w) dim-text-style)]))
   placements)
  (set-box! (DugtrioState-active-height state) active-height)
  (when (or *dugtrio-blink-border* (and *dugtrio-blink-selection* entry))
    (dugtrio-schedule-blink! state)))

(define (dugtrio-command-action! state action)
  (cond
    [(equal? action 'down) (dugtrio-move! 1) event-result/consume]
    [(equal? action 'up) (dugtrio-move! -1) event-result/consume]
    [(equal? action 'enter) (dugtrio-enter! #f)]
    [(equal? action 'back) (dugtrio-back!) event-result/consume]
    [(equal? action 'quit) (dugtrio-close!) event-result/consume]
    [(equal? action 'refresh) (dugtrio-refresh-all!) event-result/consume]
    [(equal? action 'toggle-hidden) (dugtrio-toggle-hidden!) event-result/consume]
    [(equal? action 'jump)
     ;; Render any directory preview before assigning its labels.
     (dugtrio-preview-release-hold!)
     (set-box! (DugtrioState-jump-prefix state) "")
     event-result/consume]
    [else event-result/consume]))

(define (dugtrio-focus-stack-column! index)
  (define kept (+ index 1))
  (when (< kept (length *dugtrio-stack*))
    (dugtrio-preview-invalidate!)
    (dugtrio-remember-selection!)
    (set! *dugtrio-stack* (dugtrio-stack-prefix kept))))

(define (dugtrio-follow-label! target)
  (define path (car target))
  (define entry-path (cdr target))
  (define index (dugtrio-entry-index *dugtrio-stack* DugtrioColumn-path path))
  (define col (and index (list-ref *dugtrio-stack* index)))
  ;; Only the current directory preview can name a column not on the stack.
  (define current (and (not col) (dugtrio-current-entry)))
  (define preview-dir?
    (and current (equal? path (directory-entry-path current)) (is-dir? path)))
  (cond
    [(not entry-path)
     (cond [col (dugtrio-focus-stack-column! index)]
           [preview-dir? (dugtrio-descend! path)])
     event-result/consume]
    [else
     ;; Resolve and check the row before committing to a different column.
     (define entries
       (cond
         [col (DugtrioColumn-entries col)]
         [preview-dir?
          (define preview (dugtrio-preview current))
          (if (equal? (car preview) 'dir) (cadr preview) '())]
         [else '()]))
     (define row (dugtrio-entry-index entries directory-entry-path entry-path))
     (define kind (and row (dugtrio-entry-kind entry-path #t)))
     (when kind
       (if col (dugtrio-focus-stack-column! index) (dugtrio-descend! path))
       ;; Entering immediately invalidates the preview; no cursor debounce needed.
       (set-box! (DugtrioColumn-cursor (dugtrio-active-column)) row)
       (dugtrio-enter-path! entry-path kind))
     event-result/consume]))

(define (dugtrio-handle-jump! state key)
  (define prefix (DugtrioState-jump-prefix state))
  (define targets (DugtrioState-jump-targets state))
  (define labels (unbox targets))
  (define target (and key labels (hash-try-get labels (string-append (unbox prefix) key))))
  (cond
    [(and key labels (equal? (unbox prefix) "")
          (hash-try-get labels (string-append key key)))
     (set-box! prefix key)
     event-result/consume]
    [else
     (set-box! prefix #f)
     (set-box! targets #f)
     (if target (dugtrio-follow-label! target) event-result/consume)]))

(define (dugtrio-handle-keys state event)
  (define ch (key-event-char event))
  (define mods (key-event-modifier event))
  (define plain? (or (not mods) (= mods 0) (= mods key-modifier-shift)))
  (cond
    [(unbox (DugtrioState-jump-prefix state))
     (dugtrio-handle-jump! state (and plain? (char? ch) (string ch)))]
    [(and mods (= mods key-modifier-ctrl) (or (equal? ch #\d) (equal? ch #\u)))
     (define h (unbox (DugtrioState-active-height state)))
     (when (> h 0)
       (dugtrio-move! (* (if (equal? ch #\d) 1 -1) (max 1 (quotient h 2)))))
     event-result/consume]
    ;; Ignore other Ctrl, Alt and Super chords. Shift stays, since "R" and "+"
    ;; can arrive with the shift bit set.
    [(not plain?) event-result/consume]
    [(key-event-down? event) (dugtrio-move! 1) event-result/consume]
    [(key-event-up? event) (dugtrio-move! -1) event-result/consume]
    [(key-event-right? event) (dugtrio-enter! #f)]
    [(key-event-enter? event) (dugtrio-enter! #t)]
    [(key-event-left? event) (dugtrio-back!) event-result/consume]
    [(key-event-escape? event) (dugtrio-close!) event-result/consume]

    [(char? ch)
     (define action (dugtrio-action-for-char ch))
     (if action (dugtrio-command-action! state action) event-result/consume)]

    [else event-result/consume]))

(define (dugtrio-handle-event state event)
  (cond
    ;; Mouse and other non-key events must not dismiss jump mode.
    [(not (key-event? event)) event-result/consume]
    [else
     (set-box! (DugtrioState-blink-bright? state) 'fresh)
     (dugtrio-handle-keys state event)]))

(define (dugtrio-make-component)
  (new-component! "dugtrio" (DugtrioState *dugtrio-session* (box #t) (box #f) (box 0) (box #f) (box #f))
                  dugtrio-render (hash "handle_event" dugtrio-handle-event)))

;;@doc
;; Open the file tree on the current file
(define (dugtrio-open)
  ;; Helix can drop the component without telling us, which leaves the flag
  ;; set. Clear whatever the last session left behind and start again.
  (when *dugtrio-active* (dugtrio-close!))
  (set! *dugtrio-session* (+ *dugtrio-session* 1))
  (set! *dugtrio-stack* (dugtrio-reveal-current-file! (helix-find-workspace)))
  (set! *dugtrio-active* #t)
  (push-component! (dugtrio-make-component)))

;;@doc
;; Close the file tree
(define (dugtrio-close)
  (when *dugtrio-active* (dugtrio-close!)))
