#lang racket/gui

(require ffi/unsafe
         ffi/unsafe/define
         racket/enter
         setup/dirs)

(define-ffi-definer define-syntect-easy
  (ffi-lib "syntect_easy_ffi"
           #:get-lib-dirs (lambda ()
                            (append (list
                                     (string->path "./syntect-easy-ffi/target/debug")
                                     (string->path "./syntect-easy-ffi/target/release"))
                                    (get-lib-search-dirs)))))

(define-cstruct _Color ([r _uint8] [g _uint8] [b _uint8] [a _uint8]))
(define-cstruct _Style ([foreground _Color] [background _Color] [font-style _uint8]))
(define-cstruct _StyledString ([style _Style] [string _string/utf-8]))
(define-cstruct _Highlighted ([lines _StyledString-pointer] [count _size]))

(define-syntect-easy load_default_syntaxes (_fun _string/utf-8 -> _pointer))
(define-syntect-easy load_default_themes (_fun _string/utf-8 -> _pointer))
(define-syntect-easy highlight_string (_fun _string/utf-8 _string/utf-8 _pointer _pointer -> _Highlighted))

(define config-dir (build-path (find-system-path 'home-dir) ".config/rackit"))
(define syntaxes (load_default_syntaxes (path->string (build-path config-dir "syntaxes"))))
(define themes (load_default_themes (path->string (build-path config-dir "themes"))))

(define-syntax-rule (ignore-result body ...) ((lambda () body ... (void))))

(define file-to-open
  (command-line
   #:program "rackit"
   #:args ([filename "main.rkt"])
   filename))

(define file-ext (last (string-split file-to-open ".")))
(define content (file->string file-to-open))

(define f (new frame% [label "Rackedit"] [width 800] [height 600]))
(define c (new editor-canvas% [parent f]))
(define my-text% (class text%
                   (super-new)
                   (define/private (highlight-range #:start [start 0] #:end [end 'eof])
                     (define cur_offset 0)
                     (let* ([text (send this get-text start end #t)]
                            [highlighted (highlight_string text file-ext syntaxes themes)])
                       (for ([i (Highlighted-count highlighted)])
                         (let* ([line (ptr-ref (Highlighted-lines highlighted) _StyledString i)]
                                [style (StyledString-style line)]
                                [foreground (Style-foreground style)]
                                [line-text (StyledString-string line)])
                           (define d (new style-delta%))
                           (send d set-delta-foreground (make-object color% (Color-r foreground) (Color-g foreground) (Color-b foreground)))
                           (send this change-style d (+ start cur_offset) (+ start cur_offset (string-length line-text)))
                           (set! cur_offset (+ cur_offset (string-length line-text)))))))
                   (define/augment (after-delete start len)
                     (let ([start-pos (send this line-start-position (send this position-line start))]
                           [end-pos (send this line-end-position (send this position-line start))])
                       (highlight-range #:start start-pos #:end end-pos)))
                   (define/augment (after-insert start len)
                     (let ([start-pos (send this line-start-position (send this position-line start))]
                           [end-pos (send this line-end-position (send this position-line (+ start len)))])
                       (highlight-range #:start start-pos #:end end-pos)))))
(define t (new my-text%))
(send c set-editor t)

(define d (new style-delta%))
(send d set-face "Hack Nerd Font Mono")
(send t change-style d)

(send f show #t)

(define keymap (new keymap%))
(add-text-keymap-functions keymap)
(send keymap map-function "c:c" "copy-clipboard")
(send keymap map-function "c:x" "cut-clipboard")
(send keymap map-function "c:v" "paste-clipboard")
(send keymap map-function "c:z" "undo")
(send keymap map-function "c:Z" "redo")
(send keymap map-function "c:left" "backward-word")
(send keymap map-function "c:right" "forward-word")
(send keymap map-function "c:s:left" "backward-select-word")
(send keymap map-function "c:s:right" "forward-select-word")
(send keymap map-function "home" "beginning-of-line")
(send keymap map-function "end" "end-of-line")
(send keymap map-function "s:home" "select-to-beginning-of-line")
(send keymap map-function "s:end" "select-to-end-of-line")
(send keymap add-function "save-file"
      (lambda (object key-event)
        (display-to-file (send object get-text) file-to-open #:mode 'binary #:exists 'replace)))
(send keymap map-function "c:s" "save-file")
(send t set-keymap keymap)
(send t set-max-undo-history 'forever) ; enable undo/redo

; Call the user's config if it exists
(define-namespace-anchor a)
(let ([config-rkts (build-path config-dir "config.rkts")])
  (when (file-exists? config-rkts)
    (parameterize ([current-namespace (namespace-anchor->namespace a)])
      (load config-rkts))))

(ignore-result (send t insert-file file-to-open))
(send t move-position 'home)
(send c focus)
