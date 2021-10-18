#lang racket/gui

(require ffi/unsafe
         ffi/unsafe/define
         setup/dirs)

(provide
 run-rackit
 get-frame
 set-theme)

(define-ffi-definer define-syntect-easy
  (ffi-lib (if (eq? (system-type 'os) 'windows) "syntect_easy_ffi" "libsyntect_easy_ffi")
           #:get-lib-dirs (lambda ()
                            (append (list
                                     (string->path "./syntect-easy-ffi/target/debug")
                                     (string->path "./syntect-easy-ffi/target/release"))
                                    (get-lib-search-dirs)))))

(define-cstruct _Color ([r _uint8] [g _uint8] [b _uint8] [a _uint8]))
(define-cstruct _OptionColor ([present _uint8] [color _Color]))
(define-cstruct _Style ([foreground _Color] [background _Color] [font-style _uint8]))
(define-cstruct _StyledString ([style _Style] [string _string/utf-8]))
(define-cstruct _Highlighted ([lines _StyledString-pointer] [count _size]))

(define-syntect-easy load_default_syntaxes (_fun _string/utf-8 -> _pointer))
(define-syntect-easy load_default_themes (_fun _string/utf-8 -> _pointer))
(define-syntect-easy get_theme_setting (_fun _string/utf-8 _string/utf-8 _string/utf-8 -> _OptionColor))
(define-syntect-easy highlight_string (_fun _string/utf-8 _string/utf-8 _string/utf-8 _pointer _pointer -> _Highlighted))

(define config-dir (build-path (find-system-path 'home-dir) ".config/rackit"))
(define theme-dir (path->string (build-path config-dir "themes")))
(define syntaxes (load_default_syntaxes (path->string (build-path config-dir "syntaxes"))))
(define themes (load_default_themes theme-dir))

(define-syntax-rule (ignore-result body ...) ((lambda () body ... (void))))

(define (get-frame) frame)
(define frame (void))
(define editor-canvas (void))
(define text (void))

(define theme "Solarized (light)")
(define (set-theme new-theme)
  (set! theme new-theme)
  (let* ([bg (get_theme_setting theme-dir theme "background")]
         [bg-color (OptionColor-color bg)]
         [fg (get_theme_setting theme-dir theme "foreground")]
         [fg-color (OptionColor-color fg)])
    (when (eq? (OptionColor-present bg) 1)
      (send editor-canvas set-canvas-background (make-object color% (Color-r bg-color) (Color-g bg-color) (Color-b bg-color))))
    (when (eq? (OptionColor-present fg) 1)
      (define d (new style-delta%))
      (send d set-delta-foreground (make-object color% (Color-r fg-color) (Color-g fg-color) (Color-b fg-color)))
      (send text change-style d))))

(define (run-rackit #:init [init (lambda () (void))] #:config [config (lambda () (void))])
  (init)
  (define file-to-open
    (command-line
     #:program "rackit"
     #:args ([filename "main.rkt"])
     filename))

  (define file-ext (last (string-split file-to-open ".")))
  (define content (file->string file-to-open))

  (set! frame (new frame% [label "Rackit"] [width 800] [height 600]))
  (set! editor-canvas (new editor-canvas% [parent frame]))
  (define my-text% (class text%
                     (super-new)
                     (define/private (highlight-range #:start [start 0] #:end [end 'eof])
                       (define cur_offset 0)
                       (let* ([text (send this get-text start end #t)]
                              [highlighted (highlight_string text file-ext theme syntaxes themes)])
                         (for ([i (Highlighted-count highlighted)])
                           (let* ([line (ptr-ref (Highlighted-lines highlighted) _StyledString i)]
                                  [style (StyledString-style line)]
                                  [foreground (Style-foreground style)]
                                  [background (Style-background style)]
                                  [line-text (StyledString-string line)])
                             (define d (new style-delta%))
                             (send d set-delta-foreground (make-object color% (Color-r foreground) (Color-g foreground) (Color-b foreground)))
                             (send d set-delta-background (make-object color% (Color-r background) (Color-g background) (Color-b background)))
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
  (set! text (new my-text%))
  (send editor-canvas set-editor text)

  (define d (new style-delta%))
  (send d set-face "Hack Nerd Font Mono")
  (send text change-style d)

  (send frame show #t)

  (config)

  (define keymap (new keymap%))
  (add-text-keymap-functions keymap)
  (send keymap map-function "c:a" "select-all")
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
  (send keymap add-function "open-file"
        (lambda (object key-event)
          (let ([file-to-open (get-file "Open file" (get-frame) (current-directory))])
            (when file-to-open
              (send object erase)
              (send object insert-file file-to-open)
              (current-directory (build-path file-to-open 'up))))))
  (send keymap map-function "c:o" "open-file")
  (send text set-keymap keymap)
  (send text set-max-undo-history 'forever) ; enable undo/redo

  (set-theme theme)
  (ignore-result (send text insert-file file-to-open))
  (current-directory (build-path file-to-open 'up))
  (send text move-position 'home)
  (send editor-canvas focus))
