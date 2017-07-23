#!/usr/bin/env racket

#lang racket/base

(require racket/string)
(require racket/cmdline)
(require racket/system)
(require racket/match)
(require racket/port)

(define debug-mode (make-parameter #f))
(define loop-mode (make-parameter #f))
(define loop-delay (make-parameter 5))

(define target-filename
  (command-line
   #:program "spotify-title"
   #:once-each
   [("-d" "--debug") "Include debug printing of actions taken"
    (debug-mode #t)]
   [("-l" "--loop") "Run the script continously with a delay between each run, instead of just once."
    (loop-mode #t)]
   #:args (filename) ; filename to write the title to
   filename))

; utility functions
(define (string-sanitize s)
  (string-trim (string-normalize-spaces s)))

(define (find-parentmost-process pid)
  (define new-pid? (string-sanitize (from-shell (format "ps -p \"~a\" -o ppid=" pid))))
  (cond
    [(string=? new-pid? "1") pid] ; we are at parent, return the pid
    ; otherwise recurse until we find the TRUE VICTOR
    [else (find-parentmost-process new-pid?)]))

(define (from-shell cmd)
  (with-output-to-string
    (lambda () (system cmd))))

(define (write-to-file-if-changed path content)
  (define current-content
    (call-with-input-file
     path
     (lambda (in)
       (read-line in))))
  (cond
    [(string=? content current-content) #f]
    [else
     (begin
       (displayln (format "content changed, writing ~a to ~a!" content path))
       (call-with-output-file
        path
        (lambda (out)
          (display content out))
        #:exists 'replace))]))

; constants
(define pid-and-title-regex #px"[^\\s]+\\s+\\d+\\s+(\\d+)\\s+[^\\s]+\\s+(.+)")

(let loop ([continue? (loop-mode)])
  (define spotify-pid
    (string-sanitize
     (find-parentmost-process
      (string-sanitize
       (from-shell "ps xf | grep 'Spotify' | head -n 1 | awk '{print $1}'")))))
  (define window-titles
    (string-split
     (from-shell "wmctrl -lp") "\n"))
  (for ([t window-titles])
    (define pid-and-title (cdr (regexp-match pid-and-title-regex t)))
    (match pid-and-title
      [(list pid title)
       (when (string=? spotify-pid pid)
         (cond
          [(string=? title "Spotify") (write-to-file-if-changed target-filename "No Song Playing.")]
          [else (write-to-file-if-changed target-filename (format "Now Playing: ~a" title))]))]))
  (cond
    [continue?
     (begin
       (sleep (loop-delay))
       (loop continue?))]
    [else #f]))
