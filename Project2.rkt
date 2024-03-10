#lang racket

;;(define (similaritySearch (queryHistogramFilename imageDatasetDirectory) 0))

(define (read-file-as-list file-path)
  (define (read-lines port lines)
    (let ((line (read-line port)))
      (if (eof-object? line)
          (reverse lines)
          (read-lines port (cons line lines)))))
  
  (call-with-input-file file-path
    (lambda (port)
      (read-lines port '()))))

;; Example usage:
(define file-path "imageDataset2_15_20/25.jpg.txt") 
;; (define lines (read-file-as-list file-path))
;; (define dimensions (list-ref lines 2))

;; (define x-len (string->number(substring dimensions 0 3)))
;; (define y-len (string->number(substring dimensions 4 7)))


(define (arithmetic-shift-right Y X)
  (arithmetic-shift Y (- X)))

(define (read-first-line-from-files n)
  (define (read-first-line file-path)
    (call-with-input-file file-path
      (lambda (port)
        (read-line port))))
  
  (let ((file-path (format (string-append "imageDataset2_15_20/" (number->string n) ".jpg.txt"))))

    (if (file-exists? file-path)
        (cons (read-first-line file-path) (read-first-line-from-files (+ n 1)))
        '())))



; Example usage:

(define (read-all-files file-path n)
  (if (file-exists? file-path)
      (cons (read-file-as-list file-path) (read-file-as-list (string-append "imageDataset2_15_20/" (number->string (+ n 1)) ".jpg.txt")) )
      '()))

(define lines (read-all-files file-path 25))
(display lines)


















