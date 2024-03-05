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
(define file-path "QueryImages/q00.ppm") 
(define lines (read-file-as-list file-path))
(define dimensions (list-ref lines 2))

(define x-len (string->number(substring dimensions 0 3)))
(define y-len (string->number(substring dimensions 4 7)))


(define (arithmetic-shift-right Y X)
  (arithmetic-shift Y (- X)))

; Example usage:
(define Y 255) ; Replace with your desired integer value
(define X 1)  ; Replace with the number of positions to shift

;; (define result (arithmetic-shift-right Y X))
;; (displayln result) ; Display the result

(define (read-first-line-from-files n)
  (define (read-first-line file-path)
    (call-with-input-file file-path
      (lambda (port)
        (read-line port))))
  
  (let ((file-path (format (string-append "QueryImages/q" (number->string 55) ".ppm"))))

    (if (file-exists? file-path)
        (cons (read-first-line file-path) (read-first-line-from-files (+ n 1)))
        (display file-path))))

; Example usage:
(define first-lines (read-first-line-from-files 1))
(displayln first-lines)