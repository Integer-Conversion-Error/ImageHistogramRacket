;ColorHistogram

; File reader functions

; Reads file f and puts numbers into a list
(define (readhelper f)
  (let ((num (read f)))
      (if (number? num)
         (cons num (readhelper f))
         '()
      )
  )
)

; Opens file and sends the input file to readhelper for processing
(define (readfile filename)
  (let ((file (open-input-file filename)))
    (begin
      (read file)
      (readHelper file)
      )
    )
  )

; Function that takes a text file of an image's histogram and returns the histogram as a list
(define (colorHistogram2 image)
  (readfile image)
  )

; Normalizer functions

; Function used to compare two histograms
(define (getNumPixels hist)
  (if (null? hist)
      0
      (+ (car hist) (getNumPixels (cdr hist)))
  )
)

; Function used to normalize a histogram based on its total number of pixels
(define (normalizeH hist totalPixels)
  (if (null? hist)
      '()
      (cons (/ (car hist) totalPixels) (normalizeH (cdr hist) totalPixels))
      )
  )


; Relation functions

; Returns number num rounded to two decimal places
(define (twoDP num)
  (let* ((x (* num 100))(y (round x)))
    (/ y 100)
    )
  )

; Returns R value between two histograms
(define (rValue hist)
  (if (null? hist)
      0
      (+ (car hist) (rValue (cdr hist)))
      )
  )

; Returns the smaller number of num1 and num2
(define (minimum num1 num2)
  (if (< num1 num2)
      num1
      num2
      )
  )

;Compares pixel bins of two histograms
(define (compare hist1 hist2)
  (if (null? hist1)
      '()
      (cons (minimum (car hist1) (car hist2)) (compare (cdr hist1) (cdr hist2)))
      )
  )

;Finds relation value R between 0 and 1 for two histograms
(define (relation hist1 hist2)
  (let* ((t1 (getNumPixels hist1))(t2 (getNumPixels hist2))(h1 (normalizeH hist1 t1))(h2 (normalizeH hist2 t2)))
    (let ((answer (* (rValue (compare h1 h2)) 1.0)))
      (twoDP answer)
      ) 
    )
)

; Comparator Functions

;;; (define (shortenList lst k) ;Shorten given list 'lst' to length K, and return new list
;;;   -1
;;; )

;;; (define (removeItemsFromIndex lst n)
;;;   (if (= (length lst) n)
;;;     '()



;;;   )
;;; )

(define (shortenList lst k)
  (if (<= k 0)
      '() ; Return an empty list if k is zero or negative
      (take lst k)))

(define (take lst k)
  (if (or (null? lst) (= k 0))
      '() ; Return an empty list if the input list is empty or k is zero
      (cons (car lst) (take (cdr lst) (- k 1)))))


(define (addToPQ hist imageRel k) ;Finish later
  -1
  )

(define lst1 '(1 2 4 6 7 8 9 19))
(display (shortenList lst1 5))
;;; (define h1 (colorHistogram2 "25.jpg.txt"))
;;; (define h2 (colorHistogram2 "26.jpg.txt"))
;;; (define h3 (normalizeH h1 (getNumPixels h1)))
;;; (define h4 (normalizeH h2 (getNumPixels h2)))

;;; (relation h3 h4)