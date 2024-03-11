;ColorHistogram
#lang racket
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
      (readhelper file)
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

; Note: This isn't needed anymore since I added the k value to the addToPQ function
(define (shortenList lst k)
  (if (<= k 0)
      '() ; Return an empty list if k is zero or negative
      (take lst k)))

(define (take lst k)
  (if (or (null? lst) (= k 0))
      '() ; Return an empty list if the input list is empty or k is zero
      (cons (car lst) (take (cdr lst) (- k 1)))))




(define (addToPQ hist imageRel k)
  (define (insert-at index element lst)
    (if (= index 0)
        (cons element lst)
        (cons (car lst) (insert-at (- index 1) element (cdr lst)))))

  (define (add-to-pq-helper pq element)
    (if (null? pq)
        (list element)
        (if (<= (car pq) element)
            (cons element pq)
            (cons (car pq) (add-to-pq-helper (cdr pq) element)))))

  (define (truncate-pq pq size)
    (if (or (null? pq) (= size 0))
        '()
        (cons (car pq) (truncate-pq (cdr pq) (- size 1)))))

  (let* ((max-size k)
         (updated-pq (add-to-pq-helper hist imageRel)))
    (if (> (length updated-pq) max-size)
        (truncate-pq updated-pq max-size)
        updated-pq)))




(define (file-exists-in-directory? directory file-name)
  (file-exists? (build-path directory file-name)))





(define (main-fn qFile checkedFile returnLst)
  (if (not (file-exists? (build-path checkedFile)))
      '()
      (addToPQ returnLst
               (relation
                (normalizeH
                 (colorHistogram2 qFile) (getNumPixels (colorHistogram2 qFile)
                                                       )
                 )
                (normalizeH
                 (colorHistogram2 checkedFile) (getNumPixels (colorHistogram2 checkedFile))
                 )
                )
               5)
      )
  )



(define h1 (colorHistogram2 "imageDataset2_15_20/25.jpg.txt"))
(define h2 (colorHistogram2 "queryImages/q00.jpg.txt"))
(define h3 (normalizeH h1 (getNumPixels h1)))
(define h4 (normalizeH h2 (getNumPixels h2)))

(main-fn "imageDataset2_15_20/25.jpg.txt" "queryImages/q00.jpg.txt" '())
