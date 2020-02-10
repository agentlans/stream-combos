(import srfi-1) ;; general list routines
(import srfi-41) ;; streams

(define (subsets lst)
  "Returns a stream of subsets of lst."
  (if (= (length lst) 1)
      (stream '() lst)
      (let ((the-rest (subsets (cdr lst))))
	(stream-append the-rest
		       (stream-map (lambda (x)
				     (cons (car lst) x))
				   the-rest)))))
;; (length (stream->list (subsets '(0 1 2 3))))
;; (time (stream->list 5 (subsets (iota 100))))

(define (permutations lst)
  "Returns a stream of permutations of lst."
  (if (= (length lst) 1)
      (stream lst)
      (stream-concat
       (stream-map (lambda (x)
		     (stream-map (lambda (perm)
				   (cons x perm))
				 (permutations (delete x lst))))
		   (list->stream lst)))))
;; (length (stream->list (permutations '(0 1 2 3))))
;; (stream->list 10 (permutations (iota 100)))

(define (combinations lst k)
  "Returns a stream of combinations of lst with length k."
  (if (= k 0)
      (stream '())
      (stream-concat
       (stream-map (lambda (i)
		     (let ((x (list-ref lst i))
			   (the-rest (drop lst (+ i 1))))
		       (stream-map (lambda (combo)
				     (cons x combo))
				   (combinations the-rest (- k 1)))))
		   (list->stream (iota (length lst)))))))
;; (length (stream->list (combinations (iota 5) 2)))
;; (stream->list 10 (combinations (iota 1000) 6))

(define (partitions n)
  "Returns a stream of integer partitions of n.
Unfortunately slow for n > 20."
  (if (= n 0)
      (stream '())
      (stream-concat
       (stream-map (lambda (i)
		     (stream-map (lambda (x)
				   (cons i x))
				 (stream-filter (lambda (partition)
						  (or (null? partition)
						      (<= (car partition) i)))
						(partitions (- n i)))))
		   (list->stream (iota n 1))))))
;; (stream->list (partitions 5))
;; (length (stream->list (partitions 10)))
