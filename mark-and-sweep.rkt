#lang plai/collector

(define heap-ptr 'uninitialized-heap-ptr)
(define free-list empty)

(define (init-free-list a b)
 (if (<= (+ a 4) b)
  (cons a (init-free-list (+ 4 a) b))
  empty))

(define (init-allocator)
 (set! heap-ptr 0)
 (set! free-list (init-free-list 0 (heap-size)))
)

(define (gc:deref a)
 (heap-ref (+ 2 a)))

(define (gc:set-rest! a r)
 (heap-set! (+ 3 a) r))

(define (gc:cons? a)
 (eq? (heap-ref a) 'cons))

(define (gc:first a)
 (heap-ref (+ 2 a)))

(define (gc:rest a)
 (heap-ref (+ 3 a)))

(define (gc:flat? a)
 (eq? (heap-ref a) 'prim))

(define (gc:alloc-flat p)
 (begin
  (when (empty? free-list)
   (mark-all)
   (sweep 0))
  (when (empty? free-list)
   (error 'gc:alloc-flat "out of memory"))
  (define block (first free-list))
  (heap-set! block 'prim)
  (heap-set! (+ 1 block) 'clear)
  (heap-set! (+ 2 block) p)
  (heap-set! (+ 3 block) #f)
  (set! free-list (rest free-list))
  block))

(define (gc:cons f r)
 (begin
  (when (> (+ heap-ptr 4) (heap-size))
   (error 'gc:cons "out of memory"))
  (heap-set! heap-ptr 'cons)
  (heap-set! (+ 1 heap-ptr) 'clear)
  (heap-set! (+ 2 heap-ptr) f)
  (heap-set! (+ 3 heap-ptr) r)
  (set! heap-ptr (+ 4 heap-ptr))
  (- heap-ptr 4)))

(define (gc:set-first! a f)
 (if (gc:cons? a)
  (heap-set! (+ 2 a) f)
  (error 'set-first! "expects address of cons")))

; ============================================================

(define (mark a)
 (begin
  (define b (read-root a))
  (heap-set! (+ 1 b) 'mark)
  (when (gc:cons? b) (mark (gc:first b)) (mark gc:rest b))
 )
)

(define (mark-all)
 (map mark (get-root-set))
)

(define (sweep a)
 (when (<= (+ 4 a) (heap-size))
  (begin
   (if (eq? (heap-ref (+ 1 a)) 'clear)
    (begin
     (set! free-list (cons a free-list ))
     (print free-list)
     (heap-set! a #f)
     (heap-set! (+ 1 a) #f)
     (heap-set! (+ 2 a) #f)
     (heap-set! (+ 3 a) #f))
    (heap-set! (+ 1 a) 'clear))
   (sweep (+ 4 a)))))
