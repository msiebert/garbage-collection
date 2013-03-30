#lang plai/collector

(define free-list empty)

;;Initialize the free list recursively
;;param: a (number) - the current address we're adding to the free list
;;param: b (number) - the maximum address (heap-size)
;;return: (listof number) - the free list
(define (init-free-list a b)
 (if (<= (+ a 4) b)
  (cons a (init-free-list (+ 4 a) b))
  empty))

;;Initialize the garbage collector (does free list initialization)
(define (init-allocator)
 (set! free-list (init-free-list 0 (heap-size)))
)

;;dereference a particular address and return the data stored there
;;param: a (number) - the location of the object
;;return: (any) - the data stored at the location
(define (gc:deref a)
 (heap-ref (+ 2 a)))

;;set the location of the rest of a cons
;;param: a (number) - the location of the cons
;;param: r (number) - the location of the rest
(define (gc:set-rest! a r)
 (heap-set! (+ 3 a) r))

;;determine if a cons is at the location
;;param: a (number) -  the location to check
;;return: (boolean) - whether or not it is a cons
(define (gc:cons? a)
 (eq? (heap-ref a) 'cons))

;;get the location of the first of a cons
;;param: a (number) - the location of the cons
;;return: (number) - the location of the first of the cons
(define (gc:first a)
 (heap-ref (+ 2 a)))

;;get the location of the rest of a cons
;;param: a (number) - the location of the cons
;;return: (number) - the location of the rest of the cons
(define (gc:rest a)
 (heap-ref (+ 3 a)))

;;determine if a flat is at the location
;;param: a (number) -  the location to check
;;return: (boolean) - whether or not it is a flat
(define (gc:flat? a)
 (eq? (heap-ref a) 'prim))

;;allocate a flat space on the heap
;;param: p (any) - the flat data to put on the heap
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

;;allocate a cons space on the heap
;;param: f (number) - the location of the first of the cons
;;param: r (number) - the location of the rest of the cons
(define (gc:cons f r)
 (begin
  (when (empty? free-list)
   (map mark (get-root-set f r))
   (sweep 0))
  (when (empty? free-list)
   (error 'gc:alloc-flat "out of memory"))
  (define block (first free-list))
  (heap-set! block 'cons)
  (heap-set! (+ 1 block) 'clear)
  (heap-set! (+ 2 block) f)
  (heap-set! (+ 3 block) r)
  (set! free-list (rest free-list))
  block))

;;set the location of the first of a cons
;;param: a (number) - the location of the cons
;;param: f (number) - the location of the first
(define (gc:set-first! a f)
 (if (gc:cons? a)
  (heap-set! (+ 2 a) f)
  (error 'set-first! "expects address of cons")))

; ============================================================

;;mark a location and all its references to keep them from being
;;collected
;;param: a (number or root) - the location or root to mark
(define (mark a)
 (begin
   (define b a)
   (when (root? a)
     (set! b (read-root a)))
   (heap-set! (+ 1 b) 'mark)
   (when (and (gc:flat? b) (procedure? (gc:deref b)))
                        (map mark (procedure-roots (gc:deref b))))
   (when (gc:cons? b)
     (mark (gc:first b)) 
     (mark (gc:rest b)))
 )
)

;;mark everything in the root set
(define (mark-all)
 (map mark (get-root-set))
)

;;delete everything that is not marked and put it in the free list
;;param: a (number) - the memory address we're currently sweeping
(define (sweep a)
 (when (<= (+ 4 a) (heap-size))
  (begin
   (if (eq? (heap-ref (+ 1 a)) 'clear)
    (begin
     (set! free-list (cons a free-list))
     (print free-list)
     (heap-set! a #f)
     (heap-set! (+ 1 a) #f)
     (heap-set! (+ 2 a) #f)
     (heap-set! (+ 3 a) #f))
    (heap-set! (+ 1 a) 'clear))
   (sweep (+ 4 a)))))
