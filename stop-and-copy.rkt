#lang plai/collector

(define to-ptr "uninitialized pointer")
(define from-ptr "uninitialized pointer")

;;Initialize the garbage collector
(define (init-allocator)
 (set! to-ptr 0)
 (set! from-ptr (/ (heap-size) 2))
)

;;get the location of the first of a cons
;;param: a (number) - the location of the cons
;;return: (number) - the location of the first of the cons
(define (gc:first a)
 (heap-ref (+ 1 a)))
 
;;get the location of the rest of a cons
;;param: a (number) - the location of the cons
;;return: (number) - the location of the rest of the cons
(define (gc:rest a)
 (heap-ref (+ 2 a)))
 
;;set the location of the first of a cons
;;param: a (number) - the location of the cons
;;param: f (number) - the location of the first
(define (gc:set-first! a f)
 (if (gc:cons? a)
     (heap-set! (+ 1 a) f)
     (error 'set-first! "expects address of cons")))
 
;;set the location of the rest of a cons
;;param: a (number) - the location of the cons
;;param: r (number) - the location of the rest
(define (gc:set-rest! a r)
 (if (gc:cons? a)
  (heap-set! (+ 2 a) r)
  (error 'set-first! "expects address of cons")))
 
;;determine if a flat is at the location
;;param: a (number) -  the location to check
;;return: (boolean) - whether or not it is a flat
(define (gc:flat? a)
 (eq? (heap-ref a) 'prim))
  
;;determine if a cons is at the location
;;param: a (number) -  the location to check
;;return: (boolean) - whether or not it is a cons
(define (gc:cons? a)
 (eq? (heap-ref a) 'cons))
 
;;dereference a particular address and return the data stored there
;;param: a (number) - the location of the object
;;return: (any) - the data stored at the location
(define (gc:deref a)
 (heap-ref (+ 1 a)))
 
;;allocate a flat space on the heap
;;param: p (any) - the flat data to put on the heap
(define (gc:alloc-flat p)
 (begin
   (when (or (and (> to-ptr from-ptr) (> (+ to-ptr 2) (heap-size)))
             (and (< to-ptr from-ptr) (> (+ to-ptr 2) (/ (heap-size) 2))))
     (stop-and-copy))
   ;;if it's still no good, output an error message
   (when (or (and (> to-ptr from-ptr) (> (+ to-ptr 2) (heap-size)))
             (and (< to-ptr from-ptr) (> (+ to-ptr 2) (/ (heap-size) 2))))
     (error "gc:alloc-flat: out of memory"))
   (heap-set! to-ptr 'prim)
   (heap-set! (+ 1 to-ptr) p)
   (set! to-ptr (+ 2 to-ptr))
   (- to-ptr 2)))
 
;;allocate a cons space on the heap
;;param: f (number) - the location of the first of the cons
;;param: r (number) - the location of the rest of the cons
(define (gc:cons f r)
 (begin
   (when (or (and (> to-ptr from-ptr) (> (+ to-ptr 3) (heap-size)))
             (and (< to-ptr from-ptr) (> (+ to-ptr 3) (/ (heap-size) 2))))
     (stop-and-copy-cons f r))
   ;;if it's still no good, output an error message
   (when (or (and (> to-ptr from-ptr) (> (+ to-ptr 3) (heap-size)))
             (and (< to-ptr from-ptr) (> (+ to-ptr 3) (/ (heap-size) 2))))
     (error "gc:alloc-cons: out of memory"))
   (heap-set! to-ptr 'cons)
   (heap-set! (+ 1 to-ptr) f)
   (heap-set! (+ 2 to-ptr) r)
   (set! to-ptr (+ 3 to-ptr))
   (- to-ptr 3)))

;;==========================================

;;do the stop and copy algorithm
(define (stop-and-copy)
  (begin
    (map copy (get-root-set))
    ;swap the two pointers
    (define temp to-ptr)
    (set! to-ptr from-ptr)
    (set! from-ptr to-ptr)
    
    ;reset the from pointer
    (if (> from-ptr (/ (heap-size) 2))
        (set! from-ptr 0)
        (set! from-ptr (/ (heap-size) 2)))
    )
  )

;;do stop and copy for a cons case
;;param: f (number) - the location of the first
;;param: r (number) - the location of the rest
(define (stop-and-copy-cons f r)
  (begin
    (map copy (get-root-set f r))
    ;swap the two pointers
    (define temp to-ptr)
    (set! to-ptr from-ptr)
    (set! from-ptr to-ptr)
    
    ;reset the from pointer
    (if (> from-ptr (/ (heap-size) 2))
        (set! from-ptr 0)
        (set! from-ptr (/ (heap-size) 2)))
    )
  )

;;copy a piece of data to the from space
;;param: a (number) - the location of the data to copy
(define (copy a)
  (begin
   (define b "uninitialized")
   (when (root? a)
    (set! b (read-root a)))
   (when (not (root? a))
     (set! b a))
   (when (gc:flat? b)
     (heap-set! from-ptr (heap-ref b))
     (heap-set! (+ 1 from-ptr) (heap-ref (+ 1 b)))
     (set! from-ptr (+ 2 from-ptr)))
   (when (and (gc:flat? b) (procedure? (gc:deref b)))
                        (map copy (procedure-roots (gc:deref b))))
   (when (gc:cons? b)
     (heap-set! from-ptr 'cons)
     (heap-set! (+ 1 from-ptr) (copy (gc:first b)))
     (heap-set! (+ 2 from-ptr) (copy (gc:rest b)))
     (set! from-ptr (+ 3 from-ptr))
     )
   
   ;return the pointer to the thing we just copied
   (if (gc:flat? b)
       (- 2 from-ptr)
       (- 3 from-ptr))
   )
  )