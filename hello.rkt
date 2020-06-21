#lang racket

(define *width* 5)
(define *height* 5)

(define (repeat-n-times n . body)
  (let ((quit (gensym "$quit"))
        (top (gensym "$top")))
    `(block ,quit
            (loop ,top
                  (if (i32.le_u (get_local ,n) (i32.const 0)) (br ,quit))
                  ,@body
                  (set_local ,n (i32.sub (get_local ,n) (i32.const 1)))
                  (br ,top)))))


(define (for-local local-i stack-low stack-high . body)
  (let ((quit (gensym "$quit"))
        (top (gensym "$top")))
    `(block ,quit
            (set_local ,local-i ,stack-low)
            (loop ,top
                  (if (i32.ge_u (get_local ,local-i)
                                ,stack-high)
                      (br ,quit))
                  ,@body
                  (set_local ,local-i (i32.add (get_local ,local-i) (i32.const 1)))
                  (br ,top)))))

(print
 `(module
      (func $print (import "imports" "print")
            (param i32))
      (func $print_pair (import "imports" "print_pair")
            (param i32)
            (param i32))
      (memory (export "display") 1 1)

      ;; TODO: $neighbors is not implemented
      (func $neighbors
            (param $row i32)
            (param $col i32)
            (result i32)
            (local $drow i32)
            (local $dcol i32)
            ,(for-local
              '$drow '(i32.const -1) '(i32.const 2)
              (for-local
               '$dcol '(i32.const -1) '(i32.const 2)
               `(i32.add
                 (i32.mul
                  (i32.add
                   (get_local $drow)
                   (get_local $row))
                  (i32.const ,*height*))
                 (i32.add
                  (get_local $dcol)
                  (get_local $col)))))
            (i32.const 0))

      ;; TODO: $next is not implemented
      (func (export "next")
            (local $row i32)
            (local $col i32)
            ,(for-local
              '$row '(i32.const 0) `(i32.const ,*height*)
              (for-local
               '$col '(i32.const 0) `(i32.const ,*width*)
               `(if (i32.eqz
                     (i32.load8_s
                      (i32.add
                       (i32.mul
                        (get_local $row)
                        (i32.const ,*height*))
                       (get_local $col))))
                    (then                 ; dead
                     )
                    (else                 ; alive
                     ))
               '(call $print_pair
                      (get_local $row)
                      (get_local $col)))))

    (func (export "fib")
          (param $n i32)
          (result i32)
          (local $a i32)
          (local $b i32)
          (set_local $a (i32.const 0))
          (set_local $b (i32.const 1))
          ,(repeat-n-times
            '$n
            '(i32.add
              (get_local $a)
              (get_local $b))
            '(set_local $a (get_local $b))
            '(set_local $b))
          (get_local $a)))
 (current-output-port)
 1)
