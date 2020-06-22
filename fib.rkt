#lang racket

(define (repeat-n-times n . body)
  (let ((quit (gensym "$quit"))
        (top (gensym "$top")))
    `(block ,quit
            (loop ,top
                  (if (i32.le_u (get_local ,n) (i32.const 0)) (br ,quit))
                  ,@body
                  (set_local ,n (i32.sub (get_local ,n) (i32.const 1)))
                  (br ,top)))))

(print
 `(module
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
