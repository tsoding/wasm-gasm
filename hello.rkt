#lang racket

(define *width* 100)
(define *height* 100)

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
                  (br_if ,quit
                         (i32.ge_s (get_local ,local-i)
                                   ,stack-high))
                  (block $continue
                   ,@body)
                  (set_local ,local-i (i32.add (get_local ,local-i) (i32.const 1)))
                  (br ,top)))))

(print
 `(module
      (func $print (import "imports" "print")
            (param i32))
      (func $fill_rect (import "imports" "fill_rect")
            (param f32)
            (param f32)
            (param f32)
            (param f32))
      (func $clear (import "imports" "clear"))
      (func $rand (import "imports" "rand") (result i32))
      ;; TODO: calculate amount of pages based on *width* and *height*
      (memory (export "display") 32 32)

      (func $memcpy
            (param $dst i32)
            (param $src i32)
            (param $size i32)
            (block $exit
                   (loop $top
                         (br_if $exit (i32.eqz (get_local $size)))
                         (i32.store8 (get_local $dst) (i32.load8_s (get_local $src)))
                         (set_local $size (i32.sub (get_local $size) (i32.const 1)))
                         (set_local $dst (i32.add (get_local $dst) (i32.const 1)))
                         (set_local $src (i32.add (get_local $src) (i32.const 1)))
                         (br $top))))

      (func $neighbors (export "neighbors")
            (param $row0 i32)
            (param $col0 i32)
            (result i32)
            (local $drow i32)
            (local $dcol i32)
            (local $row i32)
            (local $col i32)
            (local $result i32)
            (set_local $result (i32.const 0))
            ,(for-local
              '$drow '(i32.const -1) '(i32.const 2)
              (for-local
               '$dcol '(i32.const -1) '(i32.const 2)
               ;; Neighbor check
               '(br_if 0
                       (i32.and
                        (i32.eqz (get_local $drow))
                        (i32.eqz (get_local $dcol))))
               '(set_local $row (i32.add (get_local $row0) (get_local $drow)))
               '(set_local $col (i32.add (get_local $col0) (get_local $dcol)))

               ;; Boundary for row
               '(br_if 0 (i32.lt_s (get_local $row) (i32.const 0)))
               `(br_if 0 (i32.ge_s (get_local $row) (i32.const ,*height*)))

               ;; Boundary for col
               '(br_if 0 (i32.lt_s (get_local $col) (i32.const 0)))
               `(br_if 0 (i32.ge_s (get_local $col) (i32.const ,*width*)))

               ;; Check if alive
               `(br_if 0 (i32.eqz
                          (i32.load8_s
                           (i32.add (i32.mul (get_local $row)
                                             (i32.const ,*width*))
                                    (get_local $col)))))

               ;; Increment amount of neighbors
               `(set_local $result (i32.add (get_local $result) (i32.const 1)))))
            (get_local $result))

      (func $set_cell
            (param $row i32)
            (param $col i32)
            (param $value i32)
            (i32.store8
             (i32.add
              (i32.mul
               (get_local $row)
               (i32.const ,*width*))
              (get_local $col))
             (get_local $value)))

      (func $init_single_glider
            (call $set_cell (i32.const 0) (i32.const 1) (i32.const 1))
            (call $set_cell (i32.const 1) (i32.const 2) (i32.const 1))
            (call $set_cell (i32.const 2) (i32.const 0) (i32.const 1))
            (call $set_cell (i32.const 2) (i32.const 1) (i32.const 1))
            (call $set_cell (i32.const 2) (i32.const 2) (i32.const 1)))

      (func $init_random
            (local $row i32)
            (local $col i32)
            ,(for-local
              '$row '(i32.const 0) `(i32.const ,*height*)
              (for-local
               '$col '(i32.const 0) `(i32.const ,*width*)
               `(call $set_cell
                      (get_local $row)
                      (get_local $col)
                      (i32.rem_u
                       (call $rand)
                       (i32.const 2))))))

      (func (export "init")
            (call $init_random))

      (func (export "next")
            (local $row i32)
            (local $col i32)
            (local $index i32)
            (local $n i32)
            ,(for-local
              '$row '(i32.const 0) `(i32.const ,*height*)
              (for-local
               '$col '(i32.const 0) `(i32.const ,*width*)
               `(set_local $index
                           (i32.add
                            (i32.mul
                             (get_local $row)
                             (i32.const ,*width*))
                            (get_local $col)))
               `(set_local $n (call $neighbors (get_local $row) (get_local $col)))
               `(if (i32.eqz (i32.load8_s (get_local $index)))
                    ;; dead
                    (then
                     (i32.store8
                      (i32.add
                       (get_local $index)
                       (i32.const ,(* *width* *height*)))
                      (i32.eq (get_local $n) (i32.const 3))))
                    ;; alive
                    (else
                     (i32.store8
                      (i32.add
                       (get_local $index)
                       (i32.const ,(* *width* *height*)))
                      (i32.or
                       (i32.eq (get_local $n) (i32.const 2))
                       (i32.eq (get_local $n) (i32.const 3))))))))
            (call $memcpy
                  (i32.const 0)
                  (i32.const ,(* *width* *height*))
                  (i32.const ,(* *width* *height*))))

      (func (export "render")
            (param $width f32)
            (param $height f32)
            (local $cell_width f32)
            (local $cell_height f32)
            (local $row i32)
            (local $col i32)
            (local $i i32)
            (set_local $cell_width (f32.div (get_local $width) (f32.const ,*width*)))
            (set_local $cell_height (f32.div (get_local $height) (f32.const ,*height*)))
            (call $clear)
            ,(for-local
              '$row '(i32.const 0) `(i32.const ,*height*)
              (for-local
               '$col '(i32.const 0) `(i32.const ,*width*)
               `(br_if 0 (i32.eqz
                          (i32.load8_s
                           (i32.add
                            (i32.mul
                             (get_local $row)
                             (i32.const ,*width*))
                            (get_local $col)))))
               `(call
                 $fill_rect
                 (f32.mul
                  (f32.convert_i32_s (get_local $col))
                  (get_local $cell_width))
                 (f32.mul
                  (f32.convert_i32_s (get_local $row))
                  (get_local $cell_height))
                 (get_local $cell_width)
                 (get_local $cell_height)))))

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
