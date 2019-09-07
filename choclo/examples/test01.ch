
(letrec
  ((map
     (fun (f list)
       (if (consp list)
         (cons (f (car list))
               (map f (cdr list)))
         '())))

   (mk-counter
     (fun ()
       (let ((n 0))
         (fun ()
           (set n (+ n 1))
          n))))

   (c (mk-counter)))

 (pr (map (fun (x) (cons x (c)))
          '(a b c d))))

