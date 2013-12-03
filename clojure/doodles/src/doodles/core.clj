(ns doodles.core
  (:gen-class))

(defn our-length
  [lst]
  (if (empty? lst)
    0
    (inc (our-length (rest lst)))))

(defn our-every
  [f lst]
  (if (empty? lst)
    true
    (and (f (first lst))
         (our-every f (rest lst)))))

(defn lrec
  ([rec]
     (lrec rec nil))
  ([rec base]
     (fn self [lst]
       (if (empty? lst)
         (if (fn? base)
           (base)
           base)
         (rec (first lst)
              (fn [] (self (rest lst))))))))

(def our-length' (lrec (fn [x f] (inc (f))) 0))
(def our-every' (lrec (fn [x f] (and (odd? x) (f))) true))