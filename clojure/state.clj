
(defn return [v]
  (fn [s] [s v]))

(defn run [m s0]
  (m s0))

(defn bind [m f]
  (fn [s0]
	(let [[s2 v] (run m s0)]
	  [s2 (f v)])))


(defn id [x] x)