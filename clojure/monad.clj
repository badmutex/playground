


(defn return [a]
	 (fn [s] [s a]))


(defn bind [m f]
  (fn [s] (let [[s2 v] (m s)]
			(f v) s2)))

(defn gets []
  (fn [s] [s s]))

(defn puts [s]
  (fn [_] [s nil]))

(defn mods [f]
  (fn [s] [(f s) nil]))