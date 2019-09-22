(ns pxl-to-rem-cal.core
(:require [clojure.edn :as edn])
(:gen-class :main true :prefix myprefix-))

(defn pxl-to-rem 
  "[px-input root] [px-input] Calculates rem value from pixel input default root is 16"
  ([px-input root] (float (/ px-input root)))
  ([px-input] (float (/ px-input 16.0))))


(defn -main [& args]
  (let [pxl    (edn/read-string (first args))
        root   (nth args 1 nil)
        result (if (nil? root)
                 (pxl-to-rem pxl)
                 (pxl-to-rem pxl (edn/read-string root)))]
    (print result)))