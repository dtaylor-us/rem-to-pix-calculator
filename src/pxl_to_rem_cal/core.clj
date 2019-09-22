(ns pxl-to-rem-cal.core
  (:require [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]])
  (:gen-class :main true :prefix myprefix-))

;; TODO coordinates, borders

;; REM CALCULATION
(defn pxl-to-rem
  "[px-input root] [px-input] Calculates rem value from pixel input default root is 16"
  ([px-input root] (float (/ px-input root)))
  ([px-input] (float (/ px-input 16.0))))

(defn calc-rem [data-map]
  (let [{:keys [pxl root schema error]} data-map]
    (cond
      (some? error) error
      (nil? root) (pxl-to-rem pxl)
      :else (pxl-to-rem pxl (edn/read-string root)))))

(defn schem-msg [schema suffix rem]
  (str "schema: " schema ", suffix: " suffix ", rem value: " rem))

;; SCHEMA PROCESSOR
(defn type-schema-processor [rem-value]
  (cond
    (>= rem-value 3)         (schem-msg "type" "1" "3")
    (>= rem-value 2.25)      (schem-msg "type" "2" "2.25")
    (>= rem-value 1.5)       (schem-msg "type" "3" "1.5")
    (>= rem-value 1.25)      (schem-msg "type" "4" "1.25")
    (>= rem-value 1)         (schem-msg "type" "5" "1")
    (>= rem-value 0.875)     (schem-msg "type" "6" "0.875")
    (>= rem-value 0.75)      (schem-msg "type" "7" "0.75")
    :else                    "[E] INVALID TYPE INPUT"))

(defn max-width-schema-processor [rem-value]
  (cond
    (> rem-value 96)       "[W] RESULT EXCEEDS SCHEMA PARAMETERS"
    (>= rem-value 96)      (schem-msg "max-width" "9" "96")
    (>= rem-value 64)      (schem-msg "max-width" "8" "64")
    (>= rem-value 48)      (schem-msg "max-width" "7" "48")
    (>= rem-value 32)      (schem-msg "max-width" "6" "32")
    (>= rem-value 16)      (schem-msg "max-width" "5" "16")
    (>= rem-value 8)       (schem-msg "max-width" "4" "8")
    (>= rem-value 4)       (schem-msg "max-width" "3" "4")
    (>= rem-value 2)       (schem-msg "max-width" "2" "2")
    (>= rem-value 1)       (schem-msg "max-width" "1" "1")
    :else                  "[E] INVALID MAX WIDTH INPUT"))

(defn height-width-schema-processor [rem-value]
  (cond
    (> rem-value 16)      "[W] RESULT EXCEEDS SCHEMA PARAMETERS"
    (>= rem-value 16)     (schem-msg "height & width" "5" "16")
    (>= rem-value 8)      (schem-msg "height & width" "4" "8")
    (>= rem-value 4)      (schem-msg "height & width" "3" "4")
    (>= rem-value 2)      (schem-msg "height & width" "2" "2")
    (>= rem-value 1)      (schem-msg "height & width" "1" "1")
    :else                 "[E] INVALID HEIGHT AND WIDTH INPUT"))

(defn spacing-schema-processor [rem-value]
  (cond
    (> rem-value 16)      "[W] RESULT EXCEEDS SCHEMA PARAMETERS"
    (>= rem-value 16)     (schem-msg "spacing" "7" "16")
    (>= rem-value 8)      (schem-msg "spacing" "6" "8")
    (>= rem-value 4)      (schem-msg "spacing" "5" "4")
    (>= rem-value 2)      (schem-msg "spacing" "4" "2")
    (>= rem-value 1)      (schem-msg "spacing" "3" "1")
    (>= rem-value 0.5)    (schem-msg "spacing" "2" "0.5")
    (>= rem-value 0.25)   (schem-msg "spacing" "1" "0.25")
    :else                 "[E] INVALID SPACING INPUT"))

(defn schema-processor [input-schema rem-result]
  (do
    (case input-schema
      "s"  (spacing-schema-processor rem-result)
      "hw" (height-width-schema-processor rem-result)
      "mw" (max-width-schema-processor rem-result)
      "t"  (type-schema-processor rem-result)
      nil  "No schema provided"
      "[W] INVALID SCHEMA")))

(defn numeric? [s]
  (if-let [s (seq s)]
    (let [s (if (= (first s) \-) (next s) s)
          s (drop-while #(Character/isDigit %) s)
          s (if (= (first s) \.) (next s) s)
          s (drop-while #(Character/isDigit %) s)]
      (empty? s))))

(defn process-input [args]
  (let [px                        (edn/read-string (first args))
        second-arg                (nth args 1 nil)
        third-arg                 (nth args 2 nil)]
    (cond
      (not (number? px)) {:error "[E] INVALID PX VALUE"}
      (nil? third-arg) (if (numeric? second-arg) {:pxl px :root second-arg} {:pxl px :schema second-arg})
      :else {:pxl px :root second-arg :schema third-arg})))

;; RUNNER
(defn -main [& args]
  (let [data                    (process-input args)
        result                  (cond (contains? data :error) (:error data) :else (calc-rem data))
        tachyon-schema          (schema-processor (:schema data) result)]
    (do
      (pprint (str "[D]::: " data))
      (pprint (str "result: " result))
      (pprint (str "tachyon: " tachyon-schema)))))