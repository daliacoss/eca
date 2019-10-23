(ns eca.core
  (:require [reagent.core :as reagent :refer [atom]]
            [bitset]))

(defonce grid-data (-> #(. bitset Random)
                  (map (range 16))
                  vec
                  atom))

(defonce cell-size (atom 20))

(defn cell [{:keys [size row-index col-index state]}]
  (let [attrs {:width size
               :height size
               :x (* col-index size)
               :y (* row-index size)
               :fill (if-not state "none")}]
    [:rect attrs]))

(defn row [{:keys [col-indices cell-size row-index]}]
  (into [:g]
          (map-indexed
           #(vector cell {:size cell-size
                          :row-index row-index
                          :col-index %2
                          :state true})
           col-indices)))

(defn grid [{:keys [col-index-vecs cell-size]}]
  (into [:g]
          (map-indexed
           #(vector row {:cell-size cell-size :row-index %1 :col-indices %2})
           col-index-vecs)))

(defn flip-bit [coll, row-index, col-index]
  (update coll
          row-index
          (fn [bs]
            (let [newbs (. bs (flip col-index))]
              newbs))))

(defn bitset-nth-triad [bs full-length i]
  (map #(. bs (get (mod (+ % i) full-length))) (range 1 -2 -1)))

(defn bitset-triads [bs full-length]
  (map #(bitset-nth-triad bs full-length %) (range full-length)))

(defn bit-seq-to-int [bs]
  (->> bs
       reverse
       (map-indexed #(bit-shift-left %2 %1))
       (reduce +)))

(defn apply-rule [bs rule length]
  (map #(bit-test rule (bit-seq-to-int %)) (bitset-triads bs length)))

(defn bool-seq-to-bitset [bools]
  (->> bools
       (map-indexed #(when (true? %2) %1))
       (remove nil?)
       clj->js
       bitset))

(defn on-svg-click [e]
  (let [bound (.. e -target getBoundingClientRect)
        s @cell-size
        x (- (. e -clientX) (. bound -left))
        y (- (. e -clientY) (. bound -top))
        col-index (. js/Math floor (/ x s))
        row-index (. js/Math floor (/ y s))]
    (swap! grid-data flip-bit row-index col-index 0)))
    ;(println (. js/Math floor (/ x s)))))

(defn on-cell-mouse-over [e]
  (js/console.log e.button))

(defn app []
  (println (apply-rule (first @grid-data) 1 32))
  (println (. (bool-seq-to-bitset '(false false true false true)) toString))
  [:div {:style {:height "100%"}}
   [:div {:style {:height "320px"}}
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :fill "black"
           :width "100%"
           :height "100%"
           :stroke "none"
           :pointerEvents "all"
           :shape-rendering "crispEdges"}
     [grid {:col-index-vecs (map #(js->clj (. % toArray)) @grid-data)
            :cell-size @cell-size}]
     [:rect {:fill "none"
             :onClick on-svg-click
             ;:onMouseOver on-cell-mouse-over
             :width (* 20 32)
             :height (* 20 16)}]]
    [:button "test"]]
   ])

(defn start []
  (reagent/render-component [app]
                            (. js/document (getElementById "app"))))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))
