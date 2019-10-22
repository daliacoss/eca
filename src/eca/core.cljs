(ns eca.core
  (:require [reagent.core :as reagent :refer [atom]]
            [bitset]))

(defonce bitsets (-> #(. bitset Random)
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

(defn on-svg-click [e]
  (let [bound (.. e -target getBoundingClientRect)
        s @cell-size
        x (- (. e -clientX) (. bound -left))
        y (- (. e -clientY) (. bound -top))
        col-index (. js/Math floor (/ x s))
        row-index (. js/Math floor (/ y s))]
    (swap! bitsets flip-bit row-index col-index 0)))
    ;(println (. js/Math floor (/ x s)))))

(defn app []
  [:svg {:xmlns "http://www.w3.org/2000/svg"
         :fill "black"
         :width "100%"
         :height "100%"
         :stroke "none"
         :pointerEvents "all"
         :shape-rendering "crispEdges"}
   [grid {:col-index-vecs (map #(js->clj (. % toArray)) @bitsets)
          :cell-size @cell-size}]
   [:rect {:fill "none"
           :onClick on-svg-click
           :width (* 20 32)
           :height (* 20 16)}]])

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
