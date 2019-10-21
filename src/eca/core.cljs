(ns eca.core
  (:require [reagent.core :as reagent :refer [atom]]
            [bitset]))

(defn cell [{:keys [size row-index col-index state]}]
  (let [attrs {:width size
               :height size
               :x (* col-index size)
               :y (* row-index size)
               :fill (if-not state "none")}]
    [:rect attrs]))

(defn row [{:keys [data cell-size row-index]}]
  (println (. data toString))
  (into [:g]
          (map-indexed
           #(vector cell {:size cell-size
                          :row-index row-index
                          :col-index %2
                          :state true})
           (js->clj (. data toArray)))))

(defn grid [{:keys [rows cell-size]}]
  (into [:g]
          (map-indexed
           #(vector row {:cell-size cell-size :row-index %1 :data %2})
           rows)))

(defn app []
  (let [rows (-> #(. bitset Random)
                 (map (range 16))
                 vec
                 atom)]
    (fn []
      [:svg {:xmlns "http://www.w3.org/2000/svg"
             :fill "black"
             :width "100%"
             :height "100%"
             :stroke "none"
             :shape-rendering "crispEdges"
             :pointer-events "all"}
       [grid {:rows @rows :cell-size 20}]])))

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
