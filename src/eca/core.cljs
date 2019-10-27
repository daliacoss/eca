(ns eca.core
  (:require [reagent.core :as reagent :refer [atom]]))

(defn rand-int-with-cardinality [x, nbits]
  (reduce bit-set 0 (take x (shuffle (range nbits)))))

(defn random-grid
  ([nr nc]
   ; bit-set doesn't return large unsigned integers, which matters for rand-int
   (let [u (reduce * (repeat nc 2))]
     (repeatedly nr (partial rand-int u))))
  ([nr nc x]
   (repeatedly nr (partial rand-int-with-cardinality x nc))))

(defonce num-rows (atom 16))
(defonce num-cols (atom 32))
(defonce rule (atom 1))
(defonce iterate-down? (atom false))
(defonce cell-size (atom 20))
(defonce playing? (atom false))
(defonce grid-data (-> (random-grid @num-rows @num-cols)
                       vec
                       atom))

(defn cell [{:keys [size row-index col-index state]}]
  (let [attrs {:width size
               :height size
               :x (* col-index size)
               :y (* row-index size)
               :fill (if-not state "none")}]
    [:rect attrs]))

(defn row [{:keys [row-data cell-size row-index row-length]}]
  (into [:g]
          (map-indexed
           #(vector cell {:size cell-size
                          :row-index row-index
                          :col-index %2
                          :state (bit-test row-data %1) })
           (range row-length))))

(defn grid [{:keys [data cell-size row-length]}]
  (into [:g]
          (map-indexed
           #(vector row {:row-length row-length
                         :cell-size cell-size
                         :row-index %1
                         :row-data %2})
           data)))

(defn bit-value [x i]
  (if (bit-test x i) 1 0))

(defn flip-bit [coll, row-index, col-index]
  (update coll
          row-index
          bit-flip
          col-index))

(defn nth-bit-triad
  ([x full-length i big-endian?]
   (map
    #(bit-value x (mod (+ % i) full-length))
    (if big-endian?
        (range 1 -2 -1)
        (range -1 2 1))))
  ([x full-length i]
   (nth-bit-triad x full-length i false)))

(defn bit-triads [x full-length]
  (map #(nth-bit-triad x full-length %) (range full-length)))

(defn bits-to-int [bits]
  (->> bits
       (map-indexed #(bit-shift-left %2 %1))
       (reduce +)))

(defn apply-rule [x rule length]
  (bits-to-int
    (map #(bit-value rule (bits-to-int %)) (bit-triads x length))))

(defn on-svg-click [e]
  (let [bound (.. e -target getBoundingClientRect)
        s @cell-size
        x (- (. e -clientX) (. bound -left))
        y (- (. e -clientY) (. bound -top))
        col-index (. js/Math floor (/ x s))
        row-index (. js/Math floor (/ y s))]
    (swap! grid-data flip-bit row-index col-index 0)))

(defn on-play-button-click [e]
  (swap! playing? not))

(defn advance-grid []
  (swap! grid-data
         #(if @iterate-down?
              (do (conj
               (subvec % 1)
               (apply-rule (peek %) @rule @num-cols)))
              (vec (for [x %]
               (apply-rule x @rule @num-cols))))))

(defn on-iterate-checkbox-change [e]
  (reset! iterate-down? (.. e -target -checked)))

(defn on-randomize-button-click []
  (reset! grid-data (vec (random-grid @num-rows @num-cols))))

(defn set-interval-if-nil [x]
  (or x (do
          (advance-grid)
          (. js/window (setInterval advance-grid 250)))))

(defn clear-interval-if-not-nil [x]
  (if x (. js/window clearInterval x)))

(defn timer [initial-props]
  (let [id (atom nil)
        id-updater #(if % set-interval-if-nil clear-interval-if-not-nil)]
    (reagent/create-class
     {:reagent-render
      (fn [{:keys [on]}]
        (swap! id (id-updater on)) nil)
      :component-will-unmount
      (fn [] (swap! id clear-interval-if-not-nil))})))

(defn controls []
  [:fieldset
    [:label {:for "rule"} "Rule"]
    [:input {:type "number"
             :value @rule
             :onChange #(reset! rule (.. % -target -value))
             :min 0
             :id "rule"
             :max 255}]
    [:label {:for "iterate-down"} "Iterate downward"]
    [:input {:type "checkbox"
             :id "iterate-down"
             :onChange on-iterate-checkbox-change}]
    [:br]
    [:button {:onClick advance-grid :disabled @playing?} "Step"]
    [:button {:onClick on-play-button-click} (if @playing? "Pause" "Play")]
    [:button {:onClick on-randomize-button-click} "Randomize"]])
 
(defn app []
  [:div
   [timer {:on @playing?}]
   [:div#svg-container
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :fill "black"
           :width "100%"
           :height "100%"
           :stroke "none"
           :pointerEvents "all"
           :shape-rendering "crispEdges"}
     [grid {:data @grid-data
            :cell-size @cell-size
            :row-length @num-cols}]
     [:rect {:fill "none"
             :onClick on-svg-click
             ;:onMouseOver on-cell-mouse-over
             :width (* @cell-size @num-cols)
             :height (* @cell-size @num-rows)}]]
    [controls]
    ]
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
