(ns eca.core
  (:require [reagent.core :as reagent :refer [atom]]))

(defn rand-int-with-cardinality [x, nbits]
  (reduce bit-set 0 (take x (shuffle (range nbits)))))

(defn rand-grid
  ([nr nc]
   ; bit-set doesn't return large unsigned integers, which matters for rand-int
   (let [u (reduce * (repeat nc 2))]
     (repeatedly nr (partial rand-int u))))
  ([nr nc x]
   (if (not x)
       (rand-grid nr nc)
       (repeatedly nr (partial rand-int-with-cardinality x nc)))))

(defonce num-rows (atom 32))
(defonce num-cols (atom 32))
(defonce rule (atom (rand-int 256)))
(defonce iterate-parallel? (atom true))
(defonce cell-size (atom 20))
(defonce playing? (atom false))
(defonce grid-data (-> (rand-grid @num-rows @num-cols)
                       vec
                       atom))
(defonce reset-menu-state (atom {:cardinality-on false
                                 :reset-method nil
                                 :cardinality 1}))
(defonce interval-id (atom nil))
(defonce fps (atom 4))

(defn power-of-2 [n]
  (reduce * (repeat n 2)))

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
         #(if @iterate-parallel?
              (vec (for [x %]
               (apply-rule x @rule @num-cols)))
              (do (conj
               (subvec % 1)
               (apply-rule (peek %) @rule @num-cols))))))

(defn on-iterate-checkbox-change [e]
  (reset! iterate-parallel? (.. e -target -checked)))

(defn on-reset-form-submit [e]
  (let [{:keys [reset-method cardinality-on cardinality]} @reset-menu-state
        new-data (condp = reset-method
                        "randomize" (rand-grid
                                     @num-rows
                                     @num-cols
                                     (if cardinality-on cardinality))
                        "all-on" (repeat
                                  @num-rows
                                  (dec (power-of-2 @num-cols)))
                        "all-off" (repeat @num-rows 0))]
    (reset! grid-data (vec new-data))
    (. e preventDefault)))

(defn radio [{:keys [id] :as props}]
  [:input (conj {:value (props :id)
                 :type "radio"} props)])

(defn on-reset-form-change [e]
  (let [target (. e -target)
        k (keyword (. target -name))
        t (keyword (. target -type))
        v (condp = t :checkbox (. target -checked)
                     :number (js/Number (. target -value))
                     (. target -value))]
    (swap! reset-menu-state assoc k v)))

(defn set-interval-if-nil [x speed]
  (or x (do
          (advance-grid)
          (. js/window (setInterval advance-grid (/ 1000 speed))))))

(defn clear-interval-if-not-nil [x]
  (if x (. js/window clearInterval x)))

(defn set-interval [a speed]
  (reset! a (. js/window (setInterval advance-grid (/ 1000 speed)))))

(defn clear-interval [a]
  (swap! a (. js/window -clearInterval)))

(defn timer [initial-props]
  (let [id (atom nil)]
    (reagent/create-class
     {:reagent-render
      (fn [{:keys [on fps]}]
        (clear-interval id)
        (if on (set-interval id fps))
        nil)
      :component-will-unmount
      (fn [] (clear-interval id))}))) 

(defn reset-menu []
 (let []
   (fn [{:keys [cardinality-on cardinality reset-method]}]
     [:form {:onChange on-reset-form-change
             :onSubmit on-reset-form-submit}
      [:fieldset
       [:legend "Reset grid"]
       [:div
        [radio {:name "reset-method"
                :id "randomize"}]
                ;:checked (nil? reset-method)}]
        [:label {:for "randomize"} "Randomize"]
        [:div.indented
         [:input {:type "checkbox"
                  :disabled (not= reset-method "randomize")
                  :name "cardinality-on"
                  :id "cardinality-on"}]
         [:label {:for "cardinality-on"} "With cardinality: "]
         [:input {:type "number"
                  :value cardinality
                  :min 1
                  :max 31 ; TODO change to num-cols - 1
                  :name "cardinality"
                  :disabled (not cardinality-on)}]]]
       [:div
        [radio {:name "reset-method" :id "all-off"}]
        [:label {:for "all-off"} "All off"]]
       [:div
        [radio {:name "reset-method" :id "all-on"}]
        [:label {:for "all-on"} "All on"]]
       [:div.flex
        [:button {:disabled (not reset-method)} "Reset"]]]])))
 

(defn controls []
  [:div#controls
   [:fieldset [:div.flex
    [:button
     {:type "button" :onClick on-play-button-click}
     (if @playing? "Pause" "Play")]
    [:button
     {:type "button" :onClick advance-grid :disabled @playing?}
     "Step"]
    [:div.multiline
     [:input {:type "range"
              :min 1
              :max 15
              :value @fps
              :id "speed"
              :onChange #(reset! fps (.. % -target -value))}]
     [:label {:for "speed"} "Speed: "]
     @fps]]]
   [:fieldset 
    [:legend "Configure"]
    [:div.flex
     [:div
      [:label {:for "rule"} "Rule: "]
      [:input {:type "number"
               :value @rule
               :onChange #(reset! rule (.. % -target -value))
               :min 0
               :id "rule"
               :max 255}]]
     [:div.multiline
      [:input {:type "checkbox"
               :id "iterate-parallel"
               :checked @iterate-parallel?
               :onChange on-iterate-checkbox-change}]
      [:label {:for "iterate-parallel"} "Parallel iteration"]]]]
   [reset-menu @reset-menu-state]])
 
(defn app []
  [:div#app-inner
   [timer {:on @playing? :fps @fps }]
   (let [s (* @cell-size @num-rows)]
     [:div#svg-container {:style {:height s :width (- s 2)}}
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
               :height (* @cell-size @num-rows)}]]])
   [controls]])

(defn on-window-resize []
  (let [w (. js/window -innerWidth)
        min-w-before-shrink 640
        default-cell-size 20
        rescale-amt (min 1 (/ w min-w-before-shrink))]
    (reset! cell-size (* default-cell-size rescale-amt))))

(defn start []
  (set! (. js/window -onresize) on-window-resize)
  (on-window-resize)
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
