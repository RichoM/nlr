(ns nlr.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [oops.core :refer [oget oset! ocall!]]
            [utils.pixi :as pixi]))

(defonce pixi (atom nil))

(defn resize-canvas []
  (when-let [{:keys [html app]} @pixi]
    (doto html
      (oset! :style.height "calc(100% - 20px)")
      (oset! :style.width "calc(50%)"))
    (ocall! app :resize)))

(def cell-width 72)
(def cell-height 78)

(def offsets {"1_1" [-72 -111]
              "1_2" [-72 -111]
              "2_1" [-72 77]
              "2_2" [-72 77]
              "3_1" [0 -38]
              "3_2" [0 115]
              "4_1" [108 115]
              "4_2" [108 115]
              "5_2" [-36 -231]})

(defn load-textures! []
  (go (let [names ["1_1" "1_2" "2_1" "2_2" "3_1" "3_2" "4_1" "4_2" "5_2"]
            textures (<! (->> names
                              (map #(str "imgs/" % ".png"))
                              (map pixi/load-texture!)
                              (a/map vector)))]
        (zipmap names textures))))

(defn initialize-ui! []
  (go (let [html (js/document.getElementById "pixi-container")
            app (pixi/make-application! (js/document.getElementById "pixi-canvas"))]
        (reset! pixi {:html html, :app app})
        (.addEventListener js/window "resize" resize-canvas)
        ;(resize-canvas)
        (let [texture-map (<! (load-textures!))
              container (js/PIXI.Container.)
              line (js/PIXI.Graphics.)]
          (doto container
            (pixi/set-position! (pixi/get-screen-center app))
            (pixi/add-to! (oget app :stage)))
          (.addEventListener (js/document.getElementById "exercises")
                             "change"
                             (fn [e]
                               (let [value (oget e :target.value)]
                                 (print value)
                                 (ocall! container :removeChildren)
                                 (doto (pixi/make-sprite! (texture-map value))
                                   (pixi/add-to! container))
                                 (let [[x0 y0] (offsets value)]
                                   (doto line
                                     (ocall! :clear)
                                     (ocall! :lineStyle (clj->js {:width 3
                                                                  :color 0x5555ff
                                                                  :alpha 1}))
                                     (ocall! :moveTo x0 y0))
                                   (doseq [[x1 y1] [[1 0]
                                                    [1 1]
                                                    [0 1]
                                                    [0 2]
                                                    [1 2]
                                                    [2 2]
                                                    [2 1]
                                                    [2 0]]]
                                     (ocall! line :lineTo
                                             (+ x0 (* cell-width x1))
                                             (+ y0 (* cell-height y1))))
                                   (doto line
                                     (pixi/add-to! container))))))))))

(defn init [& args]
  (print "RICHO!")
  (initialize-ui!))

(defn ^:dev/before-load-async reload-begin* [done]
  (go (<! (initialize-ui!))
      (done)))

(defn ^:dev/after-load-async reload-end* [done]
  (try
    (let [[old _] (reset-vals! pixi nil)]
      (when-let [app (:app old)]
        (ocall! app :destroy true true)))
    (catch :default err
      (print "ERROR" err)))
  (done))


(comment
  @pixi

  (-> @pixi :app pixi/get-screen-center)

  (.addEventListener (js/document.getElementById "exercises")
                     "change"
                     (fn [e] (js/console.log e)
                       (print (oget e :target.value))))


  )