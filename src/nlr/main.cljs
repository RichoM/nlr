(ns nlr.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [oops.core :refer [oget oset! ocall!]]
            [utils.pixi :as pixi]))

(defonce pixi (atom nil))

(defn resize-canvas []
  (when-let [{:keys [html app]} @pixi]
    (doto html
      (oset! :style.height "calc(100% - 20px)")
      (oset! :style.width "calc(100%)"))
    (ocall! app :resize)))


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
        (resize-canvas)
        (let [texture-map (<! (load-textures!))
              container (js/PIXI.Container.)]
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
                                   (pixi/add-to! container)))))))))

(defn init [& args]
  (print "RICHO!")
  (initialize-ui!))

(comment
  @pixi

  (-> @pixi :app pixi/get-screen-center)

  (.addEventListener (js/document.getElementById "exercises")
                     "change"
                     (fn [e] (js/console.log e)
                       (print (oget e :target.value))))


  )