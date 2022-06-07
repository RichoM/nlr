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

(defn init [& args]
  (print "RICHO!")
  (go (let [html (js/document.getElementById "pixi-container")
            app (pixi/make-application! (js/document.getElementById "pixi-canvas"))]
        (reset! pixi {:html html, :app app})
        (.addEventListener js/window "resize" resize-canvas)
        (resize-canvas)
        (let [mickey (pixi/make-sprite! (<! (pixi/load-texture! "imgs/mickey.png")))]
          (doto mickey
            (pixi/set-position! (pixi/get-screen-center app))
            (pixi/add-to! (oget app :stage)))))))

(comment
 @pixi
  
  (-> @pixi :app pixi/get-screen-center)


  
  
  )