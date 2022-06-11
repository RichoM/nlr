(ns utils.pixi
  (:require [clojure.core.async :as a :refer [go <!]]
            [oops.core :refer [oget oget+ oset! ocall!]]))

(defn make-application! [html]
  (let [app (js/PIXI.Application. (clj->js {:resizeTo html 
                                            :transparent true
                                            ;:resolution (or js/window.devicePixelRatio 1)
                                            :antialias true}))]
    (ocall! html :appendChild (oget app :view))
    app))

(defn load-texture! [path]
  (let [result (a/promise-chan)]
    (doto (js/PIXI.Loader.)
      (ocall! :add path)
      (ocall! :load (fn [_ resources]
                      (let [texture (oget (aget resources path) :texture)]
                        (a/put! result texture)))))
    result))

(defn make-sprite! [texture]
  (doto (js/PIXI.Sprite. texture)
    (oset! :anchor.x 0.0)
    (oset! :anchor.y 0.0)))

(defn make-label! [text style]
  (doto (js/PIXI.Text. text style)
    (oset! :anchor.x 0.0)
    (oset! :anchor.y 0.0)))

(defn add-child! [parent child]
  (ocall! parent :addChild child))

(defn add-to! [child parent]
  (ocall! parent :addChild child))

(defn get-center [obj]
  [(/ (oget obj :width) 2)
   (/ (oget obj :height) 2)])

(defn get-screen-center [app]
  (get-center (oget app :screen)))

(defn set-position! [obj [x y]]
  (doto obj
    (oset! :x x)
    (oset! :y y)))

(defn set-rotation! [obj rot]
  (oset! obj :rotation rot))

(defn set-height! [obj h]
  (let [ow (oget obj :width)
        oh (oget obj :height)
        w (* ow (/ h oh))]
    (doto obj
      (oset! :width w)
      (oset! :height h))))

(defn add-ticker! [app f]
  (ocall! (oget app :ticker) :add f))


(defn dist [[x y] [x' y']]
  (let [dx (- x' x)
        dy (- y' y)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn draw-dashed-line!
  [g from to & {:keys [gap] :or {gap 10}}]
  (let [[x y] from
        [x' y'] to
        a (js/Math.atan2 (- y' y) (- x' x))
        dy (* gap (js/Math.sin a))
        dx (* gap (js/Math.cos a))
        steps (/ (dist from to) gap)]
    (doseq [[[x0 y0] [x1 y1]]
            (partition 2 (take steps
                               (map vector
                                    (iterate (partial + dx) x)
                                    (iterate (partial + dy) y))))]
      (doto g
        (ocall! :moveTo x0 y0)
        (ocall! :lineTo x1 y1)))))
