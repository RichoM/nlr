(ns nlr.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [petitparser.core :as pp]
            [utils.pixi :as pixi]))


(defn group-by-indent [[current & rest]]
  (let [[children siblings] (split-with (fn [next] (> (:indent-level next)
                                                      (:indent-level current)))
                                        rest)
        current (if (seq children)
                  (assoc current :stmts (group-by-indent (vec children)))
                  current)]
    (vec (concat [current] (when (seq siblings)
                             (group-by-indent siblings))))))



(comment
(concat [1] nil)

  (def a (clj->js (list 1 2 3)))

  (.shift a)
  
  (assert false)

  (def stream (a/into-chan [1 2 3]))

  (def lines [{:type ::avanzar :indent-level 0}
              {:type ::girar :indent-level 0}
              {:type ::repetir :indent-level 0}
              {:type ::avanzar :indent-level 1}
              {:type ::avanzar :indent-level 1}
              {:type ::avanzar :indent-level 0}])
  
  


  (group-by-indent lines)
  )

(do
(def grammar {:start (pp/end :lines)
              :lines (pp/separated-by :line (pp/or "\r\n" "\n"))
              :line (pp/seq (pp/star (pp/or "\t" " "))
                            (pp/optional (pp/or :avanzar :girar :repetir)))
              :number (pp/flatten (pp/plus pp/digit))
              :ws (pp/plus (pp/or " " "\t"))
              :ws? (pp/optional :ws)
              :avanzar (pp/seq (pp/or "avanzar" "avanza" "avanzá")
                               :ws?
                               (pp/optional :number)
                               :ws?
                               (pp/optional (pp/or "casilleros" "casillero" "celdas" "celda")))
              :derecha (pp/or "derecha" "der" "d" "right" "r")
              :izquierda (pp/or "izquierda" "izq" "i" "left" "l")
              :girar (pp/seq (pp/or "girar" "gira" "girá")
                             :ws?
                             (pp/optional "a")
                             :ws?
                             (pp/optional "la")
                             :ws?
                             (pp/or :derecha :izquierda))
              :repetir (pp/seq (pp/or "repetir" "repeat" "rep")
                               :ws?
                               (pp/optional :number)
                               :ws?
                               (pp/optional (pp/or "veces" "vez"))
                               :ws?
                               (pp/optional ":"))})

(def transformations 
  {:number (fn [d] (js/parseInt d))
   :lines (fn [lines] (group-by-indent (remove nil? (take-nth 2 lines))))
   :line (fn [[indent action]] (when action
                                 (assoc action
                                        :indent-level (count indent))))
   :avanzar (fn [[_ _ steps]] {:type ::avanzar
                               :steps (or steps 1)})
   :derecha (constantly ::derecha)
   :izquierda (constantly ::izquierda)
   :girar (fn [parts] {:type ::girar
                       :direction (last parts)})
   :repetir (fn [[_ _ times]] {:type ::repetir
                               :times times})}
  )

(def parser (pp/compose grammar transformations))

(defn parse [str]
  (pp/parse parser (str/lower-case str)))
)

(comment
  
  (parse (oget (js/document.getElementById "input") :value))

  (parse 
  "girar izq
  avanzar")
  (parse "Girar izq")
  (parse "Repetir 10 veces")

  )


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
  (go
    (doto (js/document.getElementById "input")
      (.addEventListener "keyup"
                         (fn [e]
                           (let [value (oget e :target.value)
                                 out (js/document.getElementById "variables")]
                             (try
                               (oset! out :innerText (js/JSON.stringify (clj->js (parse value)) nil 2))
                               (catch js/Error ex (oset! out :innerText ex)))))))
    (let [html (js/document.getElementById "pixi-canvas")
          app (pixi/make-application! html)]
      (reset! pixi {:html html, :app app})
        ;(.addEventListener js/window "resize" resize-canvas)
        ;(resize-canvas)
      (let [texture-map (<! (load-textures!))
            container (js/PIXI.Container.)
            line (js/PIXI.Graphics.)]
        (doto container
          (pixi/set-position! [0 0])
          (pixi/add-to! (oget app :stage)))
        (.addEventListener (js/document.getElementById "exercises")
                           "change"
                           (fn [e]
                             (let [selection (oget e :target.value)
                                   texture (texture-map selection)
                                   sprite (pixi/make-sprite! texture)]
                               (ocall! container :removeChildren)
                               (pixi/add-to! sprite container)
                               (print (oget sprite :height))
                               (doto html
                                 (oset! :style.height (str (oget sprite :height) "px"))
                                 (oset! :style.width (str (oget sprite :width) "px")))
                               (ocall! app :resize)
                               (let [[x0 y0] (map +
                                                  (pixi/get-center sprite)
                                                  (offsets selection))]
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
  (do
    (def app (:app @pixi))
    (def html (:html @pixi)))
  
  (js/console.log html)
  (doto html
    (oset! :style.height "300px !important"))
(ocall! app :resize)
  
  (oget html :style.height)
  html

  (go (load-textures!))

  (-> @pixi :app pixi/get-screen-center)

  (.addEventListener (js/document.getElementById "exercises")
                     "change"
                     (fn [e] (js/console.log e)
                       (print (oget e :target.value))))


  )