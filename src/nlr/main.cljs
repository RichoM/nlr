(ns nlr.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [petitparser.core :as pp]
            [utils.pixi :as pixi]))

(defn make-robot [direction]
  {:direction direction
   :positions [[0 0]]})


(defmulti run (fn [_robot instr] (:type instr)))

(defmethod run ::girar [robot {:keys [direction]}]
  (let [directions {::N {::derecha ::E
                         ::izquierda ::W}
                    ::E {::derecha ::S
                         ::izquierda ::N}
                    ::S {::derecha ::W
                         ::izquierda ::E}
                    ::W {::derecha ::N
                         ::izquierda ::S}}]
    (update robot :direction #(get-in directions [% direction]))))

(defmethod run ::avanzar [{:keys [direction] :as robot} {:keys [steps]}]
  (let [directions {::N [1 -] ::S [1 +]
                    ::E [0 +] ::W [0 -]}
        [index f] (directions direction)]
    (update robot :positions (fn [positions]
                               (let [pos (peek positions)]                                 
                                 (conj positions (update pos index f steps)))))))

(defmethod run ::repetir [robot {:keys [times stmts]}]
  (loop [i times
         robot robot]
    (if (> i 0)
      (recur (dec i)
             (reduce run robot stmts))
      robot)))

(defmethod run :default [robot _] (js/console.error "ACAACA") robot)

(defn run-program [robot program]
  (reduce run robot program))



(defn accepts-children? [node]
  (contains? #{::repetir ::conditional} (:type node)))

(defn group-by-indent [[current & rest]]
  (when current
    (let [[children siblings] (if (accepts-children? current)
                                (split-with (fn [next] (> (:indent-level next)
                                                          (:indent-level current)))
                                            rest)
                                [[] rest])
          current (if (seq children)
                    (assoc current :stmts (group-by-indent (vec children)))
                    current)]
      (vec (concat [current] (when (seq siblings)
                               (group-by-indent siblings)))))))


(do
(def grammar {:start (pp/end :lines)
              :lines (pp/separated-by :line (pp/or "\r\n" "\n"))
              :line (pp/seq (pp/star (pp/or "\t" " "))
                            (pp/optional (pp/or :avanzar :girar :repetir :if)))
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
                               (pp/optional ":"))
              :condition (pp/or :wall-forward?
                                :wall-left?
                                :wall-right?
                                :destination-reached?)
              :destination-reached? (pp/seq (pp/or "llegué" "llegó" "llegue" "llego")
                                            :ws?
                                            (pp/optional (pp/or "al" "a"))
                                            :ws?
                                            (pp/optional "destino")
                                            :ws?
                                            (pp/optional "?"))
              :wall-forward? (pp/seq (pp/optional (pp/or "hay"))
                                     :ws?
                                     "pared"
                                     :ws?
                                     "adelante"
                                     :ws?
                                     (pp/optional "?"))
              :wall-left? (pp/seq (pp/optional (pp/or "hay"))
                                  :ws?
                                  "pared"
                                  :ws?
                                  (pp/optional "a")
                                  :ws?
                                  (pp/optional "la")
                                  :ws?
                                  "izquierda"
                                  :ws?
                                  (pp/optional "?"))              
              :wall-right? (pp/seq (pp/optional (pp/or "hay"))
                                  :ws?
                                  "pared"
                                  :ws?
                                  (pp/optional "a")
                                  :ws?
                                  (pp/optional "la")
                                  :ws?
                                  "derecha"
                                  :ws?
                                  (pp/optional "?"))
              :if (pp/seq (pp/or "si" "if")
                          :ws?
                          :condition
                          :ws?
                          (pp/optional ":"))})

(def transformations
  {:number (fn [d] (js/parseInt d))
   :lines (fn [lines] (group-by-indent (remove nil? (take-nth 2 lines))))
   :line (fn [[indent action]]
           (when action
             (assoc action
                    :indent-level (count indent))))
   :avanzar (fn [[_ _ steps]] {:type ::avanzar :steps (or steps 1)})
   :derecha (constantly ::derecha)
   :izquierda (constantly ::izquierda)
   :girar (fn [parts] {:type ::girar :direction (last parts)})
   :repetir (fn [[_ _ times]] {:type ::repetir :times times})
   :destination-reached? (constantly ::destination-reached?)
   :wall-forward? (constantly ::wall-forward?)
   :wall-left? (constantly ::wall-left?)
   :wall-right? (constantly ::wall-right?)
   :if (fn [[_ _ condition]] {:type ::conditional :condition condition})}
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

(def cell-width 72)
(def cell-height 78)

(def mazes
  {"1_1" {:cells {[0 0] [[0 1]]
                  [0 1] [[0 0] [0 2]]
                  [0 2] [[0 1] [0 3]]
                  [0 3] [[0 2] [1 3]]
                  [1 0] [[1 1] [2 0]]
                  [1 1] [[1 2] [1 0]]
                  [1 2] [[1 3] [1 1]]
                  [1 3] [[0 3] [1 2]]
                  [2 0] [[2 1] [1 0]]
                  [2 1] [[2 0] [2 2]]
                  [2 2] [[2 1] [2 3]]
                  [2 3] [[2 2]]}}
   "1_2" {:cells {[0 0] [[1 0]]
                  [1 0] [[0 0] [1 1]]
                  [1 1] [[1 0] [1 2]]
                  [1 2] [[1 1] [1 3]]
                  [1 3] [[1 2] [2 3]]
                  [2 3] [[1 3]]}}
   })

(comment

  (doseq [[maze-name {:keys [cells]}] mazes]
    (doseq [[cell-from neighbours] cells]
      (doseq [cell-to neighbours]
        (print cell-from (cells cell-to))
        (assert (>= (.indexOf (cells cell-to) cell-from) 0)
                (str maze-name ". " cell-to " -> " cell-from ": " (.indexOf (cells cell-to) cell-from))))))
  
  )

(def exercises
  {"1_1" {:offset [-72 -111] :initial-direction ::N}
   "1_2" {:offset [-72 -111] :initial-direction ::W}
   "2_1" {:offset [-72 77] :initial-direction ::W}
   "2_2" {:offset [-72 77] :initial-direction ::S}
   "3_1" {:offset [0 -38] :initial-direction ::N}
   "3_2" {:offset [0 115] :initial-direction ::N}
   "4_1" {:offset [108 115] :initial-direction ::W}
   "4_2" {:offset [108 115] :initial-direction ::W}
   "5_2" {:offset [-36 -231] :initial-direction ::S}})

(defn load-textures! []
  (go (let [names ["1_1" "1_2" "2_1" "2_2" "3_1" "3_2" "4_1" "4_2" "5_2"]
            textures (<! (->> names
                              (map #(str "imgs/" % ".png"))
                              (map pixi/load-texture!)
                              (a/map vector)))]
        (zipmap names textures))))

(defn draw-connections [maze initial-pos container]
  (let [transform (fn [[x y]]
                    (let [[x0 y0] initial-pos]
                      [(+ x0 (* x cell-width))
                       (+ y0 (* y cell-height))]))]
    (doseq [[cell-from neighbours] (:cells maze)]
      (doseq [cell-to neighbours]
        (let [[x0 y0] (transform cell-from)
              [x1 y1] (transform cell-to)]
          (doto (js/PIXI.Graphics.)
            (ocall! :clear)
            (ocall! :lineStyle (clj->js {:width 15
                                         :color 0xff0000
                                         :alpha 0.5}))
            (ocall! :moveTo x0 y0)
            (ocall! :lineTo x1 y1)
            (pixi/add-to! container)))))))

(defn update-ui! []
  (try
    (let [selected-exercise (oget (js/document.getElementById "exercises") :value)]
      (print selected-exercise)
      (when-let [{:keys [offset initial-direction]} (exercises selected-exercise)]
        (let [{:keys [textures container html app line]} @pixi
              texture (textures selected-exercise)
              sprite (pixi/make-sprite! texture)]
          (ocall! container :removeChildren)
          (pixi/add-to! sprite container)
          (print (oget sprite :height))
          (doto html
            (oset! :style.height (str (oget sprite :height) "px"))
            (oset! :style.width (str (oget sprite :width) "px")))
          (ocall! app :resize)
          (let [[x0 y0] (map + (pixi/get-center sprite) offset)
                program (parse (oget (js/document.getElementById "input") :value))
                robot (run-program (make-robot initial-direction) program)]
            (draw-connections (mazes selected-exercise)
                              [x0 y0]
                              container)
            (oset! (js/document.getElementById "ast") 
                   :innerText (js/JSON.stringify (clj->js program) nil 2))
            (doto line
              (ocall! :clear)
              (ocall! :lineStyle (clj->js {:width 3
                                           :color 0x5555ff
                                           :alpha 1}))
              (ocall! :moveTo x0 y0))
            (doseq [[x1 y1] (:positions robot)]
              (ocall! line :lineTo
                      (+ x0 (* cell-width x1))
                      (+ y0 (* cell-height y1))))
            (doto line
              (pixi/add-to! container))))))
    (catch js/Error ex 
      (js/console.error ex)
      (oset! (js/document.getElementById "ast") :innerText ex))))

(defn initialize-ui! []
  (go    
    (let [html (js/document.getElementById "pixi-canvas")
          app (pixi/make-application! html)
          texture-map (<! (load-textures!))
          container (js/PIXI.Container.)
          line (js/PIXI.Graphics.)]
      (reset! pixi {:html html, :app app
                    :textures texture-map
                    :container container
                    :line line})
      (doto container
        (pixi/set-position! [0 0])
        (pixi/add-to! (oget app :stage)))
      (.addEventListener (js/document.getElementById "input")
                         "keyup" update-ui!)
      (.addEventListener (js/document.getElementById "exercises")
                         "change" update-ui!))))

(defn init [& args]
  (print "RICHO!")
  (initialize-ui!))