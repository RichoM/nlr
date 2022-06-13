(ns nlr.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [petitparser.core :as pp]
            [utils.pixi :as pixi]))

(defmulti run (fn [robot instr] (:type instr)))

(defn make-robot [direction]
  {:direction direction
   :positions [[0 0]]})

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

(defn run-program [robot program]
  (reduce run robot program))

(defn accepts-children? [node]
  (= ::repetir (:type node)))

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

(oget (js/document.getElementById "exercises") :value)

(defn initialize-ui! []
  (go
    (doto (js/document.getElementById "input")
      (.addEventListener "keyup" update-ui!
                         #_(fn [e]
                           (let [value (oget e :target.value)
                                 out (js/document.getElementById "ast")
                                 robot (make-robot ::N)]
                             (try
                               (oset! out :innerText (js/JSON.stringify (clj->js (run-program robot (parse value))) nil 2))
                               (catch js/Error ex (oset! out :innerText ex)))))))
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
      (.addEventListener (js/document.getElementById "exercises")
                         "change" update-ui!))))

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