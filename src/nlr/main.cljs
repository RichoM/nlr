(ns nlr.main
  (:require [clojure.core.async :as a :refer [go <!]]
            [clojure.string :as str]
            [oops.core :refer [oget oset! ocall!]]
            [petitparser.core :as pp]
            [utils.pixi :as pixi]))

(defn print-error [ex]
  (js/console.error ex)
  (oset! (js/document.getElementById "errors") 
         :innerText (ex-message ex)))

(defn make-robot [{:keys [initial-direction begin] :as maze}]
  {:direction initial-direction
   :positions [begin]
   :maze maze})

(defonce counter (atom 0))

(defn direction-between [[x0 y0] [x1 y1]]
  (cond
    (and (> x1 x0) (= y0 y1)) ::E
    (and (< x1 x0) (= y0 y1)) ::W
    (and (> y1 y0) (= x0 x1)) ::S
    (and (< y1 y0) (= x0 x1)) ::N))

(defn inside? [maze cell]
  (some? (get-in maze [:cells cell])))

(defn connected? [maze from to]
  (when-let [neighbours (get-in maze [:cells from])]
    (>= (.indexOf neighbours to) 0)))

(defn wall-forward? [maze from direction]
  (let [directions {::N [1 -] ::S [1 +]
                    ::E [0 +] ::W [0 -]}
        [index f] (directions direction)
        to (update from index f 1)]
    (not (connected? maze from to))))

(defn wall-left? [maze from direction]
  (let [directions {::N [0 -] ::S [0 +]
                    ::E [1 -] ::W [1 +]}
        [index f] (directions direction)
        to (update from index f 1)]
    (not (connected? maze from to))))

(defn wall-right? [maze from direction]
  (let [directions {::N [0 +] ::S [0 -]
                    ::E [1 +] ::W [1 -]}
        [index f] (directions direction)
        to (update from index f 1)]
    (not (connected? maze from to))))

(def operators {"+" +
                "-" -
                "*" *
                "/" /})

(defn evaluate [robot expr]
  (if (number? expr)
    expr
    (let [position (peek (:positions robot))
          direction (:direction robot)
          maze (:maze robot)]
      (case expr
        ::wall-forward? (wall-forward? maze position direction)
        ::wall-left? (wall-left? maze position direction)
        ::wall-right? (wall-right? maze position direction)
        ::destination-reached? (= position (:end maze))
        (case (:type expr)
          ::negated (not (evaluate robot (:condition expr)))
          ::binary (let [op (operators (:op expr))
                         left (evaluate robot (:left expr))
                         right (evaluate robot (:right expr))]
                     (op left right))
          ::variable (get-in robot [:variables (:name expr)]))))))

(defmulti run* (fn [_robot instr] (:type instr)))

(defn run [robot instr]
  (let [limit (js/parseInt (oget (js/document.getElementById "limit") :value))]
    (when-not (int? limit)
      (throw (ex-info "INVALID LIMIT!" {:robot robot})))
    (when (>= @counter limit)
      (throw (ex-info "LIMIT REACHED!" {:robot robot})))
    (swap! counter inc)
    (run* robot instr)))

(defn run-program [robot program]
  (reset! counter 0)
  (try 
    (reduce run robot program)
    (catch js/Error. ex 
      (print-error ex)
      (when-let [{:keys [robot]} (ex-data ex)]
        robot))))


(defn print-robot [robot]
  (let [position (peek (:positions robot))
        direction (:direction robot)
        maze (:maze robot)]
    (print {:position position
            :direction direction
            :left? (wall-left? maze position direction)
            :right? (wall-right? maze position direction)
            :forward? (wall-forward? maze position direction)})))

(defmethod run* ::girar [robot {:keys [direction]}]
  (let [directions {::N {::derecha ::E
                         ::izquierda ::W}
                    ::E {::derecha ::S
                         ::izquierda ::N}
                    ::S {::derecha ::W
                         ::izquierda ::E}
                    ::W {::derecha ::N
                         ::izquierda ::S}}]
    (update robot :direction #(get-in directions [% direction]))))

(defmethod run* ::avanzar [{:keys [direction] :as robot} {:keys [steps]}]
  (let [directions {::N [1 -] ::S [1 +]
                    ::E [0 +] ::W [0 -]}
        [index f] (directions direction)]
    (update robot :positions (fn [positions]
                               (let [pos (peek positions)]
                                 (conj positions (update pos index f steps)))))))

(defmethod run* ::repetir [robot {:keys [times stmts]}]
  (loop [i times
         robot robot]
    (if (> i 0)
      (recur (dec i)
             (reduce run robot stmts))
      robot)))

(defmethod run* ::if [robot {:keys [condition stmts]}]
  (if (evaluate robot condition)
    (reduce run robot stmts)
    robot))

(defmethod run* ::while [robot {:keys [condition stmts] :as instr}]
  (if (evaluate robot condition)
    (run (reduce run robot stmts) instr)
    robot))

(defmethod run* ::procedure [robot {:keys [name] :as proc}]
  (assoc-in robot [:procedures name] proc))

(defmethod run* ::call [robot {:keys [name]}]
  (if-let [{:keys [stmts]} (get-in robot [:procedures name])]
    (reduce run robot stmts)
    (throw (js/Error. (str "UNKNOWN PROCEDURE: " name)))))

(defmethod run* ::assignment [robot {:keys [variable value]}]
  (assoc-in robot [:variables (:name variable)]
            (evaluate robot value)))

(defmethod run* :default [robot _] (js/console.error "ACAACA") robot)


(defn accepts-children? [node]
  (not (contains? #{::avanzar ::girar} (:type node))))

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

(def grammar {:start (pp/end :lines)
              :lines (pp/separated-by :line (pp/seq :ws? (pp/or "\r\n" "\n")))
              :line (pp/seq (pp/star (pp/or "\t" " "))
                            (pp/optional (pp/or :avanzar :girar :repetir :if :while
                                                :assignment :procedure :call))
                            :ws?)
              :number (pp/flatten (pp/plus pp/digit))
              :ws (pp/plus (pp/or " " "\t"))
              :ws? (pp/optional :ws)
              :avanzar (pp/seq (pp/or "avanzar" "avanza" "avanz??")
                               :ws?
                               (pp/optional :number)
                               :ws?
                               (pp/optional (pp/or "casilleros" "casillero" "celdas" "celda")))
              :derecha (pp/or "derecha" "der" "d" "right" "r")
              :izquierda (pp/or "izquierda" "izq" "i" "left" "l")
              :girar (pp/seq (pp/or "girar" "gira" "gir??")
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
              :negated-condition (pp/seq "no" :ws? :condition)
              :destination-reached? (pp/seq (pp/or "llegu??" "lleg??" "llegue" "llego")
                                            :ws?
                                            (pp/optional (pp/or "al" "a"))
                                            :ws?
                                            (pp/optional "destino")
                                            :ws?
                                            (pp/optional "?"))
              :wall-forward? (pp/seq (pp/optional (pp/or "haya" "hay"))
                                     :ws?
                                     "pared"
                                     :ws?
                                     "adelante"
                                     :ws?
                                     (pp/optional "?"))
              :wall-left? (pp/seq (pp/optional (pp/or "haya" "hay"))
                                  :ws?
                                  "pared"
                                  :ws?
                                  (pp/optional "a")
                                  :ws?
                                  (pp/optional "la")
                                  :ws?
                                  :izquierda
                                  :ws?
                                  (pp/optional "?"))
              :wall-right? (pp/seq (pp/optional (pp/or "haya" "hay"))
                                   :ws?
                                   "pared"
                                   :ws?
                                   (pp/optional "a")
                                   :ws?
                                   (pp/optional "la")
                                   :ws?
                                   :derecha
                                   :ws?
                                   (pp/optional "?"))
              :if (pp/seq (pp/or "si" "if")
                          :ws?
                          (pp/or :negated-condition :condition)
                          :ws?
                          (pp/optional ":"))
              :while (pp/seq (pp/or "mientras" "while")
                             :ws?
                             (pp/or :negated-condition :condition)
                             :ws?
                             (pp/optional ":"))
              :procedure (pp/seq "para"
                                 :ws?
                                 (pp/flatten (pp/plus pp/word))
                                 :ws?
                                 (pp/optional ":"))
              :call (pp/flatten (pp/plus pp/word))
              :variable (pp/flatten (pp/plus pp/letter))
              :binary-operation (pp/seq (pp/or :number :variable)
                                        :ws?
                                        (pp/or "+" "-" "*" "/")
                                        :ws?
                                        (pp/or :number :variable))
              :assignment (pp/seq :variable
                                  :ws?
                                  "="
                                  :ws?
                                  (pp/or :binary-operation :number :variable))})

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
   :negated-condition (fn [[_ _ condition]] {:type ::negated :condition condition})
   :if (fn [[_ _ condition]] {:type ::if :condition condition})
   :while (fn [[_ _ condition]] {:type ::while :condition condition})
   :procedure (fn [[_ _ name]] {:type ::procedure :name (str/lower-case name)})
   :call (fn [name] {:type ::call :name (str/lower-case name)})
   :variable (fn [name] {:type ::variable :name (str/lower-case name)})
   :binary-operation (fn [[left _ op _ right]]
                       {:type ::binary :left left :op op :right right})
   :assignment (fn [[variable _ _ _ value]] {:type ::assignment
                                             :variable variable
                                             :value value})})

(def parser (pp/compose grammar transformations))

(defn parse [str]
  (pp/parse parser (str/lower-case str)))


(defonce pixi (atom nil))

(def cell-width 72)
(def cell-height 78)

(def mazes
  {"i.1_1" {:begin [0 0]
            :end [2 3]
            :offset [-72 -111]
            :initial-direction ::N
            :cells {[0 0] [[0 1]]
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
   "i.1_2" {:begin [0 0]
            :end [2 3]
            :offset [-72 -111]
            :initial-direction ::W
            :cells {[0 0] [[1 0]]
                    [1 0] [[0 0] [1 1]]
                    [1 1] [[1 0] [1 2]]
                    [1 2] [[1 1] [1 3]]
                    [1 3] [[1 2] [2 3]]
                    [2 3] [[1 3]]}}
   "i.2_1" {:begin [0 2]
            :end [2 2]
            :offset [-72 77]
            :initial-direction ::W
            :cells {[0 0] [[1 0] [0 1]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1]]
                    [1 0] [[0 0] [2 0]]
                    [1 1] [[2 1] [1 2]]
                    [1 2] [[1 1] [2 2]]
                    [2 0] [[1 0] [2 1]]
                    [2 1] [[1 1] [2 2] [2 0]]
                    [2 2] [[1 2] [2 1]]}}
   "i.2_2" {:begin [0 2]
            :end [1 1]
            :offset [-72 77]
            :initial-direction ::S
            :cells {[0 0] [[0 1] [1 0]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1]]
                    [1 0] [[0 0] [2 0]]
                    [1 1] [[1 2]]
                    [1 2] [[1 1] [2 2]]
                    [2 0] [[1 0] [2 1]]
                    [2 1] [[2 0] [2 2]]
                    [2 2] [[2 1] [1 2]]}}
   "i.3_1" {:begin [2 1]
            :end [2 3]
            :offset [0 -38]
            :initial-direction ::N
            :cells {[0 0] [[0 1] [1 0]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1] [0 3]]
                    [0 3] [[0 2] [1 3]]
                    [1 0] [[0 0] [1 1] [2 0]]
                    [1 1] [[1 0] [1 2]]
                    [1 2] [[1 1] [2 2]]
                    [1 3] [[0 3] [2 3]]
                    [2 0] [[1 0] [3 0]]
                    [2 1] [[3 1] [2 2]]
                    [2 2] [[2 1] [1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [3 0] [[2 0] [4 0]]
                    [3 1] [[2 1] [4 1]]
                    [3 2] [[2 2] [4 2]]
                    [3 3] [[2 3] [4 3]]
                    [4 0] [[3 0] [4 1]]
                    [4 1] [[4 0] [3 1]]
                    [4 2] [[3 2] [4 3]]
                    [4 3] [[4 2] [3 3]]}}
   "i.3_2" {:begin [2 3]
            :end [1 1]
            :offset [0 115]
            :initial-direction ::N
            :cells {[0 0] [[0 1] [1 0]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1] [0 3]]
                    [0 3] [[0 2] [1 3]]
                    [1 0] [[0 0] [1 1] [2 0]]
                    [1 1] [[1 0] [1 2]]
                    [1 2] [[1 1] [2 2]]
                    [1 3] [[0 3] [2 3]]
                    [2 0] [[1 0] [3 0]]
                    [2 1] [[3 1] [2 2]]
                    [2 2] [[2 1] [1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [3 0] [[2 0] [4 0]]
                    [3 1] [[2 1] [4 1]]
                    [3 2] [[2 2] [4 2]]
                    [3 3] [[2 3] [4 3]]
                    [4 0] [[3 0] [4 1]]
                    [4 1] [[4 0] [3 1]]
                    [4 2] [[3 2] [4 3]]
                    [4 3] [[4 2] [3 3]]}}
   "i.4_1" {:begin [3 3]
            :end [2 1]
            :offset [108 115]
            :initial-direction ::W
            :cells {[0 0] [[1 0] [0 1]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1] [0 3]]
                    [0 3] [[0 2] [1 3]]
                    [1 0] [[0 0] [2 0] [1 1]]
                    [1 1] [[1 2] [1 0]]
                    [1 2] [[1 1] [2 2]]
                    [1 3] [[0 3] [2 3]]
                    [2 0] [[1 0] [3 0]]
                    [2 1] [[3 1] [2 2]]
                    [2 2] [[2 1] [1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [3 0] [[2 0] [3 1]]
                    [3 1] [[3 0] [2 1]]
                    [3 2] [[2 2] [3 3]]
                    [3 3] [[3 2] [2 3]]}}
   "i.4_2" {:begin [4 3]
            :end [5 3]
            :offset [108 115]
            :initial-direction ::W
            :cells {[0 0] [[1 0] [0 1]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1] [0 3]]
                    [0 3] [[0 2] [1 3]]
                    [1 0] [[0 0] [1 1] [2 0]]
                    [1 1] [[1 0] [1 2]]
                    [1 2] [[1 1] [2 2]]
                    [1 3] [[0 3] [2 3]]
                    [2 0] [[1 0] [3 0]]
                    [2 1] [[2 2] [3 1]]
                    [2 2] [[2 1] [1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [3 0] [[2 0]]
                    [3 1] [[2 1] [4 1]]
                    [3 2] [[2 2] [4 2]]
                    [3 3] [[2 3] [4 3]]
                    [4 0] [[4 1] [5 0]]
                    [4 1] [[3 1] [4 0]]
                    [4 2] [[3 2] [4 3]]
                    [4 3] [[3 3] [4 2]]
                    [5 0] [[4 0] [5 1]]
                    [5 1] [[5 0] [5 2]]
                    [5 2] [[5 1] [5 3]]
                    [5 3] [[5 2]]}}
   "i.5_2" {:begin [3 0]
            :end [4 0]
            :offset [-36 -231]
            :initial-direction ::S
            :cells {[0 0] [[0 1] [1 0]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 3] [0 1]]
                    [0 3] [[0 2] [0 4] [1 3]]
                    [0 4] [[0 3] [1 4]]
                    [0 5] [[1 5] [0 6]]
                    [0 6] [[0 5] [1 6]]
                    [1 0] [[0 0] [2 0] [1 1]]
                    [1 1] [[1 0] [2 1]]
                    [1 2] [[2 2]]
                    [1 3] [[0 3] [2 3]]
                    [1 4] [[0 4] [2 4]]
                    [1 5] [[0 5] [2 5]]
                    [1 6] [[0 6] [2 6]]
                    [2 0] [[1 0]]
                    [2 1] [[1 1] [3 1]]
                    [2 2] [[1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [2 4] [[1 4] [3 4]]
                    [2 5] [[1 5] [3 5]]
                    [2 6] [[1 6] [3 6]]
                    [3 0] [[3 1]]
                    [3 1] [[3 0] [2 1]]
                    [3 2] [[2 2] [3 3]]
                    [3 3] [[3 2] [2 3]]
                    [3 4] [[2 4] [4 4]]
                    [3 5] [[2 5]]
                    [3 6] [[2 6] [4 6]]
                    [4 0] [[4 1]]
                    [4 1] [[4 0] [4 2]]
                    [4 2] [[4 1] [4 3]]
                    [4 3] [[4 2] [5 3]]
                    [4 4] [[3 4] [4 5]]
                    [4 5] [[4 4] [4 6]]
                    [4 6] [[3 6] [4 5] [5 6]]
                    [5 0] [[5 1] [6 0]]
                    [5 1] [[5 0] [5 2]]
                    [5 2] [[5 1] [5 3]]
                    [5 3] [[4 3] [5 2]]
                    [5 4] [[5 5] [6 4]]
                    [5 5] [[5 4] [5 6]]
                    [5 6] [[4 6] [5 5]]
                    [6 0] [[5 0] [6 1]]
                    [6 1] [[6 0] [6 2]]
                    [6 2] [[6 1] [6 3]]
                    [6 3] [[6 2] [7 3]]
                    [6 4] [[5 4] [6 5]]
                    [6 5] [[6 4] [6 6]]
                    [6 6] [[6 5] [7 6]]
                    [7 0] [[7 1]]
                    [7 1] [[7 0] [7 2]]
                    [7 2] [[7 1] [7 3]]
                    [7 3] [[6 3] [7 2] [7 4]]
                    [7 4] [[7 3] [7 5]]
                    [7 5] [[7 4] [7 6]]
                    [7 6] [[6 6] [7 5]]}}
   "f.1_1" {:begin [2 3]
            :end [0 0]
            :offset [72 111]
            :initial-direction ::N
            :cells {[0 0] [[0 1]]
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
   "f.1_2" {:begin [2 3]
            :end [0 0]
            :offset [72 111]
            :initial-direction ::W
            :cells {[0 0] [[1 0]]
                    [1 0] [[0 0] [1 1]]
                    [1 1] [[1 0] [1 2]]
                    [1 2] [[1 1] [1 3]]
                    [1 3] [[1 2] [2 3]]
                    [2 3] [[1 3]]}}
   "f.2_1" {:begin [2 2]
            :end [0 2]
            :offset [72 77]
            :initial-direction ::E
            :cells {[0 0] [[1 0] [0 1]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1]]
                    [1 0] [[0 0] [2 0]]
                    [1 1] [[2 1] [1 2]]
                    [1 2] [[1 1] [2 2]]
                    [2 0] [[1 0] [2 1]]
                    [2 1] [[1 1] [2 2] [2 0]]
                    [2 2] [[1 2] [2 1]]}}
   "f.2_2" {:begin [1 1]
            :end [0 2]
            :offset [0 0]
            :initial-direction ::S
            :cells {[0 0] [[0 1] [1 0]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1]]
                    [1 0] [[0 0] [2 0]]
                    [1 1] [[1 2]]
                    [1 2] [[1 1] [2 2]]
                    [2 0] [[1 0] [2 1]]
                    [2 1] [[2 0] [2 2]]
                    [2 2] [[2 1] [1 2]]}}
   "f.3_1" {:begin [2 3]
            :end [2 1]
            :offset [0 115]
            :initial-direction ::N
            :cells {[0 0] [[0 1] [1 0]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1] [0 3]]
                    [0 3] [[0 2] [1 3]]
                    [1 0] [[0 0] [1 1] [2 0]]
                    [1 1] [[1 0] [1 2]]
                    [1 2] [[1 1] [2 2]]
                    [1 3] [[0 3] [2 3]]
                    [2 0] [[1 0] [3 0]]
                    [2 1] [[3 1] [2 2]]
                    [2 2] [[2 1] [1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [3 0] [[2 0] [4 0]]
                    [3 1] [[2 1] [4 1]]
                    [3 2] [[2 2] [4 2]]
                    [3 3] [[2 3] [4 3]]
                    [4 0] [[3 0] [4 1]]
                    [4 1] [[4 0] [3 1]]
                    [4 2] [[3 2] [4 3]]
                    [4 3] [[4 2] [3 3]]}}
   "f.3_2" {:begin [1 1]
            :end [3 3]
            :offset [-72 -39]
            :initial-direction ::N
            :cells {[0 0] [[0 1] [1 0]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1] [0 3]]
                    [0 3] [[0 2] [1 3]]
                    [1 0] [[0 0] [1 1] [2 0]]
                    [1 1] [[1 0] [1 2]]
                    [1 2] [[1 1] [2 2]]
                    [1 3] [[0 3] [2 3]]
                    [2 0] [[1 0] [3 0]]
                    [2 1] [[3 1] [2 2]]
                    [2 2] [[2 1] [1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [3 0] [[2 0] [4 0]]
                    [3 1] [[2 1] [4 1]]
                    [3 2] [[2 2] [4 2]]
                    [3 3] [[2 3] [4 3]]
                    [4 0] [[3 0] [4 1]]
                    [4 1] [[4 0] [3 1]]
                    [4 2] [[3 2] [4 3]]
                    [4 3] [[4 2] [3 3]]}}
   "f.4_1" {:begin [2 1]
            :end [0 3]
            :offset [36 -39]
            :initial-direction ::E
            :cells {[0 0] [[1 0] [0 1]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1] [0 3]]
                    [0 3] [[0 2] [1 3]]
                    [1 0] [[0 0] [2 0] [1 1]]
                    [1 1] [[1 2] [1 0]]
                    [1 2] [[1 1] [2 2]]
                    [1 3] [[0 3] [2 3]]
                    [2 0] [[1 0] [3 0]]
                    [2 1] [[3 1] [2 2]]
                    [2 2] [[2 1] [1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [3 0] [[2 0] [3 1]]
                    [3 1] [[3 0] [2 1]]
                    [3 2] [[2 2] [3 3]]
                    [3 3] [[3 2] [2 3]]}}
   "f.4_2" {:begin [5 3]
            :end [3 0]
            :offset [180 115]
            :initial-direction ::W
            :cells {[0 0] [[1 0] [0 1]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 1] [0 3]]
                    [0 3] [[0 2] [1 3]]
                    [1 0] [[0 0] [1 1] [2 0]]
                    [1 1] [[1 0] [1 2]]
                    [1 2] [[1 1] [2 2]]
                    [1 3] [[0 3] [2 3]]
                    [2 0] [[1 0] [3 0]]
                    [2 1] [[2 2] [3 1]]
                    [2 2] [[2 1] [1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [3 0] [[2 0]]
                    [3 1] [[2 1] [4 1]]
                    [3 2] [[2 2] [4 2]]
                    [3 3] [[2 3] [4 3]]
                    [4 0] [[4 1] [5 0]]
                    [4 1] [[3 1] [4 0]]
                    [4 2] [[3 2] [4 3]]
                    [4 3] [[3 3] [4 2]]
                    [5 0] [[4 0] [5 1]]
                    [5 1] [[5 0] [5 2]]
                    [5 2] [[5 1] [5 3]]
                    [5 3] [[5 2]]}}
   "f.5_2" {:begin [4 0]
            :end [3 0]
            :offset [36 -231]
            :initial-direction ::S
            :cells {[0 0] [[0 1] [1 0]]
                    [0 1] [[0 0] [0 2]]
                    [0 2] [[0 3] [0 1]]
                    [0 3] [[0 2] [0 4] [1 3]]
                    [0 4] [[0 3] [1 4]]
                    [0 5] [[1 5] [0 6]]
                    [0 6] [[0 5] [1 6]]
                    [1 0] [[0 0] [2 0] [1 1]]
                    [1 1] [[1 0] [2 1]]
                    [1 2] [[2 2]]
                    [1 3] [[0 3] [2 3]]
                    [1 4] [[0 4] [2 4]]
                    [1 5] [[0 5] [2 5]]
                    [1 6] [[0 6] [2 6]]
                    [2 0] [[1 0]]
                    [2 1] [[1 1] [3 1]]
                    [2 2] [[1 2] [3 2]]
                    [2 3] [[1 3] [3 3]]
                    [2 4] [[1 4] [3 4]]
                    [2 5] [[1 5] [3 5]]
                    [2 6] [[1 6] [3 6]]
                    [3 0] [[3 1]]
                    [3 1] [[3 0] [2 1]]
                    [3 2] [[2 2] [3 3]]
                    [3 3] [[3 2] [2 3]]
                    [3 4] [[2 4] [4 4]]
                    [3 5] [[2 5]]
                    [3 6] [[2 6] [4 6]]
                    [4 0] [[4 1]]
                    [4 1] [[4 0] [4 2]]
                    [4 2] [[4 1] [4 3]]
                    [4 3] [[4 2] [5 3]]
                    [4 4] [[3 4] [4 5]]
                    [4 5] [[4 4] [4 6]]
                    [4 6] [[3 6] [4 5] [5 6]]
                    [5 0] [[5 1] [6 0]]
                    [5 1] [[5 0] [5 2]]
                    [5 2] [[5 1] [5 3]]
                    [5 3] [[4 3] [5 2]]
                    [5 4] [[5 5] [6 4]]
                    [5 5] [[5 4] [5 6]]
                    [5 6] [[4 6] [5 5]]
                    [6 0] [[5 0] [6 1]]
                    [6 1] [[6 0] [6 2]]
                    [6 2] [[6 1] [6 3]]
                    [6 3] [[6 2] [7 3]]
                    [6 4] [[5 4] [6 5]]
                    [6 5] [[6 4] [6 6]]
                    [6 6] [[6 5] [7 6]]
                    [7 0] [[7 1]]
                    [7 1] [[7 0] [7 2]]
                    [7 2] [[7 1] [7 3]]
                    [7 3] [[6 3] [7 2] [7 4]]
                    [7 4] [[7 3] [7 5]]
                    [7 5] [[7 4] [7 6]]
                    [7 6] [[6 6] [7 5]]}}})

(comment ;; NOTE(Richo): This code checks all mazes for inconsistent connections

  (doseq [[maze-name {:keys [cells]}] mazes]
    (doseq [[cell-from neighbours] cells]
      (doseq [cell-to neighbours]
        (assert (>= (.indexOf (cells cell-to) cell-from) 0)
                (str maze-name ". " cell-to " -> " cell-from ": " (.indexOf (cells cell-to) cell-from))))))
  
  )

(defn load-textures! []
  (go (let [names ["robot" 
                   "i.1_1" "i.1_2" "i.2_1" "i.2_2" "i.3_1" "i.3_2" "i.4_1" "i.4_2" "i.5_2" 
                   "f.1_1" "f.1_2" "f.2_1" "f.2_2" "f.3_1" "f.3_2" "f.4_1" "f.4_2" "f.5_2"]
            textures (<! (->> names
                              (map #(str "imgs/" % ".png"))
                              (map pixi/load-texture!)
                              (a/map vector)))]
        (zipmap names textures))))

;; NOTE(Richo): This function draws the connections between all cells
;; in a maze so that we can quickly see if we missed some neighbours.
(defn draw-connections [maze offset container]
  (let [transform (fn [[x y]]
                    (let [[xo yo] offset
                          [x0 y0] (:begin maze)]
                      [(+ xo (* (- x x0) cell-width))
                       (+ yo (* (- y y0) cell-height))]))]
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
      (oset! (js/document.getElementById "errors") :innerText "")
      (when-let [maze (mazes selected-exercise)]
        (let [{:keys [textures container html app line]} @pixi
              texture (textures selected-exercise)
              sprite (pixi/make-sprite! texture)]
          (ocall! container :removeChildren)
          (pixi/add-to! sprite container)
          (print (oget sprite :height))
          (doto html
            (oset! :style.width (str (oget sprite :width) "px"))
            (oset! :style.height (str (oget sprite :height) "px")))
          (ocall! app :resize)
          (let [[xo yo] (map + (pixi/get-center sprite) (:offset maze))
                [x0 y0] (:begin maze)
                program (parse (oget (js/document.getElementById "input") :value))
                robot (run-program (make-robot maze) program)
                robot-sprite (pixi/make-sprite! (textures "robot"))]
            (let [[x y] (peek (:positions robot))]
              (doto robot-sprite
                (oset! :anchor.x 0.5)
                (oset! :anchor.y 0.5)
                (oset! :x (+ xo (* cell-width (- x x0))))
                (oset! :y (+ yo (* cell-height (- y y0))))
                (oset! :tint 0x5555ff)
                (oset! :alpha 0.75)
                (oset! :rotation (case (:direction robot)
                                   ::N 0
                                   ::E 1.57
                                   ::S 3.14
                                   ::W -1.57
                                   0))
                (pixi/add-to! container)))
            #_(draw-connections (mazes selected-exercise)
                                [xo yo]
                                container)
            (when-let [variables (:variables robot)]
              (oset! (js/document.getElementById "variables")
                     :innerText (str "VARIABLES: " variables)))
            (oset! (js/document.getElementById "ast")
                   :innerText (str "INSTRUCTIONS: " @counter
                                   "\nPROGRAM: " (js/JSON.stringify (clj->js program) nil 2)))
            (doto line
              (ocall! :clear)
              (ocall! :lineStyle (clj->js {:width 3
                                           :color 0x5555ff
                                           :alpha 1}))
              (ocall! :moveTo xo yo))
            (doseq [[from to] (partition 2 1 (:positions robot))]
              (when (inside? maze from)
                (when-not (connected? maze from to)
                  (let [errors (js/document.getElementById "errors")]
                    (oset! errors :innerText (str (oget errors :innerText)
                                                  "\n" "WALKING THROUGH WALLS! "
                                                  from " -> " to))))))
            (doseq [[x1 y1] (:positions robot)]
              (ocall! line :lineTo
                      (+ xo (* cell-width (- x1 x0)))
                      (+ yo (* cell-height (- y1 y0)))))
            (doto line
              (pixi/add-to! container))))))
    (catch js/Error ex (print-error ex))))

(defn start-update-loop! [updates-chan]
  (go
    (loop []
      (when (<! updates-chan)
        (<! (a/timeout 100))
        (time (update-ui!))   
        (recur)))))

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
      (let [updates-chan (a/chan (a/dropping-buffer 1))]
        (.addEventListener (js/document.getElementById "exercises")
                           "change" #(a/put! updates-chan true))
        (.addEventListener (js/document.getElementById "limit")
                           "keyup" #(a/put! updates-chan true))
        (.addEventListener (js/document.getElementById "input")
                           "keyup" #(a/put! updates-chan true))
        (start-update-loop! updates-chan)))))

(defn init [& args]
  (print "RICHO!")
  (initialize-ui!))