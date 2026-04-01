;; Функциональная версия виртуального плоттера на Clojure.
;;
;; Идея переписывания:
;; 1. Состояние плоттера — обычная immutable map.
;; 2. Каждая команда — чистая функция: принимает состояние и возвращает
;;    новое состояние + список сообщений/действий.
;; 3. Побочные эффекты (печать в консоль) вынесены на границу программы.
;; 4. Составные операции (квадрат, треугольник) собираются из маленьких
;;    функций-команд.

(ns plotter-functional)

;; ------------------------------------------------------------
;; Типы данных в функциональном виде
;; ------------------------------------------------------------

(def carriage-up :up)
(def carriage-down :down)

(def line-black "чорный")
(def line-red "красный")
(def line-green "зелёный")

(defn make-state
  [position angle color carriage-state]
  {:position position
   :angle angle
   :color color
   :carriage-state carriage-state})

(defn initial-state []
  (make-state {:x 0.0 :y 0.0}
              0.0
              line-black
              carriage-up))

;; ------------------------------------------------------------
;; Вспомогательные функции
;; ------------------------------------------------------------

(defn degrees->radians [angle]
  (* angle (/ Math/PI 180.0)))

(defn round-point [x]
  (Math/round (double x)))

(defn calc-new-position
  [distance angle current]
  (let [angle-rads (degrees->radians angle)
        x (+ (:x current) (* distance (Math/cos angle-rads)))
        y (+ (:y current) (* distance (Math/sin angle-rads)))]
    {:x (round-point x)
     :y (round-point y)}))

(defn with-log
  "Добавляет одно сообщение в результат команды. Команда возвращает карту
   вида {:state ... :log [...]}"
  [state & messages]
  {:state state
   :log (vec messages)})

(defn combine-results
  "Склеивает результаты нескольких команд. Удобно для reduce."
  [{state1 :state log1 :log} {state2 :state log2 :log}]
  {:state state2
   :log (into (vec log1) log2)})

(defn run-command
  "Запускает одну команду над аккумулятором вида {:state ... :log [...]}"
  [{:keys [state log]} command]
  (let [{new-state :state new-log :log} (command state)]
    {:state new-state
     :log (into (vec log) new-log)}))

(defn run-commands
  "Запускает последовательность команд над состоянием."
  [state commands]
  (reduce run-command {:state state :log []} commands))

;; ------------------------------------------------------------
;; Базовые команды плоттера
;; ------------------------------------------------------------

(defn draw-line-message [from to color]
  (str "...Чертим линию из (" (:x from) ", " (:y from)
       ") в (" (:x to) ", " (:y to)
       ") используя " color " цвет."))

(defn move
  [distance]
  (fn [state]
    (let [new-position (calc-new-position distance
                                          (:angle state)
                                          (:position state))
          message (if (= (:carriage-state state) carriage-down)
                    (draw-line-message (:position state) new-position (:color state))
                    (str "Передвигаем на " distance
                         " от точки (" (get-in state [:position :x])
                         ", " (get-in state [:position :y]) ")"))]
      (with-log (assoc state :position new-position) message))))

(defn turn
  [angle]
  (fn [state]
    (with-log (update state :angle #(mod (+ % angle) 360.0))
              (str "Поворачиваем на " angle " градусов"))))

(defn carriage-up-cmd
  []
  (fn [state]
    (with-log (assoc state :carriage-state carriage-up)
              "Поднимаем каретку")))

(defn carriage-down-cmd
  []
  (fn [state]
    (with-log (assoc state :carriage-state carriage-down)
              "Опускаем каретку")))

(defn set-color
  [color]
  (fn [state]
    (with-log (assoc state :color color)
              (str "Устанавливаем " color " цвет линии."))))

(defn set-position
  [position]
  (fn [state]
    (with-log (assoc state :position position)
              (str "Устанавливаем позицию каретки в ("
                   (:x position) ", " (:y position) ")."))))

;; ------------------------------------------------------------
;; Составные команды
;; ------------------------------------------------------------

(defn repeat-commands
  "Повторяет набор команд n раз и возвращает плоскую последовательность."
  [n commands]
  (mapcat identity (repeat n commands)))

(defn draw-triangle
  [size]
  (concat
   [(carriage-down-cmd)]
   (repeat-commands 3 [(move size) (turn 120.0)])
   [(carriage-up-cmd)]))

(defn draw-square
  [size]
  (concat
   [(carriage-down-cmd)]
   (repeat-commands 4 [(move size) (turn 90.0)])
   [(carriage-up-cmd)]))

;; ------------------------------------------------------------
;; Интерпретатор эффектов
;; ------------------------------------------------------------

(defn print-log! [log]
  (doseq [line log]
    (println line)))

(defn -main []
  (let [{final-state :state log :log}
        (run-commands
         (initial-state)
         (concat
          (draw-triangle 100.0)
          [(set-position {:x 10.0 :y 10.0})
           (set-color line-red)]
          (draw-square 80.0)))]
    (print-log! log)
    final-state))

;; Для запуска из REPL:
;; (-main)
