(ns mugwump.core)

(use
 '[clojure.string :only (split trim)])

(defn random-mugwump
  "Returns a vector with two numbers from 0 to 9 representing a mugwump location"
  []
  [(rand-int 10) (rand-int 10)])

(defn place-mugwumps
  "Returns a set of X Y vectors for the specified number of mugwumps."
  [c]
  (loop [c          c
         placements #{(random-mugwump)}]
    (if (= c (count placements))
      placements
      (recur c (conj placements (random-mugwump))))))

(defn parse-guess
  "Parses a guess in x,y format into two numbers"
  [guess-string]
  (->> (split guess-string #",")
       (map trim,,,)
       (map read-string,,,)))

(defn distance
  "Calculate the distance between two points"
  [x y]
  (let [width  (- (first x) (first y))
        height (- (second x) (second y))]
    (Math/sqrt (+ (* width width) (* height height)))))


(clojure.string/replace "Part ?" #"\?" "3")
((partial clojure.string/replace "Part ?" #"\?") "3")

(defn show-distances
  "Show distances to all provided mugwumps from a location"
  [guess locations]
  (doseq [location locations]
    (println "YOU ARE" (format "%.1f" (distance guess location)) "UNITS FROM A MUGWUMP")))

(defn guess-location
  "Try a location against known locations. If one is found, remove it and return the remaining mugwumps"
  [guess locations]
  (when (locations guess)
    (println "YOU HAVE FOUND A MUGWUMP"))
  (disj locations guess))

(defn game-over
  "Show win or lose game over text"
  [locations remaining-guesses]
  (if (empty? locations)
    (println "YOU GOT THEM ALL IN" (- 10 remaining-guesses) "GUESSES")
    (println "SORRY, THAT'S 10 TRIES. HERE IS WHERE THEY'RE HIDING" locations)))

(defn ask-for-location
  "Show a prompt asking for a guess. Return the guess as a string."
  [turn-number]
  (println "TURN NO." turn-number "WHAT IS YOUR GUESS")
  (read-line))

(defn find-mugwumps
  "Keep asking for coordinates until 10 guesses or all mugwumps are found"
  [locations]
  (loop [locations         locations
         remaining-guesses 9
         guess             (parse-guess (ask-for-location 1))]
    (let [next-locations (guess-location guess locations)]
      (show-distances guess next-locations)
      (if (or (zero? remaining-guesses) (zero? (count next-locations)))
        (game-over next-locations remaining-guesses)
        (recur next-locations (dec remaining-guesses)
          (parse-guess (ask-for-location (- 11 remaining-guesses))))))))

(defn show-instructions
  "Show introductory text"
  []
  (println "MUGWUMP")
  (println "")
  (println "THE OBJECT OF THIS GTAME IS TO FIND FOUR MUGWUMPS")
  (println "HIDDEN ON A 10 BY 10 GRID. HOMEBASE IS POSITION 0,0")
  (println "ANY GUESS YOU MAKE MUST BE TWO NUMBERS WITH EACH")
  (println "NUMBER BETWEEN 0 AND 9, INCLUSIVE. FIRST NUMBER")
  (println "IS DISTANCE TO RIGHT OF HOMEBASE AND SECOND NUMBER")
  (println "IS DISTANCE ABOVE HOMEBASE.")
  (println "")
  (println "YOU GET 10 TRIES. AFTER EACH TRY, I WILL TELL")
  (println "YOU HOW FAR YOU ARE FROM EACH MUGWUMP."))

(defn -main
  "Mugwump"
  []
  (show-instructions)
  (find-mugwumps (place-mugwumps 4)))
