(ns structured-data)

(defn abs [num] (if (< num 0 ) (- num) num ))

(defn do-a-thing [x]
  (let [xx (+ x x)]
  (Math/pow xx xx)))

(defn spiff [v]
  (+ (get v 0) (get v 2) ))

(defn cutify [v]
  (conj v "<3" ))

(defn spiff-destructuring [v]
  (let [[x y z] v]
   (+ x z)))

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (- x1 x2))))

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (abs (- y1 y2))))

(defn square? [rectangle]
  ( == (width rectangle) (height rectangle)))

(defn area [rectangle]
  ( * (width rectangle) (height rectangle)))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle [X Y] point]
    (and (or (<= x1 X x2) (<= x2 X x1))
         (or (<= y1 Y y2) (<= y2 Y y1)))))

(defn contains-rectangle? [outer inner]
  (let  [[p1, p2] inner]
  (and (contains-point? outer p1) (contains-point? outer p2))))

(defn title-length [book]
      (count (:title book)))

(defn author-count [book]
      (count (:authors book)))

(defn multiple-authors? [book]
      (> (count (:authors book)) 1))


(defn add-author [book new-author]
      (assoc book :authors (conj (:authors book) new-author)))

(defn alive? [author]
      (not (contains? author :death-year)))

(defn element-lengths [collection]
      (map count collection ))

(defn second-elements [collection]
      (let [secEle (fn [col] (get col 1))]
           (map secEle collection)))

(defn titles [books]
      (map :title books))

(defn monotonic? [a-seq]
      (or (apply <= a-seq) (apply >= a-seq)))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn contains-duplicates? [a-seq]
      (not (== (count a-seq) (count (set a-seq)))))

(defn old-book->new-book [book]
      (assoc book :authors (set (:authors book))))

(defn has-author? [book author]
      (contains? (:authors book) author))

(defn authors [books]
  (apply clojure.set/union (map :authors books)))

(defn all-author-names [books]
      (set (map :name (authors books))))

(defn author->string [author]
      (if (not (contains? author :birth-year)) (:name author)
        (let [start (:birth-year author)
              end (if (contains? author :death-year) (:death-year author) "" )
              ]
             (str (:name author) " (" start " - " end ")")
           )))

(defn authors->string [authors]
      (apply str (interpose ", " (map author->string authors))))

(defn book->string [{title :title a :authors}]
      (str title ", written by " (authors->string a)))

(defn books->string [books]
      (let [booksCountStr (cond (== (count books) 0) "No books"
                                (== (count books) 1) "1 book"
                                :else (str (count books) " books"))]
           (str (apply str (interpose ". " (cons booksCountStr (seq (map book->string books))))) ".")))

(defn books-by-author [author books]
      (filter (fn [book] (has-author? book author)) books))

(defn author-by-name [name authors]
      (first (filter (fn [author] (= (:name author) name) ) authors)))

(defn living-authors [authors]
      (filter alive? authors))

(defn has-a-living-author? [book]
      (not (empty? (living-authors (:authors book)))))


(defn books-by-living-authors [books]
      (filter has-a-living-author? books))

; %________%
