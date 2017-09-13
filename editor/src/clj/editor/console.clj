(ns editor.console
  (:require [editor.ui :as ui]
            [editor.handler :as handler]
            [clojure.string :as str])
  (:import [javafx.scene.control Button TextArea TabPane TextField]
           [javafx.scene.input KeyCode KeyEvent Clipboard ClipboardContent]))

(set! *warn-on-reflection* true)

(defonce ^:private node (atom nil))
(defonce ^:private term (atom ""))
(defonce ^:private positions (atom '()))

(defn- update-highlight [idx]
  (when-let [^TextArea node @node]
    (ui/run-later
     (if (and idx (not= -1 idx))
       (.selectRange node idx (+ idx (count @term)))
       (.selectRange node 0 0)))))

(defn- next-match []
  (when-let [^TextArea node @node]
    (.indexOf (str/lower-case (.getText node))
              (str/lower-case @term)
              ^Long (inc (or (first @positions) -1)))))

(defn clear-console! []
  (when-let [^TextArea node @node]
    (reset! term "")
    (reset! positions '())
    (ui/run-later (.clear node))))

(defn- search-console [_ _ ^String new]
  (reset! term new)
  (reset! positions '())
  (let [idx (next-match)]
    (update-highlight idx)
    (when (not= -1 idx)
      (swap! positions conj idx))))

(defn- next-console! [_]
  (let [idx (next-match)]
    (when (not= -1 idx)
      (swap! positions conj idx)
      (update-highlight idx))))

(defn- prev-console! [_]
  (when (> (count @positions) 1)
    (swap! positions rest))
  (update-highlight (first @positions)))

(defn- trim-console-message! [^TextArea text-area max-line-count]
  (let [lines (.getParagraphs text-area)
        line-count (count lines)
        trimmed-line-count (max 0 (- line-count max-line-count))]
    (when (pos? trimmed-line-count)
      (let [trimmed-lines (take trimmed-line-count lines)
            trimmed-char-count (transduce (map count) + trimmed-line-count trimmed-lines)]
        (.deleteText text-area 0 (min trimmed-char-count (.getLength text-area)))))))

(def ^:private ^:const message-buffer-initial-capacity 4096)
(def ^:private ^StringBuilder message-buffer (StringBuilder. message-buffer-initial-capacity))
(def ^:private message-buffer-lock (Object.))

(defn- flip-message-buffer!
  []
  (locking message-buffer-lock
    (when (pos? (.length message-buffer))
      (let [messages (.toString message-buffer)
            new-buf (StringBuilder. message-buffer-initial-capacity)]
        (alter-var-root #'message-buffer (constantly new-buf))
        messages))))

(defn- update-console!
  []
  (when-let [^TextArea node @node]
    (when-let [messages (flip-message-buffer!)]
      (.appendText node messages)
      (trim-console-message! node 3000))))

(defn append-console-message!
  [^String message]
  (locking message-buffer-lock
    (.append message-buffer message)))

(defn show!
  []
  (let [^TabPane tab-pane (ui/closest-node-of-type TabPane @node)]
    (.select (.getSelectionModel tab-pane) 0)))

(handler/defhandler :copy :console-view
  (enabled? []
            true)
  (run []
    (let [node    ^TextArea @node
          content (ClipboardContent.)
          cb      (Clipboard/getSystemClipboard)
          str     (.getSelectedText node)]
      (.putString content str)
      (.setContent cb content))))

(defrecord DummySelectionProvider []
  handler/SelectionProvider
  (selection [this] [])
  (succeeding-selection [this] [])
  (alt-selection [this] []))

(defn setup-console! [{:keys [^TextArea text ^TextField search ^Button prev ^Button next ^Button clear]}]
  (ui/on-action! clear (fn [_] (clear-console!)))
  (ui/on-action! next next-console!)
  (ui/on-action! prev prev-console!)
  (ui/observe (.textProperty search) search-console)
  (ui/context! text :console-view {} (DummySelectionProvider.))
  (ui/timer-start! (ui/->timer 10 "update-console" (fn [_ _] (update-console!))))
  (.addEventFilter search KeyEvent/KEY_PRESSED
                   (ui/event-handler event
                                     (let [code (.getCode ^KeyEvent event)]
                                       (when (= code KeyCode/ENTER)
                                         (next-console! nil)))))
  (reset! node text)
  (append-console-message! "Welcome to Defold!\n"))
