;; Copyright 2020-2023 The Defold Foundation
;; Copyright 2014-2020 King
;; Copyright 2009-2014 Ragnar Svensson, Christian Murray
;; Licensed under the Defold License version 1.0 (the "License"); you may not use
;; this file except in compliance with the License.
;;
;; You may obtain a copy of the License, together with FAQs at
;; https://www.defold.com/license
;;
;; Unless required by applicable law or agreed to in writing, software distributed
;; under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
;; CONDITIONS OF ANY KIND, either express or implied. See the License for the
;; specific language governing permissions and limitations under the License.

(ns integration.save-data-test
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.test :refer :all]
            [dynamo.graph :as g]
            [editor.collection :as collection]
            [editor.protobuf :as protobuf]
            [editor.resource :as resource]
            [editor.workspace :as workspace]
            [internal.util :as util]
            [integration.test-util :as test-util]
            [util.coll :refer [pair]])
  (:import [com.google.protobuf Descriptors$Descriptor Descriptors$FieldDescriptor Descriptors$FieldDescriptor$JavaType Message]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ^:private project-path "test/resources/save_data_project")

;; Make it simple to re-run tests after adding content.
;; This project is not used by any other tests.
(test-util/evict-cached-system-and-project! project-path)

(def ^:private pb-ignored-fields
  {'dmGameObjectDDF.CollectionDesc
   {:default
    {"component_types" :runtime-only
     "property_resources" :runtime-only}}

   'dmGameObjectDDF.CollectionInstanceDesc
   {:default
    {"scale" :deprecated}} ; Migration tested in silent-migrations-test.

   'dmGameObjectDDF.ComponentDesc
   {:default
    {"property_decls" :runtime-only}}

   'dmGameObjectDDF.ComponentPropertyDesc
   {:default
    {"property_decls" :runtime-only}}

   'dmGameObjectDDF.EmbeddedInstanceDesc
   {:default
    {"component_properties" :unimplemented ; Edits are embedded in "data" field.
     "scale" :deprecated}} ; Migration tested in silent-migrations-test.

   'dmGameObjectDDF.InstanceDesc
   {:default
    {"scale" :deprecated}} ; Migration tested in silent-migrations-test.

   'dmGameObjectDDF.PrototypeDesc
   {:default
    {"property_resources" :runtime-only}}

   'dmGameSystemDDF.TileSet
   {:default
    {"convex_hull_points" :runtime-only}}

   'dmGraphics.VertexAttribute
   {"material"
    {"binary_values" :runtime-only
     "name_hash" :runtime-only}
    "sprite"
    {"binary_values" :runtime-only
     "coordinate_space" :unused
     "data_type" :unused
     "element_count" :unused
     "name_hash" :runtime-only
     "normalize" :unused
     "semantic_type" :unused}}

   'dmMath.Point3
   {:default
    {"d" :padding}}

   'dmMath.Vector3
   {:default
    {"d" :padding}}

   'dmRenderDDF.MaterialDesc
   {:default
    {"textures" :deprecated}} ; Migration tested in silent-migrations-test.

   'dmRenderDDF.MaterialDesc.Sampler
   {:default
    {"name_hash" :runtime-only
     "name_indirections" :runtime-only
     "texture" :unimplemented}}}) ; Default texture resources not supported yet.

(defn- valid-resource-ext? [str]
  (re-matches #"^[a-z0-9_]+$" str))

(defn- pb-valid-field-name? [str]
  (re-matches #"^[A-Za-z][A-Za-z0-9_]*$" str))

(s/def ::class-java-symbol symbol?)
(s/def ::resource-type-ext (s/and string? valid-resource-ext?))
(s/def ::resource-type-filter (s/or :default #{:default} :ext ::resource-type-ext))
(s/def ::ignore-reason #{:deprecated :padding :runtime-only :unimplemented :unused})
(s/def ::pb-field-name (s/and string? pb-valid-field-name?))
(s/def ::pb-field->ignore-reason (s/map-of ::pb-field-name ::ignore-reason))
(s/def ::ext->pb-field->ignore-reason (s/map-of ::resource-type-filter ::pb-field->ignore-reason))
(s/def ::class->ext->pb-field->ignore-reason (s/map-of ::class-java-symbol ::ext->pb-field->ignore-reason))

(deftest pb-ignored-fields-declaration-test
  (is (s/valid? ::class->ext->pb-field->ignore-reason pb-ignored-fields)
      (s/explain-str ::class->ext->pb-field->ignore-reason pb-ignored-fields)))

(defn- coll-value-comparator
  "The standard comparison will order shorter vectors above longer ones.
  Here, we compare the values before length is taken into account."
  ^long [a b]
  (let [^long value-comparison
        (reduce (fn [^long _ ^long result]
                  (if (zero? result)
                    0
                    (reduced result)))
                0
                (map compare a b))]
    (if (zero? value-comparison)
      (compare (count a) (count b))
      value-comparison)))

(def ^:private sorted-coll-set (sorted-set-by coll-value-comparator))

(defn- editable-file-resource? [resource]
  (and (resource/file-resource? resource)
       (resource/editable? resource)
       (resource/openable? resource)
       (:write-fn (resource/resource-type resource))))

(defn- editable-resource-types-by-ext [workspace]
  (into (sorted-map)
        (filter #(:write-fn (val %)))
        (workspace/get-resource-type-map workspace :editable)))

(defn- checked-resources [workspace]
  (->> (workspace/find-resource workspace "/")
       (resource/children)
       (filter editable-file-resource?)
       (sort-by (juxt resource/type-ext resource/proj-path))
       (vec)))

(defn- list-message [message items]
  (string/join "\n" (cons message (map #(str "  " %) items))))

(defn- resource-ext-message [message resource-exts]
  (list-message message (map #(str \. %) resource-exts)))

(deftest all-resource-types-covered-test
  (test-util/with-loaded-project project-path
    (let [editable-resource-exts
          (into (sorted-set)
                (map key)
                (editable-resource-types-by-ext workspace))

          checked-resource-exts
          (into (sorted-set)
                (map #(:ext (resource/resource-type %)))
                (checked-resources workspace))

          non-covered-resource-exts
          (set/difference editable-resource-exts checked-resource-exts)]

      (is (= #{} non-covered-resource-exts)
          (resource-ext-message
            (str "The following editable resource types do not have files in `editor/" project-path "`:")
            non-covered-resource-exts)))))

(deftest editable-resource-types-have-valid-test-info
  (test-util/with-loaded-project project-path
    (let [problematic-resource-exts-by-issue-message
          (-> (util/group-into
                {} (sorted-set)
                (fn key-fn [[_ext resource-type]]
                  (cond
                    (nil? (:test-info resource-type))
                    "The following editable resource types did not specify :test-info when registered:"

                    (not (contains? (:test-info resource-type) :type))
                    "The following editable resource types did not specify :type in their :test-info when registered:"

                    (not (keyword? (:type (:test-info resource-type))))
                    "The following editable resource types specified an invalid :type in their :test-info when registered:"))
                (fn value-fn [[ext _resource-type]]
                  ext)
                (editable-resource-types-by-ext workspace))
              (dissoc nil))]

      (doseq [[issue-message problematic-resource-exts] problematic-resource-exts-by-issue-message]
        (is (= #{} problematic-resource-exts)
            (resource-ext-message issue-message problematic-resource-exts))))))

(defn- merge-nested-frequencies
  ([] 0)
  ([a] a)
  ([a b]
   (cond
     (and (integer? a) (integer? b))
     (+ (long a) (long b))

     (and (map? a) (map? b))
     (merge-with merge-nested-frequencies a b)

     (and (integer? a) (zero? (long a)))
     b

     (and (integer? b) (zero? (long b)))
     a

     :else
     (assert false))))

(def ^:private xform-nested-frequencies->paths
  (letfn [(path-entries [path [key value]]
            (let [path (conj path key)]
              (if (coll? value)
                (eduction
                  (mapcat #(path-entries path %))
                  value)
                [(pair path value)])))]
    (mapcat #(path-entries [] %))))

(defn- flatten-nested-frequencies [nested-frequencies]
  {:pre [(map? nested-frequencies)]}
  (into (empty nested-frequencies)
        xform-nested-frequencies->paths
        nested-frequencies))

(definline pb-message-field? [^Descriptors$FieldDescriptor field-desc]
  `(= Descriptors$FieldDescriptor$JavaType/MESSAGE (.getJavaType ~field-desc)))

(defn- pb-field-value-count
  ^long [^Message pb ^Descriptors$FieldDescriptor field-desc]
  (if (.isRepeated field-desc)
    (.getRepeatedFieldCount pb field-desc)
    (if (.hasField pb field-desc) 1 0)))

(defn- pb-descriptor-expected-fields-raw [^Descriptors$Descriptor pb-desc ^String resource-type-ext]
  (let [ignored-fields-by-resource-type-ext (get pb-ignored-fields (symbol (.getFullName pb-desc)))
        ignore-reason-by-field-name (or (get ignored-fields-by-resource-type-ext resource-type-ext)
                                        (get ignored-fields-by-resource-type-ext :default)
                                        {})
        ignored-field? (fn [^Descriptors$FieldDescriptor field-desc]
                         (contains? ignore-reason-by-field-name (.getName field-desc)))]
    (into []
          (remove ignored-field?)
          (.getFields pb-desc))))

(def ^:private pb-descriptor-expected-fields (memoize pb-descriptor-expected-fields-raw))

(defn- pb-nested-field-frequencies [^Message pb ^String resource-type-ext]
  (into (sorted-map)
        (map (fn [^Descriptors$FieldDescriptor field-desc]
               (pair (.getName field-desc)
                     (if (pb-message-field? field-desc)
                       (if (.isRepeated field-desc)
                         (transduce (map #(pb-nested-field-frequencies % resource-type-ext))
                                    merge-nested-frequencies
                                    (.getField pb field-desc))
                         (pb-nested-field-frequencies (.getField pb field-desc) resource-type-ext))
                       (pb-field-value-count pb field-desc)))))
        (pb-descriptor-expected-fields (.getDescriptorForType pb) resource-type-ext)))

(defmulti value-path-frequencies (fn [resource] (-> resource resource/resource-type :test-info :type)))

(defmethod value-path-frequencies :code [resource]
  ;; TODO!
  )

(defmethod value-path-frequencies :ddf [resource]
  (let [resource-type (resource/resource-type resource)
        ext (:ext resource-type)
        pb-class (-> resource-type :test-info :ddf-type)
        pb (protobuf/read-pb pb-class resource)]
    (-> pb
        (pb-nested-field-frequencies ext)
        (flatten-nested-frequencies))))

(defmethod value-path-frequencies :settings [resource]
  ;; TODO!
  )

(defn- uncovered-value-paths [resources]
  (->> resources
       (map value-path-frequencies)
       (apply merge-with +)
       (into sorted-coll-set
             (keep (fn [[value-path ^long value-count]]
                     (when (zero? value-count)
                       value-path))))))

(deftest all-fields-covered-test
  (test-util/with-loaded-project project-path
    (let [uncovered-value-paths-by-ext
          (->> (checked-resources workspace)
               (group-by (comp :ext resource/resource-type))
               (into (sorted-map)
                     (keep (fn [[ext resources]]
                             (some->> resources
                                      (uncovered-value-paths)
                                      (not-empty)
                                      (into (sorted-set)
                                            (map #(string/join " -> " %)))
                                      (pair ext))))))]
      (doseq [[ext uncovered-value-paths] uncovered-value-paths-by-ext]
        (is (= #{} uncovered-value-paths)
            (list-message
              (str "The following paths do not have values in any ." ext " files:")
              uncovered-value-paths))))))

(deftest silent-migrations-test
  (test-util/with-loaded-project project-path
    (testing "collection"
      (let [uniform-scale-collection (test-util/resource-node project "/silently_migrated/uniform_scale.collection")
            referenced-collection (:node-id (test-util/outline uniform-scale-collection [0]))
            embedded-go (:node-id (test-util/outline uniform-scale-collection [1]))
            referenced-go (:node-id (test-util/outline uniform-scale-collection [2]))]
        (is (= collection/CollectionInstanceNode (g/node-type* referenced-collection)))
        (is (= collection/EmbeddedGOInstanceNode (g/node-type* embedded-go)))
        (is (= collection/ReferencedGOInstanceNode (g/node-type* referenced-go)))
        (is (= [2.0 2.0 2.0] (g/node-value referenced-collection :scale)))
        (is (= [2.0 2.0 2.0] (g/node-value embedded-go :scale)))
        (is (= [2.0 2.0 2.0] (g/node-value referenced-go :scale)))))

    (testing "material"
      (let [legacy-textures-material (test-util/resource-node project "/silently_migrated/legacy_textures.material")]
        (is (= [{:filter-mag :filter-mode-mag-linear
                 :filter-min :filter-mode-min-linear
                 :max-anisotropy 1.0
                 :name "albedo"
                 :wrap-u :wrap-mode-clamp-to-edge
                 :wrap-v :wrap-mode-clamp-to-edge}
                {:filter-mag :filter-mode-mag-linear
                 :filter-min :filter-mode-min-linear
                 :max-anisotropy 1.0
                 :name "normal"
                 :wrap-u :wrap-mode-clamp-to-edge
                 :wrap-v :wrap-mode-clamp-to-edge}]
               (g/node-value legacy-textures-material :samplers)))))))
