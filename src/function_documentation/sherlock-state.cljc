(ns sherlock.state
  (:require [ysera.test :refer [is=]]))

(defn create-state
  ([]
   (create-state {}))
  ([{projects :projects
     packages :packages
     repos    :repos}]
   (-> {:projects {}}
       ((fn [state]
          (if projects
            (reduce (fn [state project]
                      (assoc-in state [:projects (:id project)] project))
                    state
                    projects)
            state)))
       ((fn [state]
          (if packages
            (reduce (fn [state package]
                      (assoc-in state [:packages (:id package)] package))
                    state
                    packages)
            state)))
       ((fn [state]
          (if repos
            (reduce (fn [state repo]
                      (assoc-in state [:repos (:id repo)] repo))
                    state
                    repos)
            state))))))

; Project

(defn create-project
  "A function that creates a project"
  {:test (fn []
           (is= (create-project {:stash-key "proj1"})
                {:id        "proj1"
                 :stash-key "proj1"
                 :fetching  false
                 :response  nil
                 :repos     {}}))}
  [kvs]
  {:pre  [(not (nil? (:stash-key kvs)))]
   :post [(not (nil? (:id %)))]}
  {:id        (or (:id kvs) (:stash-key kvs))
   :stash-key (or (:stash-key kvs) nil)
   :fetching  (or (:fetching kvs) false)
   :response  (or (:response kvs) nil)
   :repos     (or (:repos kvs) {})})

(defn get-project
  {:test (fn []
           (is= (get-project {:projects {"proj1" :foo}}
                             "proj1")
                :foo))}
  [state project-id]
  (get-in state [:projects project-id]))

(defn get-projects
  {:test (fn []
           (is= (get-projects {:projects {"proj1" {:id "proj1"}
                                          "proj2" {:id "proj2"}}})
                [{:id "proj1"} {:id "proj2"}]))}
  [state]
  (or (vals (:projects state)) []))

(defn get-project-by-stash-key
  {:test (fn []
           (let [state {:projects {"p1" {:id "p1" :stash-key "p1key"}
                                   "p2" {:id "p2" :stash-key "p2key"}
                                   "p3" {:id "p3" :stash-key "p3key"}}}]
             (is= (:id (get-project-by-stash-key state "p1key")) "p1")
             (is= (:id (get-project-by-stash-key state "p3key")) "p3")
             (is= (:id (get-project-by-stash-key state "p4key")) nil)))}
  [state project-stash-key]
  (->> (get-projects state)
       (filter (fn [project]
                 (= (:stash-key project) project-stash-key)))
       (first)))

(defn get-project-ids
  {:test (fn []
           (is= (get-project-ids {:projects {"proj1" {:id "proj1"}
                                             "proj2" {:id "proj2"}}})
                ["proj1" "proj2"]))}
  [state]
  (or (keys (:projects state)) []))

(defn assoc-project
  [state project]
  {:pre [(not (nil? (:id project)))]}
  (assoc-in state [:projects (:id project)] project))

(defn assoc-projects
  [state projects]
  (reduce (fn [state project]
            (assoc-project state project))
          state
          projects))

(defn update-entity
  {:test (fn []
           (is= (update-entity {:coll         {:entity {:foo 1}}
                                :entity-or-id {:foo 1}
                                :args         [:foo inc]
                                :entity?      map?
                                :assoc-entity (fn [state entity] (assoc state :entity entity))})
                {:entity {:foo 2}}))}
  [{coll         :coll
    entity-or-id :entity-or-id
    args         :args
    entity?      :entity?
    get-entity   :get-entity
    assoc-entity :assoc-entity}]
  (let [entity (or (and (entity? entity-or-id) entity-or-id)
                   (get-entity coll entity-or-id))
        entity (reduce (fn [entity [key value]]
                         (if (fn? value)
                           (update entity key value)
                           (assoc entity key value)))
                       entity
                       (partition 2 args))]
    (assoc-entity coll entity)))

(defn update-project
  {:test (fn []
           (is= (update-project (create-state {:projects [{:id "p1" :foo true}]})
                                {:id "p1" :foo false})
                (create-state {:projects [{:id "p1" :foo false}]}))
           (is= (update-project (create-state {:projects [{:id "p1" :foo true}]})
                                {:id "p1" :foo false}
                                :bar 123
                                :car "hello")
                (create-state {:projects [{:id "p1" :foo false :bar 123 :car "hello"}]}))
           (is= (update-project (create-state {:projects [{:id "p1" :foo true}]})
                                "p1"
                                :bar 123
                                :car "hello")
                (create-state {:projects [{:id "p1" :foo true :bar 123 :car "hello"}]})))}
  [state project-or-id & args]
  (update-entity {:coll         state
                  :entity-or-id project-or-id
                  :args         args
                  :entity?      map?
                  :get-entity   get-project
                  :assoc-entity assoc-project}))

; Repo

(defn create-repo
  {:test (fn []
           (is= (create-repo {:stash-slug        "r1"
                              :project-stash-key "p1"})
                {:id                   "r1"
                 :stash-slug           "r1"
                 :project-stash-key    "p1"
                 :name                 nil
                 :fetching             false
                 :response             nil
                 :branches-of-interest #{}
                 :tags                 #{}
                 :branches             {}}))}
  [kvs]
  {:id                   (or (:id kvs) (:stash-slug kvs))
   :stash-slug           (or (:stash-slug kvs) nil)
   :project-stash-key    (:project-stash-key kvs)
   :name                 (or (:name kvs) nil)
   :fetching             (or (:fetching kvs) false)
   :response             (or (:response kvs) nil)
   :branches-of-interest (or (:branches-of-interest kvs) #{})
   :tags                 (or (:tags kvs) #{})
   :branches             (or (:branches kvs) {})})

(defn get-repos
  {:test (fn []
           (is= (get-repos (create-state {:projects [(create-project {:stash-key "p1"
                                                                      :repos     {"r1" :foo
                                                                                  "r2" :bar}})]})
                           "p1")
                [:foo :bar])
           (is= (get-repos (create-state {:projects [(create-project {:stash-key "p1"
                                                                      :repos     {"r1" :foo
                                                                                  "r2" :bar}})
                                                     (create-project {:stash-key "p2"
                                                                      :repos     {"rr1" :xar
                                                                                  "rr2" :car}})]}))
                [:foo :bar :xar :car]))}
  ([state]
   (->> (get-project-ids state)
        (map (fn [project-id] (get-repos state project-id)))
        (flatten)))
  ([state project-id]
   (or (vals (:repos (get-project state project-id))) [])))

(defn assoc-repo
  {:test (fn []
           ;; Should be able to assoc repo to existing project.
           (is= (-> (create-state {:projects [(create-project {:id "p1" :stash-key "p1key"})]})
                    (assoc-repo (create-repo {:stash-slug "r1" :project-stash-key "p1key"}))
                    (get-repos "p1"))
                [(create-repo {:stash-slug "r1" :project-stash-key "p1key"})])
           ;; Should be able to assoc repo to non-existing project.
           (is= (-> (create-state)
                    (assoc-repo (create-repo {:stash-slug "r1" :project-stash-key "p1key"}))
                    (get-repos))
                [(create-repo {:stash-slug "r1" :project-stash-key "p1key"})]))}
  [state repo]
  {:pre [(not (nil? (:id repo)))]}
  (let [project-stash-key (:project-stash-key repo)
        project (get-project-by-stash-key state project-stash-key)
        [state project] (if project
                          [state project]
                          (let [state (assoc-project state (create-project {:stash-key project-stash-key}))]
                            [state
                             (get-project-by-stash-key state project-stash-key)]))]
    (update-project state (:id project) :repos (fn [repos]
                                                 (assoc repos (:id repo) repo)))))

(defn get-repo
  {:test (fn []
           (is= (:id (get-repo (assoc-repo (create-state)
                                           (create-repo {:id "r1" :stash-slug "r1" :project-stash-key "p1"}))
                               "r1"))
                "r1"))}
  [state repo-id]
  (->> (get-repos state)
       (filter (fn [repo]
                 (= (:id repo) repo-id)))
       (first)))

(defn dissoc-repo
  {:test (fn []
           ;; Should be able to dissoc repo.
           (is= (-> (create-state {:projects [(create-project {:id "p1" :stash-key "p1key"})]})
                    (assoc-repo (create-repo {:id "r1" :stash-slug "r1" :project-stash-key "p1key"}))
                    (dissoc-repo "r1")
                    (get-repos "p1"))
                []))}
  [state repo-id]
  {:pre [(not (nil? repo-id))]}
  (let [repo (get-repo state repo-id)
        project-stash-key (:project-stash-key repo)
        project (get-project-by-stash-key state project-stash-key)]
    (update-project state (:id project) :repos (fn [repos]
                                                 (dissoc repos (:id repo))))))

(defn get-repo-by-stash-slug
  {:test (fn []
           (is= (:id (get-repo-by-stash-slug (assoc-repo (create-state)
                                                         (create-repo {:id "id1" :stash-slug "r1" :project-stash-key "p1"}))
                                             "r1"))
                "id1"))}
  [state stash-slug]
  (->> (get-repos state)
       (filter (fn [repo]
                 (= (:stash-slug repo) stash-slug)))
       (first)))

(defn update-repo
  {:test (fn []
           (is= (-> (update-repo (assoc-repo (create-state) (create-repo {:id "r1" :stash-slug "r1" :project-stash-key "p1"}))
                                 (create-repo {:id "r1" :stash-slug "r1" :project-stash-key "p1" :fetching :foo}))
                    (get-repo "r1"))
                (create-repo {:id "r1" :stash-slug "r1" :project-stash-key "p1" :fetching :foo})))}
  [state repo-or-id & args]
  (update-entity {:coll         state
                  :entity-or-id repo-or-id
                  :args         args
                  :entity?      map?
                  :get-entity   get-repo
                  :assoc-entity assoc-repo}))


;; Package

(defn create-package
  {:test (fn []
           (is= (create-package {:repo-stash-slug   "r1"
                                 :project-stash-key "p1"
                                 :branch            "b1"})
                {:id                "p1/r1/b1"
                 :repo-stash-slug   "r1"
                 :project-stash-key "p1"
                 :branch            "b1"
                 :fetching          false})
           (is= (create-package {:repo-stash-slug   "r1"
                                 :project-stash-key "p1"
                                 :commit-ish        "c1"})
                {:id                "p1/r1/c1"
                 :repo-stash-slug   "r1"
                 :project-stash-key "p1"
                 :commit-ish        "c1"
                 :fetching          false}))}
  [kvs]
  (->> {:id                (or (:id kvs) (str (:project-stash-key kvs) "/" (:repo-stash-slug kvs) "/" (or (:branch kvs) (:commit-ish kvs))))
        :repo-stash-slug   (or (:repo-stash-slug kvs) nil)
        :project-stash-key (:project-stash-key kvs)
        :branch            (:branch kvs)
        :commit-ish        (:commit-ish kvs)
        :name              (or (:name kvs) nil)
        :fetching          (or (:fetching kvs) false)
        :response          (or (:response kvs) nil)}
       (remove (fn [[k v]] (= nil v)))
       (into {})))

(defn assoc-package
  [state package]
  {:pre [(not (nil? (:id package)))]}
  (assoc-in state [:packages (:id package)] package))

(defn dissoc-package
  {:test (fn []
           (is= (-> (create-state)
                    (assoc-package {:id "p1"})
                    (assoc-package {:id "p2"})
                    (dissoc-package "p1"))
                (-> (create-state)
                    (assoc-package {:id "p2"}))))}
  [state package-id]
  {:pre [(string? package-id)]}
  (update state :packages dissoc package-id))

(defn assoc-packages
  [state packages]
  (reduce (fn [state package]
            (assoc-package state package))
          state
          packages))

(defn get-packages
  {:test (fn []
           (is= (get-packages {:packages {"package1" {:id "package1"}
                                          "package2" {:id "package2"}}})
                [{:id "package1"} {:id "package2"}]))}
  [state]
  (or (vals (:packages state)) []))

(defn get-package
  {:test (fn []
           (is= (get-package {:packages {"x" :foo}}
                             {:id "x"})
                :foo)
           (is= (get-package {:packages {"x" {:id              "x"
                                              :repo-stash-slug "slug"
                                              :commit-ish      "abc"}}}
                             {:repo-stash-slug "slug"
                              :commit-ish      "abc"})


                {:id              "x"
                 :repo-stash-slug "slug"
                 :commit-ish      "abc"})
           (is= (get-package (create-state {:packages [{:id              "x"
                                                        :repo-stash-slug "slug"
                                                        :commit-ish      "abc"}]
                                            :repos    [{:id              "r"
                                                        :repo-stash-slug "slug"}]})
                             {:repo-stash-slug "slug"
                              :commit-ish      "abc"})
                {:id              "x"
                 :repo-stash-slug "slug"
                 :commit-ish      "abc"})
           (is= (get-package (assoc-repo (create-state {:packages [{:id              "x"
                                                                    :repo-stash-slug "slug"
                                                                    :commit-ish      "abc"}]})
                                         (create-repo {:id                "r"
                                                       :stash-slug        "slug"
                                                       :project-stash-key "xxx"}))
                             {:repo-id    "r"
                              :commit-ish "abc"})
                {:id              "x"
                 :repo-stash-slug "slug"
                 :commit-ish      "abc"}))}
  [state {id              :id
          repo-stash-slug :repo-stash-slug
          commit-ish      :commit-ish
          repo-id         :repo-id}]
  {:pre [(or id (and repo-stash-slug commit-ish) (and repo-id commit-ish))]}
  (if id
    (get-in state [:packages id])
    (if repo-id
      (recur state {:repo-stash-slug (:stash-slug (get-repo state repo-id))
                    :commit-ish      commit-ish})
      (->> (get-packages state)
           (filter (fn [package]
                     (and (= (:repo-stash-slug package) repo-stash-slug)
                          (= (:commit-ish package) commit-ish))))
           (first)))))

(defn update-package
  {:test (fn []
           (is= (-> (update-package {:packages {"x" {:id "x"}}}
                                    {:id "x" :foo :bar})
                    (get-package {:id "x"}))
                {:id "x" :foo :bar}))}
  [state package-or-id & args]
  (update-entity {:coll         state
                  :entity-or-id package-or-id
                  :args         args
                  :entity?      map?
                  :get-entity   (fn [state id] (get-package state {:id id}))
                  :assoc-entity assoc-package}))

(defn get-package-version
  {:test (fn []
           (is= (get-package-version (create-package {:response {:status 200
                                                                 :body   {:version "1.2.3"}}}))
                "1.2.3"))}
  [package]
  (get-in package [:response :body :version]))

(defn get-package-file-content
  {:test (fn [])}
  [package filename]
  (get-in package [:files filename]))