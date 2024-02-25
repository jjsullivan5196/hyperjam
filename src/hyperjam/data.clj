(ns hyperjam.data
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hickory.render]
            [ring.middleware.params]
            [ring.adapter.jetty]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]
            [clojure.data.xml :as xml]
            [clojure.spec.alpha :as s]))

(s/def ::locator
  (s/or ::uri uri?
        ::qualified-symbol qualified-symbol?
        ::unqualified-symbol (complement qualified-symbol?)))

(defmulti ->resource
  "Find the resource for the identifier provided."
  (fn [l]
    (first (s/conform ::locator l))))

(defmethod ->resource ::uri
  [uri]
  (-> (.getPath uri)
      (.substring 1)
      edn/read-string
      ->resource))

(defmethod ->resource ::qualified-symbol
  [name-sym]
  (find-var name-sym))

(defmethod ->resource ::unqualified-symbol
  [ns-sym]
  (find-ns ns-sym))

(defn representation-dispatch
  "Dispatches for representation multifns."
  [_resource mime-type & _args]
  mime-type)

(defmulti ->representation      "Produce the representation for the resource." representation-dispatch)
(defmulti accept-representation "Send a received representation to the resource." representation-dispatch)

(defmulti hiccupify "Return a representation of `x` as hypertextual data." type)

(defmethod hiccupify java.lang.String
  [str]
  [:span {:data-type "text"} str])

(defmethod hiccupify java.lang.Long
  [n]
  [:span {:data-type "integer"} (str n)])
  
(defmethod hiccupify java.lang.Double
  [n]
  [:span {:data-type "real"} (str n)])

(defmethod hiccupify clojure.lang.Symbol
  [sym]
  (let [sym-str (str sym)]
    [:a {:data-type "name" :href (str "/" sym-str)} sym-str]))

(defmethod hiccupify clojure.lang.Keyword
  [kw]
  [:span {:data-type "keyword"} (name kw)])

(defmethod hiccupify clojure.lang.IPersistentSet
  [v]
  `[:ul {:data-type "set"}
    ~@(->> v
           (map (fn [x]
                  [:li (hiccupify x)])))])

(defmethod hiccupify clojure.lang.IPersistentVector
  [v]
  `[:ol {:data-type "random-access"}
    ~@(->> v
           (map (fn [x]
                  [:li (hiccupify x)])))])

(defmethod hiccupify clojure.lang.IPersistentList
  [v]
  `[:div {:data-type "list"} ~@(map hiccupify v)])

(defmethod hiccupify clojure.lang.IPersistentMap
  [v]
  `[:table {:data-type ""}
    [:tr [:th "key"] [:th "value"]]
    ~@(->> (seq v)
           (map (fn [[k v]]
                  [:tr
                   [:td (hiccupify k)]
                   [:td (hiccupify v)]])))])

(defrecord Foo [bar])

(defn ^:private wrap-locate-resource
  [h]
  (fn [request]
    (let [uri (-> request :uri java.net.URI.)]
      (if-let [resource (->resource uri)]
        (merge (h (merge request {:resource resource}))
               {:status 200})
        (merge (h request)
               {:status 404})))))

(defn ^:private wrap-negotiate-response-type
  [h]
  (fn [{:keys [resource] :as request}]
    (if resource
      (let [negotiated-type (->> (str/split (get-in request [:headers "accept"]) #",")
                                 first)
            response        (h (merge request {:negotiated-type negotiated-type}))]
        (merge response
               {:headers (merge (:headers response)
                                {"Content-Type" negotiated-type})}))
      (h request))))

(def ^:private implemented-http-methods
  #{:get :post})

(defn ^:private wrap-http-method
  [h]
  (fn [{:keys [negotiated-type resource request-method] :as request}]
    (merge (h request)         
           (when (and negotiated-type resource
                      (contains? implemented-http-methods request-method))
             (let [received-type (get-in request [:headers "content-type"])]
               {:body (case request-method
                        :get  (->representation resource negotiated-type)
                        :post (-> (accept-representation resource received-type request)
                                  (->representation negotiated-type))
                        #_:put  #_(replace-representation resource received-type request))})))))

(def ^:private handler
  (-> (constantly {})
      wrap-http-method
      wrap-negotiate-response-type
      wrap-locate-resource
      ring.middleware.params/wrap-params))

(defonce ^:private server
  (ring.adapter.jetty/run-jetty #'handler {:port 8080 :join? false}))

(comment

  (->> (vals (ns-interns *ns*))
       (filter #(instance? clojure.lang.Fn @%)))

  (-> [(->hiccup #'->html)]
      hickory.render/hiccup-to-html)

  (meta #'->resource)

  (str 'foo)

  (type java.lang.Class)

  (instance? clojure.lang.IPersistentSet #{})

  (class 55555.55)

  (hiccupify
   '("click this link " 55 " times"
     foo.bar/baz))

  (hiccupify
   {:foo "bar"
    :baz "buzz"})

  (->Foo "baz")

  #hyperjam.data.Foo{:bar "baz"}

  #java.lang.String["foo"]

  (class clojure.lang.IPersistentMap)

  (-> (methods print-method)
      #_(get 'clojure.lang.PersistentArrayMap)
      #_keys
      (get :default)
      class
      clojure.datafy/datafy
      :name
      clojure.repl/source-fn)

  (class {})

  (xml/element :head)

  (-> (clojure.datafy/datafy clojure.lang.MultiFn)
      :members
      (get 'dispatchFn))

  (instance? clojure.lang.IObj {})

  (isa? clojure.lang.PersistentArrayMap clojure.lang.IPersistentMap)

  (class {})

  ((.-dispatchFn print-method) {} nil)

  (clojure.repl/source clojure.core$fn__7348)

  (with-out-str
    ((-> (methods print-method) (get :default)) {} *out*))

  clojure.lang.IRef

  (s/describe 'clojure.core/defn)

  clojure.core.specs.alpha  
  print

  (ns-interns *ns*)
  
  )
