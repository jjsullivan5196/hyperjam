(ns hyperjam.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [hickory.render]
            [ring.middleware.params]
            [ring.adapter.jetty]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]))

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

(defprotocol URLEncodedParametersRepresentation
  (accept-urlencodedparams [this params] "Accept a map of parameters from the received representation."))

(extend-protocol URLEncodedParametersRepresentation
  clojure.lang.Var
  (accept-urlencodedparams [v params]
    (accept-urlencodedparams @v params))
  
  clojure.lang.Fn
  (accept-urlencodedparams [f params]
    (let [args (edn/read-string (str "(" (get params "args") ")"))]
      (apply f args))))

(defmethod accept-representation "application/x-www-form-urlencoded"
  [resource _mime-type request]
  (accept-urlencodedparams resource (:form-params request)))

(defprotocol HiccupRepresentation
  (->hiccup [this] "Convert resource to hiccup representation."))

(defn ->html
  "Produce an html representation of `resource`."
  [resource]
  (hickory.render/hiccup-to-html
   [(->hiccup resource)]))

(defmethod ->representation "text/html"
  [resource _mime-type]
  (->html resource))

(defn ^:private ns-local-uri
  "Get the URI of namespace `ns` on the classpath."
  [ns]
  (let [local-path (-> ns
                       ns-name
                       (#'clojure.core/root-resource)
                       (.substring 1))]
    (->> ["cljc" "clj" "cljs"]
         (map (fn [ext] (io/resource (str local-path "." ext))))
         (some identity))))

(extend-protocol HiccupRepresentation
  clojure.lang.Namespace
  (->hiccup [ns]
    `[:details {:open "true"}
      [:summary [:h3 {:style "display:inline;"}
                 ~(str "Namespace " (ns-name ns))]]
      ~@(->> (ns-interns ns)
             vals
             (filter #(and (instance? clojure.lang.Fn @%)
                           (not (:private (meta %)))))
             (map ->hiccup))])
  
  clojure.lang.Var
  (->hiccup [v]
    (let [vmeta (meta v)
          vname (str (ns-name (:ns vmeta)) "/" (:name vmeta))]
      [:details {:open "true"}
       [:summary [:a {:href (str "/" vname)}
                  [:h3 {:style "display:inline;"} vname]]]
       [:section [:p (or (:doc vmeta) "")]]
       [:form {:method "post" :action (str "/" vname)}
        (->hiccup @v)]]))

  clojure.lang.IPersistentCollection
  (->hiccup [x]
    [:pre (print-str x)])
  
  clojure.lang.Fn
  (->hiccup [f]
    [:div
     [:label "Arguments: "
      [:input {:name "args" :type "text"}]]
     [:input {:value "Apply" :type "submit"}]]))

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

  (.stop server)

  (->> (vals (ns-interns *ns*))
       (filter #(instance? clojure.lang.Fn @%)))

  (-> [(->hiccup #'->html)]
      hickory.render/hiccup-to-html)

  (meta #'->resource)

  (str 'foo)

  (type java.lang.Class)

  (instance? clojure.lang.IPersistentSet #{})

  (println (keys (methods print-method)))

  (with-out-str
    (print-method nil *out*))
  
  )
