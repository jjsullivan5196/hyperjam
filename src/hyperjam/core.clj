(ns hyperjam.core
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.xml :as xml]
            [ring.middleware.params]
            [ring.util.io]
            [ring.adapter.jetty]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]))

(xml/alias-uri 'xh "http://www.w3.org/1999/xhtml")

(defn ^:private uri->name
  "Return the name of the resource denoted by `uri`."
  [uri]
  (-> (.getPath uri)
      (.substring 1)
      edn/read-string))

(defn ^:private locate
  "Find the resource for `name-sym`."
  [name-sym]
  (cond
    (qualified-symbol? name-sym) (find-var name-sym)
    (symbol? name-sym)           (find-ns name-sym)
    :else                        nil))

(defmulti xhtmlify "Return a representation of `x` as hypertextual data." type)

(defmethod xhtmlify :default
  [x]
  [::xh/span {} (print-str x)])

(defmethod xhtmlify clojure.lang.Var
  [v]
  (let [vmeta (meta v)
        vname (str (-> vmeta :ns ns-name name) "/" (-> vmeta :name name))]
    [::xh/details {:open true}
     [::xh/summary {} [::xh/h3 {:style "display:inline;"} vname]]
     [::xh/span {} (:doc vmeta)]
     [::xh/form {:action (str "/" vname)
                 :method "post"}
      (xhtmlify @v)]]))

(defmethod xhtmlify clojure.lang.IFn
  [f]
  [::xh/label {}
   "Args: " [::xh/input {:name "args" :type "text"}]
   [::xh/input {:value "Apply" :type "submit"}]])

(prefer-method xhtmlify clojure.lang.Var clojure.lang.IFn)

(defn test-fn
  "test!"
  [params]
  (get params "args"))

(defn xhtml-doc-root
  [body]
  [::xh/html {:xmlns "http://www.w3.org/1999/xhtml"}
   `[::xh/body {} ~@body]])

(defn ^:private wrap-locate-resource
  [h]
  (fn [{:keys [uri] :as request}]
    (if-let [resource (locate (uri->name (java.net.URI. uri)))]
      (merge (h (merge request {:resource resource}))
             {:status 200})
      (merge (h request)
             {:status 404}))))

(def ^:private implemented-http-methods
  #{:get :post})

(defn ^:private wrap-http-method
  [h]
  (fn [{:keys [resource request-method] :as request}]
    (merge (h request)         
           (when (and resource
                      (contains? implemented-http-methods request-method))
             (let [representation (xhtml-doc-root
                                   [(-> (case request-method
                                          :get  resource 
                                          :post (resource (:params request)))
                                        xhtmlify)])
                   xml-doc        (-> representation
                                      xml/sexp-as-element
                                      (xml/emit-str {:doctype "<!DOCTYPE html>"}))]
               {:body xml-doc})))))

(def ^:private handler
  (-> (constantly {})
      wrap-http-method
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

  (meta #'+)

  (str 'foo)

  (type java.lang.Class)

  (instance? clojure.lang.IPersistentSet #{})

  (println (keys (methods print-method)))

  (ns-name *ns*)

  (with-out-str
    (print-method nil *out*))

  (xhtml-doc-root [(xhtmlify #'test-fn)])
  
  )
