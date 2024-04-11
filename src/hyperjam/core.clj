(ns hyperjam.core
  (:require [hickory.render]
            [ring.middleware.params]
            [ring.util.io]
            [ring.adapter.jetty]
            [clojure.edn :as edn]
            [clojure.pprint]))

(defn ^:private uri->name
  "Return the name of the resource denoted by `uri`."
  [uri]
  (-> (.getPath uri)
      (.substring 1)
      edn/read-string))

(defn ^:private locate
  "Find the resource for `name-sym`."
  [name-sym]
  @(find-var name-sym))

(def ^:private htmz
  "https://leanrada.com/htmz/"
  [[:iframe
    {:hidden true
     :name   "htmz"
     :onload "setTimeout(()=>document.querySelector(contentWindow.location.hash||null)?.replaceWith(...contentDocument.body.childNodes))"}]
   [:base {:target "htmz"}]])

(defn hrepl
  [h]
  (fn [request]
    (merge
     (h request)
     {:status  200
      :headers {"Content-Type" "text/html"}
      :body
      (str "<!DOCTYPE html>"
           (hickory.render/hiccup-to-html
            `[[:html
               [:head [:title "REPL"]]
               [:body
                ~@htmz
                [:table {:id "next-eval"}]
                [:form {:action "/hyperjam.core/heval#next-eval"}
                 [:input {:name "form" :type "text"}]
                 [:input {:type "submit" :value "eval"}]]]]]))})))

(defn heval
  [h]
  (fn [{:keys [params] :as request}]
    (let [form-str  (get params "form")
          form      (edn/read-string form-str)
          evaluated (eval form)]
      (merge
       (h request)
       {:status  200
        :headers {"Content-Type" "text/html"}
        :body
        (str "<!DOCTYPE html>"
             (hickory.render/hiccup-to-html
              [[:html
                [:body
                 [:table {:id "next-eval"}
                  [:thead
                   [:tr [:th "Form"] [:th "Evaluated"]]]
                  [:tbody 
                   [:tr
                    [:td [:code form-str]]
                    [:td [:code (with-out-str
                                  (clojure.pprint/pprint evaluated))]]]]]]]]))}))))

(defn ^:private wrap-locate-resource
  [h]
  (fn [{:keys [uri] :as request}]
    (if-let [resource (locate (uri->name (java.net.URI. uri)))]
      (let [handler (resource h)]
        (handler request))
      (h request))))

(def ^:private handler
  (-> (constantly {:status 404})
      wrap-locate-resource
      ring.middleware.params/wrap-params))

(defonce ^:private server
  (ring.adapter.jetty/run-jetty
   #'handler
   {:port 8080 :join? false}))

(comment
  (.stop server)

  )
