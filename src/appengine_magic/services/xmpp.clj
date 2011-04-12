(ns appengine-magic.services.xmpp
  (:require [appengine-magic.core :as core])
  (:import (com.google.appengine.api.xmpp
            JID Message MessageBuilder SendResponse
            SendResponse$Status XMPPService XMPPServiceFactory)))

(defonce *xmpp-service* (atom nil))

(defn get-xmpp-service []
  (when (nil? @*xmpp-service*)
    (reset! *xmpp-service* (XMPPServiceFactory/getXMPPService)))
  @*xmpp-service*)

(defrecord XmppMessage
    [from recipients body type])

(defn ensure-jid [jid]
  (cond
   (instance? JID jid) jid
   (string? jid) (JID. jid)
   :else (throw (IllegalArgumentException. (str "Don't know how to convert " jid " to JID")))))

(defn ensure-jid-array [jids]
  (cond
   (instance? JID jids) (into-array JID [jids])
   (string? jids) (into-array JID [(JID. jids)])
   (sequential? jids) (into-array JID (map ensure-jid jids))
   :else (throw (IllegalArgumentException. (str "Don't know how to convert: " jids " to array of JID")))))

(defn make-message
  ([^XmppMessage msg]
     (make-message (:from msg) (:recipients msg) (:body msg)))
  ([from recipients body]
     (let [builder (doto (MessageBuilder.)
                     (.withFromJid (ensure-jid from))
                     (.withRecipientJids (ensure-jid-array recipients))
                     (.withBody body))]
       (.build builder))))

(defn convert-status-message [status]
  (let [status2kw (fn [status]
                    (condp = status
                        SendResponse$Status/SUCCESS :success
                        SendResponse$Status/INVALID_ID :invalid-id
                        SendResponse$Status/OTHER_ERROR :other-error
                        :unkown-status))]
  (into {} (for [[k v] status] [(.getId k) (status2kw v)]))))

(defn send-xmpp-message
  ([^XmppMessage msg]
     (send (:from msg) (:recipients msg) (:body msg)))
  ([from recipients body]
     (let [status (.sendMessage (get-xmpp-service) (make-message from recipients body))
           result (convert-status-message (.. status getStatusMap))]
       result)))
    
(defn parse-message [req]
  "Parses the incoming HttpServletRequest as an xmpp message"
  (.parseMessage (get-xmpp-service) req))
