(defpackage #:cl-point-bot-asd
  (:use :cl :asdf))

(in-package :cl-point-bot-asd)

(defsystem cl-point-bot
  :name "cl-point-bot"
  :version "0.1"
  :maintainer "rayslava"
  :author "rayslava"
  :licence "MIT"
  :description "point.im client bot"
  :long-description "Bot is able to handle messages from point.im and repost them accordingly to rules"
  :depends-on ("cl+ssl" "usocket" "cl-json" "cl-ppcre")
  :components ((:file "main"
		      :depends-on ("connection"))
	       (:file "connection")))

