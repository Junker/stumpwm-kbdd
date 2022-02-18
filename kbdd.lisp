;;;; kbdd.lisp

(in-package :kbdd)

(defvar *path* "/ru/gentoo/KbddService")
(defvar *destination* "ru.gentoo.KbddService")
(defvar *interface* "ru.gentoo.kbdd")

(defvar *server-thread* nil)
(defvar *dbus-conn* nil)
(defvar *dbus-bus* nil)
(defvar *dbus-obj* nil)

(defvar *locales* '((0 . :en)))
(defvar *current-layout* nil)


(defmacro dbus-call (fun-name &rest args)
  `(dbus:object-invoke *dbus-obj* *interface* ,fun-name ,@args))

(defun id-to-name (id)
  (cdr (assoc id *locales*)))

(defun name-to-id (name)
  (car (rassoc name *locales*)))

(defun get-current-layout-id ()
  (dbus-call "getCurrentLayout"))

(defun set-layout-id (id)
  (check-type id number)
  (dbus-call "set_layout" id))

(defun next-layout ()
  (dbus-call "next_layout"))

(defun prev-layout ()
  (dbus-call "prev_layout"))

(defun get-current-layout ()
  (id-to-name (get-current-layout-id)))

(defun set-layout (layout)
  (check-type layout symbol)
  (set-layout-id (name-to-id layout)))

(defun init ()
  (setf *dbus-conn*
	      (dbus:open-connection
	       (make-instance 'iolib.multiplex:event-base) (dbus:session-server-addresses)))

  (dbus:authenticate (dbus:supported-authentication-mechanisms *dbus-conn*)
                     *dbus-conn*)

  (setf *dbus-bus* (make-instance 'dbus::bus
                                  :name (dbus:hello *dbus-conn*)
                                  :connection *dbus-conn*))

  (setf *dbus-obj* (dbus:make-object-from-introspection *dbus-conn*
                                                        *path*
                                                        *destination*))

  (setf *current-layout* (get-current-layout)))

(defun subscribe ()
  (setf *server-thread*
        (bt:make-thread #'(lambda()
                            (dbus:with-open-bus (bus (dbus:session-server-addresses))
                              (dbus:add-match bus :interface *interface* :type :signal)
                              (dbus:publish-objects bus)))
                        :name "kbdd-listener")))

(defun kbdd ()
  (init)
  (subscribe))

(defun modeline (ml)
  (declare (ignore ml))
  (string *current-layout*))

(dbus:define-dbus-object kbdd-service (:path *path*))

(dbus:define-dbus-signal-handler (kbdd-service layout-changed) ((id :uint32))
  (:interface *interface*)
  (:name "layoutChanged")

  (setf *current-layout* (id-to-name id)))


(defcommand kbdd-next-layout () ()
  "Switch to next layout"
  (next-layout))

(defcommand kbdd-prev-layout () ()
  "Switch to previous layout"
  (prev-layout))

(defcommand kbdd-switch-layout () ()
  "Switch layout"
  (if (eq *current-layout* (cdar (last kbdd:*locales*)))
      (set-layout-id 0)
      (set-layout-id (+ (name-to-id *current-layout*) 1))))

;; modeline formatter.
(add-screen-mode-line-formatter #\L #'modeline)
