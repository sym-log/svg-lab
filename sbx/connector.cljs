(ns svg.connectors)

(defn initSvgWorkspace [root]
  (do
    (def nodeArray (array))
    (def svgRoot root)
    (def connectorArray (array))
  ))

(deftype DragNDrop [] Object
         
         (init [ this target ]
           (do
             (.addEventListener target "mousedown"
                                (fn [evt] (. this grabit evt))
                                false)
             (.addEventListener svgRoot "mousemove"
                                (fn [evt] (. this dragit evt))
                                false)
             (.addEventListener svgRoot "mouseup"
                                (fn [evt] (. this dropit evt))
                                false)
             (set! (. this -p) (. svgRoot createSVGPoint))
             (set! (. this -offset) (. svgRoot createSVGPoint))
             (set! (. this -t) (. svgRoot createSVGPoint))
             
         ))
         
         (grabit [ this evt]
           (set! (. this -selected) (. evt -target))
           (if (. this -selected)
             (let [ ctm (.. this -selected getCTM) ]
                 (.setAttribute (. this -selected) "pointer-events" "none")
                 (.appendChild (.. this -selected -parentNode) (. this -selected))
                 (set! (. this -m) (.getScreenCTM svgRoot))
                 (set! (.. this -p -x) (. evt -clientX))
                 (set! (.. this -p -y) (. evt -clientY))
                 (set! (. this -p) (.matrixTransform (. this -p) (.. this -m inverse)))
                 (set! (.. this -offset -x) (- (.. this -p -x) (js.parseInt (. ctm -e))))
                 (set! (.. this -offset -y) (- (.. this -p -y) (js.parseInt (. ctm -f))))
          )))  
             
         (dragit [ this evt] 
           (if (. this -selected)
            (let [ ctm (.. evt -target getCTM)
                   node (aget nodeArray (.. this -selected -id))
                   connectors (. node -connectors)  ]
             (set! (.. this -p -x) (. evt -clientX))
             (set! (.. this -p -y) (. evt -clientY))
             (set! (. this -p) (.matrixTransform (. this -p) (.inverse (.getScreenCTM svgRoot))))
             (set! (.. this -p -x) (- (.. this -p -x) (.. this -offset -x)))
             (set! (.. this -p -y) (- (.. this -p -y) (.. this -offset -y)))
             (set! (.. this -t -x) (.. this -p -x))
             (set! (.. this -t -y) (.. this -p -y))
             (.setAttribute (. this -selected) "transform" (str "translate(" (.. this -p -x) "," (.. this -p -y) ")"))
             (set! (.. node -t -x) (.. this -p -x))
             (set! (.. node -t -y) (.. this -p -y))
             (.forEach connectors (fn [connector indx arr] (.drawConnector connector)))
          )))

         (dropit [this evt]
           (.setAttribute (. this -selected) "pointer-events" "all")
           (set! (. this -selected) nil)
         )
         
)

(defn getCenter [ elem ]
  (let [ centerPoint (. svgRoot createSVGPoint)
         bbox (. elem getBBox)   ]
    (set! (. centerPoint -x) (+ (. bbox -x) ( / (. bbox -width) 2)))
    (set! (. centerPoint -y) (+ (. bbox -y) ( / (. bbox -height) 2)))

    centerPoint))

(deftype Point [ elem ] Object
         
         (init [ this ] (do
                          (set! (. this -id) (. elem getAttribute "id"))
                          (set! (. this -x) (js.Number (. elem getAttribute "x")))
                          (set! (. this -y) (js.Number (. elem getAttribute "y")))
                          (set! (. this -role) (. elem getAttribute "role"))
                          (set! (. this -parent) (. elem -parentNode))
                          this))
         )

(deftype Node [ id ] Object
         
         (init [ this ]
           (set! (. this -node) (aget nodeArray id))
           (if-not (. this -node)
             (do
               (let [ elem (goog.dom.getElement (. this -id))
                      center (getCenter elem)
                      node (set! (. this -node) (js-obj))
                      ctm (. elem getCTM)
                      ports (set! (. node -ports) (array))   ]
                 
                 (set! (. node -id) id)
                 (set! (. node -elem) elem)
                 (set! (. node -x) (. center -x))
                 (set! (. node -y) (. center -y))
                 (set! (. node -t) (. svgRoot createSVGPoint))
                 (set! (.. node -t -x) (js.parseInt (. ctm -e)))
                 (set! (.. node -t -y) (js.parseInt (. ctm -f)))
                 (set! (. node -role) (. (. node -elem) getAttribute "role"))
                 (.forEach (js.Array.prototype.slice.call (.getElementsByTagName elem "point"))
                           (fn [val index array] (.push ports (.init (Point. val)))))
                 (set! (. node -port) nil)
                 (set! (. node -connectors) (array))
               )
             (aset nodeArray id (. this -node))
             (. this -node) )
           (. this -node)))
 )

(deftype Connector [ elem ] Object
         
 (init [ this ] (do
  (set! (. this -points) (array))
  (set! (. this -id) (. elem -id))
  (set! (. this -n1) (.init (Node. (.replace (. (. this -elem) getAttribute "n1") "#" ""))))
  (. (. (. this -n1) -connectors) push this)
  (set! (. this -n2) (.init (Node. (.replace (. (. this -elem) getAttribute "n2") "#" ""))))
  (. (. (. this -n2) -connectors) push this)
  (set! (. this -n1port) (. (. this -elem) getAttribute "n1port"))
  (set! (. this -n2port) (. (. this -elem) getAttribute "n2port"))
  (set! (. this -directed) (. (. this -elem) getAttribute "directed"))
  (.forEach
    (js.Array.prototype.slice.call
     (.getElementsByTagName elem "point"))
    (fn [elmt] (. ( . this -points) push (Point. elmt)) ))
  this))

 (findClosestPorts [this]
   (let [ node1x (+ (.. this -n1 -x) (.. this -n1 -t -x))
          node1y (+ (.. this -n1 -y) (.. this -n1 -t -y))
          node2x (+ (.. this -n2 -x) (.. this -n2 -t -x))
          node2y (+ (.. this -n2 -y) (.. this -n2 -t -y))
          minDistance (atom nil)
          portCouplet (array)    ]
             
   (.forEach (.. this -n1 -ports)
        (fn [n1port idx arr]
           (.forEach (.. this -n2 -ports)
              (fn [ n2port idx arr]
                (let [ n1portX (. n1port -x)
                       n1portY (. n1port -y)
                       n2portX (. n2port -x)
                       n2portY (. n2port -y)
                       distance (. this getDistance(+  n1portX node1x) (+ n1portY node1y)
                                                   (+  n2portX node2x) (+ n2portY node2y)) ]
                  (if (or (= @minDistance nil) (> @minDistance distance))
                    (do
                      (aset portCouplet 0 n1port)
                      (aset portCouplet 1 n2port)
                      (reset! minDistance distance))))
     ))))
   portCouplet))
 
 (getDistance [this x1 y1 x2 y2]
   (js.Math.sqrt (+ (js.Math.pow (- x2 x1 ) 2) (js.Math.pow (- y2 y1) 2))))

 (drawConnector [this]
   (let [ portPair (. this findClosestPorts)
          n1x (.-x (aget portPair 0))
          n1y (.-y (aget portPair 0))
          n2x (.-x (aget portPair 1))
          n2y (.-y (aget portPair 1))
          d (str "M"
            (+ n1x (.. this -n1 -t -x) (.. this -n1 -x))
            ","
            (+ n1y (.. this -n1 -t -y) (.. this -n1 -y))
            "L"
            (+ n2x (.. this -n2 -t -x) (.. this -n2 -x))
            ","
            (+ n2y (.. this -n2 -t -y) (.. this -n2 -y)) )
         ]
     
     (if (. this -path)
       (.setAttribute (. this -path) "d" d)
       (do
         (set! (. this -path) (js.document.createElementNS "http://www.w3.org/2000/svg" "path"))
         (.setAttribute (. this -path) "fill" "none")
         (.setAttribute (. this -path) "stroke-width" "1")
         (.setAttribute (. this -path) "stroke" "black")
         (.setAttribute (. this -path) "d" d)
         (.appendChild (.. this -elem -parentNode) (. this -path)))
     
    )))    

 )

(initSvgWorkspace (goog.dom.getElement "svgRoot"))

(.forEach (js.Array.prototype.slice.call (js.document.getElementsByTagName "connector"))
          (fn [connector idx arr] (.push connectorArray (.init (Connector. connector)))))

(.init (DragNDrop.) (goog.dom.getElement "nodes"))
