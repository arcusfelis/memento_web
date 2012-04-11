qx.Class.define("memento_web.Remote",
{
  extend : qx.core.Object,

  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  properties : {
    active : { init : false, check: "Boolean" }
  },

  construct : function()
  {
    var basedir = document.location.href;

    // Form url with ws:// protocol
    basedir = basedir.replace('http:', 'ws:').replace('https:', 'wss:');

    // basedir of url
    basedir = basedir.substring(0, basedir.lastIndexOf('/')) + '/';

    // url of the bullet handler
    this.__url = basedir + "stream";
  },

  members :
  {
    __bullet : null,
    __buffer : [],
    __url : null,

    
    finalize : function()
    {
      this.__openConnection();
    },



    /**
     * TODOC
     *
     * @param query {var} TODOC
     */
    sendJSON : function(query) {
      this.sendText(qx.lang.Json.stringify(query));
    },


    /**
     * TODOC
     *
     * @param query {var} TODOC
     */
    sendText : function(query) {
      if (this.getActive()) {
        this.__bullet.send(query);
      } else {
        this.__buffer.push(query);
      }
    },

    /**
     * Create new connection to the server
     *
     */
    reconnect : function()
    {
      this.__closeConnection();
      this.__openConnection();
    },

    sendBuffer : function()
    {
        var mess;
        while (mess = this.__buffer.shift())
        {
            this.sendText(mess);
        }
    },

    /* Initially send information about the object */
    registerObject : function(item)
    {
        var hash = item.toHashCode();
        this.sendJSON([{"type" : "registerObject",
            "hash" : hash, "path" : item.classname.split(".")}]);
    },


    /* Message handler */
    fromErlang : function(action)
    {
        var hash = action.hash;
        var item = qx.core.ObjectRegistry.fromHashCode(hash);
        if (item == null) 
        {
            this.warn("Item not found" + hash);
            return false;
        }
        switch (action.type)
        {
            case "fireEvent":
                var event = action.data;
                if (event.data)
                    item.fireDataEvent(event.type, event.data);
                else 
                    item.fireEvent(event.type);
                break;

            case "addListener":
                var event = action.data;
                item.addListener(event.type, this.__listener, this);
                break;
            
        }
    },


    /* Receive events from qooxdoo objects */
    __listener : function(e)
    {
        console.dir(e);
    },


    /**
     * TODOC
     *
     */
    __openConnection : function()
    {
      var bullet = $.bullet(this.__url);
      var store = this;
      this.__bullet = bullet;

      bullet.onopen = function()
      {
        store.info('WebSocket: opened');

        var i = 0;

        store.setActive(true);
        store.sendBuffer();

        this.__buffer = [];
      };

      bullet.onclose = function()
      {
        store.info('WebSocket: closed');
        store.setActive(false);
      };

      bullet.onmessage = function(e)
      {
        store.info('WebSocket: ' + e.data);
        var parsedData = qx.lang.Json.parse(e.data);
        store.fromErlang(parsedData);
      };

      bullet.onheartbeat = function() {
        store.info('WebSocket: heartbeat');
      };
    },


    /**
     * TODOC
     *
     */
    __closeConnection : function()
    {
      try {
        this.__bullet.close();
      } catch(err) {
        this.error("There are some problems with bullet.", err);
      }

      // Cannot purge it fully.
      // Avoid few connections.
      this.__bullet.onclose = function() {
        this.info("Old connection was closed");
      };

      this.__bullet.onmessage = function() {
        this.error("FIXME: deads are alive.");
      };

      this.__bullet.onheartbeat = this.__bullet.onmessage;
      this.__bullet.onclose = function() {};
      delete this.__bullet;
    }
  }
});

