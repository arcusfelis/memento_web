qx.Class.define("memento_web.Remote",
{
  extend : qx.core.Object,

  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  properties : {
    state : { 
      init : "disconnected",
      check : function (value) {
        return qx.lang.Array.contains(this.getPossibleStateValues(), value);
      },
      apply : "_applyState"
    },
    possibleStateValues : {
      init : [ "disconnected", "connected", "authenticated", "failed" ]
    }
  },

  construct : function()
  {
    this.base(arguments);
    var basedir = document.location.href;

    // Form url with ws:// protocol
    basedir = basedir.replace('http:', 'ws:').replace('https:', 'wss:');

    // basedir of url
    basedir = basedir.substring(0, basedir.lastIndexOf('/')) + '/';

    // url of the bullet handler
    this.__url = basedir + "stream";
    this.__buffer = [];

    this.addListener("sessionStarted", this.__onSessionStarted, this);
    this.addListener("authCheckResult", this.__onAuthCheckResult, this);
  },

  members :
  {
    __bullet : null,
    __buffer : null,
    __url : null,
    __sessionId : null,

    
    finalize : function()
    {
      this.__openConnection();
    },

    _applyState : function(value, old, name)
    {
      this.info("New state " + value);
      switch(value)
      {
        case "authenticated":
          this.sendBuffer();
          break;
      }
    },


    /**
     * TODOC
     *
     * @param query {var} TODOC
     */
    sendJSON : function(query, priority) {
      this.sendText(qx.lang.Json.stringify(query), priority);
    },


    /**
     * TODOC
     *
     * @param query {var} TODOC
     */
    sendText : function(query, priority) 
    {
      var allowed = false;
      switch (this.getState())
      {
        case "authenticated":
          allowed = true;
          break;

        case "connected":
          allowed = priority == true;
      }

      if (allowed) {
        this.__bullet.send(query);
      } else {
        this.__buffer.push(query);
      }
    },

    /**
     * Let the server check this client
     */
    sendAuth : function() 
    {
        var hash = this.toHashCode();
        var mess = {
            "schema"    : "auth", 
            "hash"      : hash
        };
        if (this.__sessionId)
          mess.session_id = this.__sessionId;
        this.sendJSON(mess, true);
    },

    __onSessionStarted : function(e)
    {
      var data = e.getData();
      this.__sessionId = data.session_id;
      this.setState("authenticated");
    },

    __onAuthCheckResult : function(e)
    {
      var data = e.getData();
      if (data.success)
      {
        this.setState("authenticated");
      }
      else
      {
        this.setState("failed");
        this.error("The session is dead.");
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
        while ((this.getState() == "authenticated") 
            && (mess = this.__buffer.shift()))
        {
            this.sendText(mess);
        }
    },

    /* Initially send information about the object */
    registerObject : function(item)
    {
        var hash = item.toHashCode();
        var mess = {
            "schema"    : "action", 
            "type"      : "registerObject",
            "hash"      : hash, 
            "path"      : item.classname.split(".")
        };
        this.sendJSON(mess);
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
                this.info("fireEvent: " + event.type);
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
    __listener : function(event)
    {
        var item = event.getTarget();
        var hash = item.toHashCode();
        var type = event.getType();
        var mess = {
            "schema"    : "event", 
            "type"      : type,
            "hash"      : hash
        };
        switch (event.classname)
        {
            case "qx.event.type.Data":
                mess.data = event.getData();
                break;
        }
        this.sendJSON(mess);
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

        store.setState("connected");
        store.sendAuth();

        this.__buffer = [];
      };

      bullet.onclose = function()
      {
        store.info('WebSocket: closed');
        store.setState("disconnected");
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
  },

  events :
  {
    // Both events are called from a server.
    "sessionStarted" : "qx.event.type.Data",
    "authCheckResult" : "qx.event.type.Data"
  }
});

