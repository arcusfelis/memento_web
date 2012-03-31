qx.Class.define("eqx.Remote",
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
      this.__bullet.sendText(qx.lang.Json.stringify(query));
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
        store.fireDataEvent(parsedData.event, parsedData.data);
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
  }
});

