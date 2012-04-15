qx.Class.define("memento_web.BasicForm",
{
  extend : qx.ui.form.Form,
  include : qx.locale.MTranslation,


  construct : function()
  {
      this.base(arguments);

      var saveButton = this.__saveButton = new qx.ui.form.Button(this.tr("Save"));
      saveButton.addListener("execute", this.__save, this);
      this.addButton(saveButton);

      var cancelButton = this.__cancelButton = new qx.ui.form.Button(this.tr("Cancel"));
      cancelButton.addListener("execute", this.cancel, this);
      this.addButton(cancelButton);

      this.addListener("dataLoaded", this.__onDataLoaded, this);
      this.addListener("dataUpdated", this.__onDataUpdated, this);
      this.addListener("changeState", this.updateButtonsState, this);
      this.updateButtonsState();
  },

  properties: {
    state : { 
      init : "empty",
      check : function (value) {
        return qx.lang.Array.contains(this.getPossibleStateValues(), value);
      },
      event : "changeState"
    },
    possibleStateValues : {
      init : [ "empty", "loading", "viewing", "editing", "submiting" ]
    }
  },

  members : 
  {
    __model : null,
    __controller : null,
    __saveButton : null,
    __cancelButton : null,
    __hiddenData : null,
    __names : null,
    __oldData : null,


    updateButtonsState : function()
    {
      this.info("New state " + this.getState());
      var enabled = this.getState() == "editing";
      this.__saveButton.setEnabled(enabled);
      this.__cancelButton.setEnabled(enabled);
    },


    add : function(item)
    {
      item.addListener("changeValue", this.__onItemValueChanged, this);

      // call a parent
      return arguments.callee.base.apply(this, arguments);
    },

    __onItemValueChanged : function()
    {
      switch (this.getState())
      {
        case "viewing":
          this.setState("editing");
          this.fireEvent("dataEditingStarted");
          break;
      }
    },

    request : function(id)
    {
      this.info("Request " + id);
      switch (this.getState())
      {
        case "viewing":
        case "empty":
          this.fireDataEvent("dataRequest", {"id": id});
          this.clear(true);
          this.setState("loading");
          break;
      }
    },
    
    /**
     * Load a new version of the data after submiting
     */
    __onDataUpdated : function(e)
    {
      switch (this.getState())
      {
        case "submiting":
          this.setData(e.getData());
          this.setState("viewing");
          break;
      }
    },

    __onDataLoaded : function(e)
    {
      switch (this.getState())
      {
        case "loading":
          this.setData(e.getData());
          this.setState("viewing");
          break;
      }
    },

    __save : function()
    {
      switch (this.getState())
      {
        case "editing":
          if (this.validate()) {
            this.fireDataEvent("dataSubmit", this.getData());
            this.setState("submiting");
          }
          break;
      }
    },

    finalize : function()
    {
      // create the controller with the form
      var controller = this.__controller = new qx.data.controller.Form(null, this);

      // create the model
      var model = this.__model = controller.createModel();
      var clazz = qx.Class.getByName( model.classname );
      this.__names = qx.Class.getProperties(clazz);
    },

    /**
     * If onlyClear = true, then state will not changed
     */
    clear : function(onlyClear)
    {
      var names = this.__names;

      for (var i = 0, count = names.length; i < count; i++)
        this.__model.set(names[i], null);

      this.__hiddenData = null;

      if (!onlyClear)
          this.setState("empty");
    },

    /**
     * Fallback new changes
     */
    cancel : function()
    {
        var oldData = this.__oldData;
        for (var name in oldData)
          this.__model.set(name, oldData[name]);
        this.setState("viewing");
    },

    setData : function(data)
    {
      var names = this.__names;

      var hiddenData = this.__hiddenData = {};
      var oldData = this.__oldData = {};
      var model = this.__model;

      for (var name in data)
      {
        if (names.indexOf(name) != -1)
        {
          model.set(name, data[name]);
          oldData[name] = data[name];
        }
        else
        {
          hiddenData[name] = data[name];
        }
      }
    },

    getData : function()
    {
      var names = this.__names;
      var data = this.__hiddenData;

      for (var i = 0, count = names.length; i < count; i++)
      {
        var name = names[i];
        data[name] = this.__model.get(name);
      }

      return data;
    },

    /**
     * finalize() can be runned only after renderer creation :(
     */
    render : function()
    {
      var r = new memento_web.form.renderer.Wide(this);
      this.finalize();
      return r;
    }
  },

  events :
  {
    "dataRequest" : "qx.event.type.Data",
    "dataLoaded" : "qx.event.type.Data",
    "dataSubmit" : "qx.event.type.Data",
    "dataUpdated" : "qx.event.type.Data",
    // User start to edit the data
    "dataEditingStarted" : "qx.event.type.Event"
  }
});
