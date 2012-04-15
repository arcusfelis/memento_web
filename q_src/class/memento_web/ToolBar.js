/* ************************************************************************

#asset(qx/icon/Tango/22/actions/list-add.png)
#asset(qx/icon/Tango/22/actions/list-remove.png)
#asset(qx/icon/Tango/22/places/folder.png)
#asset(qx/icon/Tango/22/actions/document-properties.png)

************************************************************************ */

/**
 * The main tool bar widget
 */
qx.Class.define("memento_web.ToolBar",
{
  extend : qx.ui.toolbar.ToolBar,


  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  /**
   * @param controller {memento_web.Application} The main application class
   */
  construct : function(controller)
  {
    this.base(arguments);


    // INIT TOOLS
    this.__spacer = this.addSpacer();
  //this.__editBtn = new qx.ui.toolbar.Button(this.tr("Edit"),
  //  "icon/22/actions/edit.png");
  //this.__editBtn.setCommand(controller.getCommand("edit"));

    this.__navBtn = new qx.ui.toolbar.CheckBox(this.tr("Navigation"),
      "icon/22/places/folder.png");
    var navCmd = controller.getCommand("navigation");
    this.__navBtn.setCommand(navCmd);
    this.__navBtn.setValue(true);
    this.__patchCommand(this.__navBtn, navCmd);

    this.__editBtn = new qx.ui.toolbar.CheckBox(this.tr("Edit"),
      "icon/22/actions/document-properties.png");
    var editCmd = controller.getCommand("edit");
    this.__editBtn.setCommand(editCmd);
    this.__editBtn.setValue(false);
    this.__patchCommand(this.__editBtn, editCmd);

    this.__states = {
        "table" : {"in": this.__tableIn, "out": this.__tableOut}
    };
    this.changeState("table");
    this.addBefore(this.__navBtn, this.__spacer);
  },

  members :
  {
    __currentState : null,
    __patchCommand : function(checkbox, cmd)
    {
        cmd.isActive = function() { return checkbox.getValue(); }
    },

    changeState: function(newState)
    {
        if (this.__currentState != null)
        {
            this.__states[this.__currentState].out.call(this);
        }
        this.__currentState = newState;
        this.__states[this.__currentState]["in"].call(this);
    },

    __tableIn: function()
    {
        this.addAfter(this.__editBtn, this.__spacer);
    },

    __tableOut: function()
    {
        this.tryRemove(this.__editBtn);
    },

    /**
     * Remove the bottom from the panel
     */
    tryRemove : function(child)
    {
      if (this.indexOf(child) == -1)
        return;

      this.remove(child);
    }
  }
});
