/* ************************************************************************

#asset(qx/icon/Tango/22/actions/list-add.png)
#asset(qx/icon/Tango/22/actions/list-remove.png)
#asset(qx/icon/Tango/22/actions/media-playback-start.png)
#asset(qx/icon/Tango/22/actions/media-playback-pause.png)
#asset(qx/icon/Tango/22/actions/view-refresh.png)

************************************************************************ */

/**
 * The main tool bar widget
 */
qx.Class.define("eqx.ToolBar",
{
  extend : qx.ui.toolbar.ToolBar,


  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  /**
   * @param controller {eqx.Application} The main application class
   */
  construct : function(controller)
  {
    this.base(arguments);
    this.__controller = controller;
  },

  members :
  {
  }
});
