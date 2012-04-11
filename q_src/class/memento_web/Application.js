/* ************************************************************************

   Copyright:

   License:

   Authors:

************************************************************************ */

/* ************************************************************************

#asset(memento_web/*)

************************************************************************ */

/**
 * This is the main application class of your custom application "memento_web"
 */
qx.Class.define("memento_web.Application",
{
  extend : qx.application.Standalone,




  /*
  *****************************************************************************
     MEMBERS
  *****************************************************************************
  */

  members :
  {
    /**
     * This method contains the initial application code and gets called 
     * during startup of the application
     *
     * @lint ignoreDeprecated(alert)
     */
    main : function()
    {
      // Call super class
      this.base(arguments);

      // Enable logging in debug variant
//    if (qx.core.Environment.get("qx.debug"))
//    {
        // support native logging capabilities, e.g. Firebug for Firefox
        qx.log.appender.Native;

        // support additional cross-browser console. 
        // Press F7 to toggle visibility
//      qx.log.appender.Console;
//    }


      /*
      -------------------------------------------------------------------------
        Below is your actual application code...
      -------------------------------------------------------------------------
      */

      // Initialize the compositor
      this.__container = new memento_web.Container(this);
      this.getRoot().add(this.__container, { edge : 0 });
    },

    finalize: function()
    {
      this.__container.finalize();
    }
  }
});
