qx.Class.define("memento_web.form.PortSpinner",
{
  extend : qx.ui.form.Spinner,

  construct : function()
  {
    this.base(arguments);
    this.set({
      minimum: 0,
      maximum: 65535
    });
  }
});
