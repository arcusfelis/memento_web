qx.Class.define("memento_web.agent.Form",
{
  extend : memento_web.BasicForm,

  construct : function()
  {
    this.base(arguments);

    this.add(new qx.ui.form.TextField(), this.tr("Address"), null, "address");
    this.add(new memento_web.form.PortSpinner(), this.tr("Port"), null, "port");
    this.add(new qx.ui.form.TextField(), this.tr("Version"), null, "version");
  },

  members : {}
});
