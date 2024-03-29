
/**
 * The Application's header
 */
qx.Class.define("memento_web.Header",
{
  extend : qx.ui.container.Composite,




  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  construct : function()
  {
    this.base(arguments);

    this.setLayout(new qx.ui.layout.HBox);
    this.setAppearance("app-header");

    var title = new qx.ui.basic.Label("Memento Web");
    var version = new qx.ui.basic.Label("0.1");
    version.setFont("default");

    var searchTextField = new qx.ui.form.TextField();

    this.add(title);
    this.add(new qx.ui.core.Spacer, { flex : 1 });
    this.add(version);
  }
});
