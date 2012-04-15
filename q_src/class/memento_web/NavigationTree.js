/**
 * The Application's header
 */
qx.Class.define("memento_web.NavigationTree",
{
  extend : qx.ui.tree.Tree,




  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  construct : function()
  {
    this.base(arguments, this.tr("Navigation"));

    this.setDecorator(null);

    this.__root = new qx.ui.tree.TreeFolder(this.tr("Root"));
    this.__root.setOpen(true);
    this.setRoot(this.__root);
    this.setSelection([this.__root]);

    this.__items = {
        "manager": new qx.ui.tree.TreeFile(this.tr("Managers")),
        "agent"  : new qx.ui.tree.TreeFile(this.tr("Agents")),
        "user"   : new qx.ui.tree.TreeFile(this.tr("Users")),
        "object" : new qx.ui.tree.TreeFile(this.tr("Objects")),
        "trap"   : new qx.ui.tree.TreeFile(this.tr("Traps"))
    };

    this.__hashCode2Id = {};

    for (var id in this.__items)
    {
        var item = this.__items[id];
        this.__root.add(item);
        this.__hashCode2Id[item.toHashCode()] = id;
    }
  },

  members: 
  {
    __hashCode2Id : null,

    getIdByItem : function(item)
    {
        return this.__hashCode2Id[item.toHashCode()];
    }
  }
});
