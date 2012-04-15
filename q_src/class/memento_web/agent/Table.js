qx.Class.define("memento_web.agent.Table",
{
  extend : memento_web.BasicTable,

  construct : function()
  {
    var n2c =
    {
      "id"             : this.tr("Id"),
      "address"        : this.tr("Address"),
      "port"           : this.tr("Port"),
      "version"        : this.tr("Version")
    };


    this.base(arguments, n2c);

    var tcm = this.getTableColumnModel();
    var n2p = this.getColumnNameToPositionIndex();

    var rb = tcm.getBehavior();

    rb.set(n2p.id,               { width:"1*", minWidth: 70 });
    rb.set(n2p.address,          { width:"1*", minWidth: 100 });
    rb.set(n2p.port,             { width:"1*", minWidth: 50 });
    rb.set(n2p.version,          { width:"1*", minWidth: 50 });

    var tm = this.getTableModel();

    /* Set the special order of sorting in the table for composite types 
       of data. */

    tm.setSortMethods(n2p.address, memento_web.Helpers.buildIPComparator(n2p.address));
  },

  members : {}
});
