
/**
 * The GUI definition of the qooxdoo unit test runner.
 */
qx.Class.define("memento_web.Container",
{
  extend : qx.ui.container.Composite,




  /*
  *****************************************************************************
     CONSTRUCTOR
  *****************************************************************************
  */

  construct : function(app)
  {
    // Create main layout
    this.__mainLayout = new qx.ui.layout.Dock();
    this.base(arguments, this.__mainLayout);
    this.__application = app;
    this._initializeCommands();

    // Create header
    this.__header = new memento_web.Header();
    this.add(this.__header, { edge : "north" });

    // Create toolbar
    this.__toolBar = new memento_web.ToolBar(this);
    this.add(this.__toolBar, { edge : "north" });

    // Create side panel
//  this.__stack = new qx.ui.container.Stack;
//  this.__stack.resetSelection();
//  this.__stack.exclude();

    this.__mainStack = new qx.ui.container.Stack;

    this.__infosplit = new qx.ui.splitpane.Pane("horizontal");
    this.__infosplit.setDecorator(null);
    this.add(this.__infosplit);

    this.__navigationTree = new memento_web.NavigationTree();
    this.__navigationTree.addListener("changeSelection", this._navigateTo, this);
    this.__infosplit.add(this.__navigationTree, 1);
    this.__infosplit.add(this.__mainStack, 4);
//  this.__infosplit.add(this.__stack, 1);

    this.__tableStack = new qx.ui.container.Stack;
    this.__mainStack.add(this.__tableStack);
    this.__mainStack.setSelection([this.__tableStack]);
    this.__tableStack.show();

    // Set "dead" zones (never will get focus)
    this.__header.setKeepFocus(true);
    this.__toolBar.setKeepFocus(true);
  },

  members :
  {
    __mainLayout : null,
    __header : null,
    __toolBar : null,
    __store : null,
    __table : null,
    __commands : null,
    __application : null,

    getRoot : function()
    {
      return this.__application.getRoot();
    },

    finalize : function()
    {
      this.__store = document.remote = new memento_web.Remote();
      this.__store.finalize();
    },

    /**
     * Get the command with the given command id
     *
     * @param commandId {String} the command's command id
     * @return {qx.ui.core.Command} The command
     */
    getCommand : function(commandId) {
      return this.__commands[commandId];
    },


    /**
     * Initialize commands (shortcuts, ...)
     *
     */
    _initializeCommands : function()
    {
      var commands = {};

      this.__commands = commands;
    },

    _navigateTo: function(/*qx.event.type.Data*/ e)
    {
      var selected = e.getData();
      var file = selected[0];
      var id = this.__navigationTree.getIdByItem(file);
      this.activateTable(id);
    },

    __tables: {},
    __activeTable: false,
    /**
     * Return true on success
     */
    activateTable: function(id)
    {
      if (!this.__tables[id] && !this.tryInitTable(id))
        return false;

      var table = this.__activeTable = this.__tables[id];
      this.__tableStack.setSelection([ table ]);
      this.__tableStack.show();
      table.focus();

      return true;
    },

    tryInitTable: function(id)
    {
      switch(id)
      {
        case "agent":
          var table = this.__tables[id] = new memento_web.agent.Table();
          this.__store.registerObject(table);
          this.__tableStack.add(table);
          return true;
            
        default:
          return false;
      }
    }
  }
});
