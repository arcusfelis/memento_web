
/**
 * The GUI definition of the qooxdoo unit test runner.
 */
qx.Class.define("eqx.Container",
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
    this.__header = new eqx.Header();
    this.add(this.__header, { edge : "north" });

    // Create toolbar
    this.__toolBar = new eqx.ToolBar(this);
    this.add(this.__toolBar, { edge : "north" });

    // Create side panel
//  this.__stack = new qx.ui.container.Stack;
//  this.__stack.resetSelection();
//  this.__stack.exclude();

    this.__mainStack = new qx.ui.container.Stack;

    this.__infosplit = new qx.ui.splitpane.Pane("horizontal");
    this.__infosplit.setDecorator(null);
    this.add(this.__infosplit);

    this.__infosplit.add(this.__mainStack, 2);
//  this.__infosplit.add(this.__stack, 1);


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
    }
  }
});
