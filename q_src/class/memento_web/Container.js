
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

    this.__mainStack = new qx.ui.container.Stack;
    this.__editPaneStack = new qx.ui.container.Stack;

    // Split beetween the right sidebar and the center pane
    this.__rsplit = new qx.ui.splitpane.Pane("horizontal");
    this.add(this.__rsplit);

    // Split beetween the left sidebar and the center pane
    this.__lsplit = new qx.ui.splitpane.Pane("horizontal");
    this.__lsplit.setDecorator(null);

    this.__navigationTree = new memento_web.NavigationTree();
    this.__navigationTree.addListener("changeSelection", this._navigateTo, this);

    this.__lsplit.add(this.__navigationTree, 1);
    this.__lsplit.add(this.__mainStack, 4);
    this.__rsplit.add(this.__lsplit, 5);
    this.__rsplit.add(this.__editPaneStack, 1);

    if (!this.getCommand("navigation").isActive())
        this.__navigationTree.exclude();

    if (!this.getCommand("edit").isActive())
        this.__editPaneStack.exclude();


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

      commands.edit = new qx.ui.core.Command("Control+E");
      commands.edit.setToolTipText("Control+E");
      commands.edit.addListener("execute", this.editPane, this);

      commands.navigation = new qx.ui.core.Command("Control+N");
      commands.navigation.setToolTipText("Control+N");
      commands.navigation.addListener("execute", this.navigationTree, this);

      this.__commands = commands;
    },

    /**
     * Change visability.
     * It is called before changing the value of the button.
     */
    navigationTree: function()
    {
        if (!this.getCommand("navigation").isActive())
            this.__navigationTree.show();
        else
            this.__navigationTree.exclude();
    },

    editPane: function()
    {
        if (!this.getCommand("edit").isActive())
            this.__editPaneStack.show();
        else
            this.__editPaneStack.exclude();
        this.__editPaneStack.scheduleLayoutUpdate();
    },

    _navigateTo: function(/*qx.event.type.Data*/ e)
    {
      var selected = e.getData();
      var file = selected[0];
      var id = this.__navigationTree.getIdByItem(file);
      this.activateTable(id);
    },

    __tables: {},
    __forms: {},
    __activeTable: false,
    /**
     * Return true on success
     */
    activateTable: function(id)
    {
      if (!this.__tables[id] && !this.tryInitTable(id))
        return false;

      var table = this.__activeTable = this.__tables[id];
      var form = this.__forms[id];
      this.__tableStack.setSelection([ table ]);
      this.__tableStack.show();

      this.__editPaneStack.setSelection([ form ]);
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


          var form = new memento_web.agent.Form();
          this.__store.registerObject(form);

          var pane = this.__forms[id] = new qx.ui.container.Composite(new qx.ui.layout.VBox());
          this.__editPaneStack.add(pane);

          pane.set({
            padding: 10
          });
          pane.add(form.render());

          var msm = table.getSelectionModel();
          msm.addListener("changeSelection", function() {
            var selected = table.getSelectedIds();
            if (selected.length > 0)
            {
              form.request(selected[0]);
            } else 
            {
              form.clear();
            }
          });
          
          form.addListener("changeState", this.__onFormChangeStateForPane, pane);
          form.addListener("changeState", this.__onFormChangeStateForTable, table);
          return true;
            
        default:
          return false;
      }
    },

    __onFormChangeStateForPane: function(e)
    {
      var state = e.getData();
      switch(state)
      {
        case "loading":
        case "submiting":
            this.setEnabled(false);
            break;
        default:
            this.setEnabled(true);
      }
    },

    __onFormChangeStateForTable: function(e)
    {
      var state = e.getData();
      switch(state)
      {
        case "viewing":
        case "empty":
            this.setEnabled(true);
            break;
        default:
            this.setEnabled(false);
      }
    }
  }
});
