dojo.require("dijit.layout.TabContainer");
dojo.require("dojox.data.ClientFilter");
dojo.require("dojox.data.JsonRestStore");
dojo.require("dojox.grid.DataGrid");

dojo.addOnLoad(function() {
    document.body.innerHTML='<div id="uiContainer" style="border: 1px solid #ccc"></div>';
    document.body.setAttribute("class", "tundra");
    document.body.setAttribute("style", "");
    var tabContainer = new dijit.layout.TabContainer({
        style: "height: 400px;"
    },
    "uiContainer");

    var tasksStructure = [{
        field: "id",
        name: "Task ID",
        width: "70px"
    },{
        field: "headline",
        name: "Headline",
        width: "auto"
    }];

    var tasksTab = new dojox.grid.DataGrid({
        title: "Tasks...",
        store: new dojox.data.JsonRestStore({target:"."}),
        structure: tasksStructure,
        clientSort: true,
        queryOptions:{cache:true}
    });

    dojo.connect(tasksTab, "onRowClick", function() {
        var selectedTask = tasksTab.selection.getSelected()[0];
        window.location = selectedTask.id;
    });

    dojo.connect(tasksTab, "onRowDblClick", function() {
        var selectedTask = tasksTab.selection.getSelected()[0];
        window.open(window.location + selectedTask.id);
    });

    tabContainer.addChild(tasksTab);

    tabContainer.startup();
});