dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.layout.BorderContainer");
dojo.require("dojox.data.JsonRestStore");
dojo.require("dojox.grid.DataGrid");

dojo.addOnLoad(function() {
    var headline = dojo.byId("headline").innerHTML;
    var description = dojo.byId("description").innerHTML;
    document.body.innerHTML='<div id="uiContainer" style="border: 1px solid #ccc"></div>';
    document.body.setAttribute("class", "tundra");
    document.body.setAttribute("style", "");
    var tabContainer = new dijit.layout.TabContainer({
        design: "sidebar",
        style: "height: 400px;"
    },
    "uiContainer");

    var taskTab = new dijit.layout.ContentPane({
        title: headline,
        content: description,
        closable: true
    });
    tabContainer.addChild(taskTab);

    var tasksTab = new dijit.layout.BorderContainer({
        title: "Tasks..."
    });

    var tasksStructure = [{
        field: "id",
        name: "Task ID",
        width: "70px"
    },{
        field: "headline",
        name: "Headline",
        width: "auto"
    }];

    var tasksGrid = new dojox.grid.DataGrid({
        store: new dojox.data.JsonRestStore({target:"."}),
        structure: tasksStructure
    });

    dojo.connect(tasksGrid, "onRowClick", function() {
        var selectedTask = tasksGrid.selection.getSelected()[0];
        window.location = selectedTask.id;
    })

    tasksTab.addChild(tasksGrid);

    tabContainer.addChild(tasksTab);
    tabContainer.startup();
});