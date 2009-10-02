dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.layout.ContentPane");

dojo.addOnLoad(function() {
    var content = dojo.byId("tasks").innerHTML;
    document.body.innerHTML='<div id="uiContainer" style="border: 1px solid #ccc"></div>';
    document.body.setAttribute("class", "tundra");
    document.body.setAttribute("style", "");
    var tabContainer = new dijit.layout.TabContainer({
        "design": "sidebar",
        "style": "height: 400px;"
    },
    "uiContainer");

    var tasksTab = new dijit.layout.ContentPane({
        "title": "Tasks...",
        "content": content
    });
    tabContainer.addChild(tasksTab);

    tabContainer.startup();
});