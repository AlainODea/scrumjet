dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.layout.ContentPane");

dojo.addOnLoad(function() {
    var headline = dojo.byId("headline").innerHTML;
    var description = dojo.byId("description").innerHTML;
    document.body.innerHTML='<div id="uiContainer" style="border: 1px solid #ccc"></div>';
    document.body.setAttribute("class", "tundra");
    document.body.setAttribute("style", "");
    var tabContainer = new dijit.layout.TabContainer({
        "design": "sidebar",
        "style": "height: 400px;"
    },
    "uiContainer");

    var taskTab = new dijit.layout.ContentPane({
        "title": headline,
        "content": description
    });
    tabContainer.addChild(taskTab);

    var tasksTab = new dijit.layout.ContentPane({
        "title": "Tasks...",
        "href":"."
    });
    tabContainer.addChild(tasksTab);

    tabContainer.startup();
});