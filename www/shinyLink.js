// init binding
var shinyLink = new Shiny.InputBinding();

// define methods
$.extend(shinyLink, {
    find: function(scope) {
        return $(scope).find("a.shiny__link");
    },
    initialize: function(el) {
        $(el).on("click", function(e) {
            e.preventDefault();

            // extract destination and find matching link
            var to = $(el).attr("href");
            var target = $(`a[data-value="${to}"]`);

            // does the link exist?
            if (target.length) {
                target.click();
                window.scrollTo(0, 0);
            } else {
                console.error("No matching link found. Is the destination correct?");
            }
        });
    }
});

// register
Shiny.inputBindings.register(shinyLink);
