$(document).keyup(function(event) {
    if ($("#search").is(":focus") && (event.key == "Enter")) {
        $("#searchButton").click();
        $("#searchButton").focus();
    }
});