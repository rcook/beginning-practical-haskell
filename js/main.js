function getFileName(url) {
    return url.split("/").pop();
}

$(function() {
    var fileName = getFileName(window.location.href);
    $("nav a[href='" + fileName + "']").addClass("disabled").removeAttr("href");
});
