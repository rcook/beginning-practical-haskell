function getFileName(path) {
    return path.split("/").pop();
}

function getRelativeUrl(url) {
    var fileName = getFileName(url);
    return fileName == ""
        ? "."
        : fileName;
}

function selectorEncode(s) {
    return s;
}

$(function() {
    var url = getRelativeUrl(window.location.href);
    $("nav a[href='" + selectorEncode(url) + "']").addClass("disabled").removeAttr("href");
});
