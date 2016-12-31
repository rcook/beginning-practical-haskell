function getFileName(url) {
    return url.split("/").pop();
}

function getLinkHRef(href) {
    var fileName = getFileName(href);
    return fileName == ""
        ? "."
        : fileName;
}

$(function() {
    var href = getLinkHRef(window.location.href);
    $("nav a[href='" + href + "']").addClass("disabled").removeAttr("href");
});
