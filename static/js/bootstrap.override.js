function linkSorter(a, b) {
    if ($(a).text() < $(b).text()) return -1;
    if ($(a).text() > $(b).text()) return 1;
    return 0;
}

$(function () {
  $('[data-toggle="popover"]').popover();
});
