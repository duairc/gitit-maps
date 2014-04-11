function Map (id) {
    this.selection = $(document.getElementById(id));
    this.points = this.selection.data("points");
    this.leaflet = L.map(id);
    this.getCategoriesFromForm();
    this.updateMarkers();
    this.leafletSetup();
}

Map.prototype.getCategoriesFromForm = function() {
    var map = this;
    map.categories = {}
    map.selection.prevAll("form").find("li").each(function (i, item) {
        var input = $(item).find("input");
        var span = $(item).find("span");
        var img = $(item).find("img");
        var id = input.attr('id');
        var name = span.text();
        var icon = img.attr('src');
        var show = input.prop('checked');
        input.change(function() {
            map.categories[id].show = $(this).prop('checked');
            map.updateMarkers();
        });
        map.categories[id] = {
            name: name,
            icon: icon,
            show: show
        }
    });
    return map.categories;
}

Map.prototype.updateMarkers = function() {
    for (var i = 0; i < this.points.length; i++) {
        var point = this.points[i];
        if (point.marker) {
            this.leaflet.removeLayer(point.marker);
            delete point.marker;
        }

        var showing = {}
        for (var j = 0; j < point.categories.length; j++) {
            if (this.categories[point.categories[j]].show) {
                showing[point.categories[j]] =
                    this.categories[point.categories[j]];
            }
        }
        if ($.isEmptyObject(showing)) {
            continue;
        }

        var location = [point.location.lat, point.location.lon];
        var width = 0;
        var html = "";
        for (var category in showing) {
            var icon = showing[category].icon;
            var name = showing[category].name;
            width += 20;
            html += "<img src=\"" + encodeURI(icon) +
                "\" alt=\"" + $('<div/>').text(name).html() +
                "\" title=\"" + $('<div/>').text(name).html() +
                "\" />";
        }
        var icon = L.divIcon({
            className: "marker",
            iconSize: [width, 20],
            iconAnchor: [0, 20],
            popupAnchor:  [(width / 2) + 4, -20],
            html: html
        });

        point.marker = L.marker(location, {icon: icon});

        var escaped = $('<div/>').text(point.title).html()
        var html = "<a href=\"" + point.url + "\">" + escaped + "</a>"

        point.marker.addTo(this.leaflet).bindPopup(html);
    }
}

Map.prototype.leafletSetup = function () {
    L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        maxZoom: 18
    }).addTo(this.leaflet);
    var northEast = {
        lat: NaN,
        lon: NaN
    }
    var southWest = {
        lat: NaN,
        lon: NaN
    }
    for (var i = 0; i < this.points.length; i++) {
        if (!(this.points[i].location.lat >= northEast.lat)) {
            northEast.lat = this.points[i].location.lat;
        }
        if (!(this.points[i].location.lon <= northEast.lon)) {
            northEast.lon = this.points[i].location.lon;
        }
        if (!(this.points[i].location.lat <= southWest.lat)) {
            southWest.lat = this.points[i].location.lat;
        }
        if (!(this.points[i].location.lon >= southWest.lon)) {
            southWest.lon = this.points[i].location.lon;
        }
    }
    var bounds = L.latLngBounds(
        [southWest.lat, southWest.lon],
        [northEast.lat, northEast.lon]
    );
    this.leaflet.fitBounds(bounds);
    this.leaflet.attributionControl.setPosition(-10000, -10000);
}

var map = new Map("map");
