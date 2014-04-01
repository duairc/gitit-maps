# `gitit-maps`

`gitit-maps` contains two plugins for gitit, to make it easy to embed Google Street View and Google Maps images into your gitit wiki.

The main plugin is `Network.Gitit.Plugins.Maps`. This replaces XML-like statements in your Pandoc like `<map location="53,-6" zoom="6" />` with a static image fetched with the Google Maps Static API and a link to the actual map. It supports road maps (`<map />`), satellite imagery (`<satellite />`), hybrid images (`<hybrid />`) and street view images (`<streetview />`).

The second plugin is `Network.Gitit.Plugins.Maps.Preprocess`. This automatically generates the XML for you. It replaces strings like `{{map https://maps.google.com/maps?...}}` with the appropriate XML (it makes intelligent guesses based on the URL as to whether you want roadmaps, street view, etc. It also tries to figure out a caption). So if you're browsing Google Maps or Google Street View, and you want to embed what you're seeing into your gitit wiki, just click the "link" logo in Google Maps, copy the link, and then do `{{maps <url>}}`, and the XML will be generated automatically when you save the page. You can then tweak the values in the XML afterwards to your liking.
