# tRainspotting

## Live positions of public transport vehicles

Inspired by the last few hours of the RUnconference2017, Melbourne, Australia.

During our search for realtime data we came across live feeds of vehicle location via the gtfs-r 
protocol. The tools for retrieving and formatting this data are available in `httr` and `RProtoBuf`,
so there isn't a really good reason to create a specialised package, but it seems a shame to
waste such a good package name. There is even a similar piece of work at [gtfsway](https://github.com/SymbolixAU/gtfsway), without the nice name, of course.

This package currently focuses on the data available for NSW, Australia. Access requires an API
key from [opendata NSW](https://opendata.transport.nsw.gov.au/)

The aim is to experiment with updating leaflet maps with live position data.

A simple demo using leaflet and shiny to display ferry positions, names and tracks is included:



