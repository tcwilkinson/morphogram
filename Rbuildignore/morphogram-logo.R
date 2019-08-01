# Creates a png logo for morphogram
# Based on Edzer Pebesma's cool sf logo script
# https://gist.github.com/edzer/f461a3a95570c4ab7edf3125c2f19d20

require("sf")
require("dplyr")
require("hexbin")
require("morphogram")

hexx_multiplier = 1.55   # how much the x coords of the hex should be multiplied for hexagon evenness
hexx_inner_m = 0.94      # how much smaller is the inner hexagon
icon_m = 1.2             # how big the morphogram is in the centre
max_features = 6^2       # how many features should be displayed in morphogram

nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
nc <- nc[1]
#nc <- nc[order(nc$AREA),]

focus_sf <- st_normalize(distribute(converge(nc),max.features=max_features)) # Focus sf
focus_geom <- st_geometry(focus_sf) - c(0.5,0.5)
st_geometry(focus_sf) <- focus_geom

focus_bbox <- st_bbox(focus_sf)
coords_hex <- hexcoords(dx = focus_bbox[["xmax"]] - focus_bbox[["xmin"]],
                        dy = focus_bbox[["ymax"]] - focus_bbox[["ymin"]])
corners <- tibble(x = coords_hex$x * hexx_multiplier, y = coords_hex$y)
corners <- rbind(corners, corners[1,])
hexagon <- st_polygon(list(as.matrix(corners), as.matrix(corners) * hexx_inner_m))
filled_hexagon = st_polygon(list(as.matrix(corners) * hexx_inner_m))

laea = st_crs("+proj=laea +lat_0=30 +lon_0=30") # Lambert equal area
# focus_sf = focus_sf * 1e4
st_crs(focus_sf) = laea
# corners = st_sfc(st_polygon(list(as.matrix(corners))) * 1e4, crs = laea)
# hexagon = st_sfc(hexagon * 1e4, crs = laea)
hexagon = st_sfc(hexagon, crs=st_crs(focus_sf))
filled_hexagon = st_sfc(filled_hexagon, crs=st_crs(focus_sf))
focus_sf = st_geometry(focus_sf) * c(2*icon_m,2*icon_m)

#focus_sf <- st_transform(focus_sf,laea)
#hexagon <- st_transform(hexagon * 1e4,laea)

png("morphogram-logo.png",width=400,height=400)
opar = par(mar = rep(0,4), oma=rep(0,4),bg=NA)

plot(filled_hexagon, col = c("white"))
plot(hexagon, col = c("grey","white"), rule = "evenodd")
#plot(st_buffer(focus_sf, dist = 10 * 1e4), col = sf.colors()[1], add = TRUE)
plot(focus_sf, add = TRUE, col = sf.colors(), border = "gray", lwd = 1.5)
dev.off()
