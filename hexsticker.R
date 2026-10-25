# Hex sticker: a checkpoint barrier raised over the road, and a "Verify you are human"
# box going through underneath, ticked. The package gets past the check the way a browser
# does, and where a person has to click, a person clicks.
# Run from the package root: Rscript hexsticker.R, writes man/figures/logo.png
suppressPackageStartupMessages({library(sf); library(ggplot2)})

night   <- "#1F3A4D"   # sky
road    <- "#2B4B60"
cream   <- "#F6EBD2"
amber   <- "#F2B544"
border  <- "#15293A"   # name band
gold    <- "#C9962E"   # frame: reads on light and dark pages alike
paper   <- "#FBF5E8"
ink     <- "#2A2F36"
grey    <- "#8A8F96"
green   <- "#2E9E5B"
font    <- "Menlo"

hex <- (function(){a <- (90+60*(0:5))*pi/180; m <- cbind(cos(a), sin(a)); st_polygon(list(rbind(m, m[1,])))})()
hex_in <- st_buffer(hex, -0.035)
rect <- function(x0,x1,y0,y1)st_polygon(list(cbind(c(x0,x1,x1,x0,x0),c(y0,y0,y1,y1,y0))))
rrect <- function(x0,x1,y0,y1,r) st_buffer(rect(x0+r,x1-r,y0+r,y1-r), r, nQuadSegs=12)
rot <- function(a){a <- a*pi/180; matrix(c(cos(a),-sin(a),sin(a),cos(a)),2)}
about <- function(g, angle, p) (g - p)*rot(angle) + p   # rotate geometry about point p

# ground
horizon <- -0.22
ground <- st_intersection(hex_in, rect(-1,1,-1,horizon))
mark <- st_intersection(hex_in, st_linestring(cbind(c(-1,1), c(horizon,horizon))))

# barrier: post with amber cap, arm pivoted at the top of the post and raised
px <- -0.60; pivot <- c(px, horizon+0.30)
post <- rrect(px-0.055, px+0.055, horizon-0.08, pivot[2]+0.02, 0.015)
cap  <- rrect(px-0.075, px+0.075, pivot[2]+0.02, pivot[2]+0.07, 0.012)
arm_len <- 1.0; arm_w <- 0.075; angle <- 36
arm  <- rrect(px-0.16, px+arm_len, pivot[2]-arm_w/2, pivot[2]+arm_w/2, arm_w/2)
stripes <- st_sfc(lapply(seq(px+0.02, px+arm_len-0.12, by=0.16), function(x) rect(x, x+0.08, pivot[2]-arm_w/2, pivot[2]+arm_w/2)))
stripes <- st_intersection(stripes, arm)
weight <- rrect(px-0.21, px-0.09, pivot[2]-0.055, pivot[2]+0.055, 0.02)
arm <- about(arm, angle, pivot); stripes <- about(stripes, angle, pivot); weight <- about(weight, angle, pivot)
hinge <- st_buffer(st_point(pivot), 0.022)

# the challenge box, passing under the raised arm
bx <- c(-0.36, 0.76); by <- c(horizon+0.03, horizon+0.31)
card <- rrect(bx[1], bx[2], by[1], by[2], 0.035)
ck <- c(bx[1]+0.065, by[1]+0.06); cs <- 0.16
checkbox <- rrect(ck[1], ck[1]+cs, ck[2], ck[2]+cs, 0.025)
tick <- st_linestring(cbind(ck[1]+c(0.035,0.068,0.125), ck[2]+c(0.08,0.045,0.115)))

top <- -0.44   # name band
band <- st_intersection(hex_in, rect(-1,1,-1,top))
band_line <- st_intersection(hex_in, st_linestring(cbind(c(-1,1),c(top,top))))

p <- ggplot() +
  geom_sf(data=st_sfc(hex_in), fill=night, colour=NA) +
  geom_sf(data=st_sfc(ground), fill=road, colour=NA) +
  geom_sf(data=st_sfc(mark), colour=cream, linewidth=0.35, alpha=0.6) +
  geom_sf(data=st_sfc(post), fill=cream, colour=NA) +
  geom_sf(data=st_sfc(cap), fill=amber, colour=NA) +
  geom_sf(data=st_sfc(arm), fill=cream, colour=NA) +
  geom_sf(data=stripes, fill=amber, colour=NA) +
  geom_sf(data=st_sfc(weight), fill=cream, colour=NA) +
  geom_sf(data=st_sfc(hinge), fill=night, colour=NA) +
  geom_sf(data=st_sfc(card), fill=paper, colour=NA) +
  geom_sf(data=st_sfc(checkbox), fill="white", colour=grey, linewidth=0.3) +
  geom_sf(data=st_sfc(tick), colour=green, linewidth=0.9, lineend="round", linejoin="round") +
  annotate("text", x=ck[1]+cs+0.06, y=ck[2]+cs/2, label="Verify you are human", family="Helvetica",
           size=1.95, colour=ink, hjust=0, vjust=0.5) +
  geom_sf(data=st_sfc(band), fill=border, colour=NA) +
  geom_sf(data=st_sfc(band_line), colour=cream, linewidth=0.6) +
  annotate("text", x=0, y=top-0.17, label="safeconduct", family=font, fontface="bold", size=3.5, colour=cream) +
  geom_sf(data=st_sfc(st_difference(hex, st_buffer(hex, -0.055))), fill=gold, colour=NA) +   # frame inside the hex, so no corner is clipped
  coord_sf(xlim=c(-sqrt(3)/2, sqrt(3)/2), ylim=c(-1,1), expand=FALSE, datum=NA) +
  theme_void() + theme(plot.margin=margin(0,0,0,0), plot.background=element_blank())

dir.create("man/figures", showWarnings=FALSE, recursive=TRUE)
ggsave("man/figures/logo.png", p, width=43.9, height=50.8, units="mm", dpi=400, device=ragg::agg_png, bg="transparent")

# favicons for pkgdown, from the same plot: the files its head template links to
dir.create("pkgdown/favicon", showWarnings=FALSE, recursive=TRUE)
ggsave("pkgdown/favicon/favicon.svg", p, width=43.9, height=50.8, units="mm", device=svglite::svglite, bg="transparent")
icon <- magick::image_read("man/figures/logo.png")
square <- function(px) magick::image_resize(magick::image_extent(icon, "800x800", gravity="center", color="none"), paste0(px, "x", px))
magick::image_write(square(96), "pkgdown/favicon/favicon-96x96.png")
magick::image_write(square(180), "pkgdown/favicon/apple-touch-icon.png")
magick::image_write(c(square(16), square(32), square(48)), "pkgdown/favicon/favicon.ico", format="ico")
