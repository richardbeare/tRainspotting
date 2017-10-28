##
.onLoad <- function(libname, pkgname)
{
  f <- system.file("proto", package="tRainspotting")
  RProtoBuf::readProtoFiles(dir=f)
  options(tRainspotting=list())
}

