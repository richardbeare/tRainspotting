##
.onLoad <- function(libname, pkgname)
{
  RProtoBuf::readProtoFiles(package="tRainspotting")
}
