//> using target.platform "native"
import scala.scalanative.unsafe.extern

@extern
def getpid(
): Long = extern
