//> using target.platform "native"
package badlang

import scala.scalanative.unsafe.extern

@extern
def getpid(
): Long = extern
