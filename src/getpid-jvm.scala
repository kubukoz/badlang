//> using target.platform "jvm"
package badlang

def getpid(
): Long = ProcessHandle.current().pid()
