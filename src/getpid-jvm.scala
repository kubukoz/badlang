//> using target.platform "jvm"

def getpid(
): Long = ProcessHandle.current().pid()
